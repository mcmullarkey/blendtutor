# Session-level cache for lesson discovery
.blendtutor_discovery_cache <- new.env(parent = emptyenv())

#' Find installed packages that contain blendtutor lessons
#'
#' Scans installed packages for those that list blendtutor in their
#' Depends, Imports, or Suggests fields and contain lesson YAML files
#' in `inst/lessons/`.
#'
#' @return Character vector of package names
#' @keywords internal
find_lesson_packages <- function() {
  pkgs <- installed.packages()

  # Always include blendtutor itself
  blendtutor_deps <- "blendtutor"

  # Find packages that reference blendtutor in Depends/Imports/Suggests
  dep_fields <- c("Depends", "Imports", "Suggests")
  for (field in dep_fields) {
    if (!(field %in% colnames(pkgs))) {
      next
    }
    has_blendtutor <- grep("\\bblendtutor\\b", pkgs[, field])
    blendtutor_deps <- union(blendtutor_deps, pkgs[has_blendtutor, "Package"])
  }

  # Filter to packages that actually have lesson YAML files
  packages_with_lessons <- character(0)
  for (pkg in blendtutor_deps) {
    lessons_dir <- system.file("lessons", package = pkg)
    if (lessons_dir == "" || !dir.exists(lessons_dir)) {
      next
    }
    yaml_files <- list.files(lessons_dir, pattern = "\\.yaml$")
    if (length(yaml_files) > 0) {
      packages_with_lessons <- c(packages_with_lessons, pkg)
    }
  }

  packages_with_lessons
}

#' Build an index of all discoverable lessons
#'
#' Returns a cached data frame of all lessons across all packages that
#' depend on blendtutor. Each row contains the lesson ID (file stem),
#' source package name, and full file path.
#'
#' @return A `data.frame` with columns `lesson_id`, `package`, `path`
#' @keywords internal
build_lesson_index <- function() {
  if (!is.null(.blendtutor_discovery_cache$lesson_index)) {
    return(.blendtutor_discovery_cache$lesson_index)
  }

  packages <- find_lesson_packages()

  rows <- list()
  for (pkg in packages) {
    lessons_dir <- system.file("lessons", package = pkg)
    yaml_files <- list.files(
      lessons_dir,
      pattern = "\\.yaml$",
      full.names = TRUE
    )
    if (length(yaml_files) == 0) {
      next
    }

    lesson_ids <- sub("\\.yaml$", "", basename(yaml_files))
    rows[[length(rows) + 1]] <- data.frame(
      lesson_id = lesson_ids,
      package = pkg,
      path = yaml_files,
      stringsAsFactors = FALSE
    )
  }

  if (length(rows) == 0) {
    index <- data.frame(
      lesson_id = character(0),
      package = character(0),
      path = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    index <- do.call(rbind, rows)
    rownames(index) <- NULL
  }

  .blendtutor_discovery_cache$lesson_index <- index
  index
}

#' Resolve a file path lesson reference
#'
#' Handles references containing "/" or ending in ".yaml".
#'
#' @param lesson_ref Character string file path
#' @return A list with `path`, `package` (NULL), and `lesson_id`
#' @keywords internal
resolve_file_path <- function(lesson_ref) {
  if (!file.exists(lesson_ref)) {
    cli_abort("Lesson file not found: {.file {lesson_ref}}")
  }
  list(
    path = normalizePath(lesson_ref),
    package = NULL,
    lesson_id = sub("\\.yaml$", "", basename(lesson_ref))
  )
}

#' Resolve a qualified (pkg::name) lesson reference
#'
#' @param lesson_ref Character string in "pkg::name" format
#' @return A list with `path`, `package`, and `lesson_id`
#' @keywords internal
resolve_qualified_name <- function(lesson_ref) {
  parts <- strsplit(lesson_ref, "::", fixed = TRUE)[[1]]
  if (length(parts) != 2 || nchar(parts[1]) == 0 || nchar(parts[2]) == 0) {
    cli_abort(c(
      "Invalid lesson reference: {.val {lesson_ref}}",
      "i" = "Use {.code package::lesson_name} format."
    ))
  }
  pkg <- parts[1]
  name <- parts[2]

  lesson_path <- system.file("lessons", paste0(name, ".yaml"), package = pkg)
  if (lesson_path == "" || !file.exists(lesson_path)) {
    cli_abort(c(
      "Lesson {.val {name}} not found in package {.pkg {pkg}}.",
      "i" = "Use {.code list_lessons(package = \"{pkg}\")} to see available lessons."
    ))
  }

  list(path = lesson_path, package = pkg, lesson_id = name)
}

#' Resolve a bare lesson name by searching all packages
#'
#' @param lesson_ref Character string bare lesson name
#' @return A list with `path`, `package`, and `lesson_id`
#' @keywords internal
resolve_bare_name <- function(lesson_ref) {
  index <- build_lesson_index()
  matches <- index[index$lesson_id == lesson_ref, , drop = FALSE]

  if (nrow(matches) == 0) {
    cli_abort(c(
      "Lesson {.val {lesson_ref}} not found.",
      "i" = "Use {.fn list_lessons} to see available lessons."
    ))
  }

  if (nrow(matches) > 1) {
    pkgs <- matches$package
    qualified <- paste0(pkgs, "::", lesson_ref)
    cli_abort(c(
      "Lesson {.val {lesson_ref}} found in multiple packages:",
      set_names(qualified, rep("*", length(qualified))),
      "i" = "Use a qualified name to disambiguate, e.g.:",
      " " = '{.code start_lesson("{qualified[1]}")}'
    ))
  }

  list(
    path = matches$path[1],
    package = matches$package[1],
    lesson_id = matches$lesson_id[1]
  )
}

#' Resolve a lesson reference to a path
#'
#' Accepts three addressing forms:
#' \itemize{
#'   \item Bare name: `"add_two_numbers"` — searches all packages, errors if ambiguous
#'   \item Qualified: `"mypkg::add_two_numbers"` — searches specific package
#'   \item File path: `"path/to/lesson.yaml"` — uses local file directly
#' }
#'
#' @param lesson_ref Character string: bare name, `pkg::name`, or file path
#' @return A list with `path`, `package`, and `lesson_id`
#' @keywords internal
resolve_lesson <- function(lesson_ref) {
  if (grepl("/", lesson_ref, fixed = TRUE) || grepl("\\.yaml$", lesson_ref)) {
    return(resolve_file_path(lesson_ref))
  }

  if (grepl("::", lesson_ref, fixed = TRUE)) {
    return(resolve_qualified_name(lesson_ref))
  }

  resolve_bare_name(lesson_ref)
}

#' Clear the lesson discovery cache
#'
#' Clears the session-level cache of discovered lesson packages and lesson
#' index. Call this after installing or removing packages that contain
#' blendtutor lessons so that they are re-discovered.
#'
#' @return Invisible NULL
#' @export
invalidate_lesson_cache <- function() {
  rm(
    list = ls(.blendtutor_discovery_cache),
    envir = .blendtutor_discovery_cache
  )
  invisible(NULL)
}
