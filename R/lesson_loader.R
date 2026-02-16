#' Get path to lesson YAML file
#'
#' Resolves a lesson reference to a file path using the cross-package
#' discovery system. Accepts bare names, qualified `pkg::name` references,
#' and direct file paths.
#'
#' @param lesson_name Lesson reference (bare name, pkg::name, or file path)
#' @return Character string path to lesson file
#' @keywords internal
get_lesson_path <- function(lesson_name) {
  resolved <- resolve_lesson(lesson_name)
  resolved$path
}

#' Read lesson YAML file with error handling
#'
#' Reads and parses YAML file, providing helpful error messages
#' if reading or parsing fails.
#'
#' @param lesson_path Path to YAML file
#' @param lesson_name Lesson name for error messages
#' @return Parsed lesson list
#' @keywords internal
read_lesson_yaml <- function(lesson_path, lesson_name) {
  lesson_file <- paste0(lesson_name, ".yaml")

  lesson <- tryCatch(
    yaml::read_yaml(lesson_path),
    error = function(e) {
      cli_abort(c(
        "Error reading lesson file {.file {lesson_file}}.",
        "x" = e$message
      ))
    }
  )

  return(lesson)
}

#' Validate lesson has required structure
#'
#' Checks that lesson contains all required top-level and exercise fields.
#' Provides clear error messages indicating which fields are missing.
#'
#' @param lesson Parsed lesson list
#' @param lesson_name Lesson name for error messages
#' @return Invisible NULL if valid, stops with error otherwise
#' @keywords internal
validate_lesson_structure <- function(lesson, lesson_name) {
  # Validate required top-level fields
  required_fields <- c("lesson_name", "exercise")
  missing_fields <- setdiff(required_fields, names(lesson))

  if (length(missing_fields) > 0) {
    cli_abort(c(
      "Lesson {.val {lesson_name}} is missing required fields:",
      "x" = "{.field {missing_fields}}"
    ))
  }

  # Validate exercise structure
  required_exercise_fields <- c("prompt", "llm_evaluation_prompt")
  missing_exercise_fields <- setdiff(required_exercise_fields, names(lesson$exercise))

  if (length(missing_exercise_fields) > 0) {
    cli_abort(c(
      "Lesson {.val {lesson_name}} exercise is missing required fields:",
      "x" = "{.field {missing_exercise_fields}}"
    ))
  }

  invisible(NULL)
}

#' Load a lesson from YAML file
#'
#' Loads and validates a lesson definition, attaching metadata about
#' the source package and lesson ID for cross-package discovery support.
#'
#' @param lesson_name Lesson reference (bare name, pkg::name, or file path)
#' @return A list containing the lesson structure with `.source_package`,
#'   `.lesson_id`, and `.source_path` metadata attributes
#' @keywords internal
load_lesson <- function(lesson_name) {
  resolved <- resolve_lesson(lesson_name)
  lesson <- read_lesson_yaml(resolved$path, resolved$lesson_id)
  validate_lesson_structure(lesson, resolved$lesson_id)

  # Attach discovery metadata
  lesson$.source_package <- resolved$package
  lesson$.lesson_id <- resolved$lesson_id
  lesson$.source_path <- resolved$path

  return(lesson)
}

#' Create an empty lessons result data.frame
#'
#' @return A zero-row data.frame with columns `lesson_id`, `package`, `description`
#' @keywords internal
empty_lessons_result <- function() {
  data.frame(
    lesson_id = character(0),
    package = character(0),
    description = character(0),
    stringsAsFactors = FALSE
  )
}

#' Read lesson descriptions from YAML files
#'
#' @param paths Character vector of file paths to lesson YAML files
#' @return Character vector of descriptions (empty string on read failure)
#' @keywords internal
read_lesson_descriptions <- function(paths) {
  vapply(paths, function(p) {
    tryCatch({
      lesson <- yaml::read_yaml(p)
      lesson$description %||% ""
    }, error = function(e) "")
  }, character(1), USE.NAMES = FALSE)
}

#' List available lessons
#'
#' Returns a data frame of all discoverable lessons across blendtutor and any
#' installed packages that depend on it. Prints a formatted table by default.
#'
#' @param package Optional character string to filter lessons to a specific
#'   package. If `NULL` (the default), lessons from all packages are shown.
#' @param quiet Logical; if `TRUE`, suppress printed output and return
#'   the data frame visibly. Default `FALSE`.
#' @return A `data.frame` with columns `lesson_id`, `package`, and
#'   `description`, returned invisibly unless `quiet = TRUE`.
#' @export
list_lessons <- function(package = NULL, quiet = FALSE) {
  index <- build_lesson_index()
  if (!is.null(package)) {
    index <- index[index$package == package, , drop = FALSE]
  }

  if (nrow(index) == 0) {
    if (!quiet) {
      msg <- if (is.null(package)) "No lessons found.\n"
             else paste0("No lessons found in package '", package, "'.\n")
      cat(msg)
    }
    result <- empty_lessons_result()
    if (quiet) return(result)
    return(invisible(result))
  }

  result <- data.frame(
    lesson_id = index$lesson_id,
    package = index$package,
    description = read_lesson_descriptions(index$path),
    stringsAsFactors = FALSE
  )
  rownames(result) <- NULL

  if (!quiet) display_lesson_table(result)

  if (quiet) result else invisible(result)
}
