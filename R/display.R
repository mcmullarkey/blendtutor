#' Display lesson header with title and reference
#'
#' Prints formatted header showing lesson name and textbook reference.
#'
#' @param lesson Lesson object with lesson_name and textbook_reference
#' @return Invisible NULL (prints to console)
#' @keywords internal
display_lesson_header <- function(lesson) {
  cat("\n")
  cat(rep("=", 60), "\n", sep = "")
  cat("  Blendtutor: Interactive Coding Lessons\n")
  cat("  Lesson: ", lesson$lesson_name, "\n", sep = "")
  source_pkg <- lesson$.source_package
  if (!is.null(source_pkg) && source_pkg != "blendtutor") {
    cat("  Package: ", source_pkg, "\n", sep = "")
  }
  if (!is.null(lesson$textbook_reference)) {
    cat("  Reference: ", lesson$textbook_reference, "\n", sep = "")
  }
  cat(rep("=", 60), "\n", sep = "")
  cat("\n")

  invisible(NULL)
}

#' Display lesson description and exercise content
#'
#' Prints lesson description, exercise prompt, and example usage.
#'
#' @param lesson Lesson object
#' @return Invisible NULL (prints to console)
#' @keywords internal
display_lesson_content <- function(lesson) {
  # Display lesson description
  if (!is.null(lesson$description)) {
    cat(lesson$description, "\n\n")
  }

  # Display exercise prompt
  cat("EXERCISE:\n")
  cat(lesson$exercise$prompt, "\n\n")

  # Display example usage if available
  if (!is.null(lesson$exercise$example_usage)) {
    cat("EXAMPLE USAGE:\n")
    cat(lesson$exercise$example_usage, "\n\n")
  }

  invisible(NULL)
}

#' Display usage instructions for lesson workflow
#'
#' Prints instructions on how to use open_editor() and submit_code().
#'
#' @return Invisible NULL (prints to console)
#' @keywords internal
display_usage_instructions <- function() {
  cat(rep("-", 60), "\n", sep = "")
  cat("To write your code:\n")
  cat("  1. open_editor()   # Opens editor with template\n")
  cat("  2. Write your code and save\n")
  cat("  3. submit_code()   # Evaluates your code\n")
  cat(rep("-", 60), "\n", sep = "")
  cat("\n")

  invisible(NULL)
}

#' Display formatted LLM feedback
#'
#' Prints feedback with header and footer separators.
#'
#' @param feedback Character string containing LLM feedback
#' @return Invisible NULL (prints to console)
#' @keywords internal
display_feedback <- function(feedback) {
  cat("FEEDBACK:\n")
  cat(rep("-", 60), "\n", sep = "")
  cat(feedback, "\n")
  cat(rep("-", 60), "\n", sep = "")
  cat("\n")

  invisible(NULL)
}

#' Display instructions for retrying submission
#'
#' Prints instructions on how to edit and resubmit code after
#' receiving feedback.
#'
#' @return Invisible NULL (prints to console)
#' @keywords internal
display_retry_instructions <- function() {
  cat("Try again!\n")
  cat("  1. open_editor()  # Edit your code\n")
  cat("  2. submit_code()  # Resubmit\n\n")

  invisible(NULL)
}

#' Format a lesson description for table display
#'
#' Truncates long descriptions and prepends a separator. Returns an
#' empty string for blank descriptions.
#'
#' @param desc Character string description
#' @param max_len Maximum description length before truncation
#' @return Formatted description string (e.g. `"  - Some desc"`) or `""`
#' @keywords internal
truncate_description <- function(desc, max_len = 45) {
  if (nchar(desc) == 0) return("")
  if (nchar(desc) > max_len) {
    desc <- paste0(substr(desc, 1, max_len - 3), "...")
  }
  paste0("  - ", desc)
}

#' Display formatted table of lessons grouped by package
#'
#' Prints a table of lessons grouped by their source package.
#'
#' @param lessons_df Data frame with `lesson_id`, `package`, and `description` columns
#' @return Invisible NULL (prints to console)
#' @keywords internal
display_lesson_table <- function(lessons_df) {
  if (nrow(lessons_df) == 0) {
    cat("No lessons found.\n")
    return(invisible(NULL))
  }

  packages <- unique(lessons_df$package)

  cat("\n")
  for (pkg in packages) {
    pkg_lessons <- lessons_df[lessons_df$package == pkg, , drop = FALSE]
    cat("-- ", pkg, " ", rep("-", max(1, 55 - nchar(pkg))), "\n", sep = "")
    for (i in seq_len(nrow(pkg_lessons))) {
      desc <- truncate_description(pkg_lessons$description[i])
      cat("  ", pkg_lessons$lesson_id[i], desc, "\n", sep = "")
    }
    cat("\n")
  }

  invisible(NULL)
}
