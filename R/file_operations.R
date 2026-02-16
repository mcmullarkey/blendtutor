#' Create temporary code file for lesson
#'
#' Creates a temporary R file and writes the lesson template or default content.
#'
#' @param lesson Lesson object containing exercise template
#' @return Character string path to the created temp file
#' @keywords internal
create_lesson_code_file <- function(lesson) {
  # Create temp file for code editing
  code_file <- tempfile(fileext = ".R")

  # Write initial template
  if (!is.null(lesson$exercise$code_template)) {
    writeLines(lesson$exercise$code_template, code_file)
  } else {
    # Default template
    writeLines(c("# Write your code here", ""), code_file)
  }

  return(code_file)
}

#' Retrieve student code from string or file
#'
#' Gets code either from direct string argument or by reading the temp file.
#' If code_string is provided, also saves it to the temp file for persistence.
#'
#' @param code_string Optional character string of code
#' @param code_file Path to temp file containing code
#' @return Character string containing the student's code
#' @keywords internal
retrieve_student_code <- function(code_string, code_file) {
  # Direct string submission - save to temp file for persistence
  if (!is.null(code_string)) {
    writeLines(code_string, code_file)
    return(code_string)
  }

  # Read from temp file
  if (!file.exists(code_file)) {
    cli_abort(c(
      "Code file not found.",
      "i" = "Use {.fn open_editor} to write your code first."
    ))
  }
  paste(readLines(code_file), collapse = "\n")
}
