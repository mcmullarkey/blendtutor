# Package environment for storing lesson state
.blendtutor_env <- new.env(parent = emptyenv())

#' Start an interactive coding lesson
#'
#' Loads a lesson and begins an interactive learning session with AI-powered
#' feedback via the Fireworks API.
#'
#' Lessons are discovered automatically from blendtutor and any installed package
#' that depends on blendtutor and contains YAML files in `inst/lessons/`.
#'
#' @param lesson_name Lesson reference in one of three forms:
#'   \describe{
#'     \item{Bare name}{`"add_two_numbers"` — searches all packages, errors
#'       if the name is found in multiple packages}
#'     \item{Qualified name}{`"mypkg::add_two_numbers"` — searches a specific
#'       package only}
#'     \item{File path}{`"path/to/lesson.yaml"` — loads a local YAML file
#'       directly (useful during lesson development)}
#'   }
#' @param model Model to use (optional; defaults to Fireworks default if not specified)
#' @return Invisible NULL. Displays lesson information and waits for code submission.
#' @export
#' @examples
#' \dontrun{
#' # Start a lesson (from blendtutor or any installed lesson package)
#' start_lesson("add_two_numbers")
#'
#' # Start a lesson from a specific package
#' start_lesson("mypkg::my_lesson")
#'
#' # Start a lesson from a local file (useful for development)
#' start_lesson("inst/lessons/my_lesson.yaml")
#'
#' # Submit your code
#' submit_code("add_two <- function(x, y) { x + y }")
#' }
start_lesson <- function(lesson_name = "add_two_numbers", model = NULL) {
  lesson <- load_lesson(lesson_name)
  code_file <- create_lesson_code_file(lesson)
  initialize_lesson_state(lesson, model, code_file)

  display_lesson_header(lesson)
  display_lesson_content(lesson)
  display_usage_instructions()

  invisible(NULL)
}

#' Open code editor for writing/editing lesson code
#'
#' Opens the lesson's code file in your system editor. Edit your code,
#' save the file, and close the editor. Then call submit_code() to evaluate.
#'
#' @return Invisible NULL
#' @export
#' @examples
#' \dontrun{
#' start_lesson("add_two_numbers")
#' open_editor()
#' # Edit code in your editor, save, and close
#' submit_code()
#' }
open_editor <- function() {
  validate_lesson_active(allow_completed = FALSE)

  cat("\nOpening editor...\n")
  cat("Write your code, save the file, and close the editor.\n")
  cat("Then call submit_code() to evaluate.\n\n")

  file.edit(.blendtutor_env$code_file)

  invisible(NULL)
}

#' Submit code for evaluation
#'
#' Submits student code to the AI for evaluation and feedback.
#' Reads code from the lesson's temp file (created by open_editor).
#'
#' @param code_string Optional character string for direct submission (advanced)
#' @return Invisible NULL. Displays AI feedback.
#' @export
#' @examples
#' \dontrun{
#' start_lesson("add_two_numbers")
#' open_editor()
#' submit_code()
#'
#' # Or directly submit a string:
#' submit_code("add_two <- function(x, y) { x + y }")
#' }
submit_code <- function(code_string = NULL) {
  validate_lesson_active(allow_completed = FALSE)

  code_string <- retrieve_student_code(code_string, .blendtutor_env$code_file)
  .blendtutor_env$current_code <- code_string

  lesson <- .blendtutor_env$current_lesson
  result <- evaluate_student_submission(
    code_string,
    lesson,
    .blendtutor_env$model
  )

  display_feedback(result$feedback)

  if (check_feedback_correct(result)) {
    handle_lesson_completion(lesson)
  } else {
    display_retry_instructions()
  }

  invisible(NULL)
}

#' Reset current lesson
#'
#' Clears the current lesson state and removes temp files.
#'
#' @return Invisible NULL
#' @export
reset_lesson <- function() {
  # Clean up temp file
  if (
    !is.null(.blendtutor_env$code_file) &&
      file.exists(.blendtutor_env$code_file)
  ) {
    unlink(.blendtutor_env$code_file)
  }

  # Clear state
  .blendtutor_env$current_lesson <- NULL
  .blendtutor_env$lesson_complete <- FALSE
  .blendtutor_env$current_code <- NULL
  .blendtutor_env$model <- NULL
  .blendtutor_env$code_file <- NULL
  .blendtutor_env$source_package <- NULL
  .blendtutor_env$lesson_id <- NULL

  cat("\nLesson state cleared.\n")
  cat("Start a new lesson with: start_lesson()\n\n")

  invisible(NULL)
}
