#' Initialize lesson state in package environment
#'
#' Stores lesson, model, and file information in the blendtutor package environment.
#'
#' @param lesson Lesson object
#' @param model Character string of model name (or NULL for default)
#' @param code_file Character string path to temp code file
#' @return Invisible NULL (modifies .blendtutor_env)
#' @keywords internal
initialize_lesson_state <- function(lesson, model, code_file) {
  .blendtutor_env$current_lesson <- lesson
  .blendtutor_env$lesson_complete <- FALSE
  .blendtutor_env$model <- model
  .blendtutor_env$code_file <- code_file
  .blendtutor_env$source_package <- lesson$.source_package
  .blendtutor_env$lesson_id <- lesson$.lesson_id

  invisible(NULL)
}
