#' Validate that a lesson is active and ready for interaction
#'
#' Checks if a lesson has been started and optionally whether it's already
#' complete. Used by both open_editor() and submit_code() to ensure a lesson
#' is in the correct state before proceeding.
#'
#' @param allow_completed Logical; if FALSE, throws error for completed lessons
#' @return Invisible NULL if valid, stops with error message otherwise
#' @keywords internal
validate_lesson_active <- function(allow_completed = FALSE) {
  # Check if a lesson is active
  if (is.null(.blendtutor_env$current_lesson)) {
    cli_abort(c(
      "No active lesson.",
      "i" = "Please start a lesson first using: {.fn start_lesson}"
    ))
  }

  # Check if lesson is already complete (unless allowed)
  if (!allow_completed && isTRUE(.blendtutor_env$lesson_complete)) {
    cli_abort(c(
      "This lesson is already complete!",
      "i" = "Start a new lesson with: {.fn start_lesson}"
    ))
  }

  invisible(NULL)
}
