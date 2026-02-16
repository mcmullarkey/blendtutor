#' Evaluate student code submission with LLM
#'
#' Sends code to Fireworks AI for evaluation and displays status message.
#'
#' @param code_string Character string of student code
#' @param lesson Lesson object containing evaluation prompt
#' @param model Model name (or NULL for default)
#' @return List with is_correct (logical) and feedback (character)
#' @keywords internal
evaluate_student_submission <- function(code_string, lesson, model) {
  # Display evaluation message
  cat("\n")
  cat("Evaluating your code with AI...\n")
  cat("\n")

  # Evaluate with LLM (returns structured result)
  result <- evaluate_with_llm(
    student_code = code_string,
    exercise_prompt = lesson$exercise$llm_evaluation_prompt,
    model = model
  )

  return(result)
}

#' Check if feedback indicates correct answer
#'
#' Extracts is_correct from structured LLM evaluation result.
#'
#' @param result List with is_correct and feedback from evaluate_student_submission()
#' @return Logical indicating whether answer is correct
#' @keywords internal
check_feedback_correct <- function(result) {
  isTRUE(result$is_correct)
}

#' Format other available lessons as start_lesson() suggestions
#'
#' @param current_id Character string lesson ID to exclude
#' @return Character vector of formatted `start_lesson(...)` strings,
#'   or empty character if none available
#' @keywords internal
format_next_lessons <- function(current_id) {
  available <- list_lessons(quiet = TRUE)
  other <- available[available$lesson_id != current_id, , drop = FALSE]
  if (nrow(other) == 0) return(character(0))

  refs <- other$lesson_id
  external <- other$package != "blendtutor"
  refs[external] <- paste0(other$package[external], "::", refs[external])

  paste0("  start_lesson(\"", refs, "\")")
}

#' Handle successful lesson completion
#'
#' Marks lesson as complete, displays congratulations message,
#' and shows available next lessons.
#'
#' @param lesson Current lesson object
#' @return Invisible NULL (prints to console, modifies .blendtutor_env)
#' @keywords internal
handle_lesson_completion <- function(lesson) {
  cat("Congratulations! Lesson complete!\n\n")
  .blendtutor_env$lesson_complete <- TRUE

  suggestions <- format_next_lessons(lesson$.lesson_id)
  if (length(suggestions) > 0) {
    cat("Try another lesson:\n")
    cat(suggestions, sep = "\n")
    cat("\n\n")
  }

  invisible(NULL)
}
