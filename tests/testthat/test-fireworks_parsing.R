# Tests for Fireworks response parsing helpers
# These test pure logic — no API calls needed.

# -- extract_text_fallback --------------------------------------------------

test_that("extract_text_fallback returns feedback from text content", {
  body <- list(
    choices = list(list(
      message = list(content = "The code is correct and works well.")
    ))
  )
  result <- extract_text_fallback(body)

  expect_true(result$is_correct)
  expect_equal(result$feedback, "The code is correct and works well.")
})

test_that("extract_text_fallback detects incorrect in text", {
  body <- list(
    choices = list(list(message = list(content = "The code is incorrect.")))
  )
  result <- extract_text_fallback(body)

  expect_false(result$is_correct)
})

test_that("extract_text_fallback returns NULL for empty content", {
  body <- list(choices = list(list(message = list(content = ""))))
  expect_null(extract_text_fallback(body))

  body2 <- list(choices = list(list(message = list(content = NULL))))
  expect_null(extract_text_fallback(body2))
})

# -- extract_tool_call ------------------------------------------------------

test_that("extract_tool_call finds respond_with_feedback call", {
  body <- list(
    choices = list(list(
      message = list(
        tool_calls = list(list(
          "function" = list(
            name = "respond_with_feedback",
            arguments = '{"is_correct": true, "feedback_message": "Great!"}'
          )
        ))
      )
    ))
  )

  tc <- extract_tool_call(body)
  expect_equal(tc[["function"]]$name, "respond_with_feedback")
})

test_that("extract_tool_call returns NULL for empty choices", {
  expect_null(extract_tool_call(list(choices = list())))
  expect_null(extract_tool_call(list()))
})

test_that("extract_tool_call returns NULL when no tool_calls", {
  body <- list(choices = list(list(message = list(content = "just text"))))
  expect_null(extract_tool_call(body))
})

test_that("extract_tool_call returns NULL for wrong function name", {
  body <- list(
    choices = list(list(
      message = list(
        tool_calls = list(list(
          "function" = list(name = "other_tool", arguments = "{}")
        ))
      )
    ))
  )
  expect_null(extract_tool_call(body))
})

# -- parse_feedback_arguments -----------------------------------------------

test_that("parse_feedback_arguments extracts correct and feedback", {
  tc <- list(
    "function" = list(
      arguments = '{"is_correct": true, "feedback_message": "Well done!"}'
    )
  )
  result <- parse_feedback_arguments(tc)

  expect_true(result$is_correct)
  expect_equal(result$feedback, "Well done!")
})

test_that("parse_feedback_arguments handles false verdict", {
  tc <- list(
    "function" = list(
      arguments = '{"is_correct": false, "feedback_message": "Try again."}'
    )
  )
  result <- parse_feedback_arguments(tc)

  expect_false(result$is_correct)
  expect_equal(result$feedback, "Try again.")
})

test_that("parse_feedback_arguments returns NULL for invalid JSON", {
  tc <- list("function" = list(arguments = "not json {{{"))
  expect_null(parse_feedback_arguments(tc))
})

test_that("parse_feedback_arguments handles nested feedback_message", {
  tc <- list(
    "function" = list(
      arguments = '{"is_correct": true, "feedback_message": {"description": "Nested msg"}}'
    )
  )
  result <- parse_feedback_arguments(tc)

  expect_true(result$is_correct)
  expect_equal(result$feedback, "Nested msg")
})

# -- parse_fireworks_tool_response (integration of above) -------------------

test_that("parse_fireworks_tool_response returns parsed result end-to-end", {
  body <- list(
    choices = list(list(
      message = list(
        tool_calls = list(list(
          "function" = list(
            name = "respond_with_feedback",
            arguments = '{"is_correct": false, "feedback_message": "Missing edge case."}'
          )
        ))
      )
    ))
  )

  result <- parse_fireworks_tool_response(body)
  expect_false(result$is_correct)
  expect_equal(result$feedback, "Missing edge case.")
})

test_that("parse_fireworks_tool_response returns NULL when no tool call", {
  body <- list(choices = list(list(message = list(content = "just text"))))
  expect_null(parse_fireworks_tool_response(body))
})
