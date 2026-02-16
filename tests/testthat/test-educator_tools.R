# Tests for lesson validation logic

test_that("collect_validation_results reports OK for complete lesson", {
  lesson <- list(
    lesson_name = "Test Lesson",
    description = "A test",
    textbook_reference = "Ch 1",
    exercise = list(
      prompt = "Do something",
      llm_evaluation_prompt = "Evaluate {student_code}",
      code_template = "# code",
      example_usage = "fn(1)",
      success_criteria = "- works"
    )
  )

  report <- collect_validation_results(lesson)

  expect_s3_class(report, "data.frame")
  expect_true(all(c("field", "status", "message") %in% names(report)))
  expect_true(all(report$status == "OK"))
  expect_equal(sum(report$status == "FAIL"), 0)
})

test_that("collect_validation_results flags missing required fields", {
  lesson <- list()
  report <- collect_validation_results(lesson)

  fails <- report[report$status == "FAIL", ]
  expect_true(nrow(fails) >= 2)
  expect_true("lesson_name" %in% fails$field)
  expect_true("exercise" %in% fails$field)
})

test_that("collect_validation_results warns on missing recommended fields", {
  lesson <- list(
    lesson_name = "Test",
    exercise = list(
      prompt = "Do it",
      llm_evaluation_prompt = "Check {student_code}"
    )
  )
  report <- collect_validation_results(lesson)

  warns <- report[report$status == "WARN", ]
  expect_true("description" %in% warns$field)
  expect_true("textbook_reference" %in% warns$field)
})

test_that("collect_validation_results warns when {student_code} is missing", {
  lesson <- list(
    lesson_name = "Test",
    exercise = list(
      prompt = "Do it",
      llm_evaluation_prompt = "Evaluate the code"
    )
  )
  report <- collect_validation_results(lesson)

  placeholder_row <- report[report$field == "{student_code} placeholder", ]
  expect_equal(nrow(placeholder_row), 1)
  expect_equal(placeholder_row$status, "WARN")
})
