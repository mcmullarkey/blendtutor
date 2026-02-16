# Tests for lesson loader helpers

test_that("empty_lessons_result returns correct structure", {
  result <- empty_lessons_result()

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 0)
  expect_equal(names(result), c("lesson_id", "package", "description"))
})

test_that("read_lesson_descriptions reads description from YAML", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines('description: "A test lesson"', tmp)

  descs <- read_lesson_descriptions(tmp)
  expect_equal(descs, "A test lesson")
})

test_that("read_lesson_descriptions returns empty string on missing description", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("lesson_name: Test", tmp)

  descs <- read_lesson_descriptions(tmp)
  expect_equal(descs, "")
})

test_that("read_lesson_descriptions returns empty string on parse error", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("invalid: yaml: [broken", tmp)

  descs <- read_lesson_descriptions(tmp)
  expect_equal(descs, "")
})
