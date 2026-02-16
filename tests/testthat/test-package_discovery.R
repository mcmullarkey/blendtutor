# Tests for lesson resolution helpers

test_that("resolve_file_path resolves an existing YAML file", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("lesson_name: Test", tmp)

  result <- resolve_file_path(tmp)

  expect_equal(result$path, normalizePath(tmp))
  expect_null(result$package)
  expect_type(result$lesson_id, "character")
})

test_that("resolve_file_path errors on missing file", {
  expect_error(resolve_file_path("/no/such/file.yaml"), "not found")
})

test_that("resolve_qualified_name errors on malformed reference", {
  expect_error(resolve_qualified_name("::name"), "Invalid lesson reference")
  expect_error(resolve_qualified_name("pkg::"), "Invalid lesson reference")
})

test_that("resolve_qualified_name errors on missing lesson in package", {
  expect_error(
    resolve_qualified_name("blendtutor::nonexistent_lesson_xyz"),
    "not found in package"
  )
})

test_that("resolve_bare_name errors on unknown lesson", {
  expect_error(resolve_bare_name("no_such_lesson_xyz"), "not found")
})

test_that("resolve_lesson dispatches to file path handler", {
  tmp <- tempfile(fileext = ".yaml")
  on.exit(unlink(tmp))
  writeLines("lesson_name: Test", tmp)

  result <- resolve_lesson(tmp)
  expect_equal(result$path, normalizePath(tmp))
  expect_null(result$package)
})

test_that("resolve_lesson dispatches to qualified name handler", {
  expect_error(
    resolve_lesson("blendtutor::nonexistent_lesson_xyz"),
    "not found in package"
  )
})

test_that("resolve_lesson dispatches to bare name handler", {
  expect_error(resolve_lesson("no_such_lesson_xyz"), "not found")
})
