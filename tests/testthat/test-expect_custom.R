test_that("expect warns if no `failure_message`", {
  expect_warning(expect_custom(TRUE), "missing, with no default")
})
