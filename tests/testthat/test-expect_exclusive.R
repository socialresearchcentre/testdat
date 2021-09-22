suppressWarnings(library(dplyr))

dat <- tribble(
  ~a1, ~a2, ~a3, ~a4, ~a5, ~a6, ~a7, ~a8, ~a9, ~a10,
  1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
  0, 1, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 1, 0, 0, 0, 0, 0, 1, 0,
  0, 0, 0, 1, 1, 0, 0, 0, 0, 0,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
  0, 0, 1, 0, 0, 1, 0, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 1, 0, 0, 0,
  0, 0, 0, 0, 0, 0, 0, 1, 0, 0,
  1, 0, 0, 0, 1, 0, 0, 0, 1, 0,
  0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
  0, 0, 0, 0, 1, 0, 0, 0, 0, 1
)

test_that("Exclusive check works", {
  expect_failure(expect_exclusive(a10, matches("^a[0-9]"), data = dat))
  expect_success(expect_exclusive(a10, matches("^a[0-4]"), data = dat))
  expect_warning(expect_exclusive(a10, matches("^a[2-4]"), data = dat))
})

