test_that("basic examples", {
  expect_success(expect_base(y, x == 1, data = tibble(x = c(1, 0), y = c(1, NA))))
  expect_failure(expect_base(y, x == 1, data = tibble(x = c(1, 0), y = c(1, 1))))
})

test_that("NA check results are handled correctly", {
  expect_success(expect_base(y, x == 1, data = tibble(x = c(1, NA), y = c(1, NA))))
  expect_failure(expect_base(y, x == 1, data = tibble(x = c(1, NA), y = c(1, 1))))
})

test_that("fails if var is not in testdata", {
  expect_error(expect_base(y, TRUE, data = tibble(x = 0:1)))
})
