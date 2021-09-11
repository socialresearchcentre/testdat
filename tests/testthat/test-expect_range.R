dat <- tibble(
  min_max_vals = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 98, 98, 99, 99),
  min_max = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4),
  min_max_real = c(1, 1.1, 1.2, 2.1, 2.5, 2.9, 3, 3.1, 3.6, 4, 4.1, 4.2, 4.3),
)

test_that("min max works with only min and max provided", {
  expect_success(expect_range(min_max, 1, 10, data = dat))
  expect_failure(expect_range(min_max, 1, 3, data = dat))
  expect_failure(expect_range(min_max_vals, 1, 10, data = dat))
})

test_that("min max works with real numbers", {
  expect_success(expect_range(min_max_real, 1, 10, data = dat))
  expect_failure(expect_range(min_max_real, 1, 4, data = dat))
})

test_that("min max works additional valid values provided", {
  expect_success(expect_range(min_max_vals, 1, 10, 98, 99, data = dat))
  expect_success(expect_range(min_max_vals, 1, 10, 98:99, data = dat))
  expect_failure(expect_range(min_max_vals, 1, 2, 98:99, data = dat))
  expect_failure(expect_range(min_max_vals, 1, 10, 99, data = dat))
})
