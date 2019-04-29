context("chk_values")

test_that("NULL missing values work", {
  expect_equal(chk_values(c(1, NA), miss = NULL), c(FALSE, FALSE))
  expect_equal(chk_values(c(1, NA)), c(FALSE, TRUE))
})
