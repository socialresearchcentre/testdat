test_that("NULL missing values work", {
  expect_equal(chk_values(c(1, NA), miss = NULL), c(FALSE, FALSE))
  expect_equal(chk_values(c(1, NA)), c(FALSE, TRUE))
})

test_that("scientific notation not applied when value checking numeric vars", {
  expect_true(chk_values(10000000, "10000000"))
  expect_true(chk_values("10000000", 10000000))
})
