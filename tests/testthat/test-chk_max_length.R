test_that("scientific notation not applied when length checking numeric vars", {
  expect_true(chk_max_length(10000000, 8))
})
