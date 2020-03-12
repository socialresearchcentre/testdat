test_that("scientific notation not applied when pattern checking numeric vars", {
  expect_true(chk_pattern(10000000, "1[0]{7}"))
})
