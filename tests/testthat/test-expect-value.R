test_that("expect_values", {
  df1 <- data.frame(
    a = c(1, 2, NA, 4, 5),
    b = c("", "2", "NA", "4", "5")
  )

  expect_success(expect_values(a, 1:5, data = df1))
  expect_failure(expect_values(b, 1:5, data = df1))
  expect_success(expect_values(b, 1:5, "NA", data = df1))
})
