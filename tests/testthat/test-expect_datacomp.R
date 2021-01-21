test_that("expect_similar", {
  df1 <- data.frame(key = 1:1000, binom_50 = c(rep(0, 500), rep(1, 500)))
  df2 <- data.frame(key = 1:1000, binom_51 = c(rep(0, 490), rep(1, 510)))
  df3 <- data.frame(key = 1:1000, binom_80 = c(rep(0, 200), rep(1, 800)))
  expect_success(expect_similar(binom_50, df2, binom_51, data = df1))
  expect_failure(expect_similar(binom_50, df3, binom_80, data = df1))
})

test_that("expect_valmatch", {
  df1 <- data.frame(key = 1:10, a = rep(1:5, 2), b = rep(0:1, 5), c = rep(c("c", "C"), 5), d = 1:10)
  df2 <- data.frame(key = 1:10, a = rep(1:5, 2), b = rep(0:4, 2), c = rep(c("c", "C"), 5))

  expect_success(expect_valmatch(df2, vars(a, c), by = "key", data = df1))
  expect_failure(expect_valmatch(df2, vars(a, b), by = "key", data = df1))
  expect_error(expect_valmatch(df2, vars(a, d), by = "key", data = df1), "specifies variables that are not common to both datasets")
})
