test_that("expect_valmatch", {
  df1 <- data.frame(key = 1:10, a = rep(1:5, 2), b = rep(0:1, 5), c = rep(c("c", "C"), 5), d = 1:10)
  df2 <- data.frame(key = 1:10, a = rep(1:5, 2), b = rep(0:4, 2), c = rep(c("c", "C"), 5))

  expect_success(expect_valmatch(df2, c(a, c), by = "key", data = df1))
  expect_failure(expect_valmatch(df2, c(a, b), by = "key", data = df1))
  expect_error(expect_valmatch(df2, c(a, d), by = "key", data = df1), "specifies variables that are not common to both data frames")
})


test_that("expect_subset", {
  df <- data.frame(
    k1 = c(1:8, 1),
    k2 = c(rep("a", 3), rep("b", 3), rep("c", 3)),
    x = runif(9)
  )
  df_subset <- df[df$k1 < 4, ]
  df_nonsubset <- rbind(
    df_subset,
    data.frame(k1 = 99, k2 = "z", x = 0)
  )

  expect_success(expect_subset(data = df_subset, data2 = df, by = c("k1", "k2")))
  names(df_subset) <- c("key1", "key2", "x")
  expect_success(expect_subset(data = df_subset, data2 = df, by = c("key1" = "k1", "key2" = "k2")))

  expect_failure(expect_subset(data = df_nonsubset, data2 = df, by = c("k1", "k2")))
  names(df_nonsubset) <- c("key1", "key2", "x")
  expect_failure(expect_subset(data = df_nonsubset, data2 = df, by = c("key1" = "k1", "key2" = "k2")))
})
