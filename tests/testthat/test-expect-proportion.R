test_that("prop_gte", {
  df1 <- data.frame(
    x = 1:100,
    y = c(1:25, NA, 27:50, NA, 52:100)
  )

  # Custom `func`
  chk_even <- function(x) (as.numeric(x) %% 2) == 0
  expect_success(expect_prop_gte(x, chk_even, prop = 0.5, data = df1))
  expect_failure(expect_prop_gte(y, chk_even, prop = 0.5, data = df1))

  # Custom expectation
  expect_prop_even <- function(var, prop, data) {
    expect_prop_gte({{var}}, func = chk_even, prop = prop, data = data)
  }
  expect_success(expect_prop_even(x, prop = 0.5, data = df1))
  expect_failure(expect_prop_even(y, prop = 0.5, data = df1))
})


test_that("proportion missing", {
  df1 <- data.frame(
    names = c("Kinto", "Al", "error", "Paddy"),
    answers = c(1, "", 0, NA)
  )

  expect_success(expect_prop_nmiss(names, prop = 0.9, data = df1))
  expect_failure(expect_prop_nmiss(names, prop = 0.9, miss = getOption("testdat.miss_text"), data = df1))
  expect_failure(expect_prop_nmiss(answers, prop = 3 / 4, data = df1))
})


test_that("proportion valid values", {
  for (i in 1:10) {
    df1 <- data.frame(
      key = 1:100,
      binary2 = sample(0:2, 100, TRUE)
    )

    if (sum(df1$binary2 %in% 0:1) / nrow(df1) >= 0.6) {
      expect_success(expect_prop_values(binary2, prop = 0.6, 0:1, data = df1))
    } else {
      expect_failure(expect_prop_values(binary2, prop = 0.6, 0:1, data = df1))
    }
  }
})
