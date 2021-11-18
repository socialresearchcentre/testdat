test_that("expect_unique", {
  # We can check against expect_make
  my_expect_unique <- expect_make(chk_unique, vars = TRUE)
  for (i in 1:25) {
    x <- data.frame(key = sample(1:3, 3, replace = TRUE))
    my_res <- testthat::capture_error(my_expect_unique(key, data = x))
    pkg_res <- testthat::capture_error(expect_unique(key, data = x))
    expect_identical(class(my_res), class(pkg_res)) # Either both should be an error or both should be NULL
  }
})

test_that("expect_unique_across", {
  df1 <- data.frame(a = 1:10, b = 99, c = 1)
  expect_success(expect_unique_across(c(a, b), data = df1))
  expect_failure(expect_unique_across(c(a, c), data = df1))
})


test_that("expect_unique_combine", {
  df1 <- data.frame(
    a = 1:10,
    b = 11:20,
    c = as.character(c(1:3, 14:16, 27:30)),
    e = rep(1, 10)
  )
  expect_success(expect_unique_combine(c(a, b), data = df1))
  expect_failure(expect_unique_combine(c(a, b, c), data = df1))
  expect_failure(expect_unique_combine(e, data = df1))
})

test_that("exclude argument works as expected", {
  df2 <- data.frame(
    var1 = c(9999L, 1234L, 1234L),
    var2 = c(9999L, 9999L, 1234L),
    var3 = c(9999L, 9999L, 9999L),
    var4 = c(9999L, 9998L, 9997L)
  )

  # expect_unique
  expect_success(expect_unique(var2, exclude = 9999, data = df2))
  expect_failure(expect_unique(var1, exclude = 9999, data = df2))

  # expect_unique_across
  expect_success(expect_unique_across(c(var1, var2, var3), exclude = 9999, flt = var2 %in% 9999, data = df2))
  expect_failure(expect_unique_across(c(var1, var2, var3), exclude = 9999, data = df2))

  # expect_unique_combine
  expect_success(expect_unique_combine(c(var2, var4), exclude = 9999, data = df2))
  expect_failure(expect_unique_combine(c(var1, var4), exclude = 9999, data = df2))
})

test_that("exclude argument works correctly with multiple vars in expect_unique()", {
  df2 <- data.frame(
    var1 = c(1234L, 1234L, 1234L),
    var2 = c(9999L, 9999L, 1234L)
  )

  expect_failure(expect_unique(c(var1, var2), data = df2))
  expect_success(expect_unique(c(var1, var2), exclude = 9999, data = df2))
})
