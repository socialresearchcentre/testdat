test_that("expect_unique", {
  # We can check against expect_make
  my_expect_unique <- expect_make(chk_unique, vars = TRUE)
  for (i in 1:25) {
    x <- data.frame(key = sample(1:3, 3, replace = TRUE))
    my_res <- testthat::capture_error(my_expect_unique(vars(key), data = x))
    pkg_res <- testthat::capture_error(expect_unique(vars(key), data = x))
    expect_identical(class(my_res), class(pkg_res)) # Either both should be an error or both should be NULL
  }
})

test_that("expect_unique_across", {
  df1 <- data.frame(a = 1:10, b = 99, c = 1)
  expect_success(expect_unique_across(vars(a,b), data = df1))
  expect_failure(expect_unique_across(vars(a,c), data = df1))
})


test_that("expect_unique_combine", {
  df1 <- data.frame(
    a = 1:10,
    b = 11:20,
    c = as.character(c(1:3, 14:16, 27:30)),
    e = rep(1, 10)
  )
  expect_success(expect_unique_combine(vars(a, b), data = df1))
  expect_failure(expect_unique_combine(vars(a, b, c), data = df1))
  expect_failure(expect_unique_combine(vars(e), data = df1))
})
