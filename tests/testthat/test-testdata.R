test_that("no testdata specified", {
  set_testdata(NULL, quosure = FALSE)
  expect_error(get_testdata())
})

test_that("set_testdata/get_testdata work correctly", {
  set_testdata(iris)
  expect_identical(get_testdata(), iris)
  expect_failure(expect_cond(Species %in% "versicolor", Sepal.Length >= 5))

  old_dat <- set_testdata(mdeaths)
  expect_identical(eval_tidy(old_dat), iris)
})

test_that("with_testdat temporarily sets test data frame", {
  set_testdata(mtcars)
  expect_identical(get_testdata(), mtcars)
  with_testdata(iris, {
    expect_identical(get_testdata(), iris)
  })
  expect_identical(get_testdata(), mtcars)
})


test_that("check that modifications to the dataframe are picked up by get_testdata", {
  tmp_data_orig <- tmp_data <- tibble(x = c(1, 0), y = c(1, NA))
  set_testdata(tmp_data)

  tmp_data <- tibble(x = c(1, 0), y = c(1, 1))
  expect_identical(get_testdata(), tmp_data)
})

test_that("check that the `quosure` argument works in set_testdata", {
  tmp_data_orig <- tmp_data <- tibble(x = c(1, 0), y = c(1, NA))
  set_testdata(tmp_data, quosure = FALSE)

  tmp_data <- tibble(x = c(1, 0), y = c(1, 1))
  expect_identical(get_testdata(), tmp_data_orig)
})

test_that("groups are removed by `get_testdata()`", {
  tmp_data <- tibble(x = c(1, 0), y = c(1, NA))
  tmp_data <- group_by(tmp_data, x)
  set_testdata(tmp_data)

  expect_length(groups(get_testdata()), 0)
})
