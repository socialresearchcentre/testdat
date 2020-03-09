test_that("no testdata specified", {
  expect_error(get_testdata())
})

test_that("set_testdata/get_testdata work correctly", {
  set_testdata(iris)
  expect_identical(get_testdata(), iris)
  expect_failure(expect_cond(Species %in% "versicolor", Sepal.Length >= 5))

  old_dat <- set_testdata(mdeaths)
  expect_identical(old_dat, iris)

  context_data(mtcars)
  expect_identical(get_testdata(), mtcars)
})

test_that("with_testdat temporarily sets test dataset", {
  set_testdata(mtcars)
  expect_identical(get_testdata(), mtcars)
  with_testdata(iris, {
    expect_identical(get_testdata(), iris)
  })
  expect_identical(get_testdata(), mtcars)
})


test_that("check that modifications to the dataframe are picked up by get_testdata", {
  skip("not yet implemented")

  tmp_data <- tibble(x = c(1, 0), y = c(1, NA))
  set_testdata(tmp_data)
  expect_success(expect_base(y, x == 1))

  tmp_data <- tibble(x = c(1, 0), y = c(1, 1))
  expect_failure(expect_base(y, x == 1))
})
