context("Example")
context_data(iris)

test_that("Versicolor has sepal length greater than 5 - will fail", {
  expect_cond(Species %in% "versicolor", Sepal.Length >= 5)
})
