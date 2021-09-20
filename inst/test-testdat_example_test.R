test_that("passes", {
  context("passing_tests")
  library(testdat)
  mydf <- data.frame(
    my_unique = 1:10,
    my_binary = sample(0:1, 10, replace = TRUE),
    my_string = rownames(mtcars)[1:10],
    my_real = 1:10 / 11:20
  )

  expect_values(my_binary, 0:1, data = mydf)
  expect_unique(my_unique, data = mydf)
  expect_range(my_real, 0, 0.5, data = mydf)
})

test_that("fails", {
  context("failing_tests")
  library(testdat)
  mydf <- data.frame(
    my_unique = 1:10,
    my_binary = sample(0:1, 10, replace = TRUE),
    my_string = rownames(mtcars)[1:10],
    my_real = 1:10 / 11:20
  )

  expect_regex(my_string, "Merc", data = mydf)
  expect_range(my_real, 0, 0.4, data = mydf)
})
