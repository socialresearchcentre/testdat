library(dplyr, warn.conflicts = FALSE)

test_that("multiple expectations in a pipe chain work", {
  expect_equal(
    mtcars %>%
      with_testdata(expect_base(mpg, TRUE)) %>%
      mutate(mpg = NA) %>%
      with_testdata(expect_base(mpg, FALSE)),
    mtcars %>% mutate(mpg = NA)
  )
})

test_that("test data pipe works", {
  expect_equal(
    mtcars %E>%
      expect_base(mpg, TRUE) %>%
      mutate(mpg = NA) %E>%
      expect_base(mpg, FALSE),
    mtcars %>% mutate(mpg = NA)
  )
})

test_that("informative error when incorrect pipe is used", {
  set_testdata(NULL, quosure = FALSE)

  expect_error(
    mtcars %>%
      expect_base(mpg, TRUE) %>%
      mutate(mpg = NA) %>%
      expect_base(mpg, FALSE),
    "If you're trying to call an expectation in a pipe, use the test data pipe"
  )

  set_testdata(iris)

  expect_error(
    mtcars %>% expect_base(mpg, TRUE),
    "If you're trying to call an expectation in a pipe, use the test data pipe"
  )

  expect_error(
    mtcars %>% expect_values(carb, 1:8),
    "If you're trying to call an expectation in a pipe, use the test data pipe"
  )
})
