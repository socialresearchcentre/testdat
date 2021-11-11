test_that("multiple expectations in a pipe chain work", {
  mtcars %>%
    with_testdata(expect_base(mpg, TRUE)) %>%
    mutate(mpg = NA) %>%
    with_testdata(expect_base(mpg, FALSE))
})

