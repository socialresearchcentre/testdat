test_that("chk_filter", {
  test_df <- data.frame(
    a = 1:10,
    b = rep(0:1, 5) * 2,
    c = c(runif(5, max = 1), runif(5, max = 100)),
    alpha = 1:10
  )

  expect_identical(
    chk_filter(test_df, c, chk_range, a <= 5, list(min = 0, max = 1)),
    data.frame(c = c(rep(TRUE, 5), rep(NA, 5)))
  )

  expect_identical(
    chk_filter_all(test_df, c(b, c), chk_range, a <= 5, list(min = 0, max = 1)),
    c(!as.logical(test_df$b[1:5]), rep(NA, 5))
  )

  expect_identical(
    chk_filter_any(test_df, c(b, c), chk_range, a <= 5, list(min = 0, max = 1)),
    c(rep(TRUE, 5), rep(NA, 5))
  )

  expect_true(
    all(chk_filter_all(mtcars, everything(), is.numeric)),
    label = "Ensure `everything()` works in chk_filter functions"
  )

  expect_identical(
    length(chk_filter(iris, where(is.numeric), chk_range, args = list(min = 1000, max = Inf))),
    4L,
  )
})
