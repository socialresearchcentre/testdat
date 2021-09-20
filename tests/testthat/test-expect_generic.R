
test_that("basic examples", {
  expect_success(expect_all(c(vs, am), chk_values, args = list(0:1), data = mtcars))
  expect_failure(expect_all(c(vs, carb), chk_values, args = list(0:1), data = mtcars))

  expect_success(expect_any(c(vs, carb), chk_values, args = list(0:1), data = mtcars))
  expect_failure(expect_any(c(cyl, carb), chk_values, args = list(0:1), data = mtcars))
})


test_that("expect_where", {
  expect_identical(
    capture_expectation(expect_all(c(vs, am), chk_values, args = list(0:1), data = mtcars)),
    capture_expectation(expect_where(where(function(x) all(x %in% 0:1)), chk_values, args = list(0:1), data = mtcars)),
  )
  expect_identical(
    capture_expectation(expect_all(qsec, chk_range, flt = mpg < 25, args = list(min = 18, max = Inf), data = mtcars)),
    capture_expectation(expect_where(all_of("qsec"), chk_range, flt = mpg < 25, args = list(min = 18, max = Inf), data = mtcars)),
  )
})
