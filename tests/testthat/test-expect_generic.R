
test_that("basic examples", {
  expect_success(expect_all(c(vs, am), chk_values, args = list(0:1), data = mtcars))
  expect_failure(expect_all(c(vs, carb), chk_values, args = list(0:1), data = mtcars))

  expect_success(expect_any(c(vs, carb), chk_values, args = list(0:1), data = mtcars))
  expect_failure(expect_any(c(cyl, carb), chk_values, args = list(0:1), data = mtcars))
})

