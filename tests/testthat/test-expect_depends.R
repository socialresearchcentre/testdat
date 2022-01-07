test_that("expect_depends works", {
  # y depends on x
  dat <- data.frame(
    x = c(1, 1, 2, 2, 3, 3),
    y = c(0, 0, 0, 0, 1, 1)
  )
  expect_success(expect_depends(y, x, data = dat))

  # y doesn't depend on x
  dat <- data.frame(
    x = c(1, 1, 2, 2, 3, 3),
    y = c(0, 0, 0, 1, 1, 1)
  )
  expect_failure(expect_depends(y, x, data = dat))

  # y and z depend on x
  dat <- data.frame(
    x = c(1, 1, 2, 2, 3, 3),
    y = c(0, 0, 0, 0, 1, 1),
    z = c(0, 0, 0, 0, 0, 0)
  )
  expect_success(expect_depends(c(y, z), x, data = dat))

  # y and z do not depend on x
  dat <- data.frame(
    x = c(1, 1, 2, 2, 3, 3),
    y = c(0, 0, 0, 1, 1, 1),
    z = c(0, 0, 0, 0, 0, 0)
  )
  expect_failure(expect_depends(c(y, z), x, data = dat))

  # y and z depend on x and w
  dat <- data.frame(
    w = c(1, 1, 1, 2, 2, 2),
    x = c(1, 1, 2, 2, 3, 3),
    y = c(0, 0, 0, 99, 1, 1),
    z = c(0, 0, 0, 0, 0, 0)
  )
  expect_success(expect_depends(c(y, z), c(w, x), data = dat))

  # y and z do not depend on x and w
  dat <- data.frame(
    w = c(1, 1, 1, 2, 2, 2),
    x = c(1, 1, 2, 2, 3, 3),
    y = c(0, 99, 0, 99, 1, 1),
    z = c(0, 0, 0, 0, 0, 0)
  )
  expect_failure(expect_depends(c(y, z), c(w, x), data = dat))

})
