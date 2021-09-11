test_that("chk_range", {
  expect_true(chk_range(1, 1, 10))
  expect_true(chk_range(10, 1, 10))
  expect_true(chk_range(NA, 1, 10))

  expect_false(chk_range(0, 1, 10))
  expect_false(chk_range(11, 1, 10))
})

test_that("chk_range handles character format correctly", {
  expect_true(chk_range("1", 1, 10))
  expect_true(chk_range("10", 1, 10))
  expect_true(chk_range("01", 1, 10))
  expect_true(chk_range("010", 1, 10))
  expect_true(chk_range("", 1, 10))

  expect_false(chk_range("0", 1, 10))
  expect_false(chk_range("00", 1, 10))
  expect_false(chk_range("011", 1, 10))
  expect_false(chk_range("a", 1, 10))
})
