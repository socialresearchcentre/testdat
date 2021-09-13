test_that("chk_blank works", {
  expect_equal(chk_blank(c(1, NA)), c(FALSE, TRUE))
  expect_equal(chk_blank(c("a", "", NA)), c(FALSE, TRUE, TRUE))
  expect_equal(chk_blank(as.factor(c("a", "", NA))), c(FALSE, TRUE, TRUE))
})
