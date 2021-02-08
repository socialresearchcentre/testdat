test_that("expect_values", {
  # We can check against expect_make
  my_expect_values <- expect_make(chk_values)
  for (i in 1:25) {
    x <- data.frame(key = sample(1:3, 2, replace = TRUE))
    my_res <- testthat::capture_error(my_expect_values(key, 1:2, data = x))
    pkg_res <- testthat::capture_error(expect_values(key, 1:2, data = x))
    expect_identical(class(my_res), class(pkg_res)) # Either both should be an error or both should be NULL
  }
})


