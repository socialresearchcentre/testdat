test_that("basic examples", {
  # Create and check custom expectation
  expect_binary_var <- expect_make(function(x) {
    suppressWarnings(as.integer(x) %in% 0:1)
  })
  expect_binary_vars <- expect_make(function(x) {
    suppressWarnings(as.integer(x) %in% 0:1)
  }, vars = TRUE)

  expect_success(expect_binary_var(vs, data = mtcars))
  expect_success(expect_binary_vars(c("vs", "am"), data = mtcars))
  expect_failure(expect_binary_var(cyl, data = mtcars))
  expect_failure(expect_binary_vars(c("cyl", "am"), data = mtcars))
})

test_that("automatically generated expectations", {
  my_dates <- data.frame(
    yyyymmdd_bad = c("20200101", "20200101", "20200102", "20200103", "2020003"),
    yyyymmdd_good = c("20200101", "20200101", "20200102", "20200103", "20200103"),
    yyyymm_bad = c("202001", "202001", "202001", "202001", "20200"),
    yyyymm_good = c("202001", "202001", "202001", "202001", "202001"),
    yyyy_missing = c("2000", "2020", "2021", "2020", "#n/a"),
    stringsAsFactors = FALSE
  )

  expect_failure(expect_date_yyyy(yyyy_missing, data = my_dates))
  expect_failure(expect_text_nmiss(yyyy_missing, data = my_dates))
  expect_failure(expect_date_yyyymm(yyyymm_bad, data = my_dates))
  expect_failure(expect_date_yyyymmdd(yyyymmdd_bad, data = my_dates))

  expect_success(expect_date_yyyymm(yyyymm_good, data = my_dates))
  expect_success(expect_date_yyyymmdd(yyyymmdd_good, data = my_dates))
})
