test_that("excel_results", {
  my_fp <- system.file("test-testdat_example_test.R", package = "testdat")
  x <- test_file(my_fp, reporter = SilentReporter)
  x_summ <- summarise_results_excel(x)

  # Check that summary looks right
  expect_s3_class(x_summ, "tbl_df")
  expect_success(expect_values(status, "success", data = x_summ[x_summ$test == "passes", ]))
  expect_success(expect_values(status, "failure", data = x_summ[x_summ$test == "fails", ]))

  file_out <- tempfile(fileext = ".xlsx")
  output_results_excel(x, file_out)
  x_xl_summary <- openxlsx::read.xlsx(file_out, "__Summary")
  x_xl_erroring <- openxlsx::read.xlsx(file_out, "erroring_tests")
  x_xl_passing <- openxlsx::read.xlsx(file_out, "passing_tests")
  x_xl_failing <- openxlsx::read.xlsx(file_out, "failing_tests")
  unlink(file_out)

  xl_summary <- data.frame(
    stringsAsFactors = FALSE,
    context = NA_real_, # Formulas evaluate to NA if workbook hasn't been opened
    tests = c(1L, 2L, 3L),
    failed = c(0L, 2L, 0L),
    error = c(1L, 0L, 0L),
    skipped = c(0L, 0L, 0L),
    warning = c(0L, 0L, 0L)
  )

  xl_failing <- data.frame(
    stringsAsFactors = FALSE,
    context = c("failing_tests", "failing_tests"),
    test = c("fails", "fails"),
    status = c("failure", "failure"),
    variable = c("my_string", "my_real"),
    description = c("`mydf` has 7 records failing pattern check on variable `my_string`. Filter: None Arguments: `pattern = \"Merc\"`",
                    "`mydf` has 4 records failing range check on variable `my_real`. Filter: None Arguments: `min = 0, max = 0.4`"),
    failed_records = c(7L, 4L),
    total_records = c(10L, 10L),
    call = c("NULL", "NULL")
  )

  xl_erroring <- data.frame(
    stringsAsFactors = FALSE,
    context = "erroring_tests",
    test = "errors",
    status = "error",
    variable = NA_real_,
    description = "<vctrs_error_subscript_oob/vctrs_error_subscript/rlang_error/error/condition> Error: Can't subset columns that don't exist. [31mx[39m Column `doesnt_exist` doesn't exist.",
    failed_records = NA_real_,
    total_records = NA_real_,
    call = "NULL"
  )

  expect_equal(x_xl_erroring[-5], xl_erroring[-5]) # Drop description
  expect_match(x_xl_erroring$description, "Error:")
  expect_equal(x_xl_summary, xl_summary)
  expect_equal(x_xl_failing, xl_failing)
  expect_equal(names(x_xl_passing), names(xl_failing))
  expect_equal(nrow(x_xl_passing), 0)
})
