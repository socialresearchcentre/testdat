test_that("excel_results", {
  my_fp <- system.file("test-testdat_example_test.R", package = "testdat")
  x <- test_file(my_fp, reporter = SilentReporter)
  x_summ <- summarise_results_excel(x)

  # Check that summary looks right
  expect_s3_class(x_summ, "tbl_df")
  expect_success(expect_values(status, "success", data = x_summ[x_summ$test == "passes", ]))
  expect_success(expect_values(status, "failure", data = x_summ[x_summ$test == "fails", ]))

  file_out <- paste0("temp_", rlang::hash("temp"), ".xlsx")
  output_results_excel(x, file_out)
  x_xl_summary <- openxlsx::read.xlsx(file_out, "__Summary")
  x_xl_passing <- openxlsx::read.xlsx(file_out, "passing_tests")
  x_xl_failing <- openxlsx::read.xlsx(file_out, "failing_tests")
  file.remove(file_out)

  # Check that .xlsx file looks good
  hlink <- function(x) glue::glue("=HYPERLINK(\"#'{str_sub(x, 1, 30)}'!A1\",\"{x}\")")

  xl_summary <- data.frame(
    stringsAsFactors = FALSE,
    context = c(hlink("failing_tests"), hlink("passing_tests")),
    tests = c(2L, 3L),
    failed = c(2L, 0L),
    error = c(0L, 0L),
    skipped = c(0L, 0L),
    warning = c(0L, 0L)
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

  expect_equal(x_xl_summary, xl_summary)
  expect_equal(x_xl_failing, xl_failing)
  expect_equal(names(x_xl_passing), names(xl_failing))
  expect_equal(nrow(x_xl_passing), 0)
})
