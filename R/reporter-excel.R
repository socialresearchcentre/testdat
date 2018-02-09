
#' Excel reporter: gather all test results along with elapsed time and
#' file information.
#'
#' This reporter gathers all results, adding additional information such as
#' test elapsed time, and test filename if available. Very useful for reporting.
#'
#' @export
#' @family reporters
ExcelReporter <-
  R6::R6Class("ExcelReporter",
              inherit = Reporter,
              public = list(
                current_start_time = NA,
                current_expectations = NULL,
                current_file = NULL,
                results = NULL,

                context = NULL,
                workbook = NULL,
                output_path = NULL,
                context_df = NULL,

                initialize = function() {
                  super$initialize()
                  self$results <- testthat:::Stack$new()
                },

                start_reporter = function() {
                  self$workbook <- self$create_workbook()
                },

                start_context = function(context) {
                  if (is.null(self$workbook)) {
                    self$workbook <- self$create_workbook()
                  }
                  self$context <- context
                  self$add_worksheet(self$context)
                  self$current_expectations <- testthat:::Stack$new()
                },

                start_test = function(context, test) {
                },

                add_result = function(context, test, result) {
                  if (!is.null(self$current_expectations)) {
                    self$current_expectations$push(result)
                  }
                },

                end_test = function(context, test) {
                },

                end_context = function(context) {
                  self$write_sheet()
                },

                end_reporter = function() {
                  self$output_results()
                },

                create_workbook = function() {
                  wb <- openxlsx::createWorkbook()
                  openxlsx::modifyBaseFont(wb, fontSize = 8, fontName = "Arial")
                  wb
                },

                add_worksheet = function(sheet) {
                  openxlsx::addWorksheet(self$workbook, sheet)
                },

                write_sheet = function() {
                  openxlsx::writeData(self$workbook, self$context,
                                      summarise_results(self$current_expectations$as_list()),
                                      headerStyle = openxlsx::createStyle(textDecoration = "bold"))
                },

                set_output = function(file) {
                  self$output_path <- file
                },

                output_results = function() {
                  self$write_sheet()
                  openxlsx::saveWorkbook(self$workbook, self$output_path, overwrite = TRUE)
                },

                get_results = function() {
                  testthat_results(self$results$as_list())
                }
              ))


summarise_results <- function(results) {
  lapply(results, function(d) {
    tibble(test = d$test,
           status = testthat:::expectation_type(d),
           description = str_replace_all(d$message, "[[:space:]]", " "),
           failed = ifelse(is.null(d$custom$failed_count), NA_real_, d$custom$failed_count),
           total_records = ifelse(is.null(d$custom$total_count), NA_real_, d$custom$total_count),
           call = expr_text(d$expectation_call[[1]]))
  }) %>% bind_rows
}

summarise_results_excel <- function(results) {
  lapply(results, function(e) {
    lapply(e$results, function(d) {
      tibble(test = d$test,
             status = testthat:::expectation_type(d),
             description = str_replace_all(d$message, "[[:space:]]", " "),
             failed_records = ifelse(is.null(d$custom$failed_count), NA_real_, d$custom$failed_count),
             total_records = ifelse(is.null(d$custom$total_count), NA_real_, d$custom$total_count),
             call = expr_text(d$expectation_call[[1]]))
    }) %>%
      bind_rows %>%
      mutate(context = e$context)
  }) %>%
    bind_rows %>%
    select(context, everything())
}

#' Output ListReporter results in Excel format
#'
#' THis function outputs ListReporter results to an Excel workbook. The workbook
#' consists of a summary sheet showing aggregarated results for each context,
#' and one sheet per context showing details of each unsuccessful test.
#'
#' @param results A object of class `testthat_results`, e.g. output from
#'   [test_dir()] or [test_file()]
#' @examples
#' \donotrun{
#' # Output the results from running all tests in a directory
#' x <- test_dir(".")
#' class(x)
#' output_results_excel(x, "Test results.xlsx")
#' }
#' @import openxlsx
#' @export
output_results_excel <- function(results, file) {
  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontSize = 8, fontName = "Arial")

  results_df <- summarise_results_excel(results)

  # Create summary page
  openxlsx::addWorksheet(wb, "__Summary")
  summary <- results_df %>%
    group_by(context) %>%
    summarise(tests = n(),
              failed = sum(status %in% "failure"),
              error = sum(status %in% "error"),
              skipped = sum(status %in% "skip"),
              warning = sum(status %in% "warning")) %>%
    ungroup %>%
    mutate(context = glue::glue("=HYPERLINK(\"#'{str_sub(context, 1, 30)}'!A1\",\"{context}\")"))

  openxlsx::writeData(wb, "__Summary", summary,
                      headerStyle = openxlsx::createStyle(textDecoration = "bold"))

  openxlsx::writeFormula(wb, "__Summary", summary$context, 1, 2)

  for (sheet in unique(results_df$context)) {
    openxlsx::addWorksheet(wb, str_sub(sheet, 1, 30))
    openxlsx::writeData(wb, str_sub(sheet, 1, 30),
                        results_df %>% filter(context == sheet, status != "success"),
                        headerStyle = openxlsx::createStyle(textDecoration = "bold"))
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
