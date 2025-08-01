#' Output `ListReporter` results in Excel format
#'
#' Output formatted `ListReporter` results to an Excel workbook using
#' [openxlsx][openxlsx::openxlsx]. The workbook consists of a summary sheet
#' showing aggregated results for each context, and one sheet per context
#' showing details of each unsuccessful test.
#'
#' @param results An object of class `testthat_results`, e.g. output from
#'   [testthat::test_dir()] or [testthat::test_file()].
#' @param file Output file name
#' @return The return value of [openxlsx::saveWorkbook()].
#' @examples
#' \dontrun{
#' # Output the results from running all tests in a directory
#' x <- test_dir(".")
#' output_results_excel(x, "Test results.xlsx")
#' }
#' @export
output_results_excel <- function(results, file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop(
      "Package \"openxlsx\" needed for this function to work.",
      "Please install it.",
      call. = FALSE)
  }

  if (!requireNamespace("crayon", quietly = TRUE)) {
    stop(
      "Package \"crayon\" needed for this function to work.",
      "Please install it.",
      call. = FALSE)
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::modifyBaseFont(wb, fontSize = 8, fontName = "Arial")

  results_df <- summarise_results_excel(results)

  # Create summary page
  openxlsx::addWorksheet(wb, "__Summary")
  summary <- results_df %>%
    group_by(.data$context) %>%
    summarise(
      tests   = n(),
      failed  = sum(.data$status %in% "failure"),
      error   = sum(.data$status %in% "error"),
      skipped = sum(.data$status %in% "skip"),
      warning = sum(.data$status %in% "warning")
    ) %>%
    ungroup %>%
    mutate(
      context = glue::glue("=HYPERLINK(\"#'{str_sub(.data$context, 1, 30)}'!A1\",\"{.data$context}\")")
    )

  openxlsx::writeData(
    wb, "__Summary",
    summary,
    headerStyle = openxlsx::createStyle(textDecoration = "bold")
  )

  openxlsx::writeFormula(wb, "__Summary", summary$context, 1, 2)

  for (sheet in unique(results_df$context)) {
    openxlsx::addWorksheet(wb, str_sub(sheet, 1, 30))
    openxlsx::writeData(
      wb, str_sub(sheet, 1, 30),
      results_df %>% filter(.data$context == sheet, .data$status != "success"),
      headerStyle = openxlsx::createStyle(textDecoration = "bold")
    )
  }
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

#' @importFrom stringr str_replace_all str_sub
summarise_results_excel <- function(results) {
  lapply(results, function(e) {
    lapply(e$results, function(d) {
      tibble(
        test = d$test,
        status = expectation_type(d),
        variable = ifelse(is.null(d$custom$var_desc), NA_character_, d$custom$var_desc),
        description = crayon::strip_style(str_replace_all(d$message, "[[:space:]]", " ")),
        failed_records = ifelse(is.null(d$custom$failed_count), NA_real_, d$custom$failed_count),
        total_records = ifelse(is.null(d$custom$total_count), NA_real_, d$custom$total_count),
        call = expr_text(d$expectation_call[[1]])
      )
    }) %>%
      bind_rows %>%
      mutate(context = e$context)
  }) %>%
    bind_rows %>%
    select("context", everything())
}
