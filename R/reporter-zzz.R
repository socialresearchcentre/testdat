
#' Get/set test data
#'
#' Changes global test data to that specified. Used by data expectation functions
#'
#' @keywords internal
#' @param data data to be used
#' @name data-accessors
NULL

#' @rdname data-accessors
#' @export
set_testdata <- function(data) {
  old <- testthat:::testthat_env$test_data
  # testthat_env$test_data <- data
  assign("test_data", data, testthat:::testthat_env)
  invisible(old)
}

#' @rdname data-accessors
#' @export
get_testdata <- function() {
  testthat:::testthat_env$test_data
}


#' @export
context_data <- function(data) {
  set_testdata(data)
}

#' @export
start_data_test <- function(context, data) {
  # exists(data)
  set_reporter(ProgressReporter$new())
  context(context)
  context_data(data)
}

#' @export
end_data_test <- function(context, data) {
  get_reporter()$.end_context()
  # get_reporter()$get_results()
}

data_reporter <- function() {
  "data"
}
