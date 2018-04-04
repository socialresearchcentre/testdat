
#' Get/set test data
#'
#' A global test data set is used to avoid having to re-specify the testing
#' dataset in every test. These functions get and set the global data or set the
#' data for the current context.
#'
#' @keywords internal
#' @param data data to be used
#' @examples
#' set_testdata(mtcars)
#' get_testdata()
#' @name global-data
NULL

#' @export
#' @rdname global-data
set_testdata <- function(data) {
  old <- testthat:::testthat_env$test_data
  # testthat_env$test_data <- data
  assign("test_data", data, testthat:::testthat_env)
  invisible(old)
}

#' @export
#' @rdname global-data
get_testdata <- function() {
  dat <- testthat:::testthat_env$test_data

  if (is.null(dat))
    stop("A dataset has not been specified for the current context. ",
         "Use `context_data()` to set the dataset.",
         call. = FALSE)

  return(dat)
}


#' @export
#' @rdname global-data
context_data <- function(data) {
  set_testdata(data)
}

#' @export
start_data_test <- function(context, data, reporter = default_reporter()) {
  current_reporter <- find_reporter(reporter)
  set_reporter(current_reporter)

  context(context)
  context_data(data)
}

#' @export
end_data_test <- function() {
  get_reporter()$.end_context()
}

data_reporter <- function() {
  "data"
}
