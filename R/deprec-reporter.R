
#' Deprecated reporter functions
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' These functions are deprecated.
#'
#' See [Get/set test data][global-data] for working with global test data.
#'
#' @inheritParams global-data
#' @name reporter-deprec
#' @keywords internal
NULL

#' @export
#' @rdname reporter-deprec
#' @seealso [Get/set test data][global-data]
context_data <- function(data) {
  lifecycle::deprecate_soft("0.3.0", "context_data()", "set_testdata()")
  set_testdata(data)
}
