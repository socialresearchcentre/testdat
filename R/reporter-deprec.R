
#' Deprecated reporter functions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{testdat:::lifecycle("soft-deprecated")}
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
  signal_soft_deprecated("`context_data()` is soft-deprecated as of testdat 0.3.0. Use `set_testdata()` instead.")
  set_testdata(data)
}
