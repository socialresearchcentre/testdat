#' Deprecated expectation functions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{testdat:::lifecycle("soft-deprecated")}
#'
#' These functions are deprecated.
#'
#' See [Generic Expectation Functions][generic-expectations] and [Cross-dataset
#' Expectations][datacomp-expectations] for current expectation functions.
#'
#' @inheritParams data-params
#' @param ... arguments to pass to `expect_allany()`
#' @name expect-deprec
#' @keywords internal
NULL

#' @export
#' @rdname expect-deprec
#' @seealso [Generic Expectation Functions][generic-expectations]
expect_func <- function(var, ...) {
  signal_soft_deprecated("`expect_func()` is soft-deprecated as of testdat 0.2.0. Use `expect_all()` instead.")
  expect_allany(vars(!!ensym(var)), ..., allany = chk_filter_all)
}

#' @export
#' @rdname expect-deprec
#' @inheritParams datacomp-expectations
#' @seealso [Cross-dataset Expectations][datacomp-expectations]
expect_join <- function(data2, by = NULL, not = FALSE, flt = TRUE, data = get_testdata()) {
  signal_soft_deprecated("`expect_join()` is soft-deprecated as of testdat 0.2.0. Use `expect_subset()` instead.")
  flt <- rlang::enexpr(flt)
  expect_subset(data2 = data2, by = by, not = not, flt = !!flt, data = data)
}

#' Filter data to expectation result
#'
#' \Sexpr[results=rd, stage=render]{testdat:::lifecycle("defunct")}
#'
#' This function is defunct
#'
#' @param data A data frame to test.
#' @param expect_function An expectation function.
#' @param ... Arguments to pass to expect_function.
#' @param not Reverse the results of the check.
#' @return The input data frame filtered to records failing the expectation.
#' @export
#' @keywords internal
filter_expect <- function(data, expect_function, ..., not = TRUE) {
  stop_defunct("`filter_expect()` is defunct as of testdat 0.2.0.")
  expect_result <- expect_function(..., data = data)
  if (not) expect_result <- !expect_result

  data %>% filter(expect_result)
}
