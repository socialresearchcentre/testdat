# nocov start --- chk-deprec --- 2021-02-01 Wed 14:53

#' Deprecated expectation functions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{testdat:::lifecycle("soft-deprecated")}
#'
#' These functions are deprecated.
#'
#' See [Generic Expectation Functions][generic-expectations] for current expectation functions.
#'
#' @inheritParams data-params
#' @param ... arguments to pass to `expect_allany()`
#' @name expect-deprec
NULL

#' @export
#' @rdname expect-deprec
#' @seealso [Generic Expectation Functions][generic-expectations]
expect_func <- function(var, ...) {
  signal_soft_deprecated("`expect_func()` is soft-deprecated as of testdat 0.2.0. Use `expect_all()` instead.")
  expect_allany(vars(!!ensym(var)), ..., allany = chk_filter_all)
}

# nocov end
