#' Deprecated checking functions
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated.
#'
#' @inheritParams chk-dummy
#' @return A logical vector flagging records that have passed or failed the
#'   check.
#' @seealso `chk_*()` functions such as [`chk_values()`][chk-values]
#' @seealso [Checking Helper Functions][chk-helper]
#' @name chk-deprec
#' @keywords internal
NULL

#' @rdname chk-deprec
#' @param len Maximum string length for checking string variables.
#' @importFrom stringr str_length
#' @export
chk_length <- function(x, len) {
  lifecycle::deprecate_warn("0.2.0", "chk_length()", "chk_max_length()")
  chk_blank(x) | str_length(x) <= len
}

#' @rdname chk-deprec
#' @export
chk_miss <- function(x, miss = getOption("testdat.miss_text")) {
  lifecycle::deprecate_warn("0.2.0", "chk_miss()", "chk_text_miss()")
  tolower(x) %in% miss
}

#' @rdname chk-deprec
#' @export
chk_nmiss <- function(x, miss = getOption("testdat.miss_text")) {
  lifecycle::deprecate_warn("0.2.0", "chk_nmiss()", "chk_text_nmiss()")
  !chk_miss(x, miss)
}


#' Defunct checking functions
#'
#' @description
#'
#' `r lifecycle::badge("defunct")`
#'
#' These functions are defunct.
#'
#' * `chk_filter_where()` works exactly like `chk_filter_all()`. When testdat
#' used `dplyr::vars()` as standard `chk_filter_where()` provided an alternative
#' interface using [`tidy-select`][dplyr_tidy_select].
#'
#' @inheritParams chk-dummy
#' @return A logical vector flagging records that have passed or failed the
#'   check.
#' @seealso `chk_*()` functions such as [`chk_values()`][chk-values]
#' @seealso [Checking Helper Functions][chk-helper]
#' @name chk-defunct
#' @keywords internal
NULL

#' @rdname chk-defunct
#' @export
chk_filter_vars <- function(data, vars, func, flt = TRUE, args = list()) {
  lifecycle::deprecate_stop("0.3.0", "chk_filter_vars()", "chk_filter()")
}

#' @rdname chk-defunct
#' @param where <[`tidy-select`][dplyr_tidy_select]> Columns to check.
#' @export
chk_filter_where <- function(data, where, func, flt = TRUE, args = list()) {
  lifecycle::deprecate_stop("0.3.0", "chk_filter_where()", "chk_filter_all()")
}
