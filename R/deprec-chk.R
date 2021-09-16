#' Deprecated checking functions
#'
#' @description
#'
#' `r lifecycle::badge("deprecated")`
#'
#' These functions are deprecated. See [Generic Checking Functions][chk-generic] for current checking functions.
#'
#' @inheritParams chk-generic
#' @return A logical vector flagging records that have passed or failed the check
#' @seealso [Generic Checking Functions][chk-generic]
#' @name chk-deprec
#' @keywords internal
NULL

#' @rdname chk-deprec
#' @param len maximum string length for checking string variables
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
