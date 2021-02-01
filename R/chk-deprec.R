# nocov start --- chk-deprec --- 2019-04-24 Wed 17:53

#' Deprecated checking functions
#'
#' @description
#'
#' \Sexpr[results=rd, stage=render]{testdat:::lifecycle("deprecated")}
#'
#' These functions are deprecated. See [Generic Checking Functions][chk-generic] for current checking functions.
#'
#' @inheritParams chk-generic
#' @return A logical vector flagging records that have passed or failed the check
#' @seealso [Generic Checking Functions][chk-generic]
#' @name chk-deprec
NULL

#' @rdname chk-deprec
#' @param len maximum string length for checking string variables
#' @importFrom stringr str_length
#' @export
chk_length <- function(x, len) {
  warn_deprecated("`chk_length()` is deprecated as of testdat 0.2.0. Use `chk_max_length()` instead.")
  chk_blank(x) | str_length(x) <= len
}

#' @rdname chk-deprec
#' @export
chk_miss <- function(x, miss = getOption("testdat.miss_text")) {
  warn_deprecated("`chk_miss()` is deprecated as of testdat 0.2.0. Use `chk_text_miss()` instead.")
  tolower(x) %in% miss
}

#' @rdname chk-deprec
#' @export
chk_nmiss <- function(x, miss = getOption("testdat.miss_text")) {
  warn_deprecated("`chk_nmiss()` is deprecated as of testdat 0.2.0. Use `chk_text_nmiss()` instead.")
  !chk_miss(x, miss)
}

# nocov end
