#' R package for data unit testing
#'
#' An adaptation of [testthat] for data. See vigenttes for details.
#'
#' @section Options:
#' - `testdat.miss`: A vector of values to consider missing (default: `c(NA, "")`).
#' - `testdat.miss_text`: A vector of values to consider missing in text variables
#'    (default: `c("error", "null", "0", ".", "-", ",", "na", "#n/a", "", NA)`).
#'    reports printed for the summary reporter (default: 10).
#' - `testdat.stop_on_fail`: Should an expectation raise an error on failure?
#'    Useful for interactive use of expectation functions (default: `FALSE`).
#'
#' @import rlang
#' @import dplyr
#' @import stringr
#' @keywords internal
"_PACKAGE"
