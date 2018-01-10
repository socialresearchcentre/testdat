#' R package for data unit testing
#'
#' An adaptation of [testthat] for data. See vigenttes for details.
#'
#' @section Options:
#' - `testdat.miss`: A vector of values to consider missing (default: `c(NA, "")`).
#' - `testdat.miss_text`: A vector of values to consider missing in text variables
#'    (default: `c("error", "null", "0", ".", "-", ",", "na", "#n/a", "", NA)`).
#'    reports printed for the summary reporter (default: 10).
#'
#' @import rlang
#' @import dplyr
#' @keywords internal
"_PACKAGE"
