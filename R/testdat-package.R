#' @section Options:
#' - `testdat.miss`: A vector of values to consider missing
#'    (default: `c(NA, "")`).
#' - `testdat.miss_text`: A vector of values to consider missing in text
#'    variables
#'    (default: `c("error", "null", "0", ".", "-", ",", "na", "#n/a", "", NA)`).
#' - `testdat.stop_on_fail`: Should an expectation raise an error on failure?
#'    Useful for interactive use of expectation functions (default: `TRUE`).
#' - `testdat.scipen`: When it is necessary to convert a numeric vector to
#'    character for checking, this value will be used for `scipen`
#'    (default: `999`).
#'
#' @import rlang
#' @import dplyr
#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
