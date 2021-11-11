#' @importFrom glue glue glue_data
NULL

# Alias required for help links in downstream packages
#' @aliases select_helpers
#' @importFrom tidyselect contains
#' @export
tidyselect::contains
#' @importFrom tidyselect ends_with
#' @export
tidyselect::ends_with
#' @importFrom tidyselect everything
#' @export
tidyselect::everything
#' @importFrom tidyselect matches
#' @export
tidyselect::matches
#' @importFrom tidyselect num_range
#' @export
tidyselect::num_range
#' @importFrom tidyselect one_of
#' @export
tidyselect::one_of
#' @importFrom tidyselect starts_with
#' @export
tidyselect::starts_with
#' @importFrom tidyselect last_col
#' @export
tidyselect::last_col
#' @importFrom tidyselect any_of
#' @export
tidyselect::any_of
#' @importFrom tidyselect all_of
#' @export
tidyselect::all_of

#' Expectation params
#' @keywords internal
#' @param var An unquoted column name to test.
#' @param vars <[`tidy-select`][dplyr::dplyr_tidy_select]> A set of columns to
#'   test.
#' @param flt <[`data-masking`][dplyr::dplyr_data_masking]> A filter specifying
#'   a subset of the data frame to test.
#' @param miss A vector of values to be treated as missing. The
#'   [testdat.miss][testdat] option is used by default.
#' @param data A data frame to test. The [global test data][global-data] is used
#'   by default.
#' @return `expect_*()` functions are mainly called for their side effects. The
#'   expectation signals its result (e.g. "success", "failure"), which is logged
#'   by the current [test reporter][testthat::Reporter]. In a non-testing
#'   context the expectation will raise an error with class
#'   `expectation_failure` if it fails.
#' @name data-params
NULL
