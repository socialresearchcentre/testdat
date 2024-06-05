#' Checks: data frame helpers
#'
#' These helper functions allowing easy checking using an arbitrary function
#' (`func`) over multiple columns (`vars`) of a data frame (`data`), with an
#' optional filter (`flt`).
#'
#' * `chk_filter()` applies `func` with `args` to `vars` in `data` filtered
#' with `flt` and returns a data frame containing the resulting logical vectors.
#'
#' * `chk_filter_all()` and `chk_filter_any()` both run `chk_filter()` and
#' return a single logical vector flagging whether *all* or *any* values in each
#' row are `TRUE` (i.e. the conjunction and disjunction, respectively, of the
#' columns in the output of `chk_filter()`).
#'
#' @param data A data frame to check.
#' @param vars <[`tidy-select`][dplyr::dplyr_tidy_select]> A set of columns to
#'   check.
#' @param func A function to use for checking that takes a vector as the first
#'   argument and returns a logical vector of the same length showing whether an
#'   element passed or failed.
#' @param flt <[`data-masking`][dplyr::dplyr_data_masking]> A filter specifying
#'   a subset of the data frame to test.
#' @param args A list of additional arguments to be added to the function calls.
#' @return A logical vector or data frame of logical vectors flagging records
#'   that have passed or failed the check, with `NA` where records do not meet
#'   the filter condition.
#'
#' @seealso Other `chk_*()` functions such as [`chk_values()`][chk-values]
#' @name chk-helper
#' @examples
#'
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches AND < 100 horsepower - return a data frame
#' chk_filter(
#'   mtcars,
#'   c("disp", "hp"),
#'   chk_range,
#'   cyl == 4,
#'   list(min = 0, max = 100)
#' )
#'
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches AND < 100 horsepower
#' chk_filter_all(
#'   mtcars,
#'   c("disp", "hp"),
#'   chk_range,
#'   cyl == 4,
#'   list(min = 0, max = 100)
#' )
#'
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches OR < 100 horsepower
#' chk_filter_any(
#'   mtcars,
#'   c("disp", "hp"),
#'   chk_range,
#'   cyl == 4,
#'   list(min = 0, max = 100)
#' )
#'
#' # Check that columns made up of whole numbers are binary
#' chk_filter_all(
#'   mtcars,
#'   where(~ all(. %% 1 == 0)),
#'   chk_values,
#'   TRUE,
#'   list(0:1)
#' )
#'
NULL

#' @rdname chk-helper
#' @export
chk_filter <- function(data, vars, func, flt = TRUE, args = list()) {
  flt <- data %>%
    transmute(.cond = {{ flt }})

  data %>%
    select({{ vars }}) %>%
    mutate(across({{ vars }}, ~func(.x, !!!args))) %>%
    mutate(across(everything(), ~ifelse(flt$.cond, ., NA)))
}

#' @rdname chk-helper
#' @export
chk_filter_all <- function(data, vars, func, flt = TRUE, args = list()) {
  chk_filter(data, {{ vars }}, func, {{ flt }}, args) %>%
    apply(1, all)
}

#' @rdname chk-helper
#' @export
chk_filter_any <- function(data, vars, func, flt = TRUE, args = list()) {
  chk_filter(data, {{ vars }}, func, {{ flt }}, args) %>%
    apply(1, any)
}
