#' Checking helper functions
#'
#' These helper functions allowing easy checking using an arbitrary function
#' (`func`) over multiple columns (`vars`) of a data frame (`data`), with an
#' optional filter (`flt`).
#'
#' * `chk_filter()` applies `func` with `args` to `var` in `data` filtered with
#' `flt` and returns the resulting logical vector.
#'
#' * `chk_filter_vars()` applies `func` with `args` to `vars` in `data` filtered
#' with `flt` and returns a data frame containing the resulting logical vectors.
#'
#' * `chk_filter_all()` and `chk_filter_any()` both run `chk_filter_vars()` and
#' return a single logical vector flagging whether *all* or *any* values in each
#' row are `TRUE` (i.e. the conjunction and disjunction, respectively, of the
#' columns in the output of `chk_filter_vars()`).
#'
#' * `chk_filter_where()` works exactly like `chk_filter_all()`. When testdat
#' used `dplyr::vars()` as standard `chk_filter_where()` provided an alternative
#' interface using [`tidy-select`][dplyr_tidy_select]. This is included for
#' backwards compatibility and may be removed in future.
#'
#' @param data A data frame to check.
#' @param var An unquoted column name to check.
#' @param vars <[`tidy-select`][dplyr::dplyr_tidy_select]> A set of columns to
#'   check.
#' @param func A function to use for checking that takes a vector as the first
#'   argument and returns a logical vector of the same length showing whether an
#'   element passed or failed.
#' @param flt <[`data-masking`][dplyr::dplyr_data_masking]> A filter specifying
#'   a subset of the data frame to test.
#' @param args A list of additional arguments to be added to the function calls.
#' @return A logical vector flagging records that have passed or failed the
#'   check, with `NA` where records do not meet the filter condition.
#'
#' @seealso [Generic Checking Functions][chk-generic]
#' @name chk-helper
#' @examples
#'
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches
#' x <- chk_filter(mtcars, disp, chk_range, cyl == 4, list(min = 0, max = 100))
#' all(x, na.rm = TRUE)
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
#' # Check that columns made up of whole numbers are binary
#' chk_filter_where(
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
chk_filter <- function(data, var, func, flt = TRUE, args = list()) {
  var <- enquo(var)
  flt <- enquo(flt)

  # data %>% mutate(.chk = if_else(!!flt, func(!!var), NA)) %>% pull(.chk)
  data %>%
    mutate(.cond = !!flt) %>%
    mutate_at(quos(!!var), func, !!!args) %>%
    mutate_at(quos(!!var), ~ifelse(.cond, ., NA)) %>%
    # select(!!!var)
    pull(!!var)
}

#' @rdname chk-helper
#' @export
chk_filter_vars <- function(data, vars, func, flt = TRUE, args = list()) {
  flt <- data %>%
    transmute(.cond = {{ flt }})

  data %>%
    mutate(across({{ vars }}, func, !!!args)) %>%
    select({{ vars }}) %>%
    mutate(across(everything(), ~ifelse(flt$.cond, ., NA)))
}

#' @rdname chk-helper
#' @export
chk_filter_all <- function(data, vars, func, flt = TRUE, args = list()) {
  chk_filter_vars(data, {{ vars }}, func, {{ flt }}, args) %>%
    apply(1, all)
}

#' @rdname chk-helper
#' @export
chk_filter_any <- function(data, vars, func, flt = TRUE, args = list()) {
  chk_filter_vars(data, {{ vars }}, func, {{ flt }}, args) %>%
    apply(1, any)
}

#' @rdname chk-helper
#' @param where <[`tidy-select`][dplyr_tidy_select]> Columns to check.
#' @export
chk_filter_where <- function(data, where, func, flt = TRUE, args = list()) {
  flt <- enquo(flt)
  where <- enquo(where)

  data <- data %>%
    mutate(.cond = !!flt) %>%
    select(!!where, .data$.cond)

  data %>%
    mutate(across(-.data$.cond, func, !!!args)) %>%
    mutate(across(-.data$.cond, ~ifelse(.data$.cond, ., NA))) %>%
    select(-.data$.cond) %>%
    apply(1, all)
}
