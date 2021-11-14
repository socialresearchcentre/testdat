#' Expectations: exclusivity
#'
#' `expect_exclusive` tests that `vars` are exclusive - that, if any one of
#' `vars` is set to `exc_val`, no other column in `vars` or `var_set` is also
#' set to `exc_val`.
#'
#' This expectation is designed to check exclusivity in survey multiple response
#' sets, where one response is only valid on its own.
#'
#' See the example data set below:
#' * No record should have `q10_98`, "None of the above", selected while also
#' having any other response selected, so we refer to this as an "exclusive"
#' response.
#' * `expect_exclusive()` checks whether `q10_98` "None of the above" or
#' `q10_99` "Don't know", the exclusive responses, have been selected alongside
#' any other `q10_*` response.
#' * The expectation fails, since the first record has both `q10_1` and
#' `q10_98` selected.
#'
#' @inheritParams data-params
#' @param var_set <[`tidy-select`][dplyr::dplyr_tidy_select]> The full set of
#'   columns to check against. This should include all columns specified in the
#'   `vars` argument.
#' @param exc_val The value that flags a variable as "selected" (default: `1`)
#' @inherit data-params return
#' @family data expectations
#' @name exclusivity-expectations
#' @examples
#'
#' my_q_block <- data.frame(
#'   resp_id = 1:5, # Unique to respondent
#'   q10_1  = c(1, 1, 0, 0, 0),
#'   q10_2  = c(0, 1, 0, 0, 0),
#'   q10_3  = c(0, 0, 1, 0, 0),
#'   q10_98 = c(1, 0, 0, 1, 0), # None of the above
#'   q10_99 = c(0, 0, 0, 0, 1)  # Item not answered
#' )
#'
#' # Make sure that if "None of the above" and "Item skipped" are selected
#' # none of the other question options are selected:
#' try(
#' expect_exclusive(
#'   c(q10_98, q10_99),
#'   starts_with("q10_"),
#'   data = my_q_block
#' )
#' )
NULL

#' @importFrom tidyselect eval_select
#' @export
#' @rdname exclusivity-expectations
expect_exclusive <- function(vars,
                             var_set,
                             exc_val = 1,
                             flt = TRUE,
                             data = get_testdata()) {

  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$var_desc     <- as_label_vars(enquo(vars))
  act$var_set_desc <- as_label_vars(enquo(var_set))
  act$exc_val_desc <- as_label(enquo(exc_val))
  act$flt_desc     <- as_label_flt(enquo(flt))

  vars_list <- names(data)[
    eval_select(enquo(vars), data, allow_rename = FALSE)
  ]
  exc_list <- names(data)[
    eval_select(enquo(var_set), data, allow_rename = FALSE)
  ]

  if (any(!vars_list %in% exc_list)) {
    warning("Some variables in `", act$var_desc, "` do not exist in ",
            "var_set `", act$var_set_desc, "`. ",
            "Variable lists will be combined.",
            call. = FALSE)
  }

  flt <- enquo(flt)
  act$result <-
    !(
    # Flags records that have the exclusive var selected
    (data %>%
       filter(!!flt) %>%
       mutate(across({{ vars }}, ~. %==% exc_val)) %>%
       select({{ vars }}) %>%
       apply(1, any)) &
    # Flags records with more than one var selected
    ((data %>%
       filter(!!flt) %>%
       mutate(across(c({{ var_set }}, {{ vars }}), ~. %==% exc_val)) %>%
       select(c({{ var_set }}, {{ vars }})) %>%
       apply(1, sum)) > 1)
    )

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records with \\
          non-exclusive values in variables `{act$var_desc}` in variable set \\
          `{act$var_set_desc}`.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}
