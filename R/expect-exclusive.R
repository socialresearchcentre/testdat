#' Expectations: exclusivity checks
#'
#' `expect_exclusive` tests one set of variables (`exc_vars`) against another
#' (`vars`) with reference to a value (`exc_val`) and fails if any observation
#' has *all* of the variables in `vars` equal to `exc_val` and and *any* of the
#' variables in `exc_vars` (aside from those which are also in `vars`) equal to
#' `exc_val`.
#'
#'
#' @inheritParams data-params
#' @param exc_vars The variable set to check, specified using [vars()]. This
#'   should include all variables specified in the `vars` argument
#' @param exc_val The value to check for exclusivity (default: `1`)
#' @family data expectations
#' @name exclusivity-expectations
#' @examples
#'
#' my_q_block <- data.frame(
#'   resp_id = 1:5, # Unique to respondent
#'   q10_1 = c(1, 1, 0, 0, 0),
#'   q10_2 = c(0, 1, 0, 0, 0),
#'   q10_3 = c(0, 0, 1, 0, 0),
#'   q10_98 = c(1, 0, 0, 1, 0), # None of the above
#'   q10_99 = c(0, 0, 0, 0, 1)  # Item not answered
#' )
#'
#' # Make sure that if "None of the above" and "Item skipped" are selected
#' # none of the other question options are selected:
#' try(
#' expect_exclusive(
#'   vars(q10_98, q10_99),
#'   vars(starts_with("q10_")),
#'   data = my_q_block
#' )
#' )
NULL

#' @export
#' @rdname exclusivity-expectations
expect_exclusive <- function(vars, exc_vars, exc_val = 1, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc     <- as_label_vars(enquo(vars))
  act$exc_var_desc <- as_label_vars(enquo(exc_vars))
  act$exc_val_desc <- as_label(enquo(exc_val))
  act$flt_desc     <- as_label_flt(enquo(flt))

  vars_list <- vars_select(tbl_vars(data), !!!vars) %>% unname
  exc_list <- vars_select(tbl_vars(data), !!!exc_vars) %>% unname

  if (any(!vars_list %in% exc_list)) {
    exc_vars <- c(exc_vars, vars)
    warning("Some variables in `vars(", act$var_desc, ")` do not exist in ",
            "exc_vars `vars(", act$exc_var_desc, ")`. ",
            "Variable lists will be combined.",
            call. = FALSE)
  }

  flt <- enquo(flt)
  act$result <-
    # Flags records with no codes or 1 code selected from all vars
    (data %>%
       filter(!!flt) %>%
       mutate_at(exc_vars, ~. %==% exc_val) %>%
       select(!!!exc_vars) %>%
       apply(1, sum) %>%
       `%in%`(c(0, 1))) |
    # Flags codes that do NOT have the exclusive var selected
    (data %>%
       filter(!!flt) %>%
       mutate_at(vars, ~. %!=% exc_val) %>%
       select(!!!vars) %>%
       apply(1, all))

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records with \\
          non-exclusive values in variables `{act$var_desc}` in variable set \\
          `{act$exc_var_desc}`.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}
