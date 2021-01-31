#' Expectations: value checks
#'
#' These functions test variable values:
#'   * `expect_values` tests one variable (`var`) against a set of values
#'     (`...`) and fails if there are any other values in the variable
#'   * `expect_regex` tests one variable (`var`) against a regex pattern
#'     (`pattern`) and fails if there are any values not conforming to that
#'     pattern in the variable
#'   * `expect_range` tests one variable (`var`) against a range (defined by
#'     `min`/`max` values) and fails if there are any values in the variable
#'     outside this range
#'   * `expect_exclusive` tests one set of variables (`exc_vars`) against
#'     another (`vars`) with reference to a value (`exc_val`) and fails if any
#'     observation has *all* of the variables in `vars` equal to `exc_val` and
#'     and *any* of the variables in `exc_vars` (aside from those which are
#'     also in `vars`) equal to `exc_val`.
#'   * `expect_unique` tests a set of variables (`vars`) and fails if the
#'     variables collectively do not uniquely identify the observations
#'   * `expect_unique_across` tests a set of variables (`vars`) and fails if
#'     each observation does not have different values in each of the variables
#'     in `vars`.
#'
#'
#'
#' @inheritParams data-params
#' @family data expectations
#' @name value-expectations
#' @examples
#'
#' # Single variable checks ----
#'
#' # Check that all values in a variable fall into a given set
#' expect_values(vs, 0:1, data = mtcars)
#'
#' # Check that all values in a variable conform to a certain regex pattern
#' expect_regex(vs, "[01]", data = mtcars)
#'
#' # Check that all values in a variable fall into a given range
#' expect_range(mpg, 10, 40, data = mtcars)
#'
#'
#' # Multi variable checks ----
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
#' \dontrun{expect_exclusive(
#'   vars(q10_98, q10_99),
#'   vars(starts_with("q10_")),
#'   data = my_q_block
#' )}
#'
#' my_sales <- data.frame(
#'   purchaser_id = c(1, 1, 1, 2, 3),
#'   purchase_number = c(1, 2, 3, 1, 1),
#'   item_id = 1:5
#' )
#'
#' # Check that all values in a combination of variables are unique
#' expect_unique(vars(purchaser_id, purchase_number), data = my_sales)
#'
#' student_fruit_preferences <- data.frame(
#'   student_id = 1:5,
#'   apple = c(1, 1, 1, 1, 1),
#'   orange = c(2, 3, 2, 3, 2),
#'   banana = c(3, 2, 3, 2, 3)
#' )
#'
#' # Check that every observation has a different value across a set of variables
#' expect_unique_across(vars(apple, orange, banana), data = student_fruit_preferences)
#'
#'
NULL

#' @export
#' @rdname value-expectations
#' @param ... vectors of valid values
expect_values <- function(var, ..., miss = getOption("testdat.miss"), data = get_testdata()) {
  act <- quasi_label(enquo(data))

  act$var_desc <- as_label(ensym(var))
  act$var <- as_name(ensym(var))
  act$vals_desc <- lapply(enexprs(...), as_label)
  act$vals_desc <- paste0(act$vals_desc, collapse = ", ")
  act$result <- act$val[[act$var]] %in% c(unlist(list(...)), miss)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has invalid values in variable `{act$var_desc}`. \\
          {sum(!act$result, na.rm = TRUE)} cases have values other than `{act$vals_desc}`."),
    data = list(table(act$val[[act$var]][!act$result], useNA = "ifany")),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    var_desc = act$var,
    result = act$result
  )

  invisible(act$result)
}

#' @export
#' @rdname value-expectations
#' @param pattern a regex to check
expect_regex <- expect_make(chk_pattern, "pattern check")

#' @export
#' @rdname value-expectations
#' @param min minimum value for range check
#' @param max maximum value for range check
expect_range <- expect_make(
  function(x, min, max, ...) {
    chk_range(x, min, max) | chk_values(x, ...)
  },
  "range check"
)

#' @export
#' @rdname value-expectations
#' @param exc_vars The variable set to check, specified using [vars()]. This
#'   should include all variables specified in the `vars` argument
#' @param exc_val The value to check for exclusivity (default: `1`)
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

  # %>%
  #   mutate(!!!var_expr) %>%
  #   select(names(var_expr)) %>%
  #   apply(1, all)

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

#' @importFrom tidyselect vars_select
#' @export
#' @rdname value-expectations
expect_unique <- function(vars, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc <- as_label_vars(enquo(vars))
  act$flt_desc <- as_label_flt(enquo(flt))

  flt <- enquo(flt)
  act$result_data <- data %>%
    filter(!!flt) %>%
    group_by(!!!vars) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    select(!!!vars, count)
    # select(!!!vars) %>%
    # duplicated %>%
    # `!`

  act$result <- act$result_data$count == 1

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} duplicate records \\
          on variable `{act$var_desc}`.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    duplicated_ids = act$result_data %>% filter(count > 1) %>% unique
  )

  invisible(act$result)
}

#' @export
#' @rdname value-expectations
expect_unique_across <- function(vars, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc <- as_label_vars(enquo(vars))
  act$flt_desc <- as_label_flt(enquo(flt))

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    select(!!!vars) %>%
    apply(1, function(x) { all(chk_unique(x)) })

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records with \\
          duplicates across variables `{act$var_desc}`.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}


