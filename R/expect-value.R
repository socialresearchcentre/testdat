#' Expectations: value checks
#'
#' These functions test variable values
#'
#' @inheritParams data-params
#' @family data expectations
#' @name value-expectations
NULL

#' @export
#' @rdname value-expectations
#' @param ... vectors of valid values
expect_values <- function(var, ..., miss = getOption("testdat.miss"), data = get_testdata()) {
  act <- quasi_label(enquo(data))

  act$var_desc <- quo_label(ensym(var))
  act$var <- as_name(ensym(var))
  act$vals_desc <- lapply(enexprs(...), quo_text) %>% paste0(collapse = ", ") %>% paste0("`", ., "`")
  act$result <- act$val[[act$var]] %in% c(unlist(list(...)), miss)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has invalid values in variable {act$var_desc}. \\
          {sum(!act$result, na.rm = TRUE)} cases have values other than {act$vals_desc}."),
    data = list(table(act$val[[act$var]][!act$val[[act$var]] %in% unlist(list(...))])),
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
expect_regex <- function(var, pattern, flt = TRUE, data = get_testdata()) {
  expect_func(!!enquo(var),
              chk_pattern,
              !!enquo(flt),
              !!enquo(data),
              args = list(pattern = pattern))
}

#' @export
#' @rdname value-expectations
#' @param min minimum value for range check
#' @param max maximum value for range check
expect_range <- function(var, min, max, ..., flt = TRUE, data = get_testdata()) {
  expect_func(!!enquo(var),
              function(x, min, max, ...) {
                chk_range(x, min, max) | chk_values(x, ...)
              },
              !!enquo(flt),
              !!enquo(data),
              args = list(min = min, max = max, ...))
}

#' @export
#' @rdname value-expectations
#' @param exc_vars The variable set to check, specified using [vars()]. This
#'   should include all variables specified in the `vars` argument
#' @param exc_val The value to check for exclusivity (default: `1`)
expect_exclusive <- function(vars, exc_vars, exc_val = 1, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc     <- quo_label_vars(enquo(vars))
  act$exc_var_desc <- quo_label_vars(enquo(exc_vars))
  act$exc_val_desc <- quo_label(enquo(exc_val))
  act$flt_desc     <- quo_label_flt(enquo(flt))

  vars_list <- vars_select(tbl_vars(data), !!!vars) %>% unname
  exc_list <- vars_select(tbl_vars(data), !!!exc_vars) %>% unname

  if (any(!vars_list %in% exc_list)) {
    exc_vars <- c(exc_vars, vars)
    warning("Some variables in vars ", act$var_desc, " do not exist in exc_vars ",
            act$exc_var_desc, ". Variable lists will be combined.",
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
          non-exclusive values in variables {act$var_desc} in variable set \\
          {act$exc_var_desc}.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
    )

  invisible(act$result)
}

#' @export
#' @rdname value-expectations
expect_unique <- function(vars, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc <- quo_label_vars(enquo(vars))
  act$flt_desc <- quo_label_flt(enquo(flt))

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
          on variable {act$var_desc}.
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
  act$var_desc <- quo_label_vars(enquo(vars))
  act$flt_desc <- quo_label_flt(enquo(flt))

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    select(!!!vars) %>%
    apply(1, function(x) { all(chk_unique(x)) })

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records with \\
          duplicates across variables {act$var_desc}.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}

# TODO
# #' @export
# #' @rdname value-expectations
expect_unique_combine <- function(vars, flt = TRUE, data = get_testdata()) {
  browser()
  act <- quasi_label(enquo(data))
  act$var_desc <- quo_label_vars(enquo(vars))
  act$flt_desc <- quo_label_flt(enquo(flt))

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    select(!!!vars) %>%
    gather(vars, val) %>%

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records with \\
          duplicates across variables {act$var_desc}.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
    )

  invisible(act$result)
}
