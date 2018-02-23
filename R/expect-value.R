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
  # act <- list(val = get_testdata(), lab = "data")
  act <- quasi_label(enquo(data))

  act$var_desc <- expr_label(get_expr(enquo(var)))
  act$var <- expr_text(get_expr(enquo(var)))

  # act$vals_desc <- expr_label(get_expr(list(...))) %>% gsub("(^`list\\()|(\\)`$)", "`", .)
  act$vals_desc <- expr_text(get_expr(lapply(list(...), as.vector))) %>% gsub("(^list\\()|(\\)$)", "`", .)
  act$result <- act$val[[act$var]] %in% c(unlist(list(...)), miss)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has invalid values in variable {act$var_desc}. \\
          {sum(!act$result, na.rm = TRUE)} cases have values other than {act$vals_desc}."),
    data = list(table(act$val[[act$var]][!act$val[[act$var]] %in% unlist(list(...))])),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}

#' @export
#' @rdname value-expectations
#' @param pattern a regex to check
expect_regex <- function(var, pattern, flt = TRUE, data = get_testdata()) {
  expect_func(!!enquo(var), chk_pattern, !!enquo(flt), data, args = list(pattern))
}

#' @export
#' @rdname value-expectations
#' @param min minimum value for range check
#' @param max maximum value for range check
expect_range <- function(var, min, max, flt = TRUE, data = get_testdata()) {
  expect_func(!!enquo(var), chk_range, !!enquo(flt), data, args = list(min, max))
}

# TODO
# #' @export
# #' @rdname value-expectations
expect_exclusive <- function(var, exc_var, data = get_testdata()) {
}

#' @export
#' @rdname value-expectations
expect_unique <- function(vars, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(vars))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

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
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(vars))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

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
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(vars))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

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
