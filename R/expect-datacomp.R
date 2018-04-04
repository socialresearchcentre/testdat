#' Expectations: cross-dataset expectations
#'
#' These functions allow for dataset comparisons
#'
#' @inheritParams data-params
#' @param data2 the dataset to compare against
#' @param not reverse the results of the check
#' @family data expectations
#' @name datacomp-expectations
NULL

#' @export
#' @rdname datacomp-expectations
#' @param var2 an unquoted variable name from data2
#' @param flt2 a filter specifying a subset of data2 to test
#' @param threshold the maximum proportional difference allowed between the two
#'   categories
#' @param min the minimum number of responses for a category to allow
#'   comparison. This avoidmall categories raising spurious errors
expect_similar <- function(var, data2, var2, flt = TRUE, flt2 = flt,
                           threshold = 0.05, min = 100, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc   <- str_replace_all(expr_label(get_expr(enquo(var))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$data2_desc <- expr_label(get_expr(enquo(data2)))
  act$var2_desc  <- str_replace_all(expr_label(get_expr(enquo(var2))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc   <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")
  act$flt2_desc  <- str_replace_all(expr_label(get_expr(enquo(flt2))), "^TRUE$", "None")

  var <- enquo(var)
  var2 <- enquo(var2)
  data_tb  <- data  %>% group_by(!!var)  %>% summarise(freq = n())
  data2_tb <- data2 %>% group_by(!!var2) %>% summarise(freq = n())

  by_var <- structure(quo_text(var), names = quo_text(var2))
  act$result <-
    left_join(data_tb, data2_tb, by = by_var) %>%
    mutate(prop_diff = abs(freq.x - freq.y) / freq.x,
           pass = prop_diff < threshold | freq.x < min)

  expect_custom(
    all(act$result$pass, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result$pass, na.rm = TRUE)} \\
          values breaking the {threshold} similarity threshold for variable \\
          {act$var_desc}
          Values: {glue::collapse(act$result %>% filter(!pass) %>% pull(!!var), ', ')}
          Filter: {act$flt_desc}"),
    table = act$result
  )

  invisible(act$result$pass)
}

# TODO
# #' @export
# #' @rdname datacomp-expectations
expect_labels_identical <- function(data2, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$data2_desc <- expr_label(get_expr(enquo(var)))

  act$result <- FALSE

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue(""),
    table = act$result
    )

  invisible(act$result)
}

#' @export
#' @rdname datacomp-expectations
#' @param by a character vector of variables to join by. For details see the man page for dplyr [join][dplyr].
expect_valmatch <- function(data2, vars, by, not = FALSE, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(vars))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$data2_desc <- expr_label(get_expr(enquo(data2)))
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")
  act$by_desc <- str_replace_all(expr_label(get_expr(enquo(by))), "(^`c\\(~?)|(\\)`$)", "`")

  var_list <- dplyr:::tbl_at_syms(data, vars) %>% sapply(quo_name)

  if (length(var_list) == 0)
    stop("Variable specification `vars(", act$var_desc, ")` does not match any variables in ", act$label, ".", call. = FALSE)

  if (any(!var_list %in% names(data)) | any(!var_list %in% names(data2)))
    stop("Variable specification `vars(", act$var_desc, ")` specifies variables that are not common to both datasets.", call. = FALSE)

  comp = ifelse(not, "%!=%", "%==%")

  var_expr <- var_list %>%
    structure(lapply(glue("{.}.x {comp} {.}.y"), parse_expr), names = .)

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    left_join(data2, by = by) %>%
    mutate(!!!var_expr) %>%
    select(names(var_expr)) %>%
    apply(1, all)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("The join of {act$lab} and {act$data2_desc} by {act$by_desc} has \\
          {sum(!act$result, na.rm = TRUE)} records with a mismatch on \\
          variables {act$var_desc}.
          Filter: {act$flt_desc}
          Comparison: {comp}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
    )

  invisible(act$result)
}

#' @export
#' @rdname datacomp-expectations
#' @param by a character vector of variables to join by. For details see the man page for dplyr [join][dplyr].
#' @importFrom tidyr replace_na
expect_join <- function(data2, by = NULL, not = FALSE, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(vars))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$data2_desc <- expr_label(get_expr(enquo(data2)))
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

  flt <- enquo(flt)

  act$result <- data %>%
    filter(!!flt) %>%
    left_join(data2 %>%
                select(one_of(suppressMessages(common_by(by, data, data2)$y))) %>%
                mutate(`__result` = TRUE) %>%
                unique,
              by = by) %>%
    replace_na(list(`__result` = FALSE)) %>%
    pull(`__result`)

  if (not) act$result <- !act$result

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records that\\
          {ifelse(not, '', ' do not')} exist in dataset {act$data2_desc}.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}
