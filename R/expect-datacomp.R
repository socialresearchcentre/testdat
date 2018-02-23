#' Expectations: cross-dataset expectations
#'
#' These functions allow for dataset comparisons
#'
#' @inheritParams data-params
#' @param data2 the dataset to compare against
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

# TODO
# #' @export
# #' @rdname datacomp-expectations
expect_valmatch <- function(data2, vars, by, flt = TRUE, data = get_testdata()) {
  browser()
  act <- quasi_label(enquo(data))
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(vars))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$data2_desc <- expr_label(get_expr(enquo(data2)))
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    left_join(data2, by = by)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records with \\
         a mismatch on variables {act$var_desc} in dataset {act$data2_desc}.
         Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
    )

  invisible(act$result)
}

# TODO
# #' @export
# #' @rdname datacomp-expectations
expect_valnmatch <- function() {
}
