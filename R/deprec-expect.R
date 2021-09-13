#' Deprecated expectation functions
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' These functions are deprecated.
#'
#' See [Generic Expectation Functions][generic-expectations] and [Cross-dataset
#' Expectations][datacomp-expectations] for current expectation functions.
#'
#' @inheritParams data-params
#' @param ... arguments to pass to `expect_allany()`
#' @name expect-deprec
#' @keywords internal
NULL

#' @export
#' @rdname expect-deprec
#' @seealso [Generic Expectation Functions][generic-expectations]
expect_func <- function(var, ...) {
  lifecycle::deprecate_soft("0.2.0", "expect_func()", "expect_all()")
  expect_allany(vars(!!ensym(var)), ..., allany = chk_filter_all)
}

#' @export
#' @rdname expect-deprec
#' @inheritParams datacomp-expectations
#' @seealso [Cross-dataset Expectations][datacomp-expectations]
expect_join <- function(data2, by = NULL, not = FALSE, flt = TRUE, data = get_testdata()) {
  lifecycle::deprecate_soft("0.2.0", "expect_join()", "expect_subset()")
  flt <- rlang::enexpr(flt)
  expect_subset(data2 = data2, by = by, not = not, flt = !!flt, data = data)
}

#' @export
#' @rdname expect-deprec
#' @inheritParams datacomp-expectations
#' @param var2 an unquoted variable name from data2
#' @param flt2 a filter specifying a subset of data2 to test
#' @param threshold the maximum proportional difference allowed between the two
#'   categories
#' @param min the minimum number of responses for a category to allow
#'   comparison. This avoidmall categories raising spurious errors
expect_similar <- function(var, data2, var2, flt = TRUE, flt2 = flt,
                           threshold = 0.05, min = 100, data = get_testdata()) {
  lifecycle::deprecate_soft("0.3.0", "expect_similar()")
  act <- quasi_label(enquo(data))
  act$var_desc   <- as_label_vars(enquo(var))
  act$data2_desc <- as_label(enquo(data2))
  act$var2_desc  <- as_label_vars(enquo(var2))
  act$flt_desc   <- as_label_flt(enquo(flt))
  act$flt2_desc  <- as_label_flt(enquo(flt2))

  var <- enquo(var)
  var2 <- enquo(var2)
  data_tb  <- data  %>% group_by(!!var)  %>% summarise(freq = n())
  data2_tb <- data2 %>% group_by(!!var2) %>% summarise(freq = n())

  by_var <- structure(as_name(var2), names = as_name(var))
  act$result <-
    left_join(data_tb, data2_tb, by = by_var) %>%
    mutate(prop_diff = abs(.data$freq.x - .data$freq.y) / .data$freq.x,
           pass = .data$prop_diff < threshold | .data$freq.x < min)

  expect_custom(
    all(act$result$pass, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result$pass, na.rm = TRUE)} \\
          values breaking the {threshold} similarity threshold for variable \\
          `{act$var_desc}`
          Values: {glue::glue_collapse(act$result %>% filter(!pass) %>% pull(!!var), ', ')}
          Filter: {act$flt_desc}"),
    table = act$result
  )

  invisible(act$result$pass)
}

#' Filter data to expectation result
#'
#' @description
#'
#' `r lifecycle::badge("defunct")`
#'
#' This function is defunct.
#'
#' @param data A data frame to test.
#' @param expect_function An expectation function.
#' @param ... Arguments to pass to expect_function.
#' @param not Reverse the results of the check.
#' @return The input data frame filtered to records failing the expectation.
#' @export
#' @keywords internal
filter_expect <- function(data, expect_function, ..., not = TRUE) {
  lifecycle::deprecate_stop("0.2.0", "filter_expect()")
  expect_result <- expect_function(..., data = data)
  if (not) expect_result <- !expect_result

  data %>% filter(expect_result)
}
