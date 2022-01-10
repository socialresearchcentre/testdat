#' Deprecated expectation functions
#'
#' @description
#'
#' `r lifecycle::badge("soft-deprecated")`
#'
#' These functions are deprecated.
#'
#' @inheritParams data-params
#' @inherit data-params return
#' @name expect-deprec
#' @keywords internal
NULL

#' @export
#' @rdname expect-deprec
#' @param ... Arguments to pass to `expect_allany()`.
#' @seealso [Generic Expectation Functions][generic-expectations]
expect_func <- function(var, ...) {
  lifecycle::deprecate_soft("0.2.0", "expect_func()", "expect_all()")
  expect_allany({{ var }}, ..., allany = chk_filter_all)
}

#' @export
#' @rdname expect-deprec
#' @inheritParams datacomp-expectations
#' @seealso [Expectations: comparisons][datacomp-expectations]
expect_join <- function(data2, by = NULL, not = FALSE, flt = TRUE, data = get_testdata()) {
  lifecycle::deprecate_soft("0.2.0", "expect_join()", "expect_subset()")
  flt <- rlang::enexpr(flt)
  expect_subset(data2 = data2, by = by, not = not, flt = !!flt, data = data)
}

#' @export
#' @rdname expect-deprec
#' @inheritParams datacomp-expectations
#' @param var2 An unquoted column name from data2.
#' @param flt2 A filter specifying a subset of data2 to test.
#' @param threshold The maximum proportional difference allowed between the two
#'   categories.
#' @param min The minimum number of responses for a category to allow
#'   comparison. This avoids small categories raising spurious errors.
expect_similar <- function(var, data2, var2, flt = TRUE, flt2 = flt,
                           threshold = 0.05, min = 100, data = get_testdata()) {
  lifecycle::deprecate_soft("0.3.0", "expect_similar()")
  check_expect_data_pipe(enquo(var))
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


#' @export
#' @rdname expect-deprec
#' @inheritParams generic-expectations
#' @param allany The function to combine the `func` results for each row.
expect_allany <- function(vars,
                          func,
                          flt = TRUE,
                          data = get_testdata(),
                          args = list(),
                          allany = c(chk_filter_all, chk_filter_any),
                          func_desc = NULL) {
  lifecycle::deprecate_soft(
    "0.4.1",
    "expect_allany()",
    details = "Please use `expect_all() or `expect_any()` instead"
  )

  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$func_desc <- if (is.null(func_desc)) paste0("`", as_label(enquo(func)), "`") else func_desc
  act$var_desc  <- as_label_vars(enquo(vars))
  act$flt_desc  <- as_label_flt(enquo(flt))
  act$args_desc <- expr_deparse_repl(args, "(^<list: |>$)", "")

  act$result <- allany(eval_tidy(enquo(data)),
                       {{ vars }},
                       eval_tidy(enquo(func)),
                       {{ flt }},
                       args)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records failing \\
          {act$func_desc} on variable `{act$var_desc}`.
          Filter: {act$flt_desc}
          Arguments: `{act$args_desc}`"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    var_desc = act$var_desc,
    result = act$result
  )

  invisible(act$result)
}

#' Defunct expectation functions
#'
#' @description
#'
#' `r lifecycle::badge("defunct")`
#'
#' These functions are defunct.
#'
#' * `expect_where()` works exactly like `expect_all()`. When testdat used
#' `dplyr::vars()` as standard `expect_where()` provided an alternative
#' interface using [`tidy-select`][dplyr_tidy_select].
#'
#' @inheritParams data-params
#' @inherit data-params return
#' @name expect-defunct
#' @keywords internal
NULL

#' @export
#' @rdname expect-defunct
#' @param expect_function An expectation function.
#' @param ... Arguments to pass to expect_function.
#' @param not Reverse the results of the check.
#' @return The input data frame filtered to records failing the expectation.
filter_expect <- function(data, expect_function, ..., not = TRUE) {
  lifecycle::deprecate_stop("0.2.0", "filter_expect()")
}

#' @export
#' @rdname expect-defunct
#' @param where <[`tidy-select`][dplyr_tidy_select]> Columns to check
expect_where <- function(where, func, flt = TRUE, data = get_testdata(), args = list(), func_desc = NULL) {
  lifecycle::deprecate_stop("0.3.0", "expect_where()", "expect_all()")
}
