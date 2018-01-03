#' @importFrom glue glue
NULL

#' @export
expect_custom <- function(ok, failure_message, info = NULL, srcref = NULL, ...) {
  exp <- testthat:::as.expectation.logical(ok, failure_message, info = info, srcref = srcref)

  exp[["custom"]] <- list(...)

  withRestarts(
    if (testthat:::expectation_broken(exp)) {
      stop(exp)
    } else {
      signalCondition(exp)
    },
    continue_test = function(e) NULL
  )

  invisible(exp)
}

#' @export
expect_base <- function(var, base, data = get_testdata()) {
  # act <- list(val = get_testdata(), lab = "data")
  act <- quasi_label(rlang::enquo(data))

  act$var_desc <- expr_label(get_expr(enquo(var)))
  act$var <- expr_text(get_expr(enquo(var)))

  base <- enquo(base)
  act$base_desc <- expr_label(get_expr(base))
  act$base <- act$val %>% transmute(!!base) %>% pull(1)
  act$base[is.na(act$base)] <- FALSE

  act$miss <- is.na(act$val[[act$var]]) & act$base
  act$nmiss <- !is.na(act$val[[act$var]]) & !act$base
  act$total <- act$miss | act$nmiss

  expect_custom(
    !any(act$total),
    glue("{act$lab} has a base mismatch in variable {act$var_desc}.
          {sum(act$miss)} cases have {act$base_desc} but {act$var_desc} is missing.
          {sum(act$nmiss)} cases do not have {act$base_desc} but {act$var_desc} is non missing."),
    failed_count = sum(act$total)
  )

  invisible(act$val)
}

#' @export
expect_cond <- function(cond1, cond2, data = get_testdata()) {
  # act <- list(val = get_testdata(), lab = "data")
  act <- quasi_label(rlang::enquo(data))

  cond1 <- enquo(cond1)
  act$cond1_desc <- expr_label(get_expr(cond1))
  act$cond1 <- act$val %>% transmute(!!cond1) %>% pull(1)
  act$cond1[is.na(act$cond1)] <- FALSE

  cond2 <- enquo(cond2)
  act$cond2_desc <- expr_label(get_expr(cond2))
  act$cond2 <- act$val %>% transmute(!!cond2) %>% pull(1)
  act$cond2[is.na(act$cond2)] <- FALSE

  act$cond_fail <- act$cond1 & !act$cond2

  expect_custom(
    !any(act$cond_fail),
    glue("{act$lab} failed consistency check.{sum(act$cond_fail)} cases have \\
          {act$cond1_desc} but not {act$cond2_desc}."),
    failed_count = sum(act$cond_fail)
  )

  invisible(act$val)
}

#' @export
expect_values <- function(var, ..., miss = TRUE, data = get_testdata()) {
  # act <- list(val = get_testdata(), lab = "data")
  act <- quasi_label(rlang::enquo(data))

  act$var_desc <- expr_label(get_expr(enquo(var)))
  act$var <- expr_text(get_expr(enquo(var)))

  act$vals_desc <- expr_label(get_expr(list(...))) %>% gsub("(^`list\\()|(\\)`$)", "`", .)
  act$vals <- !act$val[[act$var]] %in% c(unlist(list(...)),
                                         ifelse(miss, getOption("testdat.miss"), NULL))

  expect_custom(
    !any(act$vals),
    glue("{act$lab} has invalid values in variable {act$var_desc}.\\
          {sum(act$vals)} cases have values other than {act$vals_desc}."),
    data = list(table(act$val[[act$var]][!act$val[[act$var]] %in% unlist(list(...))])),
    failed_count = sum(act$vals)
  )

  invisible(act$val)
}

#' @export
expect_unique <- function(var, filter = NULL, data = get_testdata()) {
}

#' @export
expect_regex <- function(var, pattern, data = get_testdata()) {
}

#' @export
expect_func <- function(var, func, flt = NULL, data = get_testdata(), args = list()) {
  act <- quasi_label(rlang::enquo(data))
  act$func_desc <- expr_label(get_expr(enquo(func)))
  act$var_desc <- expr_label(get_expr(enquo(var)))
  act$flt_desc <- expr_label(get_expr(enquo(flt)))

  act$result <- chk_filter(data, !!enquo(var), func, !!enquo(flt))

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records failing \\
          {act$func_desc} on variable {act$var_desc} with filter {act$flt_desc}."),
    failed_count = sum(!act$result, na.rm = TRUE)
  )
}

expect_allany <- function(var, func, flt = NULL, data = get_testdata(), args = list(), allany) {
  act <- quasi_label(rlang::enquo(data))
  act$func_desc <- expr_label(get_expr(enquo(func)))
  act$var_desc <- expr_label(get_expr(enquo(var)))
  act$flt_desc <- expr_label(get_expr(enquo(flt)))

  act$result <- allany(data, var, func, !!enquo(flt), args)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records failing \\
          {act$func_desc} on variable {act$var_desc} with filter {act$flt_desc}."),
    failed_count = sum(!act$result, na.rm = TRUE)
  )
}

#' @export
expect_all <- function(...) { expect_allany(..., allany = chk_filter_all) }

#' @export
expect_any <- function(...) { expect_allany(..., allany = chk_filter_any) }
