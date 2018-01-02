
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
expect_base <- function(var, base, data = get_reporter()$get_data()) {
  # act <- list(val = get_reporter()$get_data(), lab = "data")
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
    sprintf("%s has a base mismatch in variable %s. %i cases have %s but %s is missing. %i cases do not have %s but %s is non missing.",
            act$lab, act$var_desc, sum(act$miss), act$base_desc, act$var_desc, sum(act$nmiss), act$base_desc, act$var_desc),
    failed_count = sum(act$total)
  )

  invisible(act$val)
}

#' @export
expect_cond <- function(cond1, cond2, data = get_reporter()$get_data()) {
  # act <- list(val = get_reporter()$get_data(), lab = "data")
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
    sprintf("%s failed consistency check. %i cases have %s but not %s.",
            act$lab, sum(act$cond_fail), act$cond1_desc, act$cond2_desc),
    failed_count = sum(act$cond_fail)
  )

  invisible(act$val)
}

#' @export
expect_values <- function(var, ..., miss = TRUE, data = get_reporter()$get_data()) {
  # act <- list(val = get_reporter()$get_data(), lab = "data")
  act <- quasi_label(rlang::enquo(data))

  act$var_desc <- expr_label(get_expr(enquo(var)))
  act$var <- expr_text(get_expr(enquo(var)))

  act$vals_desc <- expr_label(get_expr(list(...))) %>% gsub("(^`list\\()|(\\)`$)", "`", .)
  act$vals <- !act$val[[act$var]] %in% c(unlist(list(...)),
                                         ifelse(miss, getOption("testdat.miss"), NULL))

  expect_custom(
    !any(act$vals),
    sprintf("%s has invalid values in variable %s. %i cases have values other than %s.",
            act$lab, act$var_desc, sum(act$vals), act$vals_desc),
    data = list(table(act$val[[act$var]][!act$val[[act$var]] %in% unlist(list(...))])),
    failed_count = sum(act$vals)
  )

  invisible(act$val)
}

#' @export
expect_unique <- function(var, filter = NULL, data = get_reporter()$get_data()) {
}

#' @export
expect_regex <- function(var, pattern, data = get_reporter()$get_data()) {
}

#' @export
expect_func <- function(func, var, ..., data = get_reporter()$get_data()) {
  func(data[[var]], ...)
}

expect_allany <- function(func, vars, flt = NULL, data = get_reporter()$get_data(), allany) {
  act <- quasi_label(rlang::enquo(data))
  act$func_desc <- expr_label(get_expr(enquo(func)))
  act$var_desc <- expr_label(get_expr(enquo(vars)))
  act$flt_desc <- expr_label(get_expr(enquo(flt)))

  act$result <- allany(data, vars, func, !!enquo(flt))

  expect_custom(
    all(act$result, na.rm = TRUE),
    sprintf("%s has %i records failing %s on variables %s with filter %s.",
            act$lab, sum(act$result, na.rm = TRUE), act$func_desc, act$var_desc, act$flt_desc),
    failed_count = sum(act$result, na.rm = TRUE)
  )
}

#' @export
expect_all <- function(...) { expect_allany(..., allany = chk_filter_all) }

#' @export
expect_any <- function(...) { expect_allany(..., allany = chk_filter_any) }
