#' Expectations: consistency checks
#'
#' These functions test whether multiple conditions coexist.
#'
#' `expect_cond` checks the coexistence of two conditions. It can be read as "if
#' `cond1` then `cond2`".
#'
#' `expect_base` is a special case that checks missing data against a specified
#' condition. It can be read as "if `base` then `var` not missing, if not `base`
#' then `var` missing".
#'
#' @inheritParams data-params
#' @family data expectations
#' @name conditional-expectations
NULL

#' @export
#' @rdname conditional-expectations
#' @param base condition for missing check
expect_base <- function(var, base, miss = getOption("testdat.miss"), data = get_testdata()) {
  # act <- list(val = get_testdata(), lab = "data")
  act <- quasi_label(enquo(data))

  act$var_desc <- quasi_repl(enquo(var))
  act$var <- expr_text(get_expr(enquo(var)))

  base <- enquo(base)
  act$base_desc <- quasi_repl(base)
  act$base <- act$val %>% transmute(!!base) %>% pull(1)
  act$base[is.na(act$base)] <- FALSE

  act$miss <- (act$val[[act$var]] %in% miss) & act$base
  act$nmiss <- !(act$val[[act$var]] %in% miss) & !act$base
  act$result <- !(act$miss | act$nmiss)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has a base mismatch in variable {act$var_desc}.
          {sum(act$miss)} cases have {act$base_desc} but {act$var_desc} is missing.
          {sum(act$nmiss)} cases do not have {act$base_desc} but {act$var_desc} is non missing."),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    var_desc = act$var
  )

  invisible(act$result)
}

#' @export
#' @rdname conditional-expectations
#' @param cond1 first condition for consistency check
#' @param cond2 second condition for consistency check
expect_cond <- function(cond1, cond2, data = get_testdata()) {
  # act <- list(val = get_testdata(), lab = "data")
  act <- quasi_label(enquo(data))

  cond1 <- enquo(cond1)
  act$cond1_desc <- quasi_repl(cond1)
  act$cond1 <- act$val %>% transmute(!!cond1) %>% pull(1)
  act$cond1[is.na(act$cond1)] <- FALSE

  cond2 <- enquo(cond2)
  act$cond2_desc <- quasi_repl(cond2)
  act$cond2 <- act$val %>% transmute(!!cond2) %>% pull(1)
  act$cond2[is.na(act$cond2)] <- FALSE

  act$result <- !(act$cond1 & !act$cond2)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} failed consistency check. {sum(!act$result, na.rm = TRUE)} \\
          cases have {act$cond1_desc} but not {act$cond2_desc}."),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}

