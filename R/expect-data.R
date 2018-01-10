#' @importFrom glue glue
#' @importFrom stringr str_replace_all
NULL

#' @export
expect_custom <- function(ok, failure_message, info = NULL, srcref = NULL, ...) {
  exp <- testthat:::as.expectation.logical(ok, failure_message, info = info, srcref = srcref)

  exp[["custom"]] <- list(...)

  withRestarts(
    if (testthat:::expectation_broken(exp)) {
      if (getOption("testdat.stop_on_fail")) {
        stop(exp)
      } else {
        warning(exp)
      }
    } else {
      signalCondition(exp)
    },
    continue_test = function(e) NULL
  )

  invisible(exp)
}

#' @export
filter_expect <- function(data, expect_function, ..., not = TRUE) {
  expect_result <- expect_function(..., data = data)
  if (not) expect_result <- !expect_result

  data %>% filter(expect_result)
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
  act$result <- !(act$miss | act$nmiss)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has a base mismatch in variable {act$var_desc}.
          {sum(act$miss)} cases have {act$base_desc} but {act$var_desc} is missing.
          {sum(act$nmiss)} cases do not have {act$base_desc} but {act$var_desc} is non missing."),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
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

#' @export
expect_values <- function(var, ..., miss = TRUE, data = get_testdata()) {
  # act <- list(val = get_testdata(), lab = "data")
  act <- quasi_label(rlang::enquo(data))

  act$var_desc <- expr_label(get_expr(enquo(var)))
  act$var <- expr_text(get_expr(enquo(var)))

  act$vals_desc <- expr_label(get_expr(list(...))) %>% gsub("(^`list\\()|(\\)`$)", "`", .)
  act$result <- act$val[[act$var]] %in% c(unlist(list(...)),
                                          ifelse(miss, getOption("testdat.miss"), NULL))

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
expect_exclusive <- function(var, exc_var, data = get_testdata()) {
}

#' @export
expect_unique <- function(var, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(rlang::enquo(data))
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(var))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

  flt <- enquo(flt)
  act$result_data <- data %>%
    filter(!!flt) %>%
    group_by(!!!var) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    select(!!!var, count)
    # select(!!!var) %>%
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
expect_unique_across <- function(var, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(rlang::enquo(data))
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(var))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    select(!!!var) %>%
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

#' TODO
#' @export
expect_unique_combine <- function(var, flt = TRUE, data = get_testdata()) {
  browser()
  act <- quasi_label(rlang::enquo(data))
  act$var_desc <- str_replace_all(expr_label(get_expr(enquo(var))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    select(!!!var) %>%
    gather(var, val) %>%


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

#' @export
expect_regex <- function(var, pattern, data = get_testdata()) {
}

expect_allany <- function(var, func, flt = TRUE, data = get_testdata(), args = list(), allany) {
  act <- quasi_label(rlang::enquo(data))
  act$func_desc <- expr_label(get_expr(enquo(func)))
  act$var_desc  <- str_replace_all(expr_label(get_expr(enquo(var))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc  <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")
  act$args_desc <- str_replace_all(expr_label(get_expr(enquo(args))), "(^`list\\(~?)|(\\)`$)", "`")

  act$result <- allany(data, var, func, !!enquo(flt), args)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records failing \\
          {act$func_desc} on variable {act$var_desc}.
          Filter: {act$flt_desc}
          Arguments: {act$args_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}

#' @export
expect_all <- function(...) { expect_allany(..., allany = chk_filter_all) }

#' @export
expect_any <- function(...) { expect_allany(..., allany = chk_filter_any) }

#' @export
expect_func <- function(var, ...) { expect_allany(vars(!!enquo(var)), ..., allany = chk_filter_all) }

#' @export
expect_similar <- function(var, data2, var2, flt = TRUE, flt2 = flt,
                           threshold = 0.05, min = 100, data = get_testdata()) {
  act <- quasi_label(rlang::enquo(data))
  act$var_desc   <- str_replace_all(expr_label(get_expr(enquo(var))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$data2_desc <- expr_label(get_expr(enquo(var)))
  act$var2_desc  <- str_replace_all(expr_label(get_expr(enquo(var))), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc   <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")
  act$flt2_desc  <- str_replace_all(expr_label(get_expr(enquo(flt))), "^TRUE$", "None")

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
