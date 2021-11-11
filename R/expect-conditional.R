#' Expectations: consistency
#'
#' These functions test whether multiple conditions coexist.
#'
#' @inheritParams data-params
#' @inherit data-params return
#' @family data expectations
#' @name conditional-expectations
#' @examples
#' my_survey <- data.frame(
#'   resp_id = 1:5,
#'   q1a = c(0, 1, 0, 1, 0),
#'   q1b = c(NA, NA, NA, 1, 0), # Asked if q1a %in% 1
#'   q2a = c(90, 80, 60, 40, 90),
#'   q2b = c("", "", NA, "Some reason for low rating", "") # Asked if q2a < 50
#' )
#'
#' # Check that q1b has a value if and only if q1a %in% 1
#' try(expect_base(q1b, q1a %in% 1, data = my_survey)) # Fails for resp_id 2 and 5
#'
#' # Check that q2b has a value if and only if q2a < 50
#' expect_base(q2b, q2a < 50, data = my_survey)
#'
#' # Check that if q1a %in% 0 then q2a > 50 (but not vice-versa)
#' expect_cond(q1a %in% 0, q2a > 50, data = my_survey)
#'
NULL

#' @export
#' @describeIn conditional-expectations Checks the coexistence of two
#'   conditions. It can be read as "if `cond1` then `cond2`".
#' @param cond1 <[`data-masking`][dplyr::dplyr_data_masking]> First condition
#'   (antecedent) for consistency check.
#' @param cond2 <[`data-masking`][dplyr::dplyr_data_masking]> Second condition
#'   (consequent) for consistency check.
expect_cond <- function(cond1, cond2, data = get_testdata()) {
  act <- quasi_label(enquo(data))

  cond1 <- enquo(cond1)
  act$cond1_desc <- as_label(cond1)
  act$cond1 <- act$val %>% transmute(!!cond1) %>% pull(1)
  act$cond1[is.na(act$cond1)] <- FALSE

  cond2 <- enquo(cond2)
  act$cond2_desc <- as_label(cond2)
  act$cond2 <- act$val %>% transmute(!!cond2) %>% pull(1)
  act$cond2[is.na(act$cond2)] <- FALSE

  act$result <- !(act$cond1 & !act$cond2)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} failed consistency check. {sum(!act$result, na.rm = TRUE)} \\
          cases have `{act$cond1_desc}` but not `{act$cond2_desc}`."),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    result = act$result
  )

  invisible(act$result)
}

#' @export
#' @describeIn conditional-expectations A special case that checks missing data
#'   against a specified condition. It can be read as "if `base` then `var` not
#'   missing, if not `base` then `var` missing".
#' @param base <[`data-masking`][dplyr::dplyr_data_masking]> The condition that
#'   determines which records should be non-missing.
#' @param missing_valid Should missing values be treated as valid for records
#'   meeting the `base` condition? This allows 'one way' base checks. This is
#'   `FALSE` by default.
expect_base <- function(var,
                        base,
                        miss = getOption("testdat.miss"),
                        missing_valid = FALSE,
                        data = get_testdata()) {

  act <- quasi_label(enquo(data))

  act$var_desc <- as_label(ensym(var))
  act$var <- as_name(ensym(var))

  base <- enquo(base)
  act$base_desc <- as_label(base)
  act$base <- act$val %>% transmute(!!base) %>% pull(1)
  act$base[is.na(act$base)] <- FALSE

  act$miss <- (act$val[[act$var]] %in% miss & !missing_valid) & act$base
  act$nmiss <- !(act$val[[act$var]] %in% miss) & !act$base
  act$result <- !(act$miss | act$nmiss)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has a base mismatch in variable `{act$var_desc}`.
          {sum(act$miss)} cases have `{act$base_desc}` but `{act$var_desc}` is missing.
          {sum(act$nmiss)} cases do not have `{act$base_desc}` but `{act$var_desc}` is non missing."),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    var_desc = act$var,
    result = act$result
  )

  invisible(act$result)
}
