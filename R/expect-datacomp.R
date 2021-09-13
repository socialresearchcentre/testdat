#' Expectations: cross-dataset expectations
#'
#' \Sexpr[results=rd, stage=render]{testdat:::lifecycle("experimental")}
#'
#' These functions allow for dataset comparisons:
#'   * `expect_similar` compares the distribution of a categorical variable
#'     (`var`) from the one dataset (`data`) to that of a categorical variable
#'     (`var2`) from another dataset (`data2`). The test fails if the
#'     distributions are sufficiently dissimilar.
#'   * `expect_valmatch` compares the observations appearing in one dataset
#'     (`data`) to the same observations, as picked out by a key (`by`), in
#'     another dataset (`data2`). It fails if the selected variables (`vars`)
#'     aren't the same for those observations in both datasets.
#'   * `expect_subset` compares one dataset (`data`) to another (`data2`) and
#'     fails if all of the observations in the first, as picked out by a key
#'     (`by`), do not appear in the second
#'
#' @inheritParams data-params
#' @param data2 the dataset to compare against
#' @param not reverse the results of the check
#' @param by a character vector of variables to join by. For details see dplyr
#'   [join()][dplyr::join].
#' @family data expectations
#' @name datacomp-expectations
#' @examples
#'
#' df1 <- data.frame(
#'   id = 0:99,
#'   binomial = sample(0:1, 100, TRUE),
#'   even = abs(0:99%%2 - 1) * 0:99
#' )
#'
#' df2 <- data.frame(
#'   id = 0:99,
#'   binomial = sample(0:1, 100, TRUE),
#'   odd = 0:99%%2 *0:99
#' )
#'
#'
#' # Check categorical distribution is similar across data frames
#' \dontrun{expect_similar(binomial, df2, binomial, data = df1)}
#'
#' # Check that same records 'succeeded' across data frames
#' \dontrun{expect_valmatch(df2, vars(binomial), by = "id", data = df1)}
#'
#' # Check that all records in `df1`, as picked out by `id`, exist in `df2`
#' expect_subset(df2, by = "id", data = df1)
#'
NULL


#' @importFrom tidyselect vars_select
#' @export
#' @rdname datacomp-expectations
expect_valmatch <- function(data2, vars, by, not = FALSE, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc   <- as_label_vars(enquo(vars))
  act$data2_desc <- as_label(enquo(data2))
  act$flt_desc   <- as_label_flt(enquo(flt))
  act$by_desc    <- as_label_repl(enquo(by), "(^c\\()|(\\)$)", "")

  var_list <- vars_select(tbl_vars(data), !!!vars) %>% unname

  if (length(var_list) == 0)
    stop("Variable specification `vars(", act$var_desc, ")` does not match any variables in ", act$label, ".", call. = FALSE)

  if (any(!var_list %in% names(data)) | any(!var_list %in% names(data2)))
    stop("Variable specification `vars(", act$var_desc, ")` specifies variables that are not common to both datasets.", call. = FALSE)

  comp = ifelse(not, "%!=%", "%==%")

  var_expr <-
    var_list %>%
    lapply(function(x) { glue("{x}.x {comp} {x}.y") }) %>%
    lapply(parse_expr) %>%
    set_names(var_list)

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    left_join(data2, by = by) %>%
    mutate(!!!var_expr) %>%
    select(names(var_expr)) %>%
    apply(1, all)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("The join of {act$lab} and `{act$data2_desc}` by `{act$by_desc}` has \\
          {sum(!act$result, na.rm = TRUE)} records with a mismatch on \\
          variables `{act$var_desc}`.
          Filter: {act$flt_desc}
          Comparison: `{comp}`"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}

#' @export
#' @rdname datacomp-expectations
expect_subset <- function(data2, by = NULL, not = FALSE, flt = TRUE, data = get_testdata()) {
  act <- quasi_label(enquo(data))
  act$var_desc   <- as_label_vars(enquo(vars))
  act$data2_desc <- as_label(enquo(data2))
  act$flt_desc   <- as_label_flt(enquo(flt))

  flt <- enquo(flt)

  act$result <- data %>%
    filter(!!flt) %>%
    left_join(data2 %>%
                select(one_of(suppressMessages(common_by(by, data, data2)$y))) %>%
                mutate(`__result` = TRUE) %>%
                unique,
              by = by) %>%
    mutate(`__result` = ifelse(is.na(.data$`__result`), FALSE, .data$`__result`)) %>%
    pull(.data$`__result`)

  if (not) act$result <- !act$result

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records that\\
          {ifelse(not, '', ' do not')} exist in dataset `{act$data2_desc}`.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}
