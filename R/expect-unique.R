#' Expectations: uniqueness
#'
#' These functions test variables for uniqueness.
#'
#' * `expect_unique()` tests a set of columns (`vars`) and fails if the combined
#' columns do not uniquely identify each row.
#'
#' * `expect_unique_across()` tests a set of columns (`vars`) and fails if each
#' row does not have unique values in each column.
#'
#' * `expect_unique_combine()` tests a set of columns (`vars`) and fails if any
#' value appears more than once across all of them.
#'
#' By default the uniqueness check excludes missing values (as specified by the
#' [testdat.miss][testdat] option). Setting `exclude = NULL` will include all
#' values.
#'
#' @inheritParams data-params
#' @param exclude a vector of values to exclude from uniqueness check.  The
#'   [testdat.miss][testdat] option is used by default. To include all values,
#'   set `exclude = NULL`.
#' @inherit data-params return
#'
#' @family data expectations
#' @seealso [Checks: uniqueness][chk-uniqueness]
#' @name uniqueness-expectations
#' @examples
#'
#' student_fruit_preferences <- data.frame(
#'   student_id = c(1:5, NA, NA),
#'   apple = c(1, 1, 1, 1, 99, NA, NA),
#'   orange = c(2, 3, 2, 3, 99, NA, NA),
#'   banana = c(3, 2, 3, 2, 99, NA, NA),
#'   phone1 = c(123, 456, 789, 987, 654, NA, NA),
#'   phone2 = c(345, 678, 987, 567, 000, NA, NA)
#' )
#'
#' # Check that key is unique, excluding NAs by default
#' expect_unique(student_id, data = student_fruit_preferences)
#'
#' # Check that key is unique, including NAs
#' try(expect_unique(student_id, exclude = NULL, data = student_fruit_preferences))
#'
#' # Check each fruit has unique preference number
#' try(
#' expect_unique_across(
#'   c(apple, orange, banana),
#'   data = student_fruit_preferences
#' )
#' )
#'
#' # Check each fruit has unique preference number, allowing multiple 99 (item
#' # skipped) codes
#' expect_unique_across(
#'   c(apple, orange, banana),
#'   exclude = c(99, NA), data = student_fruit_preferences
#' )
#'
#' # Check that each phone number appears at most once
#' try(expect_unique_combine(c(phone1, phone2), data = student_fruit_preferences))
#'
NULL

#' @export
#' @rdname uniqueness-expectations
expect_unique <- function(vars,
                          exclude = getOption("testdat.miss"),
                          flt = TRUE,
                          data = get_testdata()) {

  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$var_desc <- as_label_vars(enquo(vars))
  act$flt_desc <- as_label_flt(enquo(flt))

  flt <- enquo(flt)
  act$result_data <- data %>%
    filter(!!flt) %>%
    group_by(across({{ vars }})) %>%
    mutate(count = n()) %>%
    ungroup() %>%
    select({{ vars }}, count) %>%
    filter(if_all(-count, ~ !.x %in% exclude))

  act$result <- act$result_data$count == 1

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} duplicate records \\
          on variable `{act$var_desc}`.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    duplicated_ids = act$result_data %>% filter(count > 1) %>% unique
  )

  invisible(act$result)
}

#' @export
#' @rdname uniqueness-expectations
expect_unique_across <- function(vars,
                                 exclude = getOption("testdat.miss"),
                                 flt = TRUE,
                                 data = get_testdata()) {

  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$var_desc <- as_label_vars(enquo(vars))
  act$flt_desc <- as_label_flt(enquo(flt))

  flt <- enquo(flt)
  act$result <- data %>%
    filter(!!flt) %>%
    select({{ vars }}) %>%
    apply(1, function(x) {
    # Number non-na equal to the number unique non-na
    sum(!x %in% exclude) == length(unique(x[!x %in% exclude]))
  })

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records with \\
          duplicates across variables `{act$var_desc}`.
          Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result))
  )

  invisible(act$result)
}

#' @export
#' @rdname uniqueness-expectations
expect_unique_combine <- function(vars,
                                  exclude = getOption("testdat.miss"),
                                  flt = TRUE,
                                  data = get_testdata()) {

  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$var_desc <- as_label_vars(enquo(vars))
  act$flt_desc <- as_label_flt(enquo(flt))

  flt <- enquo(flt)
  test_data <- data %>%
    filter(!!flt) %>%
    select({{ vars }})

  all_values <- unlist(lapply(test_data, function(x) as.character(x)))
  duplicate_values <- setdiff(all_values[duplicated(all_values)], exclude)

  act$result <- test_data %>%
    filter(if_any(everything(), ~ .x %in% duplicate_values))

  expect_custom(
    nrow(act$result) == 0,
    glue("{act$lab} has {nrow(act$result)} records with duplicate values \\
           across variables `{act$var_desc}`.
           Filter: {act$flt_desc}"),
    failed_count = nrow(act$result),
    total_count = nrow(test_data)
  )

  invisible(act$result)
}
