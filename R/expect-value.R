#' Expectations: value checks
#'
#' These functions test variable values:
#'   * `expect_values` tests one variable (`var`) against a set of values
#'     (`...`) and fails if there are any other values in the variable
#'   * `expect_regex` tests one variable (`var`) against a regex pattern
#'     (`pattern`) and fails if there are any values not conforming to that
#'     pattern in the variable
#'   * `expect_range` tests one variable (`var`) against a range (defined by
#'     `min`/`max` values) and fails if there are any values in the variable
#'     outside this range
#'
#'
#' @inheritParams data-params
#' @family data expectations
#' @name value-expectations
#' @examples
#'
#' # Check that all values in a variable fall into a given set
#' expect_values(vs, 0:1, data = mtcars)
#'
#' # Check that all values in a variable conform to a certain regex pattern
#' expect_regex(vs, "[01]", data = mtcars)
#'
#' # Check that all values in a variable fall into a given range
#' expect_range(mpg, 10, 40, data = mtcars)
NULL

#' @export
#' @rdname value-expectations
#' @param ... vectors of valid values
expect_values <- function(var, ..., miss = getOption("testdat.miss"), data = get_testdata()) {
  act <- quasi_label(enquo(data))

  act$var_desc <- as_label(ensym(var))
  act$var <- as_name(ensym(var))
  act$vals_desc <- lapply(enexprs(...), as_label)
  act$vals_desc <- paste0(act$vals_desc, collapse = ", ")
  act$result <- act$val[[act$var]] %in% c(unlist(list(...)), miss)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has invalid values in variable `{act$var_desc}`. \\
          {sum(!act$result, na.rm = TRUE)} cases have values other than `{act$vals_desc}`."),
    data = list(table(act$val[[act$var]][!act$result], useNA = "ifany")),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    var_desc = act$var,
    result = act$result
  )

  invisible(act$result)
}

#' @export
#' @rdname value-expectations
#' @param pattern a regex to check
expect_regex <- expect_make(chk_pattern, "pattern check")

#' @export
#' @rdname value-expectations
#' @param min minimum value for range check
#' @param max maximum value for range check
expect_range <- expect_make(
  function(x, min, max, ...) {
    chk_range(x, min, max) | chk_values(x, ...)
  },
  "range check"
)

