#' Expectations: generic expectations
#'
#' These functions allow for testing of a dataset using an arbitrary function.
#'
#' @inheritParams data-params
#' @param func a function that takes a vector as the first argument and returns
#'   a logical vector of the same length showing whether an element passed or
#'   failed
#' @param args a named list of arguments to pass to `func`
#' @seealso [check_generic] for a set of generic checking functions
#' @family data expectations
#' @name generic-expectations
NULL

#' @rdname generic-expectations
expect_allany <- function(vars, func, flt = TRUE, data = get_testdata(),
                          args = list(), allany = c(chk_filter_all, chk_filter_any)) {
  act <- quasi_label(enquo(data))
  act$func_desc <- quasi_repl(enquo(func))
  act$var_desc  <- quasi_repl(enquo(vars), "(^`vars\\(~?)|(\\)`$)", "`")
  act$flt_desc  <- quasi_repl(enquo(flt), "^TRUE$", "None")
  act$args_desc <- quasi_repl(enquo(args), "(^`list\\(~?)|(\\)`$)", "`")

  act$result <- allany(data, vars, func, !!enquo(flt), args)

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
#' @rdname generic-expectations
expect_all <- function(...) {
  expect_allany(..., allany = chk_filter_all)
}

#' @export
#' @rdname generic-expectations
expect_any <- function(...) {
  expect_allany(..., allany = chk_filter_any)
}

#' @export
#' @rdname generic-expectations
expect_func <- function(var, ...) {
  expect_allany(vars(!!enquo(var)), ..., allany = chk_filter_all)
}
