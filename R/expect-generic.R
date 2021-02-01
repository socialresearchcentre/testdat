#' Expectations: generic expectation helpers
#'
#' These functions allow for testing of a dataset using an arbitrary function.
#' Both `expect_all` and `expect_any` are wrappers around `expect_allany`. The
#' former, `expect_all`, tests the variables in `vars` to see whether `func`
#' returns TRUE for *all* of them (i.e. whether the conjunction of results of
#' applying `func` to each of the `vars` is TRUE). The latter, `expect_any`,
#' tests the `vars` to see whether `func` returns TRUE for *any* of them (i.e.
#' whether the disjunction of the results of applying `func` to each of the `vars`
#' is TRUE).
#'
#' @inheritParams data-params
#' @param func a function that takes a vector as the first argument and returns
#'   a logical vector of the same length showing whether an element passed or
#'   failed
#' @param args a named list of arguments to pass to `func`
#' @param allany function to use to combine results for each vector
#' @param func_desc A character function description to use in the expectation
#'   failure message.
#' @param ... arguments to pass to `expect_allany()`
#' @examples
#' \dontrun{
#' set_testdata(mtcars)
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches *AND* < 100 horsepower
#' expect_all(
#'   vars = c("disp", "hp"),
#'   func = chk_range,
#'   flt = (cyl == 4),
#'   args = list(min = 0, max = 100),
#' )
#'
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches *OR* < 100 horsepower
#' expect_any(
#'   vars = c("disp", "hp"),
#'   func = chk_range,
#'   flt = (cyl == 4),
#'   args = list(min = 0, max = 100),
#' )
#' }
#'
#' @seealso [chk-generic] for a set of generic checking functions
#' @family data expectations
#' @name generic-expectations
NULL

#' @export
#' @rdname generic-expectations
expect_allany <- function(vars, func, flt = TRUE, data = get_testdata(),
                          args = list(),
                          allany = c(chk_filter_all, chk_filter_any),
                          func_desc = NULL) {
  act <- quasi_label(enquo(data))
  act$func_desc <- if (is.null(func_desc)) paste0("`", as_label(enquo(func)), "`") else func_desc
  act$var_desc  <- as_label_vars(enquo(vars))
  act$flt_desc  <- as_label_flt(enquo(flt))
  act$args_desc <- as_label_repl(args, "(^list\\()|(\\)$)", "")
  # act$args_desc <- lapply(args, as_label) %>% paste0(collapse = ", ")

  act$result <- allany(eval_tidy(enquo(data)),
                       vars,
                       eval_tidy(enquo(func)),
                       !!enquo(flt),
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
