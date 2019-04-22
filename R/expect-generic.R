#' Expectations: generic expectations
#'
#' These functions allow for testing of a dataset using an arbitrary function.
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
#' @seealso [check_generic] for a set of generic checking functions
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
  act$func_desc <- if (is.null(func_desc)) quo_label(enquo(func)) else func_desc
  act$var_desc  <- quo_label_vars(enquo(vars))
  act$flt_desc  <- quo_label_flt(enquo(flt))
  act$args_desc <- quo_label_repl(args, "(^`list\\()|(\\)`$)", "`")
  # act$args_desc <- lapply(args, as_label) %>% paste0(collapse = ", ")

  act$result <- allany(eval_tidy(enquo(data)),
                       vars,
                       eval_tidy(enquo(func)),
                       !!enquo(flt),
                       args)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records failing \\
          {act$func_desc} on variable {act$var_desc}.
          Filter: {act$flt_desc}
          Arguments: {act$args_desc}"),
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

#' @export
#' @rdname generic-expectations
expect_func <- function(var, ...) {
  expect_allany(vars(!!ensym(var)), ..., allany = chk_filter_all)
}
