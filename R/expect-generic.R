#' @include expectation.R
NULL

#' Expectations: generic helpers
#'
#' These functions allow for testing of multiple columns (`vars`) of a data
#' frame (`data`), with an optional filter (`flt`), using an arbitrary function
#' (`func`).
#'
#' * `expect_allany()` tests the columns in `vars` to see whether `func`
#' returns `TRUE` for each of them, and combines the results for each row using
#' the function in `allany`. Both `expect_all()` and `expect_any()` are wrappers
#' around `expect_allany()`.
#'
#' * `expect_all()` tests the `vars` to see whether `func` returns `TRUE` for
#' *all* of them (i.e. whether the conjunction of results of applying `func` to
#' each of the `vars` is `TRUE`).
#'
#' * `expect_any()` tests the `vars` to see whether `func` returns `TRUE` for
#' *any* of them (i.e. whether the disjunction of the results of applying `func`
#' to each of the `vars` is `TRUE`).
#'
#' @inheritParams data-params
#' @param func A function to use for testing that takes a vector as the first
#'   argument and returns a logical vector of the same length showing whether an
#'   element passed or failed.
#' @param args A named list of arguments to pass to `func`.
#' @param func_desc A human friendly description of `func` to use in the
#'   expectation failure message.
#' @inherit data-params return
#'
#' @seealso `chk_*()` functions such as [`chk_values()`][chk-values]
#' @family data expectations
#' @name generic-expectations
#' @examples
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches *AND* < 100 horsepower
#' try(
#' expect_all(
#'   vars = c(disp, hp),
#'   func = chk_range,
#'   flt = (cyl == 4),
#'   args = list(min = 0, max = 100),
#'   data = mtcars
#' )
#' )
#'
#' # Check that every 4-cylinder car has an engine displacement of < 100 cubic
#' # inches *OR* < 100 horsepower
#' try(
#' expect_any(
#'   vars = c(disp, hp),
#'   func = chk_range,
#'   flt = (cyl == 4),
#'   args = list(min = 0, max = 100),
#'   data = mtcars
#' )
#' )
#'
#' # Check that all variables are numeric:
#' try(expect_all(
#'   vars = everything(),
#'   func = is.numeric,
#'   data = iris
#' ))
#'
NULL

#' @export
#' @rdname generic-expectations
expect_all <- function(vars,
                       func,
                       flt = TRUE,
                       data = get_testdata(),
                       args = list(),
                       func_desc = NULL) {
  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$func_desc <- if (is.null(func_desc)) paste0("`", as_label(enquo(func)), "`") else func_desc
  act$var_desc  <- as_label_vars(enquo(vars))
  act$flt_desc  <- as_label_flt(enquo(flt))
  act$args_desc <- expr_deparse_repl(args, "(^<list: |>$)", "")

  act$result <- chk_filter(
    eval_tidy(enquo(data)),
    {{ vars }},
    eval_tidy(enquo(func)),
    {{ flt }},
    args
  )

  act$var_fail <- act$result %>%
    select_if(~any(!., na.rm=TRUE)) %>%
    names()

  act$result <- apply(act$result, 1, all)

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records failing \\
         {act$func_desc} on variable \\
         {as_english_list(paste0('`', act$var_fail, '`'), 'and/or')}.
         Variable set: `{act$var_desc}`
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
expect_any <- function(vars,
                       func,
                       flt = TRUE,
                       data = get_testdata(),
                       args = list(),
                       func_desc = NULL) {
  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$func_desc <- if (is.null(func_desc)) paste0("`", as_label(enquo(func)), "`") else func_desc
  act$var_desc  <- as_label_vars(enquo(vars))
  act$flt_desc  <- as_label_flt(enquo(flt))
  act$args_desc <- expr_deparse_repl(args, "(^<list: |>$)", "")

  act$result <- chk_filter_any(eval_tidy(enquo(data)),
                       {{ vars }},
                       eval_tidy(enquo(func)),
                       {{ flt }},
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

