#' Expectations: proportions
#'
#' These test the proportion of data in a data frame satisfying some condition.
#' The generic functions, `expect_prop_lte()` and `expect_prop_gte()`, can be
#' used with any arbitrary function. The `chk_*()` functions, like
#' `chk_values()`, are useful in this regard.
#'
#' Given the use of quasi-quotation within these functions, to make a new
#' functions using one of the generics such as `expect_prop_gte()` one must
#' defuse the `var` argument using the embracing operator `{{ }}`. See the
#' examples sections for an example.
#'
#' @inheritParams data-params
#' @param func A function to use for testing that takes a vector as the first
#'   argument and returns a logical vector of the same length showing whether an
#'   element passed or failed.
#' @param prop The proportion of the data frame expected to satisfy the
#'   condition.
#' @param args A named list of arguments to pass to `func`.
#' @param func_desc A human friendly description of `func` to use in the
#'   expectation failure message.
#' @param ... Vectors of valid values.
#' @inherit data-params return
#'
#' @seealso `chk_*()` functions such as [`chk_values()`][chk-values]
#' @family data expectations
#' @name proportion-expectations
#'
#' @examples
#' sales <- data.frame(
#'   sale_id = 1:5,
#'   date = c("20200101", "20200101", "20200102", "20200103", "2020003"),
#'   sale_price = c(10, 20, 30, 40, -1),
#'   book_title = c(
#'     "Phenomenology of Spirit",
#'     NA,
#'     "Critique of Practical Reason",
#'     "Spirit of Trust",
#'     "Empiricism and the Philosophy of Mind"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' # Create a custom expectation
#' expect_prop_length <- function(var, len, prop, data) {
#'   expect_prop_gte(
#'     var = {{var}}, # Notice the use of the embracing operator
#'     func = chk_max_length,
#'     prop = prop,
#'     data = data,
#'     args = list(len = len),
#'     func_desc = "length_check"
#'   )
#' }
#'
#' # Use it to check that dates are mostly <= 8 char wide
#' expect_prop_length(date, 8, 0.9, sales)
#'
#' # Check price values mostly between 0 and 100
#' try(expect_prop_values(sale_price, 0.9, 1:100, data = sales))
#'
NULL


# Generic ----------------------------------------------------------------------

# internal
expect_prop <- function(var,
                        func,
                        prop,
                        cmp,
                        flt = TRUE,
                        data = get_testdata(),
                        args = list(),
                        func_desc = NULL) {

  stopifnot(length(prop) == 1)
  check_expect_data_pipe(enquo(var))
  act <- quasi_label(enquo(data))
  act$func_desc <- if (is.null(func_desc)) paste0("`", as_label(enquo(func)), "`") else func_desc
  act$var_desc  <- as_label_vars(enquo(var))
  act$flt_desc  <- as_label_flt(enquo(flt))
  act$args_desc <- expr_deparse_repl(args, "(^<list: |>$)", "")

  act$result <- data %>%
    filter(!!flt) %>%
    mutate(across(!!var, ~func(.x, !!!args))) %>%
    pull(!!var)

  act$result_prop <- sum(act$result, na.rm = TRUE) / length(act$result)

  expect_custom(
    cmp(act$result_prop, prop),
    glue("{act$lab} has {sum(act$result, na.rm = TRUE)} records \\
         ({signif(act$result_prop, 2)} of total) satisfying {act$func_desc} \\
         on variable `{act$var_desc}`
         Filter: {act$flt_desc}
         Arguments: `{act$args_desc}`"),
    failed_count = sum(act$result, na.rm = TRUE),
    total_count = length(act$result),
    result = act$result
  )

  invisible(act$result)
}

#' @export
#' @rdname proportion-expectations
expect_prop_lte <- function(var,
                            func,
                            prop,
                            flt = TRUE,
                            data = get_testdata(),
                            args = list(),
                            func_desc = NULL) {

  check_expect_data_pipe(enquo(var))
  var <- ensym(var)
  expect_prop(var, func, cmp = `<=`, prop, flt, data, args, func_desc)
}

#' @export
#' @rdname proportion-expectations
expect_prop_gte <- function(var,
                            func,
                            prop,
                            flt = TRUE,
                            data = get_testdata(),
                            args = list(),
                            func_desc = NULL) {

  check_expect_data_pipe(enquo(var))
  var <- ensym(var)
  expect_prop(var, func, cmp = `>=`, prop, flt, data, args, func_desc)
}


# Specific ---------------------------------------------------------------------

#' @export
#' @rdname proportion-expectations
expect_prop_nmiss <- function(var,
                              prop,
                              miss = getOption("testdat.miss"),
                              flt = TRUE,
                              data = get_testdata()) {

  expect_prop_gte(
    var = {{ var }},
    func = chk_text_nmiss,
    prop = prop,
    flt = flt,
    args = list(miss = miss),
    data = data,
    func_desc = "missing check"
  )
}

#' @export
#' @rdname proportion-expectations
expect_prop_values <- function(var,
                               prop,
                               ...,
                               flt = TRUE,
                               data = get_testdata()) {

  expect_prop_gte(
    var = {{ var }},
    func = chk_values,
    prop = prop,
    flt = flt,
    args = list(...),
    data = data,
    func_desc = "value check"
  )
}
