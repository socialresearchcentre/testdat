
#' Make an expectation from a check function
#'
#' `expect_make()` creates an expection from a vectorised checking function to
#' allow simple generation of domain specific data checks.
#'
#' @param func A function whose first argument takes a vector to check, and
#'   returns a logical vector of the same length with the results.
#' @param func_desc A character function description to use in the expectation
#'   failure message.
#' @param vars Should the generated function take a single variable name or a
#'   `vars()` object?
#' @param all Function to use to combine results for each vector.
#' @param env The parent environment of the function, defaults to the calling
#'   environment of `expect_make()`
#' @return An `expect_*()` style function.
#' @examples
#' expect_make(chk_pattern)
#' expect_make(chk_values)
#' @export
expect_make <- function(func, func_desc = NULL, vars = FALSE, all = TRUE, env = caller_env()) {
  enfunc <- enexpr(func)
  args <- formals(eval_tidy(enfunc))[-1] %>% as.list
  args_list <- set_names(lapply(names(args), parse_expr))

  if (all)
    enall <- expr(chk_filter_all)
  else
    enall <- expr(chk_filter_any)

  if (vars) {
    var_arg <- alist(vars = )
    var_expr <- parse_expr("vars")
  } else {
    var_arg <- alist(var = )
    var_expr <- parse_expr("vars(!!ensym(var))")
  }

  new_function(
    exprs(!!!var_arg, !!!args, flt = TRUE, data = get_testdata()),
    expr({
      expect_allany(!!var_expr,
                    !!enfunc,
                    !!parse_expr("!!enquo(flt)"),
                    !!parse_expr("!!enquo(data)"),
                    args = list(!!!args_list),
                    allany = !!enall,
                    func_desc = !!func_desc)
    }),
    env
  )
}

#' Generic Checking Expectations
#'
#' These expectations are auto-generated from the `chk_*()` functions of the
#' same name.
#'
#' @inheritParams expect_allany
#' @name chk_expect
NULL

#' @rdname chk_expect
#' @inheritParams chk_date_yyyy
#' @export
expect_date_yyyy <- expect_make(chk_date_yyyy)

#' @rdname chk_expect
#' @inheritParams chk_date_yyyymm
#' @export
expect_date_yyyymm <- expect_make(chk_date_yyyymm)

#' @rdname chk_expect
#' @inheritParams chk_date_yyyymmdd
#' @export
expect_date_yyyymmdd <- expect_make(chk_date_yyyymmdd)

#' @rdname chk_expect
#' @inheritParams chk_length
#' @export
expect_max_length <- expect_make(chk_length)