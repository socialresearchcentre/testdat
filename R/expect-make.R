#' @include expect-generic.R
NULL

#' Create an expectation from a check function
#'
#' `expect_make()` creates an expectation from a vectorised checking function to
#' allow simple generation of domain specific data checks.
#'
#' @param func A function whose first argument takes a vector to check, and
#'   returns a logical vector of the same length with the results.
#' @param func_desc A character function description to use in the expectation
#'   failure message.
#' @param vars Included for backwards compatibility only.
#' @param all Function to use to combine results for each vector.
#' @param env The parent environment of the function, defaults to the calling
#'   environment of `expect_make()`.
#' @return An `expect_*()` style function.
#' @examples
#' # Create a custom check
#' chk_binary <- function(x) {
#'   suppressWarnings(as.integer(x) %in% 0:1)
#' }
#'
#' # Create custom expectation function
#' expect_binary <- expect_make(chk_binary)
#'
#' # Validate a data frame
#' try(expect_binary(vs, data = mtcars))
#' try(expect_binary(cyl, data = mtcars))
#'
#' @export
expect_make <- function(func,
                        func_desc = NULL,
                        vars = FALSE,
                        all = TRUE,
                        env = caller_env()) {

  enfunc <- enexpr(func)
  args <- formals(eval_tidy(enfunc))[-1] %>% as.list
  args_list <- set_names(lapply(names(args), parse_expr))

  if (all)
    enall <- expect_all
  else
    enall <- expect_any

  new_function(
    exprs(vars = , !!!args, flt = TRUE, data = get_testdata()),
    expr({
      (!!enall)(!!parse_expr("!!enquo(vars)"),
            !!enfunc,
            !!parse_expr("!!enquo(flt)"),
            !!parse_expr("!!enquo(data)"),
            args = list(!!!args_list),
            func_desc = !!func_desc)
    }),
    env
  )
}
