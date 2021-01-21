
#' Create an expectation from a check function
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
#' # Create a custom check
#' chk_binary <- function(x) {
#'   suppressWarnings(as.integer(x) %in% 0:1)
#' }
#'
#' # Create custom expectation function
#' expect_binary <- expect_make(chk_binary)
#'
#' # Validate a dataset
#' \dontrun{
#' expect_binary(vs, data = mtcars)
#' expect_binary(cyl, data = mtcars)
#' }
#'
#'
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

#' Expectations: auto-generated
#'
#' These expectations are auto-generated from the `chk_*()` functions of the
#' same name.
#'
#' @inheritParams expect_allany
#' @seealso [Generic Checking Functions][chk-generic]
#' @name chk-expect
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
#' # Validate sales
#' \dontrun{
#' expect_text_nmiss(book_title, data = sales) # Titles non-missing
#' expect_date_yyyymmdd(date, data = sales) # Dates in correct format
#' }
NULL

#' @rdname chk-expect
#' @inheritParams chk_date_yyyy
#' @export
expect_date_yyyy <- expect_make(chk_date_yyyy)

#' @rdname chk-expect
#' @inheritParams chk_date_yyyymm
#' @export
expect_date_yyyymm <- expect_make(chk_date_yyyymm)

#' @rdname chk-expect
#' @inheritParams chk_date_yyyymmdd
#' @export
expect_date_yyyymmdd <- expect_make(chk_date_yyyymmdd)

#' @rdname chk-expect
#' @inheritParams chk_length
#' @export
expect_max_length <- expect_make(chk_max_length)

#' @rdname chk-expect
#' @inheritParams chk_text_miss
#' @export
expect_text_miss <- expect_make(chk_text_miss)

#' @rdname chk-expect
#' @inheritParams chk_text_nmiss
#' @export
expect_text_nmiss <- expect_make(chk_text_nmiss)
