#' Extension of 'expect' to allow inclusion of custom fields
#'
#' Use expect_custom to allow inclusion of arbitrary data in expectation
#' results. Additional data is stored in a list in an attribute called `custom`
#' in the resulting expectation. This allows data expectations to store
#' information about the number of failed and successful cases for reporting of
#' test results.
#'
#' @keywords internal
#' @param ok `TRUE` or `FALSE` indicating if the expectation was successful.
#' @param failure_message Message to show if the expectation failed.
#' @param info Character vector continuing additional information. Included for
#'   backward compatibility only and new expectations should not use it.
#' @param srcref Location of the failure. Should only needed to be explicitly
#'   supplied when you need to forward a srcref captured elsewhere.
#' @param ... Additional data to be added to a list in the `custom` attribute of
#'   the resulting expectation.
#' @return An expectation object. Signals the expectation condition with a
#'   `continue_test` restart.
#' @examples
#' # calling expect_custom directly with some custom data
#' x <- expect_custom(TRUE, "Test", extra_data = 1:5, more_data = "Hello")
#' str(x)
#'
#' # an example expectation (note additional libraries used)
#' library(rlang)
#'
#' expect_example <- function(var, data = get_testdata()) {
#'   act <- quasi_label(enquo(data))
#'   act$var_desc <- expr_label(get_expr(enquo(var)))
#'   act$var <- expr_text(get_expr(enquo(var)))
#'
#'   act$result <- act$val[[act$var]] > 0
#'   act$result[is.na(act$result)] <- FALSE
#'
#'   expect_custom(
#'     all(act$result, na.rm = TRUE),
#'     glue::glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} cases with \\
#'                 {act$var_desc} not greater than 0."),
#'     failed_count = sum(!act$result, na.rm = TRUE),
#'     total_count = sum(!is.na(act$result))
#'   )
#'
#'   invisible(act$result)
#' }
#'
#' try(expect_example(x, data = data.frame(x = c(NA, -2:2))))
#' @importFrom testthat new_expectation is.expectation quasi_label exp_signal
#' @export
expect_custom <- function(ok,
                          failure_message,
                          info = NULL,
                          srcref = NULL,
                          trace = NULL,
                          ...) {

  type <- if (ok) "success" else "failure"

  # Preserve existing API which appear to be used in package test code
  # Can remove in next major release
  if (missing(failure_message)) {
    warn("`failure_message` is missing, with no default.")
    message <- "unknown failure"
  } else {
    # A few packages include code in info that errors on evaluation
    if (ok) {
      message <- paste(failure_message, collapse = "\n")
    } else {
      message <- paste(c(failure_message, info), collapse = "\n")
    }
  }

  exp <- new_expectation(type, message, srcref = srcref, trace = trace)
  exp[["custom"]] <- list(...)

  exp_signal(exp)
}

# labelling helpers ----

expr_deparse_repl <- function(quo, pattern, replace = "") {
  stringr::str_replace_all(expr_deparse(quo), pattern, replace)
}

as_label_repl <- function(quo, pattern, replace = "") {
  stringr::str_replace_all(as_label(quo), pattern, replace)
}

as_label_vars <- function(quo) {
  as_label_repl(quo, "(^(c|vars)\\()(.*)(\\)$)", "\\3")
}

as_label_flt  <- function(quo) {
  quo_lab <- as_label(quo)
  if (quo_lab == "TRUE")
    "None"
  else
    paste0("`", quo_lab, "`")
}

# check for data frames provided as vars ----

check_expect_data_pipe <- function(first_arg) {
  if (as_label(first_arg) == ".")
    stop(
      "An expectation was called with a `.` as the first argument.\n",
      "Did you accidentally use a `%>%` pipe into an expectation?\n",
      "If you're trying to call an expectation in a pipe, ",
      "use the test data pipe `%E>%`.\n\n",
      "# Bad:\n",
      "mtcars %>% expect_base(mpg, TRUE)\n",
      "\n",
      "# Good:\n",
      "mtcars %E>% expect_base(mpg, TRUE)",
      call. = FALSE
    )
}

# expectation_type ----

expectation_type <- function(exp) {
  stopifnot(is.expectation(exp))
  gsub("^expectation_", "", class(exp)[[1]])
}
