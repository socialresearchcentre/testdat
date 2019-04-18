#' Extension of 'expect' to allow inclusion of custom fields
#'
#' Use expect_custom to allow inclusion of arbitrary data in expectation
#' results. Additional data is stored in a list in an attribute called
#' \code{custom} in the resulting expectation. This allows data expectations to
#' store information about the number of failed and successful cases for
#' reporting of test results.
#'
#' @keywords internal
#' @param ok `TRUE` or `FALSE` indicating if the expectation was successful.
#' @param failure_message Message to show if the expectation failed.
#' @param info Character vector continuing additional information. Included
#'   for backward compatibility only and new expectations should not use it.
#' @param srcref Location of the failure. Should only needed to be explicitly
#'   supplied when you need to forward a srcref captured elsewhere.
#' @param ... Additional data to be added to a list in the \code{custom}
#'   attribute of the resulting expectation.
#' @return An expectation object. Signals the expectation condition
#'   with a `continue_test` restart.
#' @examples
#' # calling expect_custom directly with some custom data
#' x <- expect_custom(TRUE, "Test", extra_data = 1:5, more_data = "Hello")
#' str(x)
#'
#' \dontrun{
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
#' expect_example(x, data = data.frame(x = c(NA, -2:2)))
#' }
#' @export
expect_custom <- function(ok, failure_message, info = NULL, srcref = NULL, ...) {
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

  exp <- expectation(type, message, srcref = srcref)
  exp[["custom"]] <- list(...)

  withRestarts(
    if (ok)
      signalCondition(exp)
    else if (getOption("testdat.stop_on_fail"))
      stop(exp)
    else
      warning(exp),
    continue_test = function(e) NULL
  )

  invisible(exp)
}

#' @export
filter_expect <- function(data, expect_function, ..., not = TRUE) {
  expect_result <- expect_function(..., data = data)
  if (not) expect_result <- !expect_result

  data %>% filter(expect_result)
}

quasi_repl <- function(quo, pattern = NULL, replace = "", label = TRUE) {
  force(quo)

  desc <- expr_label(get_expr(quo))
  if (!is.null(pattern)) desc <- str_replace_all(desc, pattern, replace)
  desc
}

# expectation_type --------------------------------------------------------

expectation_type <- function(exp) {
  stopifnot(is.expectation(exp))
  gsub("^expectation_", "", class(exp)[[1]])
}

