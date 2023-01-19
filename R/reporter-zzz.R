
#' Get/set test data
#'
#' A global test data set is used to avoid having to re-specify the testing data
#' frame in every test. These functions get and set the global data or set the
#' data for the current context.
#'
#' @param data Data frame to be used.
#' @param quosure If `TRUE`, the default, the data frame is stored as a
#'   [quosure][rlang::quosure()] and lazily evaluated when `get_testdata()` is
#'   called, so `get_testdata()` will return the current state of the data
#'   frame.
#'
#'   If `FALSE`, the data frame will be copied and `get_testdata()` will return
#'   the state of the data frame at the time `set_testdata()` was called.
#' @return
#'   * `set_testdata()` invisibly returns the previous test data. The test data
#'   is returned as it was stored - if it was stored with `quosure = TRUE` it
#'   will be returned as a quosure.
#'   * `get_testdata()` returns the current test data frame.
#'   * `with_testdata()` and the test data pipe `%E>%` invisibly return the
#'   input `data` for easy piping.
#' @examples
#' set_testdata(mtcars)
#' head(get_testdata())
#'
#' with_testdata(iris, {
#'   x <- get_testdata()
#'   print(head(x))
#' })
#'
#' mtcars %E>%
#'   expect_base(mpg, TRUE) %E>%
#'   expect_range(carb, 1, 8)
#' @name global-data
NULL

testdat_env <- new.env(parent = emptyenv())

#' @export
#' @rdname global-data
set_testdata <- function(data, quosure = TRUE) {
  old <- testdat_env$test_data
  if (quosure) data <- enquo(data)
  assign("test_data", data, testdat_env)
  invisible(old)
}

#' @export
#' @rdname global-data
get_testdata <- function() {
  dat <- testdat_env$test_data

  if (is.null(dat))
    stop("A test data frame has not been specified. ",
         "Use `set_testdata()` to set the data frame.",
         call. = FALSE)

  return(ungroup(eval_tidy(dat)))
}

#' @export
#' @rdname global-data
#' @param code Code to execute with the test data set to `data`.
with_testdata <- function(data, code, quosure = TRUE) {
  old <- set_testdata(data, quosure = quosure)
  on.exit(set_testdata(old, quosure = FALSE), add = TRUE)

  force(code)

  invisible(data)
}

#' @rdname global-data
#' @export
`%E>%` <- function(data, code) {
  with_testdata(data, code, quosure = FALSE)
}

data_reporter <- function() {
  "data"
}
