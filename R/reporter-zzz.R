
#' Get/set test data
#'
#' A global test data set is used to avoid having to re-specify the testing
#' dataset in every test. These functions get and set the global data or set the
#' data for the current context.
#'
#' @param data Data to be used.
#' @param quosure If `TRUE`, the default, the dataset is stored as a quosure and
#'   `get_testdata()` will return the current state of the dataset.
#'
#'   If `FALSE`, the dataset will be copied and `get_testdata()` will return the
#'   state of the dataset at the time `set_testdata()` was called.
#' @examples
#' set_testdata(mtcars)
#' head(get_testdata())
#'
#' with_testdata(iris, {
#'   x <- get_testdata()
#'   print(head(x))
#' })
#' @name global-data
NULL

testdat_env <- new.env(parent = emptyenv())

#' @export
#' @rdname global-data
set_testdata <- function(data, quosure = TRUE) {
  old <- testdat_env$test_data
  if (quosure) data <- enquo(data)
  assign("test_data", data, testdat_env)
  invisible(eval_tidy(old))
}

#' @export
#' @rdname global-data
get_testdata <- function() {
  dat <- testdat_env$test_data

  if (is.null(dat))
    stop("A dataset has not been specified for the current context. ",
         "Use `context_data()` to set the dataset.",
         call. = FALSE)

  return(eval_tidy(dat))
}

#' @export
#' @rdname global-data
context_data <- function(data) {
  set_testdata(data)
}

#' @export
#' @rdname global-data
#' @param code Code to execute.
with_testdata <- function(data, code, quosure = TRUE) {
  old <- set_testdata(data, quosure = quosure)
  on.exit(set_testdata(old), add = TRUE)

  force(code)

  invisible(data)
}

data_reporter <- function() {
  "data"
}
