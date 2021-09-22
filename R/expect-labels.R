#' Expectations: data label checks
#'
#' These expectations test whether or not a variable is labelled as expected.
#'
#' @inheritParams data-params
#' @inheritParams chk-generic
#' @param val_labels What value label check should be performed? One of:
#'   * A character vector of expected value labels.
#'   * A named vector of expected label-value pairs.
#'   * `TRUE` to test for the presence of value labels in general.
#'   * `FALSE` to test for the absence of value labels.
#'   * `NULL` to ignore value labels when checking.
#' @param var_label What variable label check should be performed? One of:
#'   * A character vector of expected variable labels.
#'   * `TRUE` to test for the presence of a variable labels.
#'   * `FALSE` to test for the absence of a variable labels.
#'   * `NULL` to ignore the variable label when checking.
#' @return A logical vector flagging records that have passed or failed the
#'   check.
#'
#' @family data expectations
#' @name label-expectations
#' @examples
#'
#' df <- data.frame(
#'   x = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"), "Sex"),
#'   y = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F", Other = "X")),
#'   z = c("M", "M", "F")
#' )
#'
#' # Check for a value-label pairing
#' try(expect_labels(x, c(Male = "M"), data = df))
#'
#' # Check that two variables have the same values
#' expect_labels(x, labelled::val_labels(df$y), data = df) # N.B. This passes!
#'
#' # Check for the presence of a particular label
#' try(expect_labels(x, "Male", data = df))
#' expect_labels(x, var_label = "Sex", data = df)
#'
#' # Check that a variable is labelled at all
#' try(expect_labels(z, val_labels = TRUE, data = df))
#' try(expect_labels(z, var_label = TRUE, data = df))
#'
#' # Check that a variable isn't labelled
#' expect_labels(z, val_labels = FALSE, data = df)
#' expect_labels(z, var_label = FALSE, data = df)
#'
NULL

#' @rdname label-expectations
#' @export
chk_labels <- function(x, val_labels = NULL, var_label = NULL) {
  if (!requireNamespace("labelled", quietly = TRUE)) {
    stop("Package \"labelled\" is needed for label checks. ",
         "Please install it.",
         call. = FALSE)
  }

  match <- chk_dummy(x)

  if (!is.null(val_labels)) {
    if (!is.null(names(val_labels))) {
      x_test <- labelled::labelled(labelled::remove_val_labels(x), labels = val_labels)
      match <- match & (labelled::to_character(x_test) %==% labelled::to_character(x))
    } else if (isFALSE(val_labels)) {
      match <-  match & is.null(labelled::val_labels(x))
    } else if (isTRUE(val_labels)) {
      match <-  match & !is.null(labelled::val_labels(x))
    } else {
      match <- match & labelled::to_character(x) %in% val_labels
    }
  }

  if (!is.null(var_label)) {
    if (isFALSE(var_label)) {
      match <- match & is.null(labelled::var_label(x))
    } else if (isTRUE(var_label)) {
      match <- match & !is.null(labelled::var_label(x))
    } else {
      match <- match & (labelled::var_label(x) %in% var_label)
    }
  }

  match
}

#' @rdname label-expectations
#' @export
expect_labels <- expect_make(chk_labels, "label check")
