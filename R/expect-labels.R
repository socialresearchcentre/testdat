#' Expectations: data label checks
#'
#' These expectations test whether or not a variable is labelled as expected.
#'
#'
#'
#' @inheritParams data-params
#' @inheritParams chk-generic
#' @param val_labels a character vector of expected labels, or a named vector of
#'   expected label-value pairs, or `TRUE`, or `FALSE`.
#' @param var_label a character vector, the variable label to test for, or
#'   `TRUE`, or `FALSE`
#'
#' @return A logical vector flagging records that have passed or failed the check
#'
#' @name label-expectations
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'   df <- data.frame(
#'     x = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F"), "Sex"),
#'     y = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F", Other = "X")),
#'     z = c("M", "M", "F")
#'   )
#'
#'   # Check for a value-label pairing
#'   expect_labels(x, c(Male = "M"), data = df)
#'
#'   # Check that two variables have the same values
#'   expect_labels(x, labelled::val_labels(df$y), data = df) # N.B. This passes!
#'
#'   # Check for the presence of a particular label
#'   expect_labels(x, "Male", data = df)
#'   expect_labels(x, var_label = "Sex", data = df)
#'
#'   # Check that a variable is labelled at all
#'   expect_labels(z, val_labels = TRUE, data = df)
#'   expect_labels(z, var_label = TRUE, data = df)
#'
#'   # Check that a variable isn't labelled
#'   expect_labels(z, val_labels = FALSE, data = df)
#'   expect_labels(z, var_label = FALSE, data = df)
#' }
#'
chk_labels <- function(x, val_labels = NULL, var_label = NULL) {
  match <- chk_dummy(x)

  if (!is.null(val_labels)) {
    if(!is.null(names(val_labels))) {
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
#' @inheritParams chk-expectations
#' @export
expect_labels <- expect_make(chk_labels, "label check")
