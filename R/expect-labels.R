#' Expectations: data label checks
#'
#' @inheritParams data-params
#' @inheritParams chk-generic
#' @param val_labels a character vector of expected labels or a named vector of
#'   expected label-value pairs.
#' @param var_label a character vector, the variable label to test for
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
#'     y = labelled::labelled(c("M", "M", "F"), c(Male = "M", Female = "F", Other = "X"))
#'   )
#'
#'   expect_labels(x, c(Male = "M"), data = df)
#'   expect_labels(x, labelled::val_labels(df$y), data = df) # N.B. This passes!
#'   expect_labels(x, "Male", data = df)
#'   expect_labels(x, var_label = "Sex", data = df)
#' }
#'
chk_labels <- function(x, val_labels = NULL, var_label = NULL) {
  match <- chk_dummy(x)

  if (!is.null(val_labels)) {
    if(!is.null(names(val_labels))) {
      x_test <- labelled::labelled(labelled::remove_val_labels(x), labels = val_labels)
      match <- match & (labelled::to_character(x_test) %==% labelled::to_character(x))
    } else {
      match <- match & labelled::to_character(x) %in% val_labels
    }
  }

  if (!is.null(var_label)) {
    match <- match & (labelled::var_label(x) %in% var_label)
  }

  match
}

#' @rdname label-expectations
#' @inheritParams chk-expectations
#' @export
expect_labels <- expect_make(chk_labels, "label check")
