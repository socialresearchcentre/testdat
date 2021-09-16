# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.testdat <- list(
    testdat.miss = c(NA, ""),
    testdat.miss_text = c("error", "null", "0", ".", "-", ",", "na", "#n/a", "", NA),
    testdat.stop_on_fail = TRUE,
    testdat.scipen = 999
  )
  toset <- !(names(op.testdat) %in% names(op))
  if (any(toset)) options(op.testdat[toset])

  invisible()
}
# nocov end
