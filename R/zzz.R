.onLoad <- function(libname, pkgname) {
  op <- options()
  op.testdat <- list(
    testdat.name = "TESTDAT",
    testdat.miss = c(NA, ""),
    testdat.miss_text = c("error", "null", "0", ".", "-", ",", "na", "#n/a", "", NA),
    testdat.testing = FALSE
  )
  toset <- !(names(op.testdat) %in% names(op))
  if(any(toset)) options(op.testdat[toset])

  invisible()
}
