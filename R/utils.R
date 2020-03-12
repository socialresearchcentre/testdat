
as_char_scipen <- function(x) {
  old <- options(scipen = getOption("testdat.scipen"))
  on.exit(options(old))
  as.character(x)
}
