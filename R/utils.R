
as_char_scipen <- function(x) {
  old <- options(scipen = getOption("testdat.scipen"))
  on.exit(options(old))
  as.character(x)
}

# TODO: remove when minimum R version >= 3.5
if (getRversion() < 3.5) {
  isFALSE <- function(x) is.logical(x) && length(x) == 1L && !is.na(x) && !x
}


as_english_list <- function(x, terminator = "and") {
  if (length(x) <= 1) return(as.character(x))
  out <- paste(x[-length(x)], collapse = ", ")
  paste0(out, " ", terminator, " ", x[length(x)])
}
