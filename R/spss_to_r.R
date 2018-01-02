#' modified from 
#' https://github.com/lebebr01/SPSStoR/blob/master/R/selectif_to_r.r
spss_to_r <- function(x) {
  
  x <- gsub("^\\s+|\\s+$", "", x)
  x <- gsub("\\.$", "", x)
  
  if(grepl(' ne|~=|<>', x, ignore.case = TRUE)) {
    x <- gsub(' ne|~=|<>', ' %nin% ', x, ignore.case = TRUE)
  } 
  if(grepl(' le', x, ignore.case = TRUE)) {
    x <- gsub(' le', ' <= ', x, ignore.case = TRUE)
  } 
  if(grepl(' ge', x, ignore.case = TRUE)) {
    x <- gsub(' ge', ' >= ', x, ignore.case = TRUE)
  }
  if(grepl(' eq|=', x, ignore.case = TRUE)) {
    x <- gsub(' eq|= ', ' %in% ', x, ignore.case = TRUE)
  } 
  if(grepl(' gt', x, ignore.case = TRUE)) {
    x <- gsub(' gt', ' > ', x, ignore.case = TRUE)
  } 
  if(grepl(' lt', x, ignore.case = TRUE)) {
    x <- gsub(' lt', ' < ', x, ignore.case = TRUE)
  } 
  
  if(grepl(' and ', x, ignore.case = TRUE)) {
    x <- gsub(' and ', ' & ', x, ignore.case = TRUE)
  } 
  if(grepl(' or ', x, ignore.case = TRUE)) {
    x <- gsub(' or ', ' | ', x, ignore.case = TRUE)
  } 
  if(grepl(' not ', x, ignore.case = TRUE)) {
    warning("not conversion may be a bit wonky")
    x <- gsub(' not ', ' ! ', x, ignore.case = TRUE)
  } 
  
  # missing
  if(grepl('missing\\(', x, ignore.case = TRUE)) {
    x <- gsub(' missing\\(', ' is.na(', x, ignore.case = TRUE)
  }
  if(grepl('sysmis\\(', x, ignore.case = TRUE)) {
    x <- gsub(' sysmis\\(', ' is.na(', x, ignore.case = TRUE)
  } 
  
  # any
  if(grepl('any\\(', x, ignore.case = TRUE)) {
    warning("any conversion may be a bit wonky")
    x <- gsub('(any\\()(.*?),(.*\\))', '\\2 %in% c(\\3', x, ignore.case = TRUE)
  } 

  x
}
