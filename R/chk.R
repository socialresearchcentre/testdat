#' Generic checking functions
#'
#' These functions provide common, simple data checks.
#'
#' @param x vector to check
#' @param miss vector of values to be treated as missing
#' @param ... vectors of valid values
#' @return A logical vector flagging records that have passed or failed the
#'   check
#' @examples
#' sales <- data.frame(
#'   sale_id = 1:5,
#'   date = c("20200101", "20200101", "20200102", "20200103", "2020003"),
#'   sale_price = c(10, 20, 30, 40, -1),
#'   book_title = c(
#'     "Phenomenology of Spirit",
#'     NA,
#'     "Critique of Practical Reason",
#'     "Spirit of Trust",
#'     "Empiricism and the Philosophy of Mind"
#'   ),
#'   stringsAsFactors = FALSE
#' )
#'
#' lapply(sales, function(x) any(chk_blank(x))) # Any data missing?
#'
#' with(sales, date[!chk_date_yyyymmdd(date)]) # Examine invalid dates
#'
#' all(chk_unique(sales$sale_id)) # Valid key?
#'
#' # Clean out invalid prices
#' sales$sale_price[!chk_range(sales$sale_price, 0, Inf)] <- NA
#' sales
#'
#' # detect non-ASCII characters
#' chk_ascii(c("a", "\U1f642"))
#'
#' @seealso [Checking Helper Functions][chk-helper]
#' @name chk-generic
NULL

#' @rdname chk-generic
#' @export
chk_dummy <- function(x) {
  x == x
}

#' @rdname chk-generic
#' @export
chk_blank <- function(x) {
  if (is.character(x) | is.factor(x)) {
    is.na(x) | x == ""
  } else {
    is.na(x)
  }
}

#' @rdname chk-generic
#' @param val value for equality check
#' @export
chk_equals <- function(x, val) {
  chk_blank(x) | x == val
}

#' @rdname chk-generic
#' @param min minimum value for range check
#' @param max maximum value for range check
#' @export
chk_range <- function(x, min, max, ...) {
  is_blank <- chk_blank(x)
  in_interval <- ifelse(
    suppressWarnings((as.numeric(x) >= min & as.numeric(x) <= max)) %in% NA,
    FALSE,
    suppressWarnings((as.numeric(x) >= min & as.numeric(x) <= max))
  )

  is_blank | in_interval | chk_values(x, ...)
}

#' @rdname chk-generic
#' @param pattern pattern to look for as defined in
#'   [str_detect()][stringr::str_detect()]
#' @importFrom stringr str_detect
#' @export
chk_pattern <- function(x, pattern) {
  x <- as_char_scipen(x)
  chk_blank(x) | str_detect(x, pattern)
}

#' @rdname chk-generic
#' @param len maximum string length for checking string variables
#' @importFrom stringr str_length
#' @export
chk_max_length <- function(x, len) {
  x <- as_char_scipen(x)
  chk_blank(x) | str_length(x) <= len
}

#' @rdname chk-generic
#' @export
chk_text_miss <- function(x, miss = getOption("testdat.miss_text")) {
  tolower(x) %in% miss
}

#' @rdname chk-generic
#' @export
chk_text_nmiss <- function(x, miss = getOption("testdat.miss_text")) {
  !chk_text_miss(x, miss)
}

#' @rdname chk-generic
#' @export
chk_unique <- function(x) {
  chk_blank(x) | !(duplicated(x, fromLast = TRUE) | duplicated(x, fromLast = FALSE))
}

#' @rdname chk-generic
#' @export
chk_ascii <- function(x) {
  x <- as_char_scipen(x)
  chk_blank(x) | !grepl("[^\x20-\x7E]", x)
}

#' @rdname chk-generic
#' @export
chk_values <- function(x, ..., miss = getOption("testdat.miss")) {
  old <- options(scipen = getOption("testdat.scipen"))
  on.exit(options(old))
  x %in% c(unlist(list(...)), miss)
}

#' @rdname chk-generic
#' @importFrom stringr str_detect
#' @export
chk_date_yyyymmdd <- function(x) {
  check_lubridate_installed()
  chk_blank(x) | (str_detect(x, "[0-9]{8}") & !is.na(lubridate::ymd(x, quiet = TRUE)))
}

#' @rdname chk-generic
#' @importFrom stringr str_detect
#' @export
chk_date_yyyymm <- function(x) {
  check_lubridate_installed()
  chk_blank(x) | (str_detect(x, "[0-9]{6}") & !is.na(lubridate::ymd(paste0(x, "01"), quiet = TRUE)))
}

#' @rdname chk-generic
#' @importFrom stringr str_detect
#' @export
chk_date_yyyy <- function(x) {
  check_lubridate_installed()
  chk_blank(x) | (str_detect(x, "[0-9]{4}") & !is.na(lubridate::ymd(paste0(x, "0101"), quiet = TRUE)))
}

check_lubridate_installed <- function() {
  if (!requireNamespace("lubridate", quietly = TRUE)) {
    stop("Package \"lubridate\" is needed for date format validation. ",
         "Please install it.",
         call. = FALSE)
  }
}
