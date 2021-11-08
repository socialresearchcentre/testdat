# Dummy ------------------------------------------------------------------------

#' Checks: dummy
#'
#' These functions provide common, simple data checks.
#'
#' @param x A vector to check.
#' @return A logical vector flagging records that have passed or failed the
#'   check.
#' @examples
#'
#' chk_dummy(LETTERS)
#'
#' @seealso [Checks: data frame helpers][chk-helper]
#' @family vector checks
#' @name chk-dummy
#' @export
chk_dummy <- function(x) {
  x == x
}

# Pattern ----------------------------------------------------------------------

#' Checks: patterns
#'
#' Check that a vector conforms to a certain pattern.
#'
#' @inherit chk-dummy
#' @examples
#'
#' x <- c("a_1", "b_2", "c_2", NA, "NULL")
#' chk_regex(x, "[a-z]_[0-9]")
#' chk_max_length(x, 3)
#'
#' @seealso [Checks: data frame helpers][chk-helper]
#' @seealso [Expectations: patterns][pattern-expectations]
#' @family vector checks
#' @name chk-patterns
NULL

#' @rdname chk-patterns
#' @param pattern A [str_detect()][stringr::str_detect()] pattern to match.
#' @importFrom stringr str_detect
#' @export
chk_regex <- function(x, pattern) {
  x <- as_char_scipen(x)
  chk_blank(x) | str_detect(x, pattern)
}

#' @rdname chk-patterns
#' @param len Maximum string length.
#' @importFrom stringr str_length
#' @export
chk_max_length <- function(x, len) {
  x <- as_char_scipen(x)
  chk_blank(x) | str_length(x) <= len
}


# Uniqueness -------------------------------------------------------------------

#' Checks: uniqueness
#'
#' Check that each value in a vector is unique.
#'
#' @inherit chk-dummy
#' @examples
#'
#' x <- c(NA, 1:10, NA)
#' chk_unique(x)
#'
#' x <- c(10, 1:10, 10)
#' chk_unique(x)
#'
#' @seealso [Checks: data frame helpers][chk-helper]
#' @seealso [Expectations: uniqueness][uniqueness-expectations]
#' @family vector checks
#' @name chk-uniqueness
NULL

#' @rdname chk-uniqueness
#' @export
chk_unique <- function(x) {
  chk_blank(x) | !(duplicated(x, fromLast = TRUE) | duplicated(x, fromLast = FALSE))
}

# Text -------------------------------------------------------------------------

#' Checks: text
#'
#' Check character vectors for non-ASCII characters or common NULL value
#' placeholders.
#'
#' @inherit chk-dummy
#' @param miss A vector of values to be treated as missing. The
#'   [testdat.miss][testdat] or [testdat.miss_text][testdat] option is used by
#'   default.
#' @examples
#'
#' chk_ascii(c("a", "\U1f642")) # detect non-ASCII characters
#'
#' imported_data <- c(1, "#n/a", 2, "", 3, NA)
#' chk_text_miss(imported_data)
#' chk_text_nmiss(imported_data) # Equivalent to !chk_text_miss(imported_data)
#'
#' @seealso [Checks: data frame helpers][chk-helper]
#' @seealso [Expectations: text][text-expectations]
#' @family vector checks
#' @name chk-text
NULL

#' @rdname chk-text
#' @export
chk_ascii <- function(x) {
  x <- as_char_scipen(x)
  chk_blank(x) | !grepl("[^\x20-\x7E]", x)
}

#' @rdname chk-text
#' @export
chk_text_miss <- function(x, miss = getOption("testdat.miss_text")) {
  tolower(x) %in% miss
}

#' @rdname chk-text
#' @export
chk_text_nmiss <- function(x, miss = getOption("testdat.miss_text")) {
  !chk_text_miss(x, miss)
}

# Values -----------------------------------------------------------------------

#' Checks: values
#'
#' Check that a vector contains only certain values.
#'
#' @inherit chk-dummy
#' @param ... Vectors of valid values.
#' @param miss A vector of values to be treated as missing. The
#'   [testdat.miss][testdat] or [testdat.miss_text][testdat] option is used by
#'   default.
#' @examples
#'
#' x <- c(NA, 0, 1, 0.5, 0, NA, 99)
#' chk_blank(x) # Blank
#' chk_equals(x, 0) # Either blank or 0
#' chk_values(x, 0, 1) # Either blank, 0, 1, or 99
#' chk_range(x, 0, 1) # Either blank or in [0,1]
#' chk_range(x, 0, 1, 99) # Either blank, in [0,1], or equal to 99
#'
#' @seealso [Checks: data frame helpers][chk-helper]
#' @seealso [Expectations: values][value-expectations]
#' @family vector checks
#' @name chk-values
NULL

#' @rdname chk-values
#' @param val A scalar value for the equality check.
#' @export
chk_equals <- function(x, val) {
  chk_blank(x) | x == val
}

#' @rdname chk-values
#' @export
chk_values <- function(x, ..., miss = getOption("testdat.miss")) {
  old <- options(scipen = getOption("testdat.scipen"))
  on.exit(options(old))
  x %in% c(unlist(list(...)), miss)
}

#' @rdname chk-values
#' @param min Minimum value for range check.
#' @param max Maximum value for range check.
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

#' @rdname chk-values
#' @export
chk_blank <- function(x) {
  if (is.character(x) | is.factor(x)) {
    is.na(x) | x == ""
  } else {
    is.na(x)
  }
}

# Dates ------------------------------------------------------------------------

#' Checks: dates
#'
#' Check that a vector conforms to a given date format such as YYYYMMDD.
#'
#' @inherit chk-dummy
#' @examples
#'
#' date <- c(20210101, 20211301, 20210132, 202101, 2021)
#' chk_date_yyyymmdd(date)
#'
#' date <- c(202101, 202112, 202113, 2021)
#' chk_date_yyyymm(date)
#'
#' date <- c("0001", "1688", "1775", "1789", "1791", "1848")
#' chk_date_yyyy(date)
#'
#' @seealso [Checks: data frame helpers][chk-helper]
#' @seealso [Expectations: dates][date-expectations]
#' @family vector checks
#' @name chk-dates
NULL

#' @rdname chk-dates
#' @importFrom stringr str_detect
#' @export
chk_date_yyyymmdd <- function(x) {
  check_lubridate_installed()
  chk_blank(x) | (str_detect(x, "[0-9]{8}") & !is.na(lubridate::ymd(x, quiet = TRUE)))
}

#' @rdname chk-dates
#' @importFrom stringr str_detect
#' @export
chk_date_yyyymm <- function(x) {
  check_lubridate_installed()
  chk_blank(x) | (str_detect(x, "[0-9]{6}") & !is.na(lubridate::ymd(paste0(x, "01"), quiet = TRUE)))
}

#' @rdname chk-dates
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
