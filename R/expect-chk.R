#' @include expect-make.R
NULL

# Patterns ---------------------------------------------------------------------
#' Expectations: patterns
#'
#' Test whether variables in a data frame conform to a given pattern.
#'
#' @inheritParams data-params
#' @inherit data-params return
#' @family data expectations
#' @examples
#'
#' sales <- data.frame(
#'   sale_id = 1:5,
#'   item_code = c("a_1", "b_2", "c_2", NA, "NULL")
#' )
#'
#' try(expect_regex(item_code, "[a-z]_[0-9]", data = sales)) # Codes match regex
#' try(expect_max_length(item_code,  3, data = sales)) # Code width <= 3
#'
#' @seealso [Checks: patterns][chk-patterns]
#' @name pattern-expectations
NULL

#' @rdname pattern-expectations
#' @inheritParams chk_regex
#' @export
expect_regex <- expect_make(chk_regex, "pattern check")

#' @rdname pattern-expectations
#' @inheritParams chk_length
#' @export
expect_max_length <- expect_make(chk_max_length, "length check")

# Values -----------------------------------------------------------------------
#' Expectations: values
#'
#' Test whether variables in a data frame contain only certain values.
#'
#' @inherit pattern-expectations
#' @family data expectations
#' @examples
#'
#' sales <- data.frame(
#'   sale_id = 1:5,
#'   date = c("20200101", "20200101", "20200102", "20200103", "20220101"),
#'   sale_price = c(10, 20, 30, 40, -1)
#' )
#'
#' try(expect_values(date, 20000000:20210000, data = sales)) # Dates between 2000 and 2021
#' try(expect_range(sale_price, min = 0, max = Inf, data = sales)) # Prices non-negative
#'
#' @seealso [Checks: values][chk-values]
#' @name value-expectations
NULL

#' @rdname value-expectations
#' @inheritParams chk_values
#' @export
expect_values <- expect_make(chk_values, "value check")

#' @rdname value-expectations
#' @inheritParams chk_range
#' @export
expect_range <- expect_make(chk_range, "range check")



# Text -------------------------------------------------------------------------
#' Expectations: text
#'
#' Test whether variables in a data frame contain common NULL placeholders.
#'
#' @inherit pattern-expectations
#' @family data expectations
#' @examples
#'
#' sales <- data.frame(
#'   sale_id = 1:5,
#'   date = c("20200101", "null", "20200102", "20200103", "null"),
#'   sale_price = c(10, -1, 30, 40, -1)
#' )
#'
#' # Dates not missing
#' try(expect_text_nmiss(date, data = sales))
#'
#' # Date missing if price negative
#' try(expect_text_miss(date, flt = sale_price %in% -1, data = sales))
#'
#' @seealso [Checks: text][chk-text]
#' @name text-expectations
NULL

#' @rdname text-expectations
#' @inheritParams chk_text_miss
#' @export
expect_text_miss <- expect_make(chk_text_miss, "missing check")

#' @rdname text-expectations
#' @inheritParams chk_text_nmiss
#' @export
expect_text_nmiss <- expect_make(chk_text_nmiss, "missing check")

# Dates ------------------------------------------------------------------------
#' Expectations: dates
#'
#' Test whether variables in a data frame conform to a given date format such as
#' YYYYMMDD.
#'
#' @inherit pattern-expectations
#' @family data expectations
#' @examples
#'
#' sales <- data.frame(
#'   sale_id = 1:5,
#'   date = c("20200101", "20200101", "20200102", "20200103", "20220101"),
#'   quarter = c(202006, 202009, 202012, 20203, 20200101),
#'   published = c(1999, 19991, 21, 0001, 20200101)
#' )
#'
#' try(expect_date_yyyymmdd(date, data = sales)) # Full date of sale valid
#' try(expect_date_yyyymm(quarter, data = sales)) # Quarters given as YYYYMM
#' try(expect_date_yyyy(published, data = sales)) # Publication years valid
#'
#' @seealso [Checks: date][chk-dates]
#' @name date-expectations
NULL

#' @rdname date-expectations
#' @inheritParams chk_date_yyyy
#' @export
expect_date_yyyy <- expect_make(chk_date_yyyy, "YYYY date format check")

#' @rdname date-expectations
#' @inheritParams chk_date_yyyymm
#' @export
expect_date_yyyymm <- expect_make(chk_date_yyyymm, "YYYYMM date format check")

#' @rdname date-expectations
#' @inheritParams chk_date_yyyymmdd
#' @export
expect_date_yyyymmdd <- expect_make(chk_date_yyyymmdd, "YYYYMMDD date format check")
