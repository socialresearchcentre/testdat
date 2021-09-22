#' @include expect-make.R
NULL

#' Expectations: auto-generated
#'
#' These expectations are auto-generated from the `chk_*()` functions of the
#' same name. See the [Generic Checking Functions][chk-generic] entry for
#' details.
#'
#' @inheritParams data-params
#' @family data expectations
#' @seealso [Generic Checking Functions][chk-generic]
#' @name chk-expectations
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
#' # Validate sales
#' try(expect_text_nmiss(book_title, data = sales)) # Titles non-missing
#' try(expect_date_yyyymmdd(date, data = sales)) # Dates in correct format
#' try(expect_values(date, 20000000:20210000, data = sales)) # Dates between 2000 and 2021
#' try(expect_range(sale_price, min = 0, max = Inf, data = sales)) # Prices non-negative
NULL


#' @rdname chk-expectations
#' @param ... Vectors of valid values.
#' @export
expect_values <- expect_make(chk_values, "value check")

#' @rdname chk-expectations
#' @inheritParams chk_pattern
#' @export
expect_regex <- expect_make(chk_pattern, "pattern check")

#' @rdname chk-expectations
#' @inheritParams chk_range
#' @export
expect_range <- expect_make(chk_range, "range check")

#' @rdname chk-expectations
#' @inheritParams chk_date_yyyy
#' @export
expect_date_yyyy <- expect_make(chk_date_yyyy, "YYYY date format check")

#' @rdname chk-expectations
#' @inheritParams chk_date_yyyymm
#' @export
expect_date_yyyymm <- expect_make(chk_date_yyyymm, "YYYYMM date format check")

#' @rdname chk-expectations
#' @inheritParams chk_date_yyyymmdd
#' @export
expect_date_yyyymmdd <- expect_make(chk_date_yyyymmdd, "YYYYMMDD date format check")

#' @rdname chk-expectations
#' @inheritParams chk_length
#' @export
expect_max_length <- expect_make(chk_max_length, "length check")

#' @rdname chk-expectations
#' @inheritParams chk_text_miss
#' @export
expect_text_miss <- expect_make(chk_text_miss, "missing check")

#' @rdname chk-expectations
#' @inheritParams chk_text_nmiss
#' @export
expect_text_nmiss <- expect_make(chk_text_nmiss, "missing check")
