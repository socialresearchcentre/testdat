#' Expectations: functional dependency
#'
#' Test whether one set of variables functionally depend on another set of
#' variables.
#'
#' One set of variables, X, functionally depends on another, Y, if and only if
#' each value in Y corresponds to exactly one value in X. For instance,
#' `course_duration` and `course_topic` functionally depend on `course_code` if
#' each `course_code` corresponds to just one combination of `course_duration`
#' and `course topic`. That is, if two records have the same `course_code` then
#' they must have the same `course_duration` and `course_topic`.
#'
#' See the [wikipedia page](https://en.wikipedia.org/wiki/Functional_dependency)
#' for more information.
#'
#' @inheritParams data-params
#' @param on <[`tidy-select`][dplyr::dplyr_tidy_select]> A set of columns which
#' `vars` are expected to depend on.
#' @inherit data-params return
#'
#' @family data expectations
#' @examples
#'
#' student_course <- data.frame(
#'   student_id = 1:5,
#'   course_code = c(1, 2, 1, 3, 4),
#'   course_duration = c(12, 12, 12, 12, 12),
#'   course_topic = c("Song", "Dance", "Song", "Painting", "Pottery")
#' )
#'
#' # Check that each `course_code` corresponds to exactly one combination of
#' # `course_duration` and `course_topic`
#' expect_depends(
#'   c(course_duration, course_topic),
#'   on = course_code,
#'   data = student_course
#' )
#' @export
expect_depends <- function(vars,
                           on,
                           flt = TRUE,
                           data = get_testdata()) {

  check_expect_data_pipe(enquo(vars))
  act <- quasi_label(enquo(data))
  act$var_desc <- as_label_vars(enquo(vars))
  act$on_desc <- as_label_vars(enquo(on))
  act$flt_desc <- as_label_flt(enquo(flt))

  flt <- enquo(flt)
  inconsistent <- data %>% filter(!!flt) %>% select({{vars}}, {{on}}) %>% distinct()
  inconsistent <- inconsistent %>%
    group_by(across({{on}})) %>%
    count(name='count')

  act$result_data <- data %>%
    left_join(inconsistent, by = intersect(names(inconsistent), names(data)))

  act$result <- act$result_data$count == 1

  expect_custom(
    all(act$result, na.rm = TRUE),
    glue("{act$lab} has {sum(!act$result, na.rm = TRUE)} records failing \\
         functional dependency check of variable `{act$var_desc}` \\
         on variable `{act$on_desc}`.
         Filter: {act$flt_desc}"),
    failed_count = sum(!act$result, na.rm = TRUE),
    total_count = sum(!is.na(act$result)),
    duplicated_ids = act$result_data %>% filter(count > 1) %>% unique
  )

  invisible(act$result)
}
