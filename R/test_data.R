#' Run all tests in specified file.
#'
#' @param path path to file
#' @param reporter reporter to use
#' @param env environment in which to execute the tests
#' @param start_end_reporter whether to start and end the reporter
#' @param load_helpers Source helper files before running the tests?
#' @param encoding File encoding, default is "unknown" `unknown`.
#' @param wrap Automatically wrap all code within [test_that()][testthat::testthat()]?
#' This ensures that all expectations are reported, even if outside a test block.
#' @return the results as a "testdat_results" (list)
#' @export
test_data_file <- function(path, reporter = data_reporter(), env = test_env(),
                           start_end_reporter = TRUE, load_helpers = TRUE,
                           encoding = "unknown", wrap = TRUE) {
  library(testdat)

  if (!file.exists(path)) {
    stop("`path` does not exist", call. = FALSE)
  }

  if (!missing(encoding) && !identical(encoding, "UTF-8")) {
    warning("`encoding` is deprecated; all files now assumed to be UTF-8", call. = FALSE)
  }

  reporter <- testthat:::find_reporter(reporter)
  if (reporter$is_full()) return()

  if (load_helpers) {
    testthat:::source_test_helpers(dirname(path), env = env)
  }

  # lister <- ListReporter$new()
  # if (!is.null(reporter)) {
  #   reporter <- MultiReporter$new(reporters = list(reporter, lister))
  # } else {
  #   reporter <- lister
  # }

  on.exit(testthat:::teardown_run(dirname(path)), add = TRUE)

  testthat:::with_reporter(
    reporter = reporter,
    start_end_reporter = start_end_reporter,
    {
      reporter$start_file(basename(path))

      testthat:::source_file(
        path, new.env(parent = env),
        chdir = TRUE, wrap = wrap
      )

      testthat:::end_context()
    }
  )

  invisible(reporter$get_results())
}

test_data_files <- function(paths,
                            reporter = data_reporter(),
                            env = test_env(),
                            stop_on_failure = FALSE,
                            stop_on_warning = FALSE,
                            wrap = TRUE) {
  if (length(paths) == 0) {
    stop("No matching test file in dir")
  }

  current_reporter <- testthat:::find_reporter(reporter)
  with_reporter(
    reporter = current_reporter,
    results <- lapply(
      paths,
      test_data_file,
      env = env,
      reporter = current_reporter,
      start_end_reporter = FALSE,
      load_helpers = FALSE,
      wrap = wrap
    )
  )

  results <- unlist(results, recursive = FALSE)
  results <- testdat_results(results)

  if (stop_on_failure && !all_passed(results)) {
    stop("Test failures", call. = FALSE)
  }

  if (stop_on_warning && any_warnings(results)) {
    stop("Tests generated warnings", call. = FALSE)
  }

  invisible(results)
}

#' @export
test_data_dir <- function(path,
                          filter = NULL,
                          reporter = data_reporter(),
                          env = test_env(), ...,
                          encoding = "unknown",
                          load_helpers = TRUE,
                          stop_on_failure = FALSE,
                          stop_on_warning = FALSE,
                          wrap = TRUE) {
  if (!missing(encoding) && !identical(encoding, "UTF-8")) {
    warning("`encoding` is deprecated; all files now assumed to be UTF-8", call. = FALSE)
  }

  if (load_helpers) {
    testthat:::source_test_helpers(path, env)
  }
  testthat:::source_test_setup(path, env)
  on.exit(testthat:::source_test_teardown(path, env), add = TRUE)

  withr::local_envvar(list(R_TESTS = "", TESTTHAT = "true"))

  # Promote retirement stages except on CRAN
  if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    withr::local_options(list(lifecycle_verbose_retirement = TRUE))
  }

  paths <- testthat:::find_test_scripts(path, filter, ...)

  test_data_files(
    paths,
    reporter = reporter,
    env = env,
    stop_on_failure = stop_on_failure,
    stop_on_warning = stop_on_warning,
    wrap = wrap
  )
}
