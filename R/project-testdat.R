
#' @importFrom rstudioapi getActiveProject
#' @export
use_testdat <- function(path = rstudioapi::getActiveProject(),
                        excelres = FALSE, exceldefs = FALSE, build = FALSE) {
  project_testdat(path, excelres = excelres, exceldefs = exceldefs, build = build)
}

project_testdat <- function(path, ...) {
  dots <- list(...)

  # ensure path exists
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # end if tests already setup
  if (dir.exists(file.path(path, "tests"))) {
    message("* `tests` directory already exists, leaving as is", call. = FALSE)
    return(invisible())
  }

  # create test directory
  dir.create(file.path(path, "tests"))

  # create test_dir file
  writeLines(c("library(testdat)",
               "",
               "x <- test_dir(\".\")"),
             file.path(path, "tests/_run-all.R"))

  if (dots$excelres) {
    write("output_results_excel(x, \"results.xlsx\")",
          file.path(path, "tests/_run-all.R"), append = TRUE)
  }

  # create example test file
  file.copy(system.file("project-testdat", "test-example.R", package = "testdat"),
            file.path(path, "tests"))

  # copy standard definitions file if required
  if (dots$exceldefs) {
    file.copy(system.file("project-testdat", "definitions.xlsx", package = "testdat"),
              file.path(path, "tests"))
    file.copy(system.file("project-testdat", "def-tests.R", package = "testdat"),
              file.path(path, "tests"))
    write("\nsource(\"def-tests.R\")",
          file.path(path, "tests/test-example.R"), append = TRUE)
  }

  # Create batch file and change build settings
  # skip if build settings already exist
  if (dots$build != FALSE) {
    if (any(grepl("BuildType", readLines(list.files('.', '.Rproj$'))))) {
      message("* Build settings already set up, leaving as is", call. = FALSE)
    }

    if (dots$build == "win" | dots$build == TRUE) {
      writeLines(c("Rscript.exe \"_run-all.R\""),
                 file.path(path, "tests/_run-all.bat"))
      write('\nBuildType: Custom\nCustomScriptPath: tests/_run-all.bat',
            list.files('.', '.Rproj$'), append = TRUE)
    } else if (dots$build == "linux") {
      writeLines(c("#!/bin/bash", "Rscript \"_run-all.R\""),
                 file.path(path, "tests/_run-all.sh"))
      write('\nBuildType: Custom\nCustomScriptPath: tests/_run-all.sh',
            list.files('.', '.Rproj$'), append = TRUE)
    }
  }

  invisible()
}
