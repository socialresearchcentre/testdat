# testdat (development version)

* Soft deprecated `context_data()` (#43). `context_data()` is just a wrapper for `set_testdata()`, which has a much more intuitive name.

* Soft deprecated `expect_similar()` (#18). It was a silly way of comparing data frames and we're better off making something new.

# testdat 0.2.0

In addition to minor updates and bug fixes, this release does three main things:
  * Behind the scenes, it (a) reorganises and refactors the code base to reduce
  redundancies and improve clarity, and (b) significantly builds out the testing
  infrastructure.
  * On the wings, it expands the documentation for existing functions considerably.
  * At centre stage, it extends the `expect_*()` framework, e.g. by introducing
  'fuzzy' expectations in the form of `expect_prop_*()`.

## Breaking changes 

* Renamed arguments in `chk_filter()` and `chk_filter_*()` functions to remove the `.` prefix to bring them into line with the expectation functions (#9).

## Deprecations

* Retired `filter_expect()` (#25).

* Hard deprecated `chk_length()`, `chk_miss()` and `chk_nmiss()` (#5).

* Soft deprecated `expect_func()` (#24).

* Soft deprecated `expect_join()` in favour of `expect_subset()` (#21).

## Additions

* Added `chk_filter_where()` and `expect_where()` which can be used to perform scoped expectations (#8).

* Added `expect_unique_combine()` for checking uniqueness across a combination of variables (#22).

* Added `expect_prop_*()` functions for 'fuzzy' expectations. An example is `expect_prop_nmiss()` which can be used to validate a dataset allowing for a certain amount of missingness (#12).

* Added `expect_labels()` for testing variable and value labels (#20).

* Added `exclude` argument to `expect_unique()` and `expect_unique_*()` to allow users to exclude specific values from the uniqueness check. This is particularly useful when the dataset contains missing codes (#26). 

* Added `quosure` argument to `use_testdata()` (#1). This allows the dataset to be specified as a quosure, so all tests will be run against the current version.


## Bug fixes

* Fixed bug in `chk_filter()` which prevented the user from using `vars = vars(everything())`.

* Fixed deprecation warning that cropped up when using `expect_similar` (#19).

* Fixed bug in `expect_similar()` which prevented variables with different names from being compared (#17).

* Fixed deprecation warning that cropped up when using `chk_text_nmiss` (#16).

## Other minor changes

* Documentation for existing functions has been expanded considerably.

* Added `...` argument to `chk_range()` to bring it into line with `expect_range()` (#28).

* Refactored `expect_values()` to implement it using `expect_make()` (#27).

* Added a new `testdat.scipen` (default: `999`) to avoid issues from checks converting numeric variables to scientific notation (#3).

* Added >= 0.8.0 dplyr version dependency (#2).

* All calls to `quo_label()` now use `as_label()` instead as recommended by rlang (#10). This may cause minor changes to printing of test results.

# testdat 0.1.0

Initial release.

## Major changes from early development releases

* Removed `start_data_test()` and `end_data_test()` as they don't have a clear use case.

* Added a new `with_testdata()` function for simpler interactive testing.

* Moved `use_testdat()` into srcproj, it's a more natural home.

* Removed `ExcelReporter`, it is unnecessary since we have Excel output from a ListReporter.

* New expectation function factory `expect_make()` allows users to automagically convert a logical check function of the form used by `chk_*()` into an expectation. See `?expect_make` for details.

* `chk_values()` now takes the vector of missing values as an argument instead of a logical.

* Renamed some `chk_*()` functions for clarity. For the moment the old names are soft-deprecated and will give a warning, _these will be hard-deprecated in the next minor release_.

  * `chk_length()` => `chk_max_length()`

  * `chk_miss()` => `chk_text_miss()`

  * `chk_nmiss()` => `chk_text_nmiss()`

## Minor changes

* Added new expectations and options:

  * `expect_range()` now takes extra allowable values in `...`. Specifically for cases where there is a bounded range with specified values outside (e.g. missing value codes).

  * `expect_base()` has a new flag `missing_valid`. If set to `TRUE`, missing values are considered valid responses for records that meet the base condition. This allows for one-way base checks, i.e. records *not* meeting the base condition should be missing, but for records meeting the base condition we don't care.

* Extensive cleanup of rlang usage to follow better practices. Expression capture for expectation messages should be much cleaner.

* Simplified dependencies.

* Documentation cleanup.

* Remove R CMD Check warnings.

* Improved test coverage
