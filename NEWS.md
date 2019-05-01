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
