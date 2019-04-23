# testdat 0.0.1

Initial release.

## Major changes from early development releases

* Removed `start_data_test()` and `end_data_test()` as they don't have a clear use case.

* Added a new `with_testdata()` function for simpler interactive testing.

* Moved `use_testdat()` into srcproj, it's a more natural home.

* Removed `ExcelReporter`, it is unnecessary since we have Excel output from a ListReporter.

* New expectation factory function `expect_make()` allows users to automagically convert a logical check function of the form used by `chk_*()` into an expectation.

## Minor changes

* Added new expectations and options:

  * `expect_range()` now takes extra allowable values in `...`. Specifically for cases where there is a bounded range with specified values outside (e.g. missing value codes).

  * `expect_base()` has a new flag `missing_valid`. If set to `TRUE`, missing values are considered valid responses for records that meet the base condition. This allows for one-way base checks, i.e. records *not* meeting the base condition should be missing, but for records meeting the base condition we don't care.

* Extensive cleanup of rlang usage to follow better practices. Expression capture for expectation messages should be much cleaner.

* Simplified dependencies.

* Documentation cleanup.

* Remove R CMD Check warnings.

* Improved test coverage
