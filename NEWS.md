# testdat 0.4.1

* Updated the failure message on `expect_all()` to include more specific information on the location of erring values. In particular, variables failing the test are now highlighted in the error message. If, for instance, only one of three variables passed in as the `vars` argument fails the test, then only that variable will be mentioned as failing the test in the failure message. This makes it easier to track down the cause of expectation failures when checking multiple variables at once. For example,

```r
expect_all(everything(), is.numeric, data = iris):
#> Error: `iris` has 150 records failing  `is.numeric` on variable `Species`.
#>  Vars: everything()
#>  Filter: None
#>  Arguments: `` 
```

`expect_all()` sits behind many other expectations, which will also see a change to their failure message.

* Soft deprecated `expect_allany()`. This function was implemented purely as a back-end for `expect_all()` and `expect_any()`. The change to `expect_all()` mentioned above differentiates it from `expect_any()` so that the two functions can no longer share a common back-end. Hence `expect_allany()` is now redundant.

* Added `expect_depends()` which allows you to test for functional dependency among variables. See the [wikipedia page](https://en.wikipedia.org/wiki/Functional_dependency) for more information about functional dependencies.

* Fixed a bug that was causing a corrupted excel file to be produced by `output_results_excel()` if the test suite encountered an error (#56).

# testdat 0.4.0

* testdat now has a test data pipe (#60)! You can use the test data pipe `%E>%` to add expectations to a pipe chain.
```r
mtcars %E>%
  expect_base(mpg, TRUE) %>%
  mutate(mpg = NA) %E>%
  expect_base(mpg, FALSE)
```

This is a shorthand way of piping into the `with_testdata()` function.
```r
mtcars %>%
  with_testdata(expect_base(mpg, TRUE)) %>%
  mutate(mpg = NA) %>%
  with_testdata(expect_base(mpg, FALSE))
```

* `set_testdata()` previously always returned a data frame, and evaluated the test data if it was stored as a quosure. It now returns the data as it was stored, to get around a bug when piping data into the `with_testdata()` function (#60).

# testdat 0.3.0

## Breaking changes

### tidyselect (#36)

As of testdat 0.3.0 we have moved to the [tidyselect](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html) framework for variable selections instead of `dplyr::vars()`. tidyselect is the successor to `vars()` - it's a bit cleaner, provides some nifty features like selecting columns with a predicate function using `where()` and finally allows us to get rid of the distinction between multi and single variable expectations.

Unfortunately this will break some code, but it's horrendously difficult to support both methods and it's best to switch before publishing to CRAN. Fortunately it's a simple fix - anywhere that you are currently using `vars()`, replace it with `c()`:
```r
# Old
expect_unique(vars(x, y))
# New
expect_unique(c(x, y))
```

The affected expectations are:

* The uniqueness expectations: `expect_unique()`, `expect_unique_across()`, `expect_unique_combine()`

* The exclusivity expectation: `expect_exclusive()`

* The generic expectation helpers: `expect_all()`, `expect_any()`, `expect_allany()` 

* The checking helper functions:  `chk_filter()`, `chk_filter_vars()`, `chk_filter_all()`, `chk_filter_any()`

A small number of functions have been hard deprecated as they are now redundant:

* `expect_where()` and `chk_filter_where()` have been hard deprecated. They are now equivalent to the corresponding `*_all()` function.

* `chk_filter_vars()` has been renamed to `chk_filter()` and the existing `chk_filter()` has been removed to simplify the set of generic checking functions.

Auto-generated expectations have also changed slightly - previously they could either accept a single unquoted variable name or a group of variables specified with `vars()`. They now always accept multiple columns using tidyselect syntax. As a result, the name of the first argument for these expect has changed from `var` to `vars`, so be careful if you're using this as a named argument.

tidyselect syntax allows single unquoted variable names as well as arbitrary groups of variable specifications, and all of the auto-generated expectations in the package used the single variable variant so this shouldn't break existing code.

These are all valid column specifications using tidyselect:
```r
expect_values(a, 1:10)
expect_values(c(a, b), 1:10)
expect_values(a:c, 1:10)
expect_values(matches("^[ab]$"), 1:10)
expect_values(c(matches("^[ab]$"), c), 1:10)
expect_values(where(is.numeric), 1:10)
```

### Others

* `expect_exclusive()` has much improved documentation, and has had the argument name `exc_vars` updated to `var_set` to better reflect its purpose.

* `chk_pattern()` has been renamed to `chk_regex()` to better reflect its purpose.

## Deprecations

* Soft deprecated `context_data()` (#43). `context_data()` is just a wrapper for `set_testdata()`, which has a much more intuitive name.

* Soft deprecated `expect_similar()` (#18). It was a silly way of comparing data frames and we're better off making something new.

## Bug fixes / minor updates

* `chk_blank()` performs checks slightly differently and is much faster as a result (#46).

* `chk_ascii()` was returning `FALSE` if it detected non-ASCII printable characters anywhere in the input vector. It now checks each element of the vector individually.

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

