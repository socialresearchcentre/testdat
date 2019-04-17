# testdat

<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

testdat is designed to ease data validation, particularly for complex data
processing, inspired by software unit testing. testdat extends the strong and
flexible unit testing framework already provided by
[testthat](https://testthat.r-lib.org/) with a family of functions and reporting
tools focused on checking of data frames.

Features include:

* A fully fledged test framework so you can spend more time specifying tests and
  less time running them

* A set of common methods for simply specifying data validation rules

* Repeatability of data tests (avoid unintentionally breaking your dataset!)

* Data-focused reporting of test results

## Installation

You can install the released version of testdat from srclib with:

``` r
srcproj::add_srclib()
install.packages("testdat")
```
