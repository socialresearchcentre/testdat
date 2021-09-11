
<!-- README.md is generated from README.Rmd. Please edit that file -->

# testdat

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/socialresearchcentre/testdat/workflows/R-CMD-check/badge.svg)](https://github.com/socialresearchcentre/testdat/actions)
[![Codecov test
coverage](https://codecov.io/gh/socialresearchcentre/testdat/branch/master/graph/badge.svg)](https://codecov.io/gh/socialresearchcentre/testdat?branch=master)
<!-- badges: end -->

## Overview

testdat is designed to ease data validation, particularly for complex
data processing, inspired by software unit testing. testdat extends the
strong and flexible unit testing framework already provided by
[testthat](https://testthat.r-lib.org/) with a family of functions and
reporting tools focused on checking of data frames.

Features include:

-   A fully fledged test framework so you can spend more time specifying
    tests and less time running them

-   A set of common methods for simply specifying data validation rules

-   Repeatability of data tests (avoid unintentionally breaking your
    dataset!)

-   Data-focused reporting of test results

## Installation

You can install the released version of testdat from srclib with:

``` r
# srcproj::add_srclib()
install.packages("testdat")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("socialresearchcentre/testdat")
```
