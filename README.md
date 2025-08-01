
<!-- README.md is generated from README.Rmd. Please edit that file -->

# testdat <a href='https://socialresearchcentre.github.io/testdat/'><img src='man/figures/logo.png' align="right" height="139" />

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/socialresearchcentre/testdat/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/socialresearchcentre/testdat/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/socialresearchcentre/testdat/graph/badge.svg)](https://app.codecov.io/gh/socialresearchcentre/testdat)
[![CRAN
status](https://www.r-pkg.org/badges/version/testdat)](https://CRAN.R-project.org/package=testdat)
<!-- badges: end -->

## Overview

testdat is designed to ease data validation, particularly for complex
data processing, inspired by software unit testing. testdat extends the
strong and flexible unit testing framework already provided by
[testthat](https://testthat.r-lib.org/) with a family of functions and
reporting tools focused on checking of data frames.

Features include:

- A fully fledged test framework so you can spend more time specifying
  tests and less time running them

- A set of common methods for simply specifying data validation rules

- Repeatability of data tests (avoid unintentionally breaking your data
  set!)

- Data-focused reporting of test results

## Installation

You can install the released version of testdat from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("testdat")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("socialresearchcentre/testdat")
```

## Usage

See the [Introduction to
testdat](https://socialresearchcentre.github.io/testdat/articles/testdat.html)
vignette for a detailed introduction.

``` r
library(testdat, warn.conflicts = FALSE)
#> Loading required package: testthat
library(dplyr, warn.conflicts = FALSE)

x <- tribble(
  ~id, ~pcode, ~state, ~nsw_only,
  1,   2000,   "NSW",  1,
  2,   3123,   "VIC",  NA,
  3,   2123,   "NSW",  3,
  4,   12345,  "VIC",  3
)

with_testdata(x, {
  test_that("id is unique", {
    expect_unique(id)
  })
  
  test_that("variable values are correct", {
    expect_values(pcode, 2000:2999, 3000:3999)
    expect_values(state, c("NSW", "VIC"))
    expect_values(nsw_only, 1:3) # by default expect_values allows NAs
  })
  
  test_that("filters applied correctly", {
    expect_base(nsw_only, state == "NSW")
  })
})
#> Test passed 😀
#> ── Failure: variable values are correct ────────────────────────────────────────
#> get_testdata() has 1 records failing value check on variable `pcode`.
#> Variable set: `pcode`
#> Filter: None
#> Arguments: `<int: 2000L, 2001L, 2002L, 2003L, 2004L, ...>, <int: 3000L, 3001L, 3002L,`
#> get_testdata() has 1 records failing value check on variable `pcode`.
#> Variable set: `pcode`
#> Filter: None
#> Arguments: `  3003L, 3004L, ...>, miss = <chr: NA, "">`
#> Error:
#> ! Test failed
```
