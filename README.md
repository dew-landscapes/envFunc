---
output:
  rmarkdown::github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# `envFunc`: an R package of tools to help with other `env`Packages

<!-- badges: start -->
<!-- badges: end -->

The goal of `envFunc` is to store functions that help across the other `env`Packages.

## Installation

`envFunc` is not on [CRAN](https://CRAN.R-project.org).

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Acanthiza/envFunc")
```

Load `envFunc`


```r
library("envFunc")
```

## Add time stamp

`add_time_stamp` adds the creation time of an object as an attribute.


```r

  temp <- cars %>%
    envFunc::add_time_stamp()

  attr(temp, "ctime")
#> [1] "2023-08-03 11:00:22 ACST"
```

## Add a likelihood classification


```r

  x <- tibble::tibble(x = rbeta(10, 1, 1)) %>%
    add_likelihood(x)
#> Error in `dplyr::mutate()`:
#> i In argument: `likelihood = purrr::map(...)`.
#> Caused by error in `purrr::map()`:
#> i In index: 1.
#> Caused by error:
#> ! Can't find `lulikelihood` in envFunc.
  
  x
#> Error in eval(expr, envir, enclos): object 'x' not found
```

## What else is in `envFunc`

The following functions and data sets are provided in `envFunc`. See https://acanthiza.github.io/envFunc/ for more examples.


```
#> Error in `dplyr::mutate()`:
#> i In argument: `class = purrr::map_chr(object, ~envFunc::vec_to_sentence(class(get(.))))`.
#> Caused by error in `purrr::map_chr()`:
#> i In index: 19.
#> Caused by error in `get()`:
#> ! lazy-load database 'C:/Users/sysnw/AppData/Local/R/win-library/4.3/envFunc/R/envFunc.rdb' is corrupt
#> Error in eval(expr, envir, enclos): object 'manuals' not found
```




