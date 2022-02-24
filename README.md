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
#> [1] "2022-02-24 10:35:52 ACDT"
```

## What else is in `envFunc`

The following functions and data sets are provided in `envFunc`. See https://acanthiza.github.io/envFunc/ for more examples.


```
#> Error: Problem with `mutate()` column `class`.
#> i `class = purrr::map_chr(object, ~envFunc::vec_to_sentence(class(get(.))))`.
#> x lazy-load database 'C:/Users/nwilloughby/Documents/R/win-library/4.0/envFunc/R/envFunc.rdb' is corrupt
#> Error in knitr::kable(manuals[, 2:4]): object 'manuals' not found
```




