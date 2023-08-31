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
#> [1] "2023-08-31 11:30:17 ACST"
```

## Add a likelihood classification


```r

  x <- tibble::tibble(x = rbeta(10, 1, 1)) %>%
    add_likelihood(x)
#> Joining with `by = join_by(likelihood)`
  
  x
#> # A tibble: 10 x 8
#>         x likelihood             maxVal range         loose very  extreme exceptional
#>     <dbl> <fct>                   <dbl> <fct>         <fct> <fct> <fct>   <fct>      
#>  1 0.604  About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  2 0.357  About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  3 0.401  About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  4 0.906  Very likely             0.95  (0.9,0.95]    -     --    --      --         
#>  5 0.626  About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  6 0.486  About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  7 0.0867 Very unlikely           0.1   (0.05,0.1]    +     ++    ++      ++         
#>  8 0.406  About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  9 0.966  Extremely likely        0.99  (0.95,0.99]   -     --    ---     ---        
#> 10 0.543  About as likely as not  0.667 (0.333,0.667] 0     0     0       0
```

## What else is in `envFunc`

The following functions and data sets are provided in `envFunc`. See https://acanthiza.github.io/envFunc/ for more examples.


```
#> Error in `dplyr::mutate()`:
#> i In argument: `class = purrr::map_chr(object, ~envFunc::vec_to_sentence(class(get(.))))`.
#> Caused by error in `purrr::map_chr()`:
#> i In index: 1.
#> Caused by error in `.f()`:
#> ! lazy-load database 'C:/Users/sysnw/AppData/Local/R/win-library/4.3/envFunc/R/envFunc.rdb' is corrupt
#> Error in eval(expr, envir, enclos): object 'manuals' not found
```




