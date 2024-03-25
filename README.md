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
#> [1] "2024-03-25 11:01:16 ACDT"
```

## Add a likelihood classification


```r

  x <- tibble::tibble(x = rbeta(10, 1, 1)) %>%
    add_likelihood(x)
#> Joining with `by = join_by(likelihood)`
  
  x
#> # A tibble: 10 × 8
#>        x likelihood             maxVal range         loose very  extreme exceptional
#>    <dbl> <fct>                   <dbl> <fct>         <fct> <fct> <fct>   <fct>      
#>  1 0.561 About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  2 0.164 Unlikely                0.333 (0.1,0.333]   +     +     +       +          
#>  3 0.243 Unlikely                0.333 (0.1,0.333]   +     +     +       +          
#>  4 0.740 Likely                  0.9   (0.667,0.9]   -     -     -       -          
#>  5 0.958 Extremely likely        0.99  (0.95,0.99]   -     --    ---     ---        
#>  6 0.364 About as likely as not  0.667 (0.333,0.667] 0     0     0       0          
#>  7 0.844 Likely                  0.9   (0.667,0.9]   -     -     -       -          
#>  8 0.265 Unlikely                0.333 (0.1,0.333]   +     +     +       +          
#>  9 0.313 Unlikely                0.333 (0.1,0.333]   +     +     +       +          
#> 10 0.687 Likely                  0.9   (0.667,0.9]   -     -     -       -
```

## What else is in `envFunc`

The following functions and data sets are provided in `envFunc`. See https://acanthiza.github.io/envFunc/ for more examples.


```
#> Error in `dplyr::mutate()`:
#> ℹ In argument: `class = purrr::map_chr(object, ~envFunc::vec_to_sentence(class(get(.))))`.
#> Caused by error in `purrr::map_chr()`:
#> ℹ In index: 25.
#> Caused by error in `get()`:
#> ! object 'timer' not found
#> Error in eval(expr, envir, enclos): object 'manuals' not found
```




