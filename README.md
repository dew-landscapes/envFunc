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
#> [1] "2021-12-08 14:59:21 ACDT"
```

## What else is in `envFunc`

The following functions and data sets are provided in `envFunc`. See https://acanthiza.github.io/envFunc/ for more examples.


|object           |class                      |description                                               |
|:----------------|:--------------------------|:---------------------------------------------------------|
|add_freq_class   |function                   |Convert percentages to frequency classes                  |
|add_time_stamp   |function                   |Add time stamp                                            |
|filter_test_func |function                   |Test rows against a function                              |
|first_up         |function                   |Change the first letter of a string to capital            |
|get_or_make      |function                   |Make an object if it is not available from provided file  |
|git_commit_env   |function                   |Add, commit and push all current changes to github        |
|lulsa            |tbl_df, tbl and data.frame |Lookup for Landscapes South Australia regions             |
|make_package     |function                   |Make package workflow                                     |
|mirror_directory |function                   |Mirror a directory                                        |
|numbers2words    |function                   |Convert a numeric to its corresponding english character. |
|prop_cpu         |function                   |Proportion of current CPU usage                           |
|prop_mem         |function                   |Proportion of current memory usage                        |
|vec_to_sentence  |function                   |Vector to text list                                       |




