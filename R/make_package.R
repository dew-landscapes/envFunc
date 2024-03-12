

#' Make package workflow
#'
#' document, install, knit README.Rmd, build_site, commit
#'
#' WARNING. Runs rm(list = ls()), leaving only arguments to this function.
#'
#' @param do_commit Logical. Commit to github?
#' @param m Character. Commit message to include
#' @param ... Passed to `pkgdown::build_site()`
#'
#' @return Called for side effect of running `devtools::document()`,
#' `devtools::install()`, `knitr::knit()` "README.Rmd", `pkgdown::build_site()`
#' and, optionally, `envFunc::git_commit_env()`.
#' @export
#'
#' @examples
make_package <- function(do_commit = FALSE
                         , m
                         , ...
                         ) {

  dots_list <- list(...)

  rm(list = ls() %>%
       grep("do_commit|m|dots_list"
            , .
            , value = TRUE
            , invert = TRUE
            )
     )

  if(file.exists("data-raw/make_data.R")) source("data-raw/make_data.R")

  devtools::document()

  # devtools::install(dependencies = FALSE)

  if(file.exists("README.Rmd")){

    knitr::knit("README.Rmd")

  }

  pkgdown::clean_site()

  do.call(pkgdown::build_site
          , dots_list
          )

  if(do_commit) {

    envFunc::git_commit_env(m)

  }

}
