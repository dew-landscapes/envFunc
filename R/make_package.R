

#' Make package workflow
#'
#' document, install, knit README.Rmd, build_site, commit
#'
#' @param do_commit Logical. Commit to github?
#' @param m Character. Commit message to include.
#'
#' @return Called for side effect of running [devtools::document()],
#' [devtools::install()], [knitr::knit()] "README.Rmd", [pkgdown::build_site()]
#' and, optionally, [envFunc::git_commit_env()].
#' @export
#'
#' @examples
make_package <- function(do_commit = FALSE
                         , m
                         ) {

  rm(list = ls() %>%
       grep("do_commit|m"
            , .
            , value = TRUE
            , invert = TRUE
            )
     )

  if(file.exists("data-raw/make_data.R")) source("data-raw/make_data.R")

  devtools::document()

  devtools::install(dependencies = FALSE)

  if(file.exists("README.Rmd")){

    knitr::knit("README.Rmd")

  }

  pkgdown::build_site()

  pkgdown::clean_site()

  if(do_commit) {

    envFunc::git_commit_env(m)

  }

}
