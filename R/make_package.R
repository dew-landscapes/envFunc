

#' Make package workflow
#'
#' All now wrapped in if(FALSE) so doesn't do anything. Best run as line by line
#' manually.
#'
#' document, install, knit README.Rmd, build_site, commit
#'
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

  if(FALSE) {

    dots_list <- list(...)

    if(file.exists("data-raw/make_data.R")) source("data-raw/make_data.R")

    devtools::document()

    # devtools::install(dependencies = FALSE)

    if(file.exists("README.Rmd")){

      rmarkdown::render("README.Rmd")

    }

    pkgdown::clean_site()

    # restart R session between these steps
    .rs.restartR() # might not be enough - doesn't unload the packages

    pkgdown::build_site()

    if(do_commit) {

      envFunc::git_commit_env(m)

    }

  }

  return(invisible(NULL))

}
