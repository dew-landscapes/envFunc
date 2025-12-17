
#' Make package workflow
#'
#' make any data-raw; document; knit README.Rmd; clean_site; build_site; commit
#'
#'
#' @param m Character. Commit message to include. If `NULL`, then no commit.
#' @param ... Passed to `pkgdown::build_site()`
#'
#' @return Called for side effect of: making any data in data-raw; and then
#' running `devtools::document()`, `knitr::knit()`,
#' "README.Rmd", `pkgdown::clean_site()`, `pkgdown::build_site()`, and,
#' optionally, `envFunc::git_commit_env()`.
#' @export
#'
#' @examples
make_package <- function(m = NULL
                         , ...
                         ) {

  if(file.exists("data-raw/make_data.R")) source("data-raw/make_data.R")

  devtools::document()

  if(file.exists("README.Rmd")){

    rmarkdown::render("README.Rmd")

  }

  pkgdown::clean_site()

  pkgdown::build_site(...)

  if(!is.null(m)) {

    envFunc::git_commit_env(m)

  }

  return(invisible(NULL))

}
