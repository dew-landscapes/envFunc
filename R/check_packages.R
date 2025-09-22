#' Check packages
#'
#' Check that packages are installed and install them if not. Optionally: make
#' sure that the most recent version of 'env' packages is installed; load the
#' packages; and/or write a bibliography for the packages.
#'
#' @param packages Character. Packages to check
#' @param update_env Logical. Check for any updates to 'env' packages?
#' @param lib Logical. Call `base::library()` for each `packages`?
#' @param bib Logical. Call `knitr::write_bib()` on `packages`?
#' @param ... Passed to the bib_file argument of `knitr::write_bib()`.
#'
#' @return Unique, sorted vector of package names. Optionally, loads packages
#' and/or writes bibliography.
#' @export
#'
#' @examples check_packages(c("dplyr", "envFunc"))
check_packages <- function(packages
                           , update_env = FALSE
                           , env_deps = FALSE
                           , lib = FALSE
                           , bib = FALSE
                           , ...
                           ) {

  packages <- sort(unique(packages))

  # check for missing
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

  new_packages <- new_packages[!grepl("*env", new_packages)]

  if(length(new_packages)) install.packages(new_packages)

  if(update_env) {

    # update 'env' packages
    env_packages <- packages[grepl("*env", packages)]

    purrr::walk(env_packages
                , \(x) remotes::install_github(paste0("dew-landscapes/", x)
                                               , dependencies = env_deps
                                               )
                )

  }

  if(lib) {

    purrr::walk(packages
                , library
                , character.only = TRUE
                )

  }

  if(bib) {

    knitr::write_bib(packages
                     , ...
                     )

  }

  return(packages)

}
