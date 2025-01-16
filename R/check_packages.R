#' Check packages
#'
#' Check that packages are installed and install them if not. Optionally, make
#' sure that the most recent version of 'env' packages is installed.
#'
#' @param packages Character. Packages to check
#' @param update_env Logical. Check for any updates to 'env' packages?
#'
#' @return Unique, sorted vector of package names
#' @export
#'
#' @examples setup_packages(c("dplyr", "envFunc"))
check_packages <- function(packages, update_env = FALSE) {

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
                                               , dependencies = FALSE
                                               )
                )

  }

  return(packages)

}
