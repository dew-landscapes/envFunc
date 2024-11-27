
#' Generate a path to a storage directory
#'
#' @param set_list Named, nested list. Passed to `set_list` argument of
#' `envFunc::name_env_out()` 
#' @param base_dir Character. Path to outputs store.
#'
#' @return Full path to a store directory for a project
#' @export
#'
#' @example inst/examples/store_dir_ex.R
store_dir <- function(set_list, base_dir = fs::path("..", "..", "out")) {
  
  fs::path(base_dir
           , basename(here::here())
           , envFunc::name_env_out(set_list)$path
           )
  
}
