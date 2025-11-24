#' Return operating system independent 'env root' directory
#'
#' @param which_dir Character. Optional folder path below 'env root' directory
#' @param linux_default Character. If linux, where is 'env root'
#' @param windows_default Character. If windows, where is 'env root'
#'
#' @returns Directory path ("fs_path", "character")
#' @export
#'
#' @examples
get_env_dir <- function(which_dir = NULL
                        , linux_default = "/projects"
                        , windows_default = "H:"
                        ) {

  os <- Sys.info()["sysname"]

  if(os == "Windows") return_dir <- fs::path(windows_default)

  if(os == "Linux") return_dir <- fs::path(linux_default)

  if(!is.null(which_dir)) {

    return_dir <- fs::path(return_dir, which_dir)

  }

  return(return_dir)

}
