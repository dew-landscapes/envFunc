#' Change the first letter of a string to capital
#'
#' Doesn't alter the capitalisation of any other characters, just the first one.
#'
#' @param string
#'
#' @return
#' @export
#'
#' @examples
#' first_up("DEW")
#' first_up("water from space")
#' first_up("geoscience Australia")
first_up <- function(string) {

  paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))

}
