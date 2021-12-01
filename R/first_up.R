#' Change the first letter of a string to capital
#'
#' Doesn't alter the capitalisation of any other characters, just the first one.
#'
#' @param string Character vector.
#'
#' @return Character vector with first character upper case.
#' @export
#'
#' @examples
#' first_up("string")
#' first_up(c("string", "another string"))
#' first_up(c("1 string", "another string", "Yet another string"))
first_up <- function(string) {

  paste0(toupper(substr(string, 1, 1)), substr(string, 2, nchar(string)))

}
