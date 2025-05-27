#' Return a named object from within another object
#'
#' from https://stackoverflow.com/questions/58400176/r-find-object-by-name-in-deeply-nested-list
#'
#' @param haystack Any object
#' @param needle A named object within `haystack`
#'
#' @return The object named `needle` or `NULL` if `needle` was not found within
#' `haystack`
#' @export
#'
#' @examples
#' l <- list("a" = sample(letters, 10), "b" = list("c" = sample(letters, 2), "d" = sample(letters, 4)))
#' find_name(l, "d")
#' find_name(l, "b")
find_name <- function(haystack, needle) {

  if (hasName(haystack, needle)) {

    haystack[[needle]]

  } else if (is.list(haystack)) {

    for (obj in haystack) {

      ret <- Recall(obj, needle)

      if (!is.null(ret)) return(ret)

    }

  } else {

    NULL

  }

}
