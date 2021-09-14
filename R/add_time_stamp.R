

#' Add time stamp
#'
#' Add a time stamp attribute to an object. From Stack Exchange Network
#' [posts](https://stackoverflow.com/questions/5327555/object-creation-timestamp)
#' by
#' [Dirk Eddelbuettel](https://stackoverflow.com/users/143305/dirk-eddelbuettel).
#'
#' @param obj Any object to add a `ctime` attribute.
#'
#' @return
#' @export
#'
#' @examples
  add_time_stamp <- function(obj) {

    attr(obj, "ctime") <- Sys.time()

  }
