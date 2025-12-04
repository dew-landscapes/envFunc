#' Return the most frequently occurring element of a vector
#'
#' Ties are dealt with by either: randomly selecting from the available values,
#' thus always returning a vector of length 1; or returning all tied values,
#' thus potentially returning a vector of variable length.
#'
#' @param x Vector
#' @param ties Character. If `ties = "sample"`, one value from the available
#' values will be randomly selected. Otherwise all ties are returned. Use
#' anything other than "sample" to return all ties.
#' @param ... Passed to `base::table()`. Use for `NA` handling. Note default
#' handling is `useNA = "no"`, thus `NA` values will be ignored.
#'
#' @returns Vector of the most frequent element(s) of `x`, with the same
#' class as `x`. If `ties = "sample"`, the length of the returned vector will be
#' 1.
#' @export
#'
#' @example inst/examples/get_mode_ex.R
#'

get_mode <- function(x
                     , ties = "sample"
                     , ...
                     ) {

  stopifnot(is.vector(x) | is.factor(x))

  tbl <- table(x, ...)

  tbl <- tbl[tbl > 0] # in case all elements are 0

  suppressWarnings(
    mode_val <- names(which(tbl == max(tbl)))
  )

  if(all(ties == "sample", length(mode_val) > 1)) {
    mode_val <- sample(mode_val, 1)
  }

  mode_val <- if(is.null(mode_val) | length(mode_val) == 0) NA else mode_val

  if("factor" %in% class(x)) {
    mode_val <- factor(mode_val, levels = levels(x))
  } else if(!"factor" %in% class(x)) {
    class(mode_val) <- class(x)
  }

  return(mode_val)

}
