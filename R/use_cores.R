#' Returns a suggested number of cores
#'
#' Provide the output of `use_cores()` to, say, `crew::crew_controller_local()`
#' `workers` argument.
#'
#' @param prop_max_detected What proportion of `parallel::detect_cores()` to
#' use?
#' @param absolute_max Integer. Absolute maximum number of cores to use.
#'
#' @returns Integer
#' @export
#'
#' @examples
#' use_cores()
use_cores <- function(prop_max_detected = 0.5
                      , absolute_max = 128
                      ) {

  possible_cores <- parallel::detectCores() * prop_max_detected
  use_cores <- min(possible_cores, absolute_max)

  return(as.integer(use_cores))

}
