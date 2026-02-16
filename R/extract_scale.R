#' Extract env scales from a yaml
#'
#' Requires a scale named 'default' in `scales`. All other elements describe
#' change from 'default'. `NULL` values in default are maintained. Set a
#' non-default list element to `NULL` to remove it from default.
#'
#' @param element Name (or index) of element in `scales` for which to extract
#' the scale.
#' @param scales Path to yaml containing the scales relevant to a project.
#' @param element_default Name (or index) of default element in `scales`.
#'
#' @returns Named, nested list describing `element`
#' @export
#'
#' @examples
#' extent <- list(extent = list(geo = "sa_br_dissolve", temp = "P10Y", blah = NULL))
#' grain <- list(grain = list(geo = "90", temp = "P1Y"))
#' settings <- list(default = c(extent, grain))
#' change <- list(change = list(extent = list(blah = NULL), grain = list(geo = "30")))
#' settings <- c(settings, change)
#' extract_scale(scales = settings)
#' extract_scale(element = "change", scales = settings)
#'
extract_scale <- function(element = 1
                          , scales = "settings/scales.yaml"
                          , element_default = 1
                          ) {

  if(is.character(scales)) scales <- yaml::read_yaml(scales)

  # check that element is a name within scales
  stopifnot(element %in% names(scales))

  # check that scales is now a list
  stopifnot(is.list(scales))

  use_element <- if(is.character(element)) which(names(scales) == element) else element
  use_default <- if(is.character(element_default)) which(names(scales) == element_default) else element_default

  scale <- scales[[use_default]]

  if(isTRUE(use_element != use_default)) {

    scale <- modifyList(scale
                        , scales[[use_element]]
                        )

  }

  return(scale)

}
