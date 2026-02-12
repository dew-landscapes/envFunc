#' Extract env scales from a yaml
#'
#' Requires a scale named 'default' in `scales`. All other elements describe
#' change from 'default'. `NULL` values in default are maintained. Set a
#' non-default list element to `NULL` to remove it from default.
#'
#' @param element Name of element in `scales` for which to extract the
#' scale.
#' @param scales Path to yaml containing the scales relevant to a project.
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
extract_scale <- function(element = "default"
                          , scales = "settings/scales.yaml"
                          ) {

  if(is.character(scales)) scales <- yaml::read_yaml(scales)

  scale <- scales[["default"]]

  if(element != "default") {

    scale <- modifyList(scale
                        , scales[[element]]
                        )

  }

  return(scale)

}
