#' Extract bin information from the grain element of a scales yaml
#'
#' A `bin` defines the columns in an `env` workflow that collectively define
#' the grain around which cleaning processes are aimed. This function is
#' intended to explicitly link the grain in a `scales.yaml` with the bins used
#' in the workflow.
#'
#' @param scales_file Character. Path to the `scales.yaml`
#' @param scales_element Character or numeric. Element within `scales_file` with
#' the grain from which to extract bin information
#' @param add Character. Any extra bin columns that cannot be extracted from the
#' grain
#' @param remove Character. Any regular expressions to exclude from the bin
#' columns
#'
#' @returns
#' @export
#'
#' @examples
grain_to_bin <- function(scales_file = "settings/scales.yaml"
                         , scales_element = basename(here::here())
                         , add = c("cell_lat", "cell_long")
                         , remove = c("res_", "grain_time", "taxonomic")
                         ) {

  scales <- envFunc::extract_scale(element = scales_element)

  grain <- scales$grain

  bin <- c(add
           , envFunc::extract_temporal_grain(grain)
           , names(grain)
           )

  if(!is.null(remove)) {

    bin <- bin[! grepl(paste0(remove, collapse = "|"), bin)]

  }

  bin <- bin[! is.numeric(bin)]

  return(bin)

}
