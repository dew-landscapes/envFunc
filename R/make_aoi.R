

#' Make a region (area) of interest
#'
#' @param layer sf
#' @param filt_col Character name of column in `layer` to filter on (or `NULL`
#' for no filter)
#' @param level Level(s) of `filt_col` to filter
#' @param buffer Numeric (or `FALSE`). Distance to buffer in units of `layer`
#' @param bbox Logical. Return a bounding box around the result?
#' @param out_crs \href{https://epsg.io/}{EPSG} code for coordinate system of
#' result
#'
#' @return
#' @export
#'
#' @examples
  make_aoi <- function(layer
                       , filt_col = NULL
                       , level = NULL
                       , simplify = TRUE
                       , buffer = FALSE
                       , bbox = FALSE
                       , out_crs = 4326 # WGS84
                       ) {

    aoi <- layer %>%
      {if(!is.null(filt_col)) (.) %>% dplyr::filter(grepl(paste0(level, collapse = "|"), !!rlang::ensym(filt_col))) else (.)} %>%
      {if(simplify) (.) %>% dplyr::summarise() else(.)} %>%
      {if(buffer) (.) %>% sf::st_buffer(buffer) else (.)} %>%
      sf::st_transform(crs = out_crs) %>%
      {if(bbox) (.) %>% sf::st_bbox() %>% sf::st_as_sfc() else (.)} %>%
      sf::st_make_valid()

    return(aoi)

  }
