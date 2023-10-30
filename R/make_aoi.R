

#' Make a region (area) of interest
#'
#' @param layer sf
#' @param filt_col Character name of column in `layer` to filter on (or `NULL`
#' for no filter)
#' @param level Level(s) of `filt_col` to filter
#' @param buffer Numeric (or `FALSE`). Distance to buffer in units of `buf_crs`
#' @param bbox Logical. Return a bounding box around the result?
#' @param clip sf. If not null, this will be used to clip back the original
#' `layer` +/- filtered +/- buffered +/- bbox. CHECK both CRS and `clip_buf`
#' @param clip_buf Numeric (or `FALSE`). Distance to buffer `clip` in units of `clip`
#' @param buf_crs \href{https://epsg.io/}{EPSG} code to use when buffering
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
                       , clip = NULL
                       , clip_buf = 100
                       , buf_crs = 7845
                       , out_crs = 4326 # WGS84
                       ) {

    if(!is.null(clip)) {

      clip <- clip %>%
        sf::st_transform(crs = buf_crs) %>%
        # always buffer slightly to get away from coastline
        sf::st_buffer(clip_buf) %>%
        # to out_crs
        sf::st_transform(crs = out_crs) %>%
        sf::st_make_valid()

    }

    aoi <- layer %>%
      {if(!is.null(filt_col)) (.) %>% dplyr::filter(grepl(paste0(level, collapse = "|"), !!rlang::ensym(filt_col))) else (.)} %>%
      {if(simplify) (.) %>% dplyr::summarise() else(.)} %>%
      {if(buffer) (.) %>% sf::st_transform(crs = buf_crs) %>% sf::st_buffer(buffer) else (.)} %>%
      # to out_crs
      sf::st_transform(crs = out_crs) %>%
      {if(bbox) (.) %>% sf::st_bbox() %>% sf::st_as_sfc() else (.)} %>%
      {if(!is.null(clip)) (.) %>% sf::st_intersection(clip) else (.)} %>%
      sf::st_make_valid()

    return(aoi)

  }
