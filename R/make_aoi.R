

#' Make a region (area) of interest
#'
#' @param polygons sf
#' @param filt_col Character name of column in `polygons` to filter on (or `NULL`
#' for no filter)
#' @param filt_level Level(s) of `filt_col` to filter
#' @param buffer Numeric (or `FALSE`). Distance to buffer in units of `buf_crs`
#' @param bbox Logical. Return a bounding box around the result?
#' @param clip sf. If not null, this will be used to clip back the original
#' `polygons` +/- filtered +/- buffered +/- bbox. CHECK both CRS and `clip_buf`
#' @param clip_buf Numeric (or `FALSE`). Distance to buffer `clip` in units of `clip`
#' @param buf_crs \href{https://epsg.io/}{EPSG} code to use when buffering
#' @param out_crs \href{https://epsg.io/}{EPSG} code for coordinate system of
#' result
#'
#' @return
#' @export
#'
#' @examples
  make_aoi <- function(polygons
                       , filt_col = NULL
                       , filt_level = NULL
                       , simplify = TRUE
                       , buffer = FALSE
                       , bbox = FALSE
                       , clip = NULL
                       , clip_buf = 100
                       , buf_crs = 7845
                       , out_crs = 4326 # WGS84
                       ) {

    if(!is.null(clip)) {

      if(clip_buf > 0) clip <- clip %>%
        sf::st_transform(crs = buf_crs) %>%
        sf::st_buffer(clip_buf)

      clip <- clip %>%
        sf::st_transform(crs = out_crs) %>%
        sf::st_make_valid()

    }

    aoi <- if(!is.null(filt_col)) polygons %>%
        dplyr::filter(grepl(paste0(filt_level, collapse = "|"), !!rlang::ensym(filt_col))) %>%
        sf::st_make_valid() else polygons

    if(simplify) aoi <- aoi %>%
        dplyr::summarise() %>%
        sf::st_make_valid()

    if(buffer > 0) aoi <- aoi %>%
        sf::st_transform(crs = buf_crs) %>%
        sf::st_buffer(buffer)

    aoi <- aoi %>%
      sf::st_transform(crs = out_crs)

    if(!is.null(clip)) aoi <- aoi %>%
        sf::st_intersection(clip)

    # bbox last to ensure
    if(bbox) aoi <- aoi %>%
        sf::st_bbox() %>%
        sf::st_as_sfc() %>%
        sf::st_sf()

    aoi <- aoi %>%
      sf::st_make_valid()

    return(aoi)

  }
