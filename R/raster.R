
#' Add cell to df with lat/long
#'
#' @param df Dataframe with lat/long columns
#' @param ras Raster from which cell IDs will be extracted
#' @param x Character. Name of latitude column
#' @param y Character. Name of longitude column
#'
#' @return
#' @export
#'
#' @examples
  add_raster_cell <- function(df
                              , ras = aoiRaster
                              , x = "long"
                              , y = "lat"
                              , crsDf = 4326
                              ) {

    # Needs lat and long to work with
    points <- df %>%
      dplyr::select(x = !!ensym(x), y = !!ensym(y)) %>%
      dplyr::distinct() %>%
      st_as_sf(coords = c("x","y")
               , crs = crsDf
               , remove = FALSE
               ) %>%
      st_transform(crs = st_crs(ras))

    latlong <- points %>%
      as_Spatial()

    cells <- cellFromXY(object = ras
                        , xy = latlong
    )

    res <- points %>%
      st_set_geometry(NULL) %>%
      dplyr::mutate(cell = cells) %>%
      dplyr::rename(!!ensym(x) := x, !!ensym(y) := y) %>%
      as_tibble()

    df %>%
      dplyr::left_join(res) %>%
      dplyr::select(-!!ensym(x),-!!ensym(y))

  }
