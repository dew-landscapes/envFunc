
#' Add cell to df with lat/long
#'
#' @param ras Raster object with cell numbers to extract
#' @param df Dataframe with lat/long columns
#' @param x Character. Name of latitude column
#' @param y Character. Name of longitude column
#' @param crs_df Single length vector. What crs are x and y?
#' @param add_xy Logical. Generate (centroid) x and y coords from cell?
#'
#' @return df with additional column 'cell' of cell numbers from ras and,
#' dependent on `add_xy` columns `x` and `y`.
#' @export
#'
#' @examples
add_raster_cell <- function(ras
                            , df
                            , x = "long"
                            , y = "lat"
                            , crs_df = 4326
                            , add_xy = FALSE
                            ) {

  df <- df %>%
    dplyr::rename(old_x = !!rlang::ensym(x), old_y = !!rlang::ensym(y))

  df_xy <- df %>%
    dplyr::distinct(old_x, old_y) %>%
    dplyr::filter(!is.na(old_x)
                  , !is.na(old_y)
                  )

  points <- df_xy %>%
    sf::st_as_sf(coords = c("old_x","old_y")
                 , crs = crs_df
                 , remove = FALSE
                 ) %>%
    sf::st_transform(crs = crs(ras)) %>%
    dplyr::bind_cols(as_tibble(sf::st_coordinates(.)) %>%
                       dplyr::rename(ras_x = X, ras_y = Y)
                     ) %>%
    sf::st_set_geometry(NULL)

  cells <- terra::cellFromXY(ras
                             , as.matrix(points[c("ras_x", "ras_y")])
                             ) %>%
    as.integer()

  res <- points %>%
    dplyr::mutate(cell = cells)

  if(add_xy) {

    xy_res <- terra::xyFromCell(ras
                                 , cells
                                 ) %>%
      tibble::as_tibble() %>%
      dplyr::bind_cols(cell = cells) %>%
      dplyr::filter(!is.na(x)
                    , !is.na(y)
                    ) %>%
      sf::st_as_sf(coords = c("x","y")
                   , crs = crs(ras)
                   ) %>%
      sf::st_transform(crs = crs_df) %>%
      dplyr::mutate(!!rlang::ensym(x) := sf::st_coordinates(.)[,1]
                    , !!rlang::ensym(y) := sf::st_coordinates(.)[,2]
                    ) %>%
      sf::st_set_geometry(NULL) %>%
      unique()

    res <- merge(res, xy_res)

  }

  res <- res %>%
    dplyr::right_join(df) %>%
    dplyr::select(cell,everything(),-(contains("old"))) %>%
    dplyr::distinct() %>%
    tibble::as_tibble()

}
