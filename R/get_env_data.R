

#' Get raster values for each unique location in a data frame.
#'
#'
#'
#' @param ras_paths Paths to the rasters.
#' @param df Dataframe with `x` and `y`.
#' @param x,y Columns in `df` with x and y coordinates.
#' @param crs_df Coordinate reference system for `x` and `y`. Passed to the
#' `crs` argument of [sf::st_as_sf()].
#' @param rename_layers If there are multi-band rasters, should the names of
#' those bands be kept? If `FALSE` the output will be
#'
#' @return Dataframe with columns
#' \itemize{
#'   \item{name}{Name of the raster file (with path removed).}
#'   \item{`x`}{Same as `x`.}
#'   \item{`y`}{Same as `y`.}
#'   \item{layer}{Layer from multiband raster. Will be `1` for single band
#'   raster with `rename_layers = TRUE` or the same as `name` with
#'   `rename_layers = FALSE`.}
#'   \item{value}{Value of the raster at `x` and `y` coordinates.}
#'   \item{path}{Full path to raster (from `ras_paths`).}
#' }
#' @export
#'
#' @examples
get_env_data <- function(ras_paths
                         , df
                         , x = "long"
                         , y = "lat"
                         , crs_df = 4326
                         , rename_layers = FALSE
                         ) {

  crs_ras <- crs(terra::rast(ras_paths[[1]]))

  points <- df %>%
    dplyr::distinct(!!rlang::ensym(x), !!rlang::ensym(y)) %>%
    na.omit() %>%
    dplyr::mutate(point_id = row_number()) %>%
    sf::st_as_sf(coords = c(x, y)
                 , crs = crs_df
                 , remove = FALSE
                 ) %>%
    sf::st_transform(crs = crs_ras) %>%
    dplyr::bind_cols(as_tibble(sf::st_coordinates(.)) %>%
                       dplyr::rename(ras_x = X, ras_y = Y)
                     ) %>%
    sf::st_set_geometry(NULL)


  points_spatvect <- points %>%
    terra::vect(crs = crs_ras
                , geom = c("ras_x", "ras_y")
                )

  env_df <- tibble::tibble(path = ras_paths) %>%
    dplyr::mutate(r = purrr::map(path, terra::rast)
                  , vals = purrr::map(r
                                      , terra::extract
                                      , y = points_spatvect
                                      )
                  , vals = if(rename_layers) {

                    purrr::map(vals
                               , ~stats::setNames(., c("point_id",paste0("lyr",1:(ncol(.)-1))))
                               )

                  } else {

                    purrr::map(vals
                               , ~stats::setNames(., gsub("ID", "point_id", names(.)))
                               )

                    }
                  ) %>%
    dplyr::select(negate(is.list), vals) %>%
    tidyr::unnest(cols = c(vals)) %>%
    tidyr::pivot_longer(3:ncol(.)
                        , names_to = "layer"
                        , values_to = "value"
                        ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(name = fs::path_file(path)
                  , layer = if(rename_layers) {

                    as.numeric(gsub("lyr", "", layer))

                    } else layer
                  ,
                  ) %>%
    dplyr::left_join(points) %>%
    dplyr::select(name, !!ensym(x), !!ensym(y), layer, value, path)

}

