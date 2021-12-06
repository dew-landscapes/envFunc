

#' Get raster values for each unique location in a data frame.
#'
#'
#'
#' @param ras Either SpatRaster or path(s) from which to create SpatRaster.
#' @param df Dataframe with `x` and `y`.
#' @param x,y Columns in `df` with x and y coordinates.
#' @param crs_df Coordinate reference system for `x` and `y`. Passed to the
#' `crs` argument of [sf::st_as_sf()].
#'
#' @return Dataframe with columns
#' \itemize{
#'   \item{`x`}{Same as `x`.}
#'   \item{`y`}{Same as `y`.}
#'   \item{name}{Concept being extracted - often the same as `file`.}
#'   \item{value}{Value of the raster at `x` and `y` coordinates.}
#'   \item{layer}{Layer from multiband raster. Will be `1` for single band
#'   raster.}
#'   \item{path_abs}{Full path to raster.}
#'   \item{file}{filename component of `path_abs`.}
#' }
#' @export
#'
#' @examples
get_env_data <- function(ras
                         , df
                         , x = "long"
                         , y = "lat"
                         , crs_df = 4326
                         ) {


  if(!"SpatRaster" %in% class(ras[[1]])) {

    ras <- terra::rast(ras)

  }

  crs_ras <- crs(ras)

  layer_names <- tidyr::uncount(sources(ras)
                                , weights = nlyr
                                ) %>%
    dplyr::rename(path_abs = source) %>%
    dplyr::group_by(path_abs) %>%
    dplyr::mutate(name = paste0(gsub("\\..{2,4}|_aligned"
                                     , ""
                                     , fs::path_file(path_abs)
                                     )
                                , "_"
                                , row_number()
                                )
                  ) %>%
    dplyr::ungroup()

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

  res <- terra::extract(ras
                        , y = points_spatvect
                        ) %>%
    stats::setNames(c("point_id", layer_names$name)) %>%
    tibble::as_tibble() %>%
    dplyr::left_join(points %>%
                       dplyr::select(point_id
                                     , !!rlang::ensym(x)
                                     , !!rlang::ensym(y)
                                     )
                     ) %>%
    dplyr::select(!!rlang::ensym(x)
                  , !!rlang::ensym(y)
                  , layer_names$name
                  ) %>%
    tidyr::pivot_longer(layer_names$name
                        , names_to = "name"
                        , values_to = "value"
                        ) %>%
    dplyr::left_join(layer_names) %>%
    dplyr::mutate(layer = stringr::str_extract(name, "_\\d{1,2}$")
                  , name = map2_chr(paste0(layer,"$")
                                    , name
                                    , ~gsub(.x, "", .y)
                                    )
                  , layer = as.character(readr::parse_number(layer))
                  , file = fs::path_file(path_abs)
                  ) %>%
    dplyr::select(-path_abs, -file, everything(), file, path_abs)

}

