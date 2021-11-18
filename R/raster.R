
#' Environmental summary statistics
#'
#' Given a data frame of environmental values, calculate summary statistics
#'
#' @param env_df Data frame of environmental values.
#' @param context Character name of column(s) in `env_df` that define the context.
#' @param luenv_df Data frame with information about each layer.
#'
#' @return Data frame of summary values for each layer, particularly `units` and
#' `transform` to convert raster layer values to `units`.
#' @export
#'
#' @examples
summarise_env <- function(env_df
                          , context
                          , luenv_df = NULL
                          ) {

  res <- env_df %>%
    tidyr::pivot_longer(grep(paste0(context, collapse = "|")
                             , names(.)
                             , invert = TRUE
                             , value = TRUE
                             )
                        ) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(mean = mean(value)
                     , median = median(value)
                     , sd = sd(value)
                     , iqrLo = quantile(value, probs = 0.25)
                     , iqrUp = quantile(value, probs = 0.75)
                     ) %>%
    dplyr::ungroup()

  if(isTRUE(!is.null(luenv_df))) {

    res <- res %>%
      dplyr::left_join(luenv_df %>%
                         dplyr::select(name = layer
                                       , transform
                                       , units
                                       , desc
                                       )
                       ) %>%
      dplyr::mutate(transform = as.character(transform)) %>%
      dplyr::mutate(across(where(is.numeric),~./as.numeric(transform))) %>%
      dplyr::mutate(transform = as.numeric(transform))

  }

  return(res)

}



#' Add cell to df with lat/long
#'
#' @param ras Raster object with cell numbers to extract
#' @param df Dataframe with lat/long columns
#' @param x Character. Name of latitude column
#' @param y Character. Name of longitude column
#' @param crs_df Single length vector. What crs are x and y?
#' @param add_xy Logical. Generate (central) x and y coords from cell?
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
      sf::st_transform(crs = st_crs(ras))

    sp_points <- points %>%
      sf::as_Spatial()

    cells <- raster::cellFromXY(object = ras
                                , xy = sp_points
                                ) %>%
      as.integer()

    res <- points %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::mutate(cell = cells)

    if(add_xy) {

      xy_res <- raster::xyFromCell(ras
                                   , cells
                                   ) %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(cell = cells) %>%
        dplyr::filter(!is.na(x)
                      , !is.na(y)
                      ) %>%
        sf::st_as_sf(coords = c("x","y")
                     , crs = sf::st_crs(ras)
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


