

#' Transform the coordinates in a dataframe
#'
#' @param df Dataframe with x and y coordinates to transform.
#' @param df_crs,to_crs Anything that will be recognised by the `crs` argument
#' of `sf::st_as_sf()` and `sf::st_transform()`.
#' @param x,y Character name of column in `df` with x and y coordinates.
#' @param remove Logical. If true, original coordinate columns (`x` and `y`)
#' will be removed.
#'
#' @return Dataframe with added columns `X` and `Y` representing the transformed
#'  coordinates.
#' @export
#'
#' @examples
  sp_transform_df <- function(df
                              , to_crs
                              , df_crs = 4283
                              , x = "long"
                              , y = "lat"
                              , remove = TRUE
                              ) {

    df <- df %>%
      dplyr::rename(old_x = !!rlang::ensym(x)
                    , old_y = !!rlang::ensym(y)
                    )

    coords <- df %>%
      dplyr::distinct(old_x, old_y) %>%
      sf::st_as_sf(coords = c("old_x", "old_y")
                   , crs = df_crs
                   , remove = FALSE
                   ) %>%
      sf::st_transform(crs = to_crs) %>%
      dplyr::bind_cols(sf::st_coordinates(.)) %>%
      sf::st_set_geometry(NULL)

    res <- df %>%
      dplyr::left_join(coords)

    if(remove) {

      res <- res %>%
        dplyr::select(-old_x, -old_y)

    }

    return(res)

  }
