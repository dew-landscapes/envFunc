#' Transform the coordinate columns of a dataframe
#'
#' Uses `sf::sf_project()` internally
#'
#' @param df Dataframe. Needs coordinate columns
#' @param x,y  Character. Name of column with x and coords
#' @param new_x,new_y Character. Name to give the transformed x and columns
#' @param crs_from,crs_to Passed to the `from` and `to` arguments of
#' `sf::sf_project()`
#'
#' @return Dataframe with additional, projected, x and y columns
#' @export
#'
#' @examples
project_df <- function(df
                       , x = "long"
                       , y = "lat"
                       , new_x = "x"
                       , new_y = "y"
                       , crs_from = "epsg:4326"
                       , crs_to = "epsg:8059"
                       ) {

  points <- df |>
    dplyr::distinct(dplyr::across(tidyselect::any_of(c(x,y))))

  points |>
    dplyr::bind_cols(sf::sf_project(from = crs_from
                                    , to = crs_to
                                    , pts = points
                                    , keep = TRUE
                                    ) |>
                       tibble::as_tibble(.name_repair = "unique_quiet") |>
                       dplyr::rename(!!rlang::ensym(new_x) := 1, !!rlang::ensym(new_y) := 2)
                     ) |>
    dplyr::inner_join(df)

}

