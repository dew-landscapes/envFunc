
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
      dplyr::rename(old_x = !!ensym(x), old_y = !!ensym(y))

    df_xy <- df %>%
      dplyr::distinct(old_x, old_y)

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
                                )

    res <- points %>%
      sf::st_set_geometry(NULL) %>%
      dplyr::mutate(cell = cells)

    if(add_xy) {

      xy_res <- raster::xyFromCell(ras,cells) %>%
        tibble::as_tibble() %>%
        dplyr::bind_cols(cell = cells) %>%
        dplyr::filter(!is.na(x)
                      , !is.na(y)
                      ) %>%
        sf::st_as_sf(coords = c("x","y")
                     , crs = sf::st_crs(ras)
                     ) %>%
        sf::st_transform(crs = crs_df) %>%
        dplyr::mutate(!!ensym(x) := sf::st_coordinates(.)[,1]
                      , !!ensym(y) := sf::st_coordinates(.)[,2]
                      ) %>%
        sf::st_set_geometry(NULL)

      res <- res %>%
        dplyr::left_join(xy_res)

    }

    res <- res %>%
      dplyr::right_join(df) %>%
      dplyr::select(cell,everything(),-(contains("old"))) %>%
      dplyr::distinct()

  }

#' Create dataframe of 'cells' and their associated env data
#'
#' Multicore `raster::extract` adapted from the Stack Exchange Network [post](https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points)
#' by [thiagoveloso](https://gis.stackexchange.com/users/41623/thiagoveloso).
#'
#' @param x Raster* object.
#' @param df Dataframe. Must have a column called 'cell' that corresponds to the
#' cell numbers in x.
#' @param cores Numeric. Number of cores to use in `snowfall::sfInit()`.
#' @param out_file Character. Path to save outputs.
#' @param limit Logical. If TRUE, only cells in `df` will be returned.
#'
#' @return Dataframe with column `cell` plus columns equal to the length of x.
#' @export
#'
#' @examples
  make_env <- function(x
                       , df
                       , cores = 1
                       , out_file = tempfile()
                       , limit = TRUE
                       ) {

    out_file <- paste0(gsub("\\..*$","",out_file),".feather")

    ras_list <- raster::unstack(x) %>%
      stats::setNames(names(x))

    cells_done <- if(file.exists(out_file)) {

      env_df <- rio::import(out_file)

      done <- env_df %>%
        dplyr::pull(cell)

    } else NULL

    to_check <- df %>%
      dplyr::distinct(cell) %>%
      dplyr::pull(cell)

    todo <- setdiff(to_check,cells_done)

    if(length(todo) > 0) {

      # mulitcore extract -
      # create a cluster of cores
      snowfall::sfInit(parallel=TRUE, cpus=cores)

      # Load the required packages inside the cluster
      snowfall::sfLibrary(raster)
      snowfall::sfLibrary(sp)

      # run the extract
      env_extract <- snowfall::sfSapply(ras_list, raster::extract, y=todo) %>%
        {if(is.null(nrow(.))) (.) %>% tibble::as_tibble_row() else (.) %>% tibble::as_tibble()}

      # close the cluster
      snowfall::sfStop()

      # Fix result
      res_env <- tibble::tibble(cell = todo) %>%
        dplyr::bind_cols(env_extract) %>%
        {if(file.exists(out_file)) (.) %>% dplyr::bind_rows(env_df) else (.)}

      rio::export(res_env,out_file)

    }

    cols_done <- if(file.exists(out_file)) {

      env_df <- rio::import(out_file)

      names(env_df)

    } else NULL

    to_check_cols <- names(ras_list)

    todo <- setdiff(to_check_cols,cols_done)

    if(length(todo) > 0) {

      # mulitcore extract - https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
      # create a cluster of cores
      snowfall::sfInit(parallel=TRUE, cpus=cores)

      # Load the required packages inside the cluster
      snowfall::sfLibrary(raster)
      snowfall::sfLibrary(sp)

      # run the extract
      env_extract <- snowfall::sfSapply(ras_list[todo], raster::extract, y=to_check) %>%
        {if(is.null(nrow(.))) (.) %>% tibble::as_tibble_row() else (.) %>% tibble::as_tibble()}

      # close the cluster
      snowfall::sfStop()

      # Fix result
      res_env <- tibble::tibble(cell = to_check) %>%
        dplyr::bind_cols(env_extract) %>%
        {if(file.exists(outfile)) (.) %>% dplyr::left_join(env_df) else (.)}

      rio::export(res_env,out_file)

    }

    rio::import(out_file) %>%
      dplyr::select(1,names(ras_list)) %>%
      {if(limit) (.) %>% dplyr::filter(cell %in% to_check) else (.)} %>%
      tibble::as_tibble()

  }
