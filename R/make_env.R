#' Create dataframe of 'cells' and their associated env data
#'
#' Multicore [raster::extract()] adapted from the Stack Exchange Network [post](https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points)
#' by [thiagoveloso](https://gis.stackexchange.com/users/41623/thiagoveloso).
#'
#' This function has been surpassed by `get_env_data` which uses
#' `terra::extract`.
#'
#' @param x Raster* object.
#' @param df Dataframe. Must have a column called 'cell' that corresponds to the
#' cell numbers in x.
#' @param cores Numeric. Number of cores to use in [snowfall::sfInit()].
#' @param out_file Character. Path to save outputs.
#' @param limit Logical. If TRUE, only cells in `df` will be returned. Can be
#' useful if cells already in `out_file` have been orphaned.
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
