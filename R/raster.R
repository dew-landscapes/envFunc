
#' Add cell to df with lat/long
#'
#' @param ras Raster object with cell numbers to extract
#' @param df Dataframe with lat/long columns
#' @param x Character. Name of latitude column
#' @param y Character. Name of longitude column
#' @param crsdf Single length vector. What crs are x and y?
#'
#' @return df with additional column 'cell' of cell numbers from ras.
#' @export
#'
#' @examples
  add_raster_cell <- function(ras
                              , df
                              , x = "long"
                              , y = "lat"
                              , crsdf = 4326
                              ) {

    # Needs lat and long to work with
    points <- df %>%
      dplyr::select(x = !!ensym(x), y = !!ensym(y)) %>%
      dplyr::distinct() %>%
      st_as_sf(coords = c("x","y")
               , crs = crsdf
               , remove = FALSE
               ) %>%
      st_transform(crs = st_crs(ras))

    latlong <- points %>%
      as_Spatial()

    cells <- raster::cellFromXY(object = ras
                                , xy = latlong
                                )

    dfresult <- points %>%
      st_set_geometry(NULL) %>%
      dplyr::mutate(cell = cells) %>%
      dplyr::rename(!!ensym(x) := x, !!ensym(y) := y) %>%
      as_tibble()

    df %>%
      dplyr::left_join(dfresult) %>%
      dplyr::select(names(df),cell,-!!ensym(x),-!!ensym(y)) %>%
      dplyr::distinct()

  }

#' Create dataframe of 'cells' and their associated env data
#'
#' @param x Raster* object.
#' @param df Dataframe. Must have a column called 'cell' that corresponds to
#' the cell numbers in x.
#' @param outfile Character. Path to save outputs.
#'
#' @return Dataframe with 'sites' column, plus columns equal to the length of x.
#' @export
#'
#' @examples
  create_env <- function(x, df, cores = 1, outfile = tempfile()) {

    raslist <- raster::unstack(x) %>%
      stats::setNames(names(x))

    cellsDone <- if(file.exists(outfile)) {

      envdf <- rio::import(outfile)

      done <- envdf %>%
        dplyr::pull(cell)

    } else NULL

    toCheck <- df %>%
      dplyr::distinct(cell) %>%
      pull(cell)

    toDo <- setdiff(toCheck,cellsDone)

    if(length(toDo) > 0) {

      library("snow")

      # mulitcore extract - https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
      # create a cluster of cores
      snowfall::sfInit(parallel=TRUE, cpus=cores)

      # Load the required packages inside the cluster
      snowfall::sfLibrary(raster)
      snowfall::sfLibrary(sp)

      # run the extract
      envExtract <- snowfall::sfSapply(rasList, raster::extract, y=toDo) %>%
        {if(is.null(nrow(.))) (.) %>% tibble::as_tibble_row() else (.) %>% tibble::as_tibble()}

      # close the cluster
      snowfall::sfStop()

      # Fix result
      resEnv <- tibble(cell = toDo) %>%
        dplyr::bind_cols(envExtract) %>%
        {if(file.exists(outfile)) (.) %>% dplyr::bind_rows(cellsEnvDf) else (.)}

      rio::export(resEnv,outfile)

    }

    colsDone <- if(file.exists(outfile)) {

      cellsEnvDf <- rio::import(outfile)

      names(cellsEnvDf)

    } else NULL

    toCheckCols <- names(rasList)

    toDo <- setdiff(toCheckCols,colsDone)

    if(length(toDo) > 0) {

      library("snow")

      # mulitcore extract - https://gis.stackexchange.com/questions/253618/r-multicore-approach-to-extract-raster-values-using-spatial-points
      # create a cluster of cores
      snowfall::sfInit(parallel=TRUE, cpus=cores)

      # Load the required packages inside the cluster
      snowfall::sfLibrary(raster)
      snowfall::sfLibrary(sp)

      # run the extract
      envExtract <- snowfall::sfSapply(rasList[toDo], raster::extract, y=toCheck) %>%
        {if(is.null(nrow(.))) (.) %>% tibble::as_tibble_row() else (.) %>% tibble::as_tibble()}

      # close the cluster
      snowfall::sfStop()

      # Fix result
      resEnv <- tibble::tibble(cell = toCheck) %>%
        dplyr::bind_cols(envExtract) %>%
        {if(file.exists(outfile)) (.) %>% dplyr::left_join(cellsEnvDf) else (.)}

      rio::export(resEnv,outfile)

    }

    rio::import(outfile) %>%
      dplyr::select(1,names(rasList)) %>%
      tibble::as_tibble()

  }
