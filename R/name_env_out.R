

#' Use meta data to name or parse output paths
#'
#' Either create file path for saving outputs or parse the meta data from a path
#'
#' @param x Either dataframe with path(s) to parse (in column `path`), character
#' vector of path(s) to search, or named list object.
#' @param context_defn Character vector of meta data names (in order)
#' @param parse Logical. If `FALSE` (default) an `out_dir` path will be returned rather
#' than parsed. Assumes the appropriate names can be found in `x`.
#' @param fill_null Logical. If `TRUE`, will fill up to `x_null` definitions
#' with NULL (and issue a warning).
#' @param x_null Numeric. Even if `fill_null` is `TRUE`, if there are more than
#' `x_null` missing definitions, an error will be thrown.
#' @param ... Not used
#'
#'
#' @return If `!parse`, tibble with columns:
#' \describe{
#'   \item{vector}{Vector layer originally used to define area of interest}
#'   \item{filt_col}{Column name from vector to filter to define area of interest}
#'   \item{level}{Level(s) of filt_col originally filtered to define area of interest}
#'   \item{buffer}{Any buffer around area of interest}
#'   \item{res}{Resolution of output raster in units of rasters crs}
#'   \item{path}{Full (relative) path including `file_type`.}
#' }
#'
#' If `!parse`, `df` with additional column `out_dir`
#'
#' @export
#'
#' @examples
name_env_out <- function(x
                         , context_defn = c("vector"
                                            , "filt_col"
                                            , "level"
                                            , "buffer"
                                            , "res"
                                            )
                         , parse = FALSE
                         , fill_null = FALSE
                         , x_null = 2
                         , ...
                         ) {

  df <- if(!"data.frame" %in% class(x)) {

    if("character" %in% class(x)) {
      
      tibble::tibble(path = x)

    } else if("list" %in% class(x)) {

      get_names <- names(x) %in% context_defn

      x[get_names] %>%
        purrr::map(paste0, collapse = "--") %>%
        list2DF() %>%
        tibble::as_tibble()

    }

  } else x

  if(parse) {

      res <- df %>%
        dplyr::mutate(context = basename(path)) %>%
        tidyr::separate(context
                        , into = context_defn
                        , sep = "__"
                        )
      
      res <- res %>%
        dplyr::relocate(-path)

  } else {
    
    if(fill_null) {
      
      # check all names are in df
      
      missing <- setdiff(context_defn
                         , names(df) 
                         )
      
      if(length(missing) > x_null) {
        
        stop(length(missing)
             , " definitions are missing: "
             , envFunc::vec_to_sentence(missing)
             )
        
      }
      
      warning("All of "
              , envFunc::vec_to_sentence(missing)
              , " will be NULL"
              )
      
      if(length(missing)) {
        
        df <- cbind(df
                    , purrr::map(missing
                                 , \(x) tibble::tibble(!!rlang::ensym(x) := list(NULL))
                                 )
                    )
        
      }
      
    }

    res <- df %>%
      tidyr::unite("path"
                  , tidyselect::any_of(context_defn)
                   , sep = "__"
                   )

  }

  return(res)

}
