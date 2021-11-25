
#' Environmental summary statistics
#'
#' Given a data frame of environmental values, calculate summary statistics
#'
#' @param env_df Data frame of environmental values.
#' @param context Character name of column(s) in `env_df` that define the context.
#' @param luenv_df Data frame with information about each layer.
#' @param trans_col Character. Name of column in `luenv_df` containing
#' information on how to convert raster values to the scale of `units`.
#'
#' @return Data frame of summary values for each layer, particularly `units` and
#' `transform` to convert raster layer values to `units`.
#' @export
#'
#' @examples
summarise_env <- function(env_df
                          , context
                          , luenv_df = NULL
                          , trans_col = "transform"
                          ) {

  res <- env_df %>%
    tidyr::pivot_longer(grep(paste0(context, collapse = "|")
                             , names(.)
                             , invert = TRUE
                             , value = TRUE
                             )
                        ) %>%
    dplyr::mutate(name = gsub("\\.","-",name)) %>%
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
                                       , !!rlang::ensym(trans_col)
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

