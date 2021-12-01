
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
    dplyr::ungroup() %>%
    tidyr::pivot_longer(grep("name", names(.), invert = TRUE, value = TRUE)
                        , names_to = "func"
                        , values_to = "value"
                        ) %>%
    tidyr::separate(name
                    , into = c("process", "layer", "method", "season")
                    , remove = FALSE
                    )

  if(isTRUE(!is.null(luenv_df))) {

    res <- res %>%
      dplyr::left_join(luenv_df %>%
                         dplyr::select(any_of(c(names(res), trans_col)))
                       ) %>%
      dplyr::mutate(value = dplyr::if_else(!!rlang::ensym(trans_col) < 0
                                           , value + !!rlang::ensym(trans_col)
                                           , value / !!rlang::ensym(trans_col)
                                           )
                    ) %>%
      dplyr::select(-!!rlang::ensym(trans_col))

  }

  res %>%
    tidyr::pivot_wider(names_from = func
                       , values_from = value
                       )

}

