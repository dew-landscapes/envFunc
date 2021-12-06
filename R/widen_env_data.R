
#' Widen env data, especially AusCover data from `get_env_data`.
#'
#' Output is a context by environmental variable data frame.
#'
#' @param env_data_long Dataframe, in long format. Usually resulting from
#' `get_env_data`.
#' @param orig_df Dataframe containing the locations passed to `get_env_data`.
#' @param context Character. Columns in `orig_df` defining the context.
#' @param env_groups Character. Name of columns in `env_data_long` that define
#' the context of the raster. Default to 'process', 'layer', 'method' and
#' 'season'. See [envEcosystems::env] for definitions.
#' @param epoch_step Numeric. If summarising several (years) rasters to one for
#' the given `context`, how many years to summarise?
#' @param min_years Numeric. What is the minimum acceptable number of years with
#' data. Points with less than `min_years` of data will be filtered.
#'
#' @return Site (context) by environmental data with one row of env data for
#' each context.
#' @export
#'
#' @examples
widen_env_data <- function(env_data_long
                           , orig_df
                           , context
                           , env_groups = c("process", "layer", "method", "season")
                           , epoch_step = 10
                           , min_years = 3
                           ){

  orig_df %>%
    dplyr::distinct(across(any_of(context))) %>%
    dplyr::left_join(env_data_long) %>%
    dplyr::group_by(across(any_of(env_groups))) %>%
    dplyr::mutate(year_diff_min = year - min(.$year_ras, na.rm = TRUE)
                  , year_thresh_min = dplyr::if_else(year_diff_min >= epoch_step
                                                     , year - epoch_step
                                                     , year - year_diff_min
                                                     )
                  , year_thresh_max = dplyr::if_else(year_diff_min >= epoch_step
                                                     , year
                                                     , year + epoch_step - year_diff_min
                                                     )
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year_ras >= year_thresh_min
                  , year_ras <= year_thresh_max
                  ) %>%
    dplyr::group_by(across(any_of(context)), across(any_of(env_groups))) %>%
    dplyr::summarise(func_mean = mean(value, na.rm = TRUE)
                     , func_median = median(value, na.rm = TRUE)
                     , func_min = min(value, na.rm = TRUE)
                     , func_max = max(value, na.rm = TRUE)
                     , func_sd = sd(value, na.rm = TRUE)
                     , func_n = sum(!is.na(value))
                     ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(func_n >= min_years) %>%
    tidyr::pivot_longer(contains("func")
                        , names_to = "method"
                        , values_to = "value"
                        ) %>%
    dplyr::mutate(method = gsub("func_","",method)) %>%
    tidyr::unite(col = "name"
                 , any_of(env_groups)
                 , na.rm = TRUE
                 ) %>%
    tidyr::pivot_wider(names_from = "name"
                       , values_from = "value"
                       ) %>%
    dplyr::left_join(orig_df %>%
                       dplyr::distinct(across(any_of(context)))
                     ) %>%
    dplyr::select(any_of(context), everything()) %>%
    janitor::remove_empty("cols")

}
