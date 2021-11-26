
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
                     , if(min_years > 1) func_sd = sd(value, na.rm = TRUE)
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
