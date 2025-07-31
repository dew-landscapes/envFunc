

#' Use a set of (continuous) columns to choose a good set of rows
#'
#' @param df Dataframe with columns over which to find good rows
#' @param mets_df Dataframe mapping the name of possible metrics to cases
#' (columns) in which to use that metric.
#' @param context Character. Name of columns in df that define context.
#' @param mets_col Character. Name of `mets_df` column to use in this instance.
#' @param summarise_method Character. Name of method to use in summarising if
#' there is more than one row per context.
#' @param top_thresh Numeric specifying the proportion of rows considered 'top'.
#' @param best_thresh Numeric specifying the absolute number of rows considered
#' 'best'.
#'
#' @return
#' @export
#'
#' @examples
make_metric_df <- function(df
                      , mets_df = tibble::tibble(metric = "av_clust_size"
                                                 , high_good = TRUE
                                                 , level = "clustering"
                                                 )
                      , context = c("method"
                                    , "groups"
                                    )
                      , mets_col = "summary_mets"
                      , summarise_method = median
                      , top_thresh = 0.25
                      , best_thresh = 5
                      , scale = lifecycle::deprecated()
                      , level = lifecycle::deprecated()
                      ) {

  if(lifecycle::is_present(scale)) {

    lifecycle::deprecate_warn(when = "2025-07-31"
                              , "envFunc::make_metric_df(scale = )"
                              , details = "scale is now always calculated"
                              )

    lifecycle::deprecate_warn(when = "2025-07-31"
                              , "envFunc::make_metric_df(level = )"
                              , details = "supply an approprite mets_df"
                              )

  }

  mets_df_use <- mets_df %>%
    dplyr::mutate(metric = forcats::fct_inorder(metric)) %>%
    dplyr::filter(!base::is.na(!!rlang::ensym(mets_col))) %>%
    dplyr::select(metric, high_good, within_mets, !!rlang::ensym(mets_col)) %>%
    dplyr::mutate(weight = !!rlang::ensym(mets_col))

  ret <- df %>%
    dplyr::select(any_of(context)
                  , any_of(mets_df_use$metric)
                  ) %>%
    dplyr::group_by(across(any_of(context))) %>%
    dplyr::summarise(across(any_of(mets_df_use$metric)
                            , \(x) summarise_method(x, na.rm = TRUE)
                            )
                     ) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(any_of(mets_df_use$metric)
                        , names_to = "metric"
                        , values_to = "value"
                        ) %>%
    dplyr::left_join(mets_df_use) %>%
    dplyr::group_by(across(any_of(names(mets_df_use)))) %>%
    dplyr::mutate(zscore = (value - mean(value)) / sd(value)
                  , scale = dplyr::if_else(high_good
                                           , zscore
                                           , -1 * zscore
                                           )
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
    dplyr::mutate(n_metrics = dplyr::n()
                  , scale_weight = scale * weight
                  , combo = sum(scale_weight) / n_metrics
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(top_thresh = top_thresh
                  , best_thresh = best_thresh
                  , top = combo >= stats::quantile(combo, probs = 1 - top_thresh, na.rm = TRUE)
                  , top = dplyr::if_else(base::is.na(top), FALSE, top)
                  , best = combo >= sort(unique(.$combo), TRUE)[best_thresh]
                  , best = dplyr::if_else(base::is.na(best), FALSE, best)
                  , metric = factor(metric
                                    , levels = base::levels(mets_df_use$metric)
                                    )
                  )

  return(ret)

}
