

#' Use a set of (continuous) columns to choose a good set of rows
#'
#' @param df Dataframe with columns over which to find good rows
#' @param mets_df Dataframe mapping the name of possible metrics to cases
#' (columns) in which to use that metric.
#' @param context Character. Name of columns in df that define context.
#' @param mets_col Character. Name of `mets_df` column to use in this instance.
#' @param summarise_method Character. Name of method to use in summarising if
#' there is more than one row per context.
#' @param scale Logical. If true, all metrics will be rescale 0 ('worst') to 1
#' ('best').
#' @param top_thresh Numeric specifying the proportion of rows considered 'top'.
#' @param best_thresh Numeric specifying the absolute number of rows considered
#' 'best'.
#' @param level Either 'across' or 'within'. If the latter, only metrics that
#' are set up to work 'within' clusters (rather than 'across' clusterings) are
#' used.
#'
#' @return
#' @export
#'
#' @examples
make_metric_df <- function(df
                      , mets_df = tibble::tibble(metric = "av_clust_size"
                                                 , high_good = TRUE
                                                 , clust_sum = TRUE
                                                 , level = "clustering"
                                                 )
                      , context = c("method"
                                    , "groups"
                                    )
                      , mets_col = "summary_mets"
                      , summarise_method = median
                      , scale = FALSE
                      , top_thresh = 0.25
                      , best_thresh = 5
                      , level = c("across", "within")
                      ) {

  level <- level[1]

  mets_df_use <- mets_df %>%
    dplyr::mutate(metric = forcats::fct_inorder(metric)) %>%
    dplyr::filter(!base::is.na(!!rlang::ensym(mets_col))
                  , if(level == "within") within_mets else across_mets
                  ) %>%
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
    {if(scale) (.) %>%
        dplyr::mutate(scale = dplyr::if_else(high_good
                                             , scales::rescale(value
                                                               , to = c(0.001
                                                                        , 1
                                                                        )
                                                               )
                                             , scales::rescale(desc(value)
                                                               , to = c(0.001
                                                                        , 1
                                                                        )
                                                               )
                                             )
                      ) else (.) %>%
        dplyr::mutate(scale = value) %>%
        dplyr::mutate(scale = dplyr::if_else(high_good
                                             , scale
                                             , 1 - scale
                                             )
                      )
      } %>%
    dplyr::ungroup() %>%
    dplyr::mutate(combo_init = scale * !!rlang::ensym(mets_col)) %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(context))
                    , dplyr::across(!!rlang::ensym(mets_col))
                    ) %>%
    dplyr::mutate(combo = base::prod(combo_init)
                  , combo = dplyr::if_else(base::is.na(combo)
                                           , 0
                                           , combo
                                           )
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::across(tidyselect::any_of(context))) %>%
    dplyr::mutate(combo = base::max(combo)
                  , combo = dplyr::if_else(base::is.na(combo)
                                           , 0
                                           , combo
                                           )
                  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(top_thresh = top_thresh
                  , best_thresh = best_thresh
                  , top = combo >= stats::quantile(combo, probs = 1 - top_thresh, na.rm = TRUE)
                  , top = dplyr::if_else(base::is.na(top), FALSE, top)
                  , best = combo >= sort(unique(.$combo), TRUE)[best_thresh]
                  , best = dplyr::if_else(base::is.na(best), FALSE, best)
                  , metric = factor(metric, levels = base::levels(mets_df_use$metric))
                  )

  return(ret)

}
