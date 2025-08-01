

#' Plot the results from `make_metric_df`
#'
#' @param metric_df Dataframe with results from call to `make_metric_df`.
#' @param group_col Character. Name of column with numeric data for x-axis.
#' @param facet_col Character. Optional name of column to facet on (along with
#' label).
#' @param label Character label for the diagnostics (choose another column from
#' `metric_df`).
#' @param value Character. Name of column with values to plot.
#' @param display_all Display all diagnostics or only those used to select best.
#'
#' @return ggplot object
#' @export
#'
#' @examples
make_metric_plot <- function(metric_df
                             , group_col = "groups"
                             , facet_col = "method"
                             , label = "metric"
                             , value = "scale"
                             , display_all = FALSE
                             ) {

  df <- metric_df %>%
    {if(display_all) (.) else (.) %>% dplyr::filter(weight)} %>%
    dplyr::mutate(across(where(is.factor),factor)) %>%
    dplyr::filter(!is.na(value))


  facet_formula <- if(isTRUE(!is.null(facet_col))) {

    as.formula(paste0(label,"~",facet_col))

    } else label


  ggplot2::ggplot(df
                  , ggplot2::aes(!!rlang::ensym(group_col)
                                 , !!rlang::ensym(value)
                                 , colour = combo
                                 , alpha = if(display_all) weight else NULL
                                 , label = !!rlang::ensym(group_col)
                                 , size = top
                                 )
                  ) +
    ggplot2::geom_point() +
    ggrepel::geom_text_repel(data = df %>%
                               dplyr::filter(best)
                             , size = 2
                             , show.legend = FALSE
                             , box.padding = 1
                             , min.segment.length = 0
                             , colour = "black"
                             ) +
    ggplot2::facet_grid(facet_formula
                        , scales = "free_y"
                        , labeller = ggplot2::label_wrap_gen(20, multi_line = TRUE)
                        ) +
    ggplot2::labs(colour = "Combination"
                  , alpha = "Metric used"
                  , title = paste0("Labels indicate top ", numbers2words(unique(df$best_thresh)), " results")
                  , size = paste0("Best ", unique(df$top_thresh) * 100, "%")
                  ) +
    ggplot2::scale_colour_viridis_c() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 0)
                   , strip.text.x = ggplot2::element_text(angle = 90)
                   , axis.text.x = ggplot2::element_text(angle = 90)
                   )

}
