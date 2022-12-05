
#' Add likelihood based on IPCC thresholds
#'
#' Thresholds and defition based on the International Panel on Climate Change
#' [guidance note](https://www.ipcc.ch/site/assets/uploads/2017/08/AR5_Uncertainty_Guidance_Note.pdf))
#'
#' @param df Dataframe with `col` raw values to assign to a likelihood class
#' @param col Column in `df` with raw values.
#'
#' @return `df` with extra columns from `envFunc::lulikelihood`.
#' @export
#'
#' @examples
  add_likelihood <- function(df, col) {

    likelihood <- envFunc::lulikelihood

    df %>%
      dplyr::mutate(likelihood = purrr::map({{ col }}
                                            , ~cut(.
                                                   , breaks = c(0,lulikelihood$maxVal)
                                                   , labels = lulikelihood$likelihood
                                                   , include.lowest = TRUE
                                                   )
                                            )
                    ) %>%
      tidyr::unnest(cols = c(likelihood)) %>%
      dplyr::mutate(likelihood = forcats::fct_expand(likelihood
                                                     , levels(lulikelihood$likelihood)
                                                     )
                    ) %>%
      dplyr::left_join(lulikelihood %>%
                         dplyr::select(likelihood
                                       , any_of(names(lulikelihood))
                                       )
                       )

  }
