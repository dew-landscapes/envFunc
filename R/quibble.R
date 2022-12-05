#' Make a wide, single row, data frame of quantiles (percentiles)
#'
#' Edited from the blog post https://www.tidyverse.org/blog/2020/03/dplyr-1-0-0-summarise/
#'
#' @param x Numeric. Values passed to `stats::quantile` argument `x`
#' @param q Numeric. Proportions passed to `stats::quantile` argument `probs`
#' @param ... Passed to `stats::quantile`
#'
#' @return Single row data frame. Names are percentiles equivalent to `q`
#' values.
#' @export
#'
#' @examples
#' x <- rnorm(100)
#'
#' quibble(x)
  quibble <- function(x
                      , q = c(0.25, 0.5, 0.75)
                      , ...
                      ) {


    setNames(data.frame(t(stats::quantile(x
                                          , q
                                          , names = TRUE
                                          , ...
                                          )
                          )
                        )
             , paste0("q"
                      , stringr::str_pad(100 * q
                                         , width = 2
                                         , pad = "0"
                                         )
                      )
             )

  }
