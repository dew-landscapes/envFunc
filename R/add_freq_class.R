

#' Convert percentages to frequency classes
#'
#' @param per_pres Numeric. Percentage (i.e. 0-100)
#'
#' @return Factor with levels always, often, frequent, occasional, and infrequent
#' @export
#'
#' @examples
#' freq_class(c(2,20,40,60,100))
add_freq_class <- function(per_pres) {

  if_else(per_pres == 100
          , "always"
          , if_else(per_pres > 75
                    , "often"
                    , if_else(per_pres > 50
                              , "frequent"
                              , if_else(per_pres > 5
                                        , "occasional"
                                        , "infrequent"
                                        )
                              )
                    )
          ) %>%
    factor(levels = c("always","often","frequent","occasional","infrequent"))

}
