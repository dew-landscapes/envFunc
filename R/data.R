#' Lookup for Landscapes South Australia regions
#'
#'
#' @format A data frame with 9 rows and 9 variables:
#' \describe{
#'   \item{LSA}{Character. LSA code}
#'   \item{REGION}{Character. LSA Region name}
#'   \item{LSA_region}{Character. Same as REGION}
#'   \item{Zone}{Character. Agricultural or Arid zone}
#'   \item{System}{Character. High- or Low-rainfaill or Arid}
#'   \item{R,G,B,A}{Numeric. Red, Green, Blue and Alpha values to create colour
#'   values for `LSA`.}
#'   ...
#' }
"lulsa"


#' Dataframe of likelihood thresholds and definitions
#'
#' Following the intergovernmental panel on climate change [IPCC](https://www.ipcc.ch/site/assets/uploads/2017/08/AR5_Uncertainty_Guidance_Note.pdf).
#'
#' @format A data frame with `r nrow(lulikelihood)` rows and
#' `r ncol(lulikelihood)` variables:
#' \describe{
#'   \item{likelihood}{Factor. Definitions.}
#'
#'   \item{maxVal}{Double. Maximum proportion to fall in likelihood category}
#'
#'   \item{range}{Factor. Result of
#'   `cut(lulikelihood$maxVal, breaks = c(0,lulikelihood$maxVal))`}
#'
#'   \item{loose}{Factor with levels:
#'   `r levels(lulikelihood$loose) %>% envFunc::vec_to_sentence()`}
#'
#'   \item{very}{Factor with levels:
#'   `r levels(lulikelihood$very) %>% envFunc::vec_to_sentence()`}
#'
#'   \item{extreme}{Factor with levels:
#'   `r levels(lulikelihood$extreme) %>% envFunc::vec_to_sentence()`}
#'
#'   \item{exceptional}{Factor with levels:
#'   `r levels(lulikelihood$exceptional) %>% envFunc::vec_to_sentence()`}
#'
#'   ...
#' }
#' @source <https://www.ipcc.ch/site/assets/uploads/2017/08/AR5_Uncertainty_Guidance_Note.pdf>
"lulikelihood"
