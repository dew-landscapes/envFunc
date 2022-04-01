
#' Generate a tibble of epochs.
#'
#' @param min_year Minimum year in epochs
#' @param max_year Maximum year in epochs
#' @param epoch_step Numeric. How many years in an epoch?
#' @param epoch_overlap Logical. Should epochs overlap by one year? i.e.
#' `epoch_step = 10` with `epoch_overlap = TRUE` with  gives, say, 2000-2010 and
#' 2010-2020 whereas `epoch_overlap = FALSE` gives 2000-2009 and
#' 2010-2019.
#'
#' @return Tibble with columns
#' \describe{
#'   \item{start}{First year of epoch}
#'   \item{end}{Last year of epoch}
#'   \item{year}{List column containing the years for the epoch}
#'   \item{epoch}{Last two digits of first year and last year in epoch.
#'   e.g. 10-19.}
#'   \item{data}{List column of full path of raster files to summarise.}
#' }
#' @export
#'
#' @examples
#'
make_epochs <- function(min_year = 1987
                        , max_year = 2020
                        , epoch_step = 5
                        , epoch_overlap = TRUE
                        ) {

  now <- as.numeric(format(Sys.Date(), "%Y"))

  if(is.null(min_year)) {

    min_year <- 1987

  }

  if(is.null(max_year)) {

    max_year <- as.integer(now)

  }

  epochs <- (max_year - min_year) / epoch_step

  ends <- max_year - epoch_step*0:epochs
  if(!epoch_overlap) epoch_step <- epoch_step - 1
  starts <- ends - epoch_step
  starts <- starts[starts >= min_year]
  if(min(starts) != min_year) starts <- c(starts, min_year)

  eps <- tibble::tibble(start = starts
                        , end = ends[1:length(starts)]
                        ) %>%
    dplyr::mutate(year = purrr::map2(start, end, ~.x:.y)
                  , epoch = paste0(substr(start,3,4), "-", substr(end,3,4))
                  , epoch = forcats::fct_reorder(epoch, start)
                  , epoch = factor(epoch, ordered = TRUE)
                  , epoch_now = purrr::map_lgl(year, ~ now %in% .)
                  )

  return(eps)

}
