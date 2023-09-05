
#' Generate a tibble of epochs.
#'
#' @param start_year Minimum year in epochs
#' @param end_year Maximum year in epochs
#' @param epoch_step Numeric. How many years in an epoch?
#' @param epoch_overlap Logical. Should epochs overlap by one year? i.e.
#' `epoch_step = 10` with `epoch_overlap = TRUE` gives, say, 2000-2010 and
#' 2010-2020 whereas `epoch_overlap = FALSE` gives 2000-2009 and
#' 2010-2019.
#'
#' @return Tibble with columns
#' \describe{
#'   \item{start}{First year of epoch}
#'   \item{end}{Last year of epoch}
#'   \item{years}{List column containing the years for the epoch}
#'   \item{epoch}{Last two digits of first year and last year in epoch.
#'   e.g. 10-19.}
#'   \item{data}{List column of full path of raster files to summarise.}
#' }
#' @export
#'
#' @examples
#' now <- as.numeric(format(Sys.Date(), "%Y"))
#' make_epochs(now - 30, now, 10)
#' make_epochs(now - 30, now, 10, F)
#'
make_epochs <- function(start_year = 1987
                        , end_year = 2020
                        , epoch_step = 5
                        , epoch_overlap = TRUE
                        ) {

  now <- as.numeric(format(Sys.Date(), "%Y"))

  if(is.null(start_year)) {

    start_year <- 1987

  }

  if(is.null(end_year)) {

    end_year <- as.integer(now)

  }

  epochs <- (end_year - start_year) / epoch_step

  ends <- end_year - epoch_step*0:epochs
  if(!epoch_overlap) epoch_step <- epoch_step - 1
  starts <- ends - epoch_step
  starts <- starts[starts >= start_year]
  if(min(starts) != start_year) starts <- c(starts, start_year)

  eps <- tibble::tibble(start_year = starts
                        , end_year = ends[1:length(starts)]
                        ) %>%
    dplyr::mutate(years = purrr::map2(start_year, end_year, ~.x:.y)
                  , epoch = paste0(substr(start_year,3,4), "-", substr(end_year,3,4))
                  , epoch = forcats::fct_reorder(epoch, start_year)
                  , epoch = factor(epoch, ordered = TRUE)
                  , epoch_now = purrr::map_lgl(years, ~ now %in% .)
                  ) %>%
    dplyr::arrange(start_year)

  return(eps)

}
