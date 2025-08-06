

#' Make a list of data frames for months and seasons
#'
#'
#' @param start_year,end_year Numeric. Year of first and last season
#' @param seasons Dataframe mapping each month to a season. Default (`NULL`)
#' will use south hemisphere seasons. Needs columns `mon` (numeric 1:12), `month`
#' (character name to use for months) and `season` (character name of season to
#' use for that month)`
#' @param dec_adjust Logical. Adjust the year in which December is used? If
#' `TRUE` (default) December will be included in the following year's first
#' season (i.e. December 1999 will be included in, say, summer 2000)
#' @param include_all Logical. Include a 'season' that is 'all' (and starts at
#' calendar year).
#'
#' @return List with elements
#'  item{luseasons}{Dataframe. Lookup used to translate months to seasons. Equal
#'  to `seasons` if that was provided by `seasons` argument}
#'  item{months}{Dataframe for each month from `start_year` to `end_year`,
#'  including start and end dates}
#'  item{seasons}{Dataframe for each season from `start_year` to `end_year`,
#'  including start and end dates}
#' @export
#'
#' @examples
  make_seasons <- function(start_year
                           , end_year
                           , seasons = NULL
                           , dec_adjust = 1
                           , include_all = TRUE
                           ) {

    result <- list()

    if(is.null(seasons)) {

      result$luseasons <- tibble::tibble(mon = 1:12
                                         , month = month.name
                                         ) %>%
        dplyr::mutate(season = dplyr::case_when(month %in% c("December", "January", "February") ~ "summer"
                                                , month %in% c("March", "April", "May") ~ "autumn"
                                                , month %in% c("June", "July", "August") ~ "winter"
                                                , month %in% c("September", "October", "November") ~ "spring"
                                                )
                      )

      if(include_all) {

        result$luseasons <- result$luseasons |>
          dplyr::bind_rows(tibble::tibble(mon = 1
                                          , month = "all"
                                          , season = "all"
                                          )
                           )

      }

    } else result$luseasons <- seasons

    result$luseasons <- result$luseasons %>%
      dplyr::mutate(dec_adjust = dplyr::if_else(month == "December"
                                                , if(dec_adjust) as.integer(-(dec_adjust)) else 0L
                                                , 0L
                                                )
                    )

    result$months <- tibble::tibble(year = start_year:end_year) %>%
      dplyr::cross_join(result$luseasons) %>%
      dplyr::mutate(year_use = year
                    , year = year + dec_adjust
                    ) %>%
      dplyr::arrange(year, mon) %>%
      dplyr::mutate(start_date = paste0(year
                                        , "/"
                                        , stringr::str_pad(mon, 2, pad = "0")
                                        , "/"
                                        , "01"
                                        )
                    , start_date = as.Date(start_date)
                    , end_day = lubridate::days_in_month(start_date)
                    , end_date = paste0(year
                                        , "/"
                                        , stringr::str_pad(mon, 2, pad = "0")
                                        , "/"
                                        , end_day
                                        )
                    , end_date = as.Date(end_date)
                    )


    result$seasons <- result$months %>%
      tidyr::pivot_longer(contains("_date")) %>%
      dplyr::group_by(year_use, season) %>%
      dplyr::filter(value == min(value) | value == max(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(year = year_use, season, name, value) %>%
      tidyr::pivot_wider()

    return(result)


  }
