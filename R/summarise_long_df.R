#' Summarise a long data frame with consistent set of summary functions
#'
#' Summary functions are: count; max; min; mean; quantiles at each value of
#' `qs`; count of `NA` values; and proportion of `NA` values.
#'
#' @param long_df Dataframe in long format
#' @param name_col Character. Group values in `long_df` by this column name. Can
#' be `NULL` to summarise the entire `long_df`
#' @param value_col Character. Name of column in `long_df` with the values to
#' summarise
#' @param group_cols Character. Any extra columns in `long_df` to group by.
#' @param qs Numeric. Passed to the `q` argument of `stats::quantile()` (via
#' `envFunc::quibble()`)
#'
#' @returns tibble of summarised values with one row for: each unique value in
#' `name_col`; and, if supplied, the combination with `group_cols`.
#' @export
#'
#' @examples
summarise_long_df <- function(long_df
                              , name_col = "name"
                              , value_col = "value"
                              , group_cols = NULL
                              , qs = c(0.05, 0.25, 0.5, 0.75, 0.95)
                              ) {

  long_df |>
    dplyr::summarise(max = max(!!rlang::ensym(value_col), na.rm = TRUE)
                     , min = min(!!rlang::ensym(value_col), na.rm = TRUE)
                     , mean = mean(!!rlang::ensym(value_col), na.rm = TRUE)
                     , sd = sd(!!rlang::ensym(value_col), na.rm = TRUE)
                     , q = envFunc::quibble(!!rlang::ensym(value_col)
                                             , na.rm = TRUE
                                             , q = qs
                                             )
                     , n = dplyr::n()
                     , NAs = sum(is.na(!!rlang::ensym(value_col)))
                     , propNA = NAs / n
                     , .by = tidyselect::any_of(c(name_col, group_cols))
                     ) |>
    tidyr::unnest(cols = c(q)) |>
    tibble::as_tibble()

}
