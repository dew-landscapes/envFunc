

#' Test rows against a function
#'
#' Filter the rows of a dataframe that fail a function applied to a column.
#'
#' @param df Dataframe
#' @param test_col Character name of column in `df` to test against `test_func`.
#' @param test_func Name of function to test `test_col` with. `test_col` must be
#' the first argument to `test_func`.
#' @param ... Other arguments passed to `test_func`.
#'
#' @return Filtered `df`.
#' @export
#'
#' @examples
filter_test_func <- function(df
                             , test_col = "path"
                             , test_func = terra::rast
                             , ...
                             ) {

  test_func <- purrr::safely(test_func)

  keep_names <- names(df)

  test_res <- df %>%
    dplyr::mutate(test = purrr::map(!!rlang::ensym(test_col)
                                    , ~ test_func(.)
                                    )
                  , pass = purrr::map_lgl(test, ~is.null(.$error))
                  )

  fail_res <- test_res %>%
    dplyr::filter(!pass)

  cat(paste0("Filtering the following from df[test_col], due to test_func failure\n  "
             , paste0(fail_res$path
                      , collapse = "\n  "
                      )
             )
      )

  test_res %>%
    dplyr::filter(pass) %>%
    dplyr::select(keep_names)

}
