

#' Create output directory paths or parse output directory paths
#'
#' Either create file path for saving outputs or parse the meta data from a path
#'
#' @param set_list List (optionally nested) of critical settings (only) for use
#' in output path names. The first elements of the list will return directories.
#' Any elements within each of the first elements of the list are concatenated
#' to form the name of each directory (see examples)
#' @param base_dir Character. Directory prepended to the output path
#' @param all_files Logical or numeric. Return files within the `path` column
#' provided in the resulting dataframe? If numeric, passed to the `recurse`
#' argument of `fs::dir_ls()`.
#' @param show_null Logical. Display "NULL" or "NA" in names
#' (or gsub it out with "")
#'
#'
#' @return Tibble containing:
#' * all contexts: named as per the second level elements of `set_list`
#' * directories: a directory for each element in the first level of `set_list`
#' * path: the output path, prepended with base_dir
#'
#' @export
#'
#' @example inst/examples/name_env_out_ex.R
name_env_out <- function(set_list
                         , base_dir = NULL
                         , all_files = FALSE
                         , show_null = FALSE
                         ) {

  df <- set_list %>%
    purrr::map(\(x) paste0(x, collapse = "__")) %>%
    list2DF() %>%
    tibble::as_tibble() %>%
    {if(show_null) (.) else (.) %>% dplyr::mutate(dplyr::across(dplyr::where(is.character)
                                                                , \(x) gsub("NULL|NA", "", x)
                                                                )
                                                  )
      } %>%
    tidyr::unite(col = "path"
                 , tidyselect::everything()
                 , remove = FALSE
                 , sep = "/"
                 )

  df <- purrr::map(names(set_list)
             , \(x) df %>%
               tidyr::separate(col = !!rlang::ensym(x)
                               , into = names(set_list[[x]])
                               , remove = FALSE
                               , sep = "__"
                               )
             ) %>%
    purrr::reduce(dplyr::left_join) %>%
    dplyr::relocate(tidyselect::any_of(names(set_list))
                    , path
                    , .after = everything()
                    ) %>%
    {if(!is.null(base_dir)) (.) %>% dplyr::mutate(path = fs::path(base_dir, path)) else (.)}

  if(all_files) {

    df <- df %>%
      dplyr::mutate(dir_exists = dir.exists(path)
                    , files = purrr::map2(path
                                          , dir_exists
                                          , \(x, y) if(y) fs::dir_ls(x, recurse = all_files)
                                          )
                    )

  }

  return(df)

}
