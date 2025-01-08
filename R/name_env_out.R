

#' Create and/or parse output directory paths
#'
#' Either create directory path for saving outputs or parse the meta data from
#' an output directory path. The first elements in the list will form
#' directories. The second elements in the list form the name of each directory,
#' separated by "__".
#'
#' @param set_list Nested list, with two levels, of critical settings (only) for
#' use in output path names. The first elements of the list will return
#' directories. Elements within each of the first elements of the list are
#' concatenated to form the name of each directory (see examples).
#' @param base_dir Character. Directory prefix to the output path.
#' @param show_null Logical. Display "NULL" or "NA" in names
#' (or gsub it out with "").
#' @param return_contexts Logical. If TRUE (default) contexts, directories and
#' path are returned, otherwise just directories and path.
#' @param dir_with_context Logical. If FALSE (default) the first elements in
#' `set_list` are used as prefix to each context, otherwise no prefix is added.
#' Set to TRUE if there are contexts repeated across any elements of
#' `set_list`.
#' @param all_files Logical or numeric. Return files within the `path` column
#' provided in the resulting dataframe? If numeric, passed to the `recurse`
#' argument of `fs::dir_ls()`.
#' @param search_dir Character. Path(s) to search for the `path` in the returned
#' tibble. Ignored unless `base_dir` is null. Allows for searching several
#' different paths for the same `path` in the returned tibble.
#' @param reg_exp Character. Combined with `path` in the returned tibble to
#' search for files.
#' @param ... Passed to `fs::dir_ls()`. Arguments `path` and `regexp` are
#' already provided, so providing them here will cause an error.
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
                         , show_null = FALSE
                         , return_contexts = TRUE
                         , dir_with_context = FALSE
                         , all_files = FALSE
                         , search_dir = if(is.null(base_dir)) here::here() else NULL
                         , reg_exp = NULL
                         , ...
                         ) {

  df <- set_list %>%
    purrr::modify_tree(leaf = \(x) paste0(x, collapse = "--")) %>%
    purrr::map(\(x) paste0(x, collapse = "__")) %>%
    tibble::as_tibble() %>%
    {if(show_null) (.) else (.) %>% dplyr::mutate(dplyr::across(dplyr::where(is.character)
                                                                , \(x) gsub("NULL", "", x)
                                                                )
                                                  )
      } %>%
    tidyr::unite(col = "path"
                 , tidyselect::everything()
                 , remove = FALSE
                 , sep = "/"
                 ) %>%
    {if(!is.null(base_dir)) (.) %>% dplyr::mutate(path = fs::path(base_dir, path)) else (.)}

  if(return_contexts) {

    df <- purrr::map(names(set_list)
                     , \(x) {

                       use_names <- if(dir_with_context) {

                         paste0(x, "_", names(set_list[[x]]))

                       } else {

                         names(set_list[[x]])

                       }

                       df %>%
                         tidyr::separate(col = !!rlang::ensym(x)
                                         , into = use_names
                                         , remove = FALSE
                                         , sep = "__"
                                         )

                     }
                     ) %>%
      purrr::reduce(dplyr::left_join)

  }

  if(all_files) {

    search_dir <- if(!is.null(base_dir)) unique(df$path) else search_dir

    df <- df %>%
      dplyr::mutate(files = purrr::map(path
                                       , \(x) {

                                         use_regex <- if(is.null(reg_exp)) x else paste(x, reg_exp, sep = ".*", collapse = "|")

                                         fs::dir_ls(path = search_dir
                                                    , regexp = use_regex
                                                    , ...
                                                    )

                                       }
                                       )
                    )

  }



  df <- df %>%
    dplyr::relocate(tidyselect::matches(names(set_list))
                    , path
                    , tidyselect::matches("\\bfiles\\b")
                    , .after = everything()
                    )

  return(df)

}
