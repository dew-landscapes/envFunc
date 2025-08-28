

#' Name, create, parse and/or search output directories
#'
#' @param env_out Nested list, with two levels, of critical settings (only) for
#' use in output path names. The first elements of the list will return
#' directories. Elements within each of the first elements of the list are
#' concatenated to form the name of each directory (see examples).
#' @param base_dir Character. Directory prefix to the output path.
#' @param dir_with_context Logical. If FALSE (default) the first elements in
#' `env_out` are used as prefix to each context, otherwise no prefix is added.
#' Set to TRUE if there are contexts repeated across any elements of
#' `env_out`.
#' @param all_files Logical or numeric. Return files within the `path` column
#' provided in the resulting dataframe? If numeric, passed to the `recurse`
#' argument of `fs::dir_ls()`.
#' @param search_dir Character. Path(s) to search for the `path` in the returned
#' tibble. Ignored unless `base_dir` is null. Allows for searching several
#' different paths for the same `path` in the returned tibble.
#' @param remove_stop Logical. Should `.` be removed from elements within the
#' list? e.g. `0.95` becomes `095`
#' @param ret Character. Return a dataframe (`"df"`) or path (`"path"`)?
#' @param create_path Logical. If `TRUE` the path is created via
#' `fs::dir_create()`. This is independent of `ret == "path"`.
#' @param ... Passed to `fs::dir_ls()`. Argument `path` is already provided, so
#' providing here will cause an error.
#'
#'
#' @return If `ret == "df"`, tibble containing:
#' * all contexts: named as per the second level elements of `env_out`
#' * directories: a directory for each element in the first level of `env_out`
#' * path: the output path, prefixed with `base_dir`
#'
#' If `ret == "path"` a path.
#'
#' If `create_path` the path is created via `fs::dir_create()`.
#'
#' @export
#'
#' @example inst/examples/name_env_out_ex.R
name_env_out <- function(env_out
                         , base_dir = NULL
                         , dir_with_context = FALSE
                         , all_files = FALSE
                         , search_dir = if(is.null(base_dir)) here::here() else NULL
                         , remove_stop = TRUE
                         , ret = if(all_files) "df" else "path"
                         , create_path = ret == "path"
                         , ...
                         ) {

  df <- env_out %>%
    purrr::modify_tree(leaf = \(x) paste0(if(remove_stop) gsub("\\.", "", x) else x, collapse = "--")) |>
    purrr::map(\(x) paste0(x, collapse = "__")) |>
    tibble::as_tibble() |>
    dplyr::mutate(dplyr::across(dplyr::where(is.character)
                                , \(x) gsub("NULL", "", x)
                                )
                  ) |>
    tidyr::unite(col = "path"
                 , tidyselect::everything()
                 , remove = FALSE
                 , sep = "/"
                 ) %>%
    {if(!is.null(base_dir)) (.) %>% dplyr::mutate(path = fs::path(base_dir, path)) else (.)}

  res <- purrr::map(names(env_out)
                   , \(x) {

                     use_names <- if(dir_with_context) {

                       paste0(x, "_", names(env_out[[x]]))

                     } else {

                       names(env_out[[x]])

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

  if(any(all_files, ret == "df")) {

    search_dir <- if(!is.null(base_dir)) unique(res$path) else search_dir

    if(dir.exists(search_dir)) {

      res <- res %>%
        dplyr::mutate(files = purrr::map(path
                                         , \(x) {

                                           fs::dir_ls(path = search_dir
                                                      , ...
                                                      )

                                         }
                                         )
                      ) |>
        dplyr::relocate(tidyselect::matches(names(env_out))
                        , path
                        , tidyselect::matches("\\bfiles\\b")
                        , .after = everything()
                        )

    }

  } else if(ret == "path") {

    res <- res$path

  }

  if(create_path) {

    path_to_create <- if(is.character(res)) res else res$path

    fs::dir_create(path_to_create)

    purrr::walk(1:length(env_out)
                , \(x) yaml::write_yaml(env_out[x]
                                        , fs::path(base_dir
                                                   , fs::path_join(df |>
                                                                     dplyr::select(tidyselect::any_of(names(env_out[1:x]))) |>
                                                                     unlist()
                                                                   )
                                                   , "metadata.yaml"
                                                   )
                                        )
                )

  }

  return(res)

}
