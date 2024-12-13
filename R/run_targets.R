
#' Run targets
#'
#' Runs `targets::tar_source()`, reads the context from `settings_context_file`,
#' creates _targets.yaml from any files matching the pattern
#' `"^\\d{3}_.*\\.R$"` and then runs all the targets.
#'
#' @param settings_context_file
#'
#' @return as per `targets::tar_make()`
#' @export
#'
#' @examples
run_targets <- function(settings_context_file = "settings/setup.yaml") {

  targets::tar_source()

  settings_context <- yaml::read_yaml(settings_context_file)$context

  # tars --------
  ## tars df ------
  tars_df <- tibble::tibble(script = fs::dir_ls(regexp = "^\\d{3}_.*\\.R$")) |>
    dplyr::mutate(project = purrr::map_chr(script, \(x) gsub("\\d{3}_|\\.R", "", x))
                  , store = envFunc::store_dir(set_list = c(settings_context))
                  , store = fs::path(store, project)
                  ) %>%
    dplyr::select(project, script, store) %>%
    dplyr::arrange(desc(project))

  ## _targets.yaml -------
  fs::file_delete("_targets.yaml")

  purrr::pmap(tars_df
              , targets::tar_config_set
              )

  ## tars list ------
  tars <- yaml::read_yaml("_targets.yaml")


  # run everything ----------
  # in _targets.yaml
  purrr::walk2(purrr::map(tars, "script")
               , purrr::map(tars, "store")
               , \(x, y) targets::tar_make(script = x, store = y)
               )

}

