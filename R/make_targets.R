
#' Make targets
#'
#' Runs `targets::tar_source()`, reads the context from `settings_context_file`,
#' and creates _targets.yaml from any files matching the pattern
#' `"^\\d{3}_.*\\.R$"`.
#'
#' @param settings_context_file
#'
#' @return list of 'projects' each with elements 'script' and 'store'. Saves
#' _targets.yaml
#' @export
#'
#' @examples
make_targets <- function(settings_context_file = "settings/setup.yaml") {

  targets::tar_source()

  settings_context <- yaml::read_yaml(settings_context_file)$context

  # tars --------
  ## tars df ------
  tars_df <- tibble::tibble(script = fs::dir_ls(regexp = "^\\d{3}_.*\\.R$")) |>
    dplyr::mutate(project = purrr::map_chr(script, \(x) gsub("\\d{3}_|\\.R", "", x))
                  , order = readr::parse_number(script)
                  , store = envFunc::store_dir(set_list = c(settings_context))
                  , store = fs::path(store, project)
                  ) |>
    dplyr::arrange(order) |>
    dplyr::select(project, script, store)

  ## _targets.yaml -------
  if(file.exists("_targets.yaml")) fs::file_delete("_targets.yaml")

  purrr::pmap(tars_df
              , targets::tar_config_set
              )

  ## tars list ------
  tars <- yaml::read_yaml("_targets.yaml")

  return(tars)

}

