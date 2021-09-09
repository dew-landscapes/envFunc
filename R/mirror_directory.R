
#' Mirror a directory
#'
#' Check for any new/updated files on `from_dir` and copy them to `to_dir`.
#' Check for any extra files in `to_dir` and delete them.
#'
#' @param from_dir Path to directory to mirror.
#' @param to_dir Path to directory you wish to mirror `from_dir`.
#' @param ... Arguments to \link[fs]{dir_ls}.
#'
#' @return `to_dir` will mirror `from_dir`
#' @export
#'
#' @examples
mirror_directory <- function(from_dir, to_dir, ...) {

  if(file.exists(from_dir)) {

    from <- fs::dir_info(from_dir, ...) %>%
      dplyr::mutate(check = paste0(fs::path_file(path),modification_time))

    to <- fs::dir_info(to_dir, ...) %>%
      dplyr::mutate(check = paste0(fs::path_file(path),modification_time))

    to_import <- setdiff(from$check,to$check)
    to_remove <- setdiff(to$check,from$check)

    imp <- from %>%
      dplyr::filter(check %in% to_import) %>%
      dplyr::mutate(new_path = paste0(to_dir,"/",fs::path_file(path)))

    del <- to %>%
      dplyr::filter(check %in% to_remove)

    if(length(to_import) > 0){

      cat(paste0("copying "
                   , paste0(fs::path_file(imp$path)
                            , collapse = "; "
                            )
                   , " to "
                   , to_dir
                   )
            )

      purrr::walk2(imp$path
                   , imp$new_path
                   , fs::file_copy
                   , overwrite = TRUE
                   )

    }

    if(length(to_remove) > 0){

      cat(paste0("deleting "
                 , paste0(fs::path_file(del$path)
                          , collapse = "; "
                          )
                 , " from "
                 , to_dir
                 )
          )

      purrr::walk(del$path
                  , fs::file_delete
                  )

      }

    }

}
