
#' Empty a directory
#'
#' Any files or directories in `dir_to_clean` are moved to the new directory
#' `dir_to_clean/old/moved_%Y%m%d_%H%M%S` UNLESS THERE ARE MORE THAN `max_files`
#' IN WHICH CASE ALL ARE DELETED
#'
#' @param dir_to_clean Path to directory to 'clean'
#' @param skip Character. Any strings that identify files to skip (identified
#' via `base::grep()`)
#' @param max_files Numeric. If there are more than `max_files` to be moved,
#' files will be DELTETED instead of moved.
#'
#' @return Returns `NULL`. Any files in `dir_to_clean` are moved (or deleted).
#' Log written to `dirname(dir_to_clean)`
#' @export
#'
#' @examples
  clean_dir <- function(dir_to_clean
                        , skip = "old"
                        , max_files = 10000
                        ) {

    files <- fs::dir_ls(dir_to_clean) %>%
        grep(paste0(skip, collapse = "|"), ., value = TRUE, invert = TRUE)

    now <- base::format(lubridate::now(), "%Y%m%d_%H%M%S")

    if(length(files) > 0) {

      if(length(files) > max_files) {

        fs::dir_delete(dir_to_clean)
        fs::dir_create(dir_to_clean)

        mes <- paste0(now
                      , ":"
                      , length(files), " files DELETED from "
                      , dir_to_clean
                      )

      } else {

        new_dir <- fs::path(dir_to_clean, "old", paste0("moved__", now))

        dir_create(new_dir)

        # clean out any prior work
        fs::file_move(files
                      , fs::path(new_dir, basename(files))
                      )

        mes <- paste0(now
                      , ":"
                      , length(files), " files moved from "
                      , dir_to_clean
                      , " to "
                      , new_dir
                      )

      }

      message(mes)

      readr::write_lines(mes
                         , file = fs::path(dirname(dir_to_clean), "directory_clean.log")
                         , append = TRUE
                         , num_threads = 1
                         )

    } else {

      message("No files found to empty from "
              , dir_to_clean
              )

    }

    return(invisible(NULL))

  }
