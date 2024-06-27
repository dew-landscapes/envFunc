
#' Empty a directory
#'
#' Any files in `dir_to_clean` are moved to the new directory
#' `dir_to_clean/old/moved_%Y%m%d_%H%M%S`
#'
#' @param dir_to_clean Path to directory to 'clean'
#' @param skip Character. Any strings that identify files to skip (identified
#' via `base::grep()`)
#'
#' @return Returns `NULL`. Any files in `dir_to_clean` are moved.
#' @export
#'
#' @examples
  clean_dir <- function(dir_to_clean
                        , skip = "old"
                        ) {

    files <- fs::dir_ls(dir_to_clean) %>%
        grep(paste0(skip, collapse = "|"), ., value = TRUE, invert = TRUE)

    if(length(files) > 0) {

      new_dir <- fs::path(dir_to_clean, "old", paste0("moved__", format(now(), "%Y%m%d_%H%M%S")))

      dir_create(new_dir)

      message(format(now(), "%Y%m%d_%H%M%S")
              , ":"
              , length(files), " files moved from "
              , dir_to_clean
              , " to "
              , new_dir
              )

      # clean out any prior work
      fs::file_move(files
                    , fs::path(new_dir, basename(files))
                    )

    }

    return(invisible(NULL))

  }
