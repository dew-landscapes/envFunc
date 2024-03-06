
#' Run the scrips in a project
#'
#' Assumes the file names of the scripts to run start with four-digits that order the files. i.e. 0010_setup.R will run before 0020_clean.R
#'
#' @param from numeric. minimum script to run
#' @param to numeric. maximum script to run
#' @param skips character. anything to grep from the file names that you do not want to run
#'
#' @return side effect of sourcing the scripts
#' @export
#'
#' @examples
  run <- function(from = 0
                  , to
                  , skips = NULL
                  ) {

    # This runs all scripts (from run_from to run_to) in order, other than 'skips'
    fs::dir_ls(path = here::here()
               , regexp = "\\d{4}_.*\\.R$"
               , type = "file"
               ) %>%
      {if(!is.null(skips)) (.) %>% grep(paste(skips, collapse = "|"), ., value = TRUE, invert = TRUE) else (.)} %>%
      setNames(stringr::str_extract(.
                                    , "\\d{4}"
                                    )
               ) %>%
      `[` (names(.)[as.numeric(names(.)) <= run_to & as.numeric(names(.)) >= if(run_from == 0) 1 else run_from]) %>%
      unique() %>%
      purrr::walk(source
                  , verbose = TRUE
                  )


  }
