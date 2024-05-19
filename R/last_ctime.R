
#' Object with the latest `ctime` attribute
#'
#'
#'
#' @param prefix
#'
#' @return Character. Object name with latest `ctime` attribute.
#' @export
#'
#' @examples
#'
  last_ctime <- function(prefix = "bio_") {

    ls(pattern = prefix
       , envir = .GlobalEnv
       ) %>%
      tibble::enframe(name = NULL
                      , value = "name"
                      ) %>%
      dplyr::mutate(ctime = purrr::map(name
                                       , ~attr(get(.), "ctime")
                                       )
                    ) %>%
      tidyr::unnest(cols = c(ctime)) %>%
      dplyr::filter(format(ctime, digits = 4) == max(format(ctime, digits = 4))) %>%
      dplyr::pull(name) %>%
      tail(1)

  }
