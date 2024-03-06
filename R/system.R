
#' Proportion of current memory usage
#'
#' Edited from various posts on the [Stack Exchange Network](https://stackoverflow.com/questions/27788968/how-would-one-check-the-system-memory-available-using-r-on-a-windows-machine).
#'
#' @return Numeric. Proportion
#' @export
#'
#' @examples
#' if(.Platform$OS.type == "windows") prop_mem()
  prop_mem <- function() {

    # https://stackoverflow.com/questions/27788968/how-would-one-check-the-system-memory-available-using-r-on-a-windows-machine

    if(Sys.info()['sysname'] == "Windows") {

      res <- tibble::tibble(type = "mem"
                            , total = readr::parse_number(paste0(system2("wmic"
                                                                         , args =  "OS get TotalVisibleMemorySize /Value"
                                                                         , stdout = TRUE
                                                                         )
                                                                 , collapse = "_"
                                                                 )
                                                          )
                            , free = readr::parse_number(paste0(system2("wmic"
                                                                        , args =  "OS get FreePhysicalMemory /Value"
                                                                        , stdout = TRUE
                                                                        )
                                                                , collapse = "_"
                                                                )
                                                         )
                            ) |>
        dplyr::mutate(prop = (total - free) / total)

    } else warning("Only runs on windows")

  }


#' Proportion of current CPU usage
#'
#' Edited from various posts on the [Stack Exchange Network](https://stackoverflow.com/questions/27788968/how-would-one-check-the-system-memory-available-using-r-on-a-windows-machine).
#'
#' @return Numeric. Proportion
#' @export
#'
#' @examples
#' if(.Platform$OS.type == "windows") prop_cpu()
  prop_cpu <- function() {

    if(Sys.info()['sysname'] == "Windows") {

      a <- system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime", intern = TRUE)

      df <- tibble::as_tibble(a) |>
        purrr::map(stringr::str_squish) |>
        tibble::as_tibble(name = NULL) |>
        tidyr::separate_wider_delim(cols = value
                                    , delim = " "
                                    , names = c("process", "cpu")
                                    , too_many = "merge"
                                    , too_few = "align_start"
                                    ) |>
        dplyr::mutate(cpu = as.numeric(cpu))

      res <- tibble::tibble(type = "cpu"
                            , total = df$cpu[grepl("Total", df$process)]
                            , free = df$cpu[grepl("Idle", df$process)]
                            ) |>
        dplyr::mutate(prop = (total-free)/total)

      } else warning("Only runs on windows")

    }
