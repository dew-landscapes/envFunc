#-------System---------

#' Proportion of current memory usage
#'
#' @return Numeric. Proportion
#' @export
#'
#' @examples
  prop_mem <- function() {

    # https://stackoverflow.com/questions/27788968/how-would-one-check-the-system-memory-available-using-r-on-a-windows-machine

    if(Sys.info()['sysname'] == "Windows") {

      total <- stingr::parse_number(paste0(system2("wmic", args =  "OS get TotalVirtualMemorySize /Value", stdout = TRUE),collapse = "_"))
      free <- stringr::parse_number(paste0(system2("wmic", args =  "OS get FreeVirtualMemory /Value", stdout = TRUE),collapse = "_"))

      (total-free)/total

    } else warning("Only runs on windows")

  }

#' Proportion of current CPU usage
#'
#' @return Numeric. Proportion
#' @export
#'
#' @examples
  prop_cpu <- function() {

    if(Sys.info()['sysname'] == "Windows") {

      a <- system("wmic path Win32_PerfFormattedData_PerfProc_Process get Name,PercentProcessorTime", intern = TRUE)
      df <- as_tibble(do.call(rbind, lapply(strsplit(a, " "), function(x) {x <- x[x != ""];data.frame(process = x[1], cpu = x[2])}))) %>%
        dplyr::mutate(cpu = as.numeric(cpu))

      sum(df$cpu[!grepl("Idle|Total",df$process)],na.rm = TRUE)/(df %>% dplyr::filter(grepl("Total",process)) %>% dplyr::pull(cpu))

      } else warning("Only runs on windows")

    }
