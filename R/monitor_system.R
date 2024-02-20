#' Monitor system resources
#'
#' A simple version of windows task manager
#'
#' @param monitor_df NULL or results from previous call to envFunc::monitor_system()
#'
#' @return Dataframe
#' @export
#'
#' @examples
#'   df <- monitor_system()
#'
#'   counter <- 1
#'
#'   while(counter < 3) {
#'
#'     counter <- counter + 1
#'
#'     Sys.sleep(1)
#'
#'     df <- monitor_system(df)
#'
#'   }
  monitor_system <- function(monitor_df = NULL) {

      new <- tibble::tibble(
        now = Sys.time()
        , prop_cpu = envFunc::prop_cpu()
        , prop_mem = envFunc::prop_mem()
        )

      monitor_df <- if(!is.null(monitor_df)) {

        monitor_df |>
          dplyr::bind_rows(new)

      } else new

      mon_long <- monitor_df |>
        tidyr::pivot_longer(contains("prop")
                            , names_to = "process"
                            , values_to = "value"
                            )

      print(ggplot(mon_long, aes(now, value)) +
        geom_point() +
        geom_smooth() +
        facet_wrap(~ process, scales = "free_y"))

      return(monitor_df)

    }
