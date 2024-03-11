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
  monitor_system <- function(monitor_df = NULL
                             , plot = TRUE
                             , plot_time = 1/24
                             ) {

      new <- tibble::tibble(now = Sys.time())

      new_cpu <- new |> dplyr::bind_cols(prop_cpu())
      new_mem <- new |> dplyr::bind_cols(prop_mem())

      new <- dplyr::bind_rows(new_cpu, new_mem)

      monitor_df <- if(!is.null(monitor_df)) {

        monitor_df |>
          dplyr::bind_rows(new)

      } else new

      if(plot) {

        print(ggplot(monitor_df |>
                       tidyr::pivot_longer(dplyr::where(is.numeric)) |>
                       dplyr::filter(!grepl("free", name)) |>
                       dplyr::filter(as.numeric(difftime(Sys.time(), now, units = "days")) <= plot_time)
                     , aes(now, value)
                     ) +
                geom_point() +
                facet_wrap(name ~ type, scales = "free_y") +
                scale_x_datetime(labels = scales::date_format("%H:%M", tz = "Australia/Adelaide"))
              )

      }

      return(monitor_df)

    }
