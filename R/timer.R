

#' A (rough) timer
#'
#' Useful mainly in a script `file` to time, then log, each step (or `process`)
#' in the file.
#'
#' Timer can be re-started by including 'start' in a process.
#'
#' @param process Character. Name of the process being timed.
#' @param notes Character. Any notes associated with the process.
#' @param file Character. Name of the (script) file.
#' @param name Character. Might be the same as `process`, or an element over
#' which `process` is being run. e.g. `name` could be a species name where the
#' same function is being run over many different species.
#' @param log Character. Path to write the log file.
#' @param time_df Object name or null for the initiation of a timer.
#' @param write_log Write the log after this process. Default is only where
#' process contains 'end'.
#'
#' @return A dataframe (time_df), possibly with an additional row for the
#' current process. If `write_log` then `log` file is written.
#' @export
#'
#' @examples
#' time_df <- timer("start script", file = "file 01", name = "example", log = "example.log")
#' Sys.sleep(1)
#' time_df <- timer("process 01", time_df = time_df)
#' Sys.sleep(1)
#' time_df
#' time_df <- timer("end script", time_df = time_df) # log.log written
#' Sys.sleep(1)
#' time_df <- timer("start script", file = "file 02", time_df = time_df)
#' Sys.sleep(1)
#' time_df <- timer("process 01", time_df = time_df, write_log = TRUE) # log.log written
#' time_df <- timer("end script", time_df = time_df) # log.log written
#'
#' # clean up
#' rm(time_df)
#' unlink("example.log")
#'
  timer <- function(process
                    , notes = NULL
                    , file = NULL
                    , name = NULL
                    , log = NULL
                    , time_df = NULL
                    , write_log = grepl("end", process)
                    ) {

    if(is.null(file)) file <- time_df$file[nrow(time_df)]
    if(is.null(name)) name <- time_df$name[nrow(time_df)]
    if(is.null(log)) log <- time_df$log[nrow(time_df)]

    new_df <- tibble::tibble(name = name
                             , file = file
                             , process = process
                             , time = Sys.time()
                             , notes = if(is.null(notes)) NA_character_ else notes
                             , log = log
                             )

    new_df <- if(grepl("start", process)) {

      new_df |>
        dplyr::mutate(elapsed = hms::as_hms(0))

    } else {

      new_df |>
        dplyr::mutate(elapsed = hms::as_hms(difftime(Sys.time()
                                                    , tail(time_df$time[!time_df$process %in% c("warning", process)], 1)
                                                    )
                                           )
                      )

    }

    time_df <- if(!is.null(time_df)) {

        dplyr::bind_rows(time_df, new_df)

      } else new_df


    if(write_log) {

      text <- time_df |>
        dplyr::group_by(file) |> # to ensure difftime gets max and min within 'file'
        dplyr::mutate(text = dplyr::case_when(grepl("start", process) ~ paste0(process
                                                                               , ": "
                                                                               , time
                                                                               )
                                              , grepl("end", process) ~ paste0(process
                                                                               , ". total "
                                                                               , file
                                                                               , " elapsed time: "
                                                                               , hms::as_hms(difftime(max(time)
                                                                                                      , min(time)
                                                                                                      )
                                                                                             )
                                                                               )
                                              , grepl("warning|error", process) ~ paste0(process
                                                                                        , ": "
                                                                                        , time
                                                                                        )
                                              , TRUE ~ paste0(process
                                                              , ": "
                                                              , elapsed
                                                              )
                                              )
                      , text = dplyr::case_when(is.na(notes) ~ paste0(text, ".")
                                                , TRUE ~ paste0(text, ". ", notes, ".")
                                                )
                      , text = gsub("\\.\\.", ".", text)
                      ) |>
        dplyr::ungroup()

      writeLines(c(unique(text$name), text$text)
                 , con = log
                 )

    }

    return(time_df |>
             dplyr::relocate(log, .after = last_col())
           )

  }
