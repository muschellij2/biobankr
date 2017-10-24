#' Convert Date to Day/Minute
#'
#' @param df A \code{data.frame} or \code{tbl} from \code{\link{bb_read}}
#' @param from_baseline Should day be day from baseline or
#' the day in Date format
#'
#' @return A summarized \code{tbl}
#' @export
#'
#' @examples
#' file = system.file("test2.csv", package = "biobankr")
#' df = bb_read(file)
#' day_stuff = date_day_min(df)
#' date_stuff = date_day_min(df, from_baseline = FALSE)
#' @importFrom lubridate floor_date hour minute as.period
date_day_min = function(
  df,
  from_baseline = TRUE) {

  day = NULL
  rm(list = "day")

  # df = df %>%
  #   mutate(
  #     minute = time_to_min(date),
  #     day = lubridate::floor_date(date, unit = "day")) %>%
  #   select(-date)
  df$minute = time_to_min(df$date)
  df$date = lubridate::floor_date(df$date, unit = "day")
  df = dplyr::rename(df, day = date)

  if (from_baseline) {
    # need mutate for grouping
    df = df %>%
      mutate(day = as.numeric(day - min(day), unit = "days")
      )
  }
  return(df)
}

