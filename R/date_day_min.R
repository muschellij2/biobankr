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

  df = df %>%
    mutate(
      minute = lubridate::hour(date) * 60 + lubridate::minute(date),
      day = lubridate::floor_date(date, unit = "day")) %>%
    select(-date)
  if (from_baseline) {
    df = df %>%
      mutate(day = lubridate::as.period(day - min(day), unit = "day"))
  }
  return(df)
}
