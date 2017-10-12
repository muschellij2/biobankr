
#' Summarize Biobank Acceleration File
#'
#' @param df A \code{data.frame} or \code{tbl} from \code{\link{bb_read}}
#' @param ... Additional arguments to pass to \code{\link{bb_summarize}}
#'
#' @return A table of the day and 1440 columns
#' @export
#'
#' @examples
#' @importFrom tidyr spread
#' @importFrom lubridate floor_date hour minute
#' @importFrom dplyr select
bb_1440 = function(
  df,
  ...) {

  minute = acceleration = NULL
  rm(list = c("acceleration", "minute"))

  df = bb_summarize_minute(df, ...)
  df = df %>%
    dplyr::select(date, acceleration) %>%
    mutate(
      minute = lubridate::hour(date) * 60 + lubridate::minute(date),
      date = lubridate::floor_date(date, unit = "day")) %>%
    spread(key = minute, value = acceleration)
  return(df)
}
