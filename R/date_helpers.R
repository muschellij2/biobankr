#' Time helper functions
#'
#' @param x an object of class Date or numeric time.  Note,
#' only seconds are kept, not milliseconds
#' @param day A date given in YMD format
#'
#' @return A Date or numeric object
#' @export
#'
#' @examples
#' tm = Sys.time()
#' print(tm)
#' mins = time_to_min(tm)
#' mins
#' min_to_time(mins)
#' min_to_time(mins, day = "2017-01-24")
#' @importFrom lubridate floor_date hour minute as.period ymd tz
#' @rdname time_helpers
time_to_min = function(x) {
  x = as.POSIXlt(x, tz = lubridate::tz(x))
  x = x$hour * 60 + x$min
  # x = lubridate::hour(x) * 60 + lubridate::minute(x)
  return(x)
}

#' @export
#' @rdname time_helpers
min_to_time  = function(x, day = NULL) {
  hr = floor(x / 60)
  mins = x - hr * 60
  str = sprintf("%02i:%02i", hr, mins)
  fmt = "%H:%M"
  if (!is.null(day)) {
    day = lubridate::ymd(day)
    str = sprintf("%s %s", day, str)
    fmt = paste0("%Y-%m-%d", fmt)
  }
  tm = as.POSIXct(
    str,
    format = fmt)
  return(tm)
}

