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
#' @importFrom lubridate floor_date hour minute as.period ymd tz is.POSIXlt
#' @rdname time_helpers
time_to_min = function(x) {
  if (lubridate::is.POSIXlt(x)) {
    x = x$hour * 60 + x$min
  } else {
    x = as.integer(format(x, "%H")) * 60 + as.integer(format(x, "%M"))
  }
  x = as.integer(x)
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



#' @export
#' @rdname time_helpers
floor_1day = function(x) {
  x <- as.POSIXlt(x)
  x$sec <- 0
  x$min <- 0L
  x$hour <- 0L
  x = as.Date(x)
  return(x)
}



#' @export
#' @rdname time_helpers
yyyymmdd = function(x) {
  # x is POSIXlt
  x = (x$year + 1900)*10000 + (x$mon + 1) * 100 + (x$mday)
  x = as.integer(x)
  x
}
