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
#' min_to_hr_min(600)
#' @importFrom lubridate floor_date hour minute as.period ymd tz is.POSIXlt
#' @rdname time_helpers
time_to_min = function(x) {
  if (lubridate::is.POSIXlt(x)) {
    x = x$hour * 60 + x$min
  } else {
    x = as.integer(format(x, "%H")) * 60L + as.integer(format(x, "%M"))
  }
  x = as.integer(x)
  # x = lubridate::hour(x) * 60L + lubridate::minute(x)
  return(x)
}

#' @export
#' @rdname time_helpers
min_to_hr_min = function(x) {
  if (is.integer(x)) {
    func = as.integer
    val = 60L
  } else {
    func = identity
    val = 60
  }
  hr = func(floor(x / val))
  x = x - func(hr * val)
  cbind(hour = hr, min = x)
}

#' @export
#' @rdname time_helpers
#' @param tz timezone passed to \code{lubridate} functions
min_to_time  = function(x, day = NULL, tz = "UTC") {
  x = min_to_hr_min(x)

  x = sprintf("%02i:%02i", x[, "hour"], x[, "min"])
  fmt = "%H:%M"
  if (!is.null(day)) {
    day = lubridate::ymd(day, tz = tz)
    x = sprintf("%s %s", day, x)
    fmt = paste0("%Y-%m-%d", fmt)
  }
  x = as.POSIXct(
    x,
    format = fmt,
    tz = tz)
  return(x)
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
  x = (x$year + 1900L)*10000L + (x$mon + 1L) * 100L + (x$mday)
  x = as.integer(x)
  x
}
