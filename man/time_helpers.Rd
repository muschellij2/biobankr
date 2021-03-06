% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa_date_helpers.R
\name{time_to_min}
\alias{time_to_min}
\alias{min_to_hr_min}
\alias{min_to_time}
\alias{floor_1day}
\alias{yyyymmdd}
\title{Time helper functions}
\usage{
time_to_min(x)

min_to_hr_min(x)

min_to_time(x, day = NULL, tz = "UTC")

floor_1day(x)

yyyymmdd(x)
}
\arguments{
\item{x}{an object of class Date or numeric time.  Note,
only seconds are kept, not milliseconds}

\item{day}{A date given in YMD format}

\item{tz}{timezone passed to \code{lubridate} functions}
}
\value{
A Date or numeric object
}
\description{
Time helper functions
}
\examples{
tm = Sys.time()
print(tm)
mins = time_to_min(tm)
mins
min_to_time(mins)
min_to_time(mins, day = "2017-01-24")
min_to_hr_min(600)
}
