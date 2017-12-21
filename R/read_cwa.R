
#' Read CWA from Axtivity
#'
#' @param file filename of cwa file
#' @param end End point for reading data.  Default is all the data
#' @param convert_time Should times be converted to \code{POSIXct}?
#' @param tz A time zone specification to be used for the conversion,
#' if one is required.  System-specific (see \link{timezones}),
#' but "" is the current time zone,
#' and "GMT" is UTC (Universal Time, Coordinated).
#' @param verbose print diagnostic messages
#'
#' @return A list with header information and a tbl
#' @export
#'
#' @importFrom dplyr as_data_frame
#' @importFrom GGIR g.cwaread
#' @importFrom R.utils isGzipped isBzipped decompressFile isCompressedFile
#' @importFrom tools file_ext
read_cwa = function(file, end = Inf, convert_time = TRUE, verbose = TRUE,
                    tz = "") {
  ext = tools::file_ext(file)
  isXzipped = function(...) {
    R.utils::isCompressedFile(..., ext = "xz", fileClass = "xzfile")
  }
  if (isGzipped(file) || isBzipped(file) || isXzipped(file)) {
    FUN = switch(ext,
                 gz = gzfile,
                 xz = xzfile,
                 bz2 = bzfile,
                 bz = bzfile)
    file = decompressFile(
      filename = file,
      temporary = TRUE,
      ext = ext,
      FUN = FUN,
      overwrite = TRUE,
      remove = FALSE)
  }
  ext = tools::file_ext(file)
  ext = tolower(ext)
  res = GGIR::g.cwaread(
    fileName = file, start = 0, end = end, progressBar = verbose)
  res$data = dplyr::as_data_frame(res$data)
  if (convert_time) {
    res$data$time = as.POSIXct(res$data$time, origin = "1970-01-01",
                               tz = tz)
    # won't show the full hertz
    dsecs = getOption("digits.secs")
    if (is.null(dsecs)) {
      warning(
        paste0("digit.secs option not defined, try options(digits.secs = 2)")
      )
    }
    time1 = res$data$time[1]
    if (res$header$start != time1) {
      msg = paste0("Header start date is not same time as data$time,",
                   " may want to use convert_time = FALSE.")
      warning(msg)
    }
  }

  return(res)
}
