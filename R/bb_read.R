#' Read Biobank Acceleration File
#'
#' @param file path to filename
#' @param force_numeric Should the acceleration be
#' set to numeric even if read in as character
#' (odd but happens)
#' @param max_days Maximum number of days the data should have.  Has no effect
#' on reading in the data, but prints a warning if there are more than the
#' maximum.
#'
#' @return A \code{tibble}
#' @export
#' @importFrom readr read_csv
#' @importFrom lubridate seconds as.period
#' @examples
#' file = system.file("test2.csv", package = "biobankr")
#'   df = bb_read(file)
bb_read = function(file,
                   force_numeric = TRUE,
                   max_days = 7) {

  imputed = NULL
  rm(list = "imputed");
  #############################
  # read in the data
  #############################
  df = readr::read_csv(file = file)

  #############################
  # get header information
  #############################
  info = colnames(df)[1]
  info = trimws(info)

  #############################
  # sampling rate
  #############################
  srate = extract_sampling_rate(info)
  srate = as.numeric(srate)
  if (is.na(srate)) {
    stop("Sampling rate returning NA")
  }

  #############################
  # date range
  #############################
  date_range = extract_date(info)
  date_range = lubridate::ymd_hms(date_range)
  if (any(is.na(date_range))) {
    stop("Sampling rate returning NA")
  }

  # change first column to just acceleration
  colnames(df)[1] = "acceleration"

  if (force_numeric) {
    bad = is.na(as.numeric(df$acceleration)) &
      !is.na(df$acceleration)
    stopifnot(all(!bad))
    df$acceleration = as.numeric(df$acceleration)
  }

  # create date
  n_time_points = nrow(df)
  df$date = date_range[1] +
    lubridate::as.period(
      seq(0, n_time_points - 1) * srate,
      unit = "seconds")

  # check that header and data match up
  last_date = df$date[n_time_points]
  if (last_date != date_range[2]) {
    warning("Sequence of days/times does not match date range on header")
  }
  df = df %>%
    mutate(imputed = as.logical(imputed))

  nr = nrow(df)
  n_days = nr * srate / (60 * 60 * 24)
  too_many_days = FALSE
  if (n_days > max_days) {
    msg = paste0("Over ", max_days, " days worth of data!",
           " Device was probably left on!")
    warning(msg)
    too_many_days = TRUE
  }
  attr(df, "too_many_days") = too_many_days


  attr(df, "sampling_rate") = srate

  return(df)
}


