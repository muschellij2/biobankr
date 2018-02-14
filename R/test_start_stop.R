
#' Testing if there are too many cases
#'
#' @param df The \code{data.frame} output from \code{\link{bb_read}}.  Should have
#' a \code{too_many_days} attribute
#' @param start_hour Hour to check if the data has started on this hour
#' @param end_hour Hour to check if the data has ended on this hour
#'
#' @importFrom lubridate hour
#' @return A filtered \code{data.frame}
#' @export
test_start_stop = function(df, start_hour = 10, end_hour = 10){
  too_many = attributes(df)$too_many_days
  have_data = acceleration = NULL
  rm(list = c("have_data", "acceleration"))

  cn = colnames(df)
  # handle cases where it stays off or on
  if (too_many) {
    df = df %>% mutate(
      have_data = !is.na(acceleration),
      cs = cumsum(have_data))
    test = df$cs > 0
    if (any(test)) {
      index = min(which(test))
      min_val = df$date[index]
      if (hour(min_val) == start_hour) {
        df = df %>%
          filter(date >= min_val)
      } else {
        msg = paste0(
          "The beginning seems too long but ",
          "doesn't start at ", start_hour, "!")
        stop(msg)
      }
    }

    # reverse to get from the end of the reading
    df = df %>% mutate(
      cs = rev(cumsum(rev(have_data)))
    )
    test = df$cs == 0
    if (any(test)) {
      # subtract 1 for last konwn value
      index = min(which(test) - 1)
      min_val = df$date[index]
      if (hour(min_val) == end_hour) {
        df = df %>%
          filter(date <= min_val)
      } else {
        msg = paste0(
          "The ending seems too long but ",
          "doesn't end at ", end_hour, "!")
        stop(msg)
      }
    }
  }
  # remove any added columns
  df = df[, cn]
  return(df)

}
