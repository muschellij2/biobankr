
#' Summarize Biobank Acceleration File into 1440 format
#'
#' @param df A \code{data.frame} or \code{tbl} from \code{\link{bb_read}}
#' @param summarize_day_func function to summarize days over
#' @param summarize_over_day Should the data be summarized over days?
#' @param na.rm Should NAs be removed?
#' @param keep_imputed Should the imputed values be kept in this data set
#' or counted before summarization
#' @param ... Additional arguments to pass to \code{\link{bb_summarize}}
#'
#' @return A table of the day (if \code{summarize_over_day = FALSE})
#' and 1440 columns
#' @export
#'
#' @importFrom tidyr spread
#' @importFrom dplyr select
bb_1440 = function(
  df,
  summarize_over_day = FALSE,
  summarize_day_func = "mean",
  na.rm = TRUE,
  keep_imputed = FALSE,
  ...) {

  minute = acceleration = NULL
  rm(list = c("acceleration", "minute"))

  df = bb_summarize_minute(df, ...)
  df = df %>%
    dplyr::select(date, acceleration) %>%
    date_day_min

  func = function(x, na.rm = TRUE) {
    do.call(summarize_day_func, list(x, na.rm = na.rm))
  }

  if (summarize_over_day) {
    df = df %>%
      ungroup %>%
      group_by(minute) %>%
      summarize(acceleration = func(acceleration, na.rm = na.rm)) %>%
      ungroup
  }
  df = df %>%
    spread(key = minute, value = acceleration)

  return(df)
}

#' @export
#' @rdname bb_1440
#' @note \code{\link{bb_1440_count}} counts the number of non-imputed
#' measures
bb_1440_count = function(
  df,
  summarize_over_day = FALSE,
  keep_imputed = TRUE,
  ...) {

  x = minute = imputed = n = NULL
  rm(list = c("imputed", "minute", "n", "x"))

  df = bb_summarize_minute(df, ...)
  df = df %>%
    dplyr::select(date, imputed, n) %>%
    date_day_min

  if (keep_imputed) {
    df$x = df$n
  } else {
    df$x = df$imputed
  }

  if (summarize_over_day) {
    df = df %>%
      ungroup %>%
      group_by(minute) %>%
      summarize(x = sum(x)) %>%
      ungroup
  }

  df = df %>%
    spread(key = minute, value = x)
  df[ is.na(df)] = 0

  return(df)
}
