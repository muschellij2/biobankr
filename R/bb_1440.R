
#' Summarize Biobank Acceleration File
#'
#' @param df A \code{data.frame} or \code{tbl} from \code{\link{bb_read}}
#' @param unit unit of summarization, passed to
#' \code{\link[lubridate]{floor_date}}
#' @param summarize_func Function to use for summarization,
#' passed to \code{\link{summarize}}
#' @param keep_imputed Should the imputed values be kept in this data set
#' before summarization
#' @param ... Additional arguments to pass to \code{summarize_func}
#'
#' @return A summarized \code{tbl}
#' @export
#'
#' @examples
#' @importFrom tidyr spread
bb_1440 = function(
  df,
  ...) {

  df = bb_summarize_minute(df, ...)
  df = df %>%
    mutate(
      minute = lubridate::hour(date) * 60 + lubridate::minute(date),
      date = floor_date(date, unit = "day")) %>%
    spread(key = minute, value = acceleration)
  return(df)
}
