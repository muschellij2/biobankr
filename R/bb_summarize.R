
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
#' file = system.file("test2.csv", package = "biobankr")
#' df = bb_read(file)
#' collapsed = bb_summarize(df, unit = "10 seconds")
#' collapsed = bb_summarize(df, unit = "10 seconds", summarize_func = "median")
#' collapsed = bb_summarize(df, unit = "15 seconds",
#' summarize_func = "quantile", probs = 0.25)
#' @importFrom dplyr mutate group_by "%>%" summarize summarise ungroup n
#' @importFrom dplyr filter
#' @importFrom lubridate floor_date
bb_summarize = function(
  df,
  unit = "1 minute",
  summarize_func = "mean",
  keep_imputed = TRUE,
  ...) {
  imputed = acceleration = not_na = NULL
  rm(list = c("acceleration", "imputed", "not_na"))

  func = function(x, ...) {
    do.call(summarize_func, list(x, ...))
  }
  if (!keep_imputed) {
    df = df %>% dplyr::filter(imputed == 0)
  }
  df = df %>%
    mutate(date = floor_date(date, unit = unit),
           not_na = !is.na(acceleration)) %>%
    group_by(date) %>%
    summarize(acceleration = func(acceleration, ...),
              imputed = sum(imputed == 1 & not_na),
              n = sum(not_na))
  df = ungroup(df)
  attr(df, "summarize_func") = summarize_func
  attr(df, "time_unit") = unit
  return(df)
}

#' @rdname bb_summarize
#' @export
bb_summarise = bb_summarize


#' @rdname bb_summarize
#' @export
bb_summarize_minute = function(..., unit = "1 minute") {
  df = bb_summarize(..., unit = unit)
  return(df)
}
