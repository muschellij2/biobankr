#' Extract information from header
#'
#' @param info Header information from csv header
#'
#' @return Sampling rates or dates
#' @export
#'
#' @rdname info_selector
#' @examples
#' file = system.file("test2.csv", package = "biobankr")
#'   df = readr::read_csv(file = file)
#'  info = colnames(df)[1]
#'  info = trimws(info)
#'  srate = extract_sampling_rate(info)
#' date_range = extract_date(info)
extract_sampling_rate = function(info){
  srate = gsub(".*sampleRate =(.*)", "\\1", info)
  srate = trimws(srate)
  srate = gsub("seconds", "", srate)
  srate = trimws(srate)
  return(srate)
}

#' @rdname info_selector
#' @export
extract_date = function(info) {

  info = gsub("(.*)sampleRate =.*", "\\1", info)
  info = tolower(info)
  info = trimws(info)
  info = gsub("acceleration (mg)", "", info, fixed = TRUE)
  info = trimws(info)

  info = gsub("-$", "", info)
  info = gsub("^-", "", info)
  info = gsub(" - ", " , ", info)
  info = trimws(info)
  info = strsplit(info, split = ",")[[1]]
  info = trimws(info)
  return(info)
}
