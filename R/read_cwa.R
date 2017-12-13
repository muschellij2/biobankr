
#' Read CWA from Axtivity
#'
#' @param file filename of cwa file
#' @param end End point for reading data.  Default is all the data
#' @param verbose print diagnostic messages
#'
#' @return A list with header information and a tbl
#' @export
#'
#' @importFrom dplyr as_data_frame
#' @importFrom GGIR g.cwaread
read_cwa = function(file, end = Inf, verbose = TRUE) {
  res = GGIR::g.cwaread(
    fileName = file, start = 0, end = end, progressBar = verbose)
  res$data = dplyr::as_data_frame(res$data)
  return(res)
}
