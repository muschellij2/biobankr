
#' Read Biobank Acceleration File
#'
#' @param file path to filename
#'
#' @return A \code{tibble}
#' @export
#'
# #' @examples
bb_read = function(file) {
  file = readr::ead_csv(file = file)
}
