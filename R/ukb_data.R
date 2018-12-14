#' Extract UKB Data
#'
#' @param fileset The prefix for a UKB fileset, e.g., \code{ukbxxxx}
#' @param path The path to the directory containing your UKB fileset.
#' The default value is the current directory.
#'
#' @note This function is a light wrapper of \code{\link{ukb_df}} and
#' \code{\link{ukb_df_field}} but does things using a temporary
#' directory.
#'
#' @return A list of the data and a lookup table
#' @export
#' @importFrom ukbtools ukb_df ukb_df_field
ukb_data = function(fileset, path = ".") {
  html_file <- paste0(fileset, ".html")
  r_file <- paste0(fileset, ".r")
  tab_file <- paste0(fileset, ".tab")

  all_files = list(html_file = html_file,
                   r_file = r_file,
                   tab_file = tab_file)
  all_files = lapply(all_files, function(x) {
    x = file.path(path, x)
    x = path.expand(x)
    x = normalizePath(x, winslash = "/", mustWork = FALSE)
    x
  })
  out_dir = tempfile()
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  res = sapply(all_files, function(x) {
    if (file.exists(x)) {
      file.copy(x, to = out_dir, copy.mode = FALSE, overwrite = FALSE)
    } else {
      return(FALSE)
    }
  })

  df = ukbtools::ukb_df(fileset = fileset, path = out_dir)
  lookup = ukbtools::ukb_df_field(fileset = fileset, path = out_dir)

  L = list(data = df,
           lookup = lookup)
  return(L)
}
