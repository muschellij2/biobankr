
#' Read Biobank Document HTML
#'
#' @param file Name of HTML file
#'
#' @return A `data.frame` of the main table
#' @export
#' @importFrom tidyr separate
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table
bb_read_html = function(file) {

  varname = NULL
  rm(list = "varname")

  xx = readLines(file)
  bad_string = "NEWLINE"
  xx = gsub("<br>", bad_string, xx)
  tfile = tempfile(fileext = ".html")
  writeLines(xx, tfile)
  doc = read_html(tfile)

  tabs = html_nodes(doc,
                    xpath = "//table")
  tab = tabs[[2]]

  xdf = html_table(tab, trim = FALSE)

  ##############################
  # Make the coding column
  ##############################
  df = xdf
  df$Description = gsub(
    bad_string, " ", df$Description)
  df$coding = NA
  have_coding = grepl("coding", df$Description)
  df$coding[ have_coding ] = gsub(
    "(.*) (Uses data-coding.*)",
    "\\2", df$Description[have_coding])
  df$Description[ have_coding ] = gsub(
    "(.*) (Uses data-coding.*)",
    "\\1", df$Description[ have_coding ])
  df$coding = gsub(
    ".*data-coding (.*).*",
    "\\1", df$coding)
  df$coding = gsub(
    "(.*) comprises.*",
    "\\1", df$coding)


  ##############################
  # make variable names to
  # match pheno.tab
  ##############################
  df$varname = gsub("-", ".", df$UDI)

  df = df %>%
    separate(varname,
             into = c("var_id", "visit", "level"),
             remove = FALSE)

  df$varname = paste0("f.", df$varname)

  return(df)
}
