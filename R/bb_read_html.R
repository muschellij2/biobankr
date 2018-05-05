
#' Read Biobank Document HTML
#'
#' @param file Name of HTML file
#'
#' @return A list of `data.frames` of the tables
#' @export
#' @importFrom tidyr separate
#' @importFrom xml2 read_html
#' @importFrom rvest html_nodes html_table html_attr
bb_read_html = function(file) {

  varname = NULL
  rm(list = "varname")

  xx = readLines(file, warn = FALSE)
  bad_string = "NEWLINE"
  xx = gsub("<br>", bad_string, xx)
  tfile = tempfile(fileext = ".html")
  writeLines(xx, tfile)
  doc = read_html(tfile)

  tabs = html_nodes(doc,
                    xpath = "//table")

  all_tabs = lapply(tabs, function(tab) {
    xdf = html_table(tab, trim = FALSE)
  })

  summaries = sapply(tabs, html_attr, "summary")
  names(all_tabs) = summaries
  the_tab = sapply(all_tabs, function(x) {
    cn = colnames(x)
    any(grepl("Column", cn) )
  })
  if (sum(the_tab) != 1) {
    warning("Main table not found")
    return(all_tabs)
  }
  main_tab = is.na(summaries) & the_tab
  names(all_tabs)[the_tab] = "main_table"
  is_date_extracted = is.na(names(all_tabs))
  if (any(is_date_extracted)) {
    meta_col = sapply(all_tabs[is_date_extracted], function(x) {
      any(grepl("Date Extracted", x) )
    })
    names(all_tabs)[is_date_extracted][meta_col] = "meta_information"
  }
  xdf = all_tabs$main_table

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

  all_tabs$main_table = df


  return(all_tabs)
}


#' @export
#' @rdname bb_read_html
bb_read_main_table = function(file) {
  tab = bb_read_html(file)
  return(tab$main_table)

}
