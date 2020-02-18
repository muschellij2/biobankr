testthat::context("Trying CWA File")

zipfile = tempfile(fileext = ".zip")
download.file("http://biobank.ndph.ox.ac.uk/showcase/showcase/examples/accsamp.zip",
              destfile = zipfile)
fname = unzip(zipfile, files = "accsamp.cwa",
              exdir = tempdir())


testthat::test_that("Header information", {

  testthat::expect_warning({
    res = read_cwa(file = fname, end = 1000, configtz = "")
  })
  options(digits.secs = 2)
  testthat::expect_silent({
    res = read_cwa(file = fname, end = 1000,
                   configtz = "", verbose = FALSE)
  })

  testthat::expect_equal(names(res), c("header", "data"))

  testthat::expect_equal({
    colnames(res$data)
    c("time", "x", "y", "z", "temp", "battery", "light")
  })




})
