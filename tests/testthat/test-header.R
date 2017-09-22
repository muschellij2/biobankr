test_that("Header information", {
  file = system.file("test2.csv", package = "biobankr")
  df = readr::read_csv(file = file)
  info = colnames(df)[1]
  info = trimws(info)
  srate = extract_sampling_rate(info)
  date_range = extract_date(info)


  expect_equal(as.numeric(srate), 5)

  if (is.character(date_range)) {
    date_range = lubridate::ymd_hms(date_range)
  }

  expect_equal(
    date_range[1],
    structure(1423648800,
              class = c("POSIXct", "POSIXt"), tzone = "UTC")
  )


})
