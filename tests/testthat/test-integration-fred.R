test_that("INTEGRATION: FRED returns expected economic daily schema", {
  testthat::skip_if(Sys.getenv("RUN_INTEGRATION") != "true")
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  # Optional: require key set as env var
  # If you want this test to run only when a key exists:
  testthat::skip_if(Sys.getenv("FRED_API_KEY") == "")
  
  start <- "2026-01-01"
  end   <- "2026-01-05"
  
  # Your function takes lat/lon but does not use them for FRED
  out <- get_daily_economic_data(start, end, lat = 49.2827, lon = -123.1207)
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(nrow(out) == 5)
  
  testthat::expect_true("date" %in% names(out))
  expected_cols <- c(
    "interest_rate", "mortgage_rate", "unemployment",
    "cpi_inflation", "gdp", "public_debt"
  )
  testthat::expect_true(all(expected_cols %in% names(out)))
  testthat::expect_true(inherits(out$date, "Date"))
  
  # Values can be NA if series missing, but types should be numeric-ish
  for (nm in expected_cols) {
    testthat::expect_true(is.numeric(out[[nm]]) || all(is.na(out[[nm]])))
  }
})
