library(testthat)

#' Merge weather, economic, and GDELT features into one macro dataset
#'
#' @param start_date Character. Start date in "YYYY-MM-DD".
#' @param end_date Character. End date in "YYYY-MM-DD".
#' @param latitude Numeric. Latitude.
#' @param longitude Numeric. Longitude.
#' @param weather_fun Function. Weather fetcher (default: get_daily_weather).
#' @param econ_fun Function. Economic fetcher (default: get_daily_economic_data).
#' @param gdelt_fun Function. GDELT fetcher (default: gdelt_timeline_daily).
#'
#' @return A data.frame with DATE and merged feature columns.
#' @export

test_that("get_macroeconomic_data merges sources correctly (UNIT, no API calls)", {
  
  fake_weather <- function(start_date, end_date, latitude, longitude) {
    data.frame(
      date = seq(as.Date(start_date), as.Date(end_date), by = "day"),
      avg_temp = 1,
      max_temp = 2,
      tot_rain = 3
    )
  }
  
  fake_econ <- function(start, end, lat, lon) {
    data.frame(
      date = seq(as.Date(start), as.Date(end), by = "day"),
      interest_rate = 0.1,
      mortgage_rate = 0.2,
      unemployment  = 0.3,
      cpi_inflation = 0.4,
      gdp           = 0.5,
      public_debt   = 0.6
    )
  }
  
  fake_gdelt <- function(query_text, out_col, start, end) {
    df <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
    df[[out_col]] <- 9
    df
  }
  
  out <- get_macroeconomic_data(
    start_date = "2026-01-01",
    end_date   = "2026-01-03",
    latitude   = 49.2827,
    longitude  = -123.1207,
    weather_fun = fake_weather,
    econ_fun    = fake_econ,
    gdelt_fun   = fake_gdelt
  )
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3)
  
  expected_cols <- c(
    "DATE",
    "avg_temp","max_temp","tot_rain",
    "natural_disaster_score","political_unrest_score",
    "interest_rate","mortgage_rate","unemployment","cpi_inflation","gdp","public_debt"
  )
  expect_true(all(expected_cols %in% names(out)))
  expect_s3_class(out$DATE, "Date")
})

test_that("get_macroeconomic_data works with real APIs (integration)", {
  skip_if_not(Sys.getenv("RUN_INTEGRATION") == "true")
  
  out <- get_macroeconomic_data(
    start_date = "2026-01-01",
    end_date   = "2026-01-03",
    latitude   = 49.2827,
    longitude  = -123.1207
  )
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3)
  expect_true("DATE" %in% names(out))
})
