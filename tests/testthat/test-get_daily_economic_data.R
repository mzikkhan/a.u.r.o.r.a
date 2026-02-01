library(testthat)

test_that("get_daily_economic_data validates inputs", {
  expect_error(
    get_daily_economic_data(start = NULL, end = "2026-01-02", lat = 49.28, lon = -123.12),
    "Please provide start, end, lat and lon"
  )
  
  expect_error(
    get_daily_economic_data(start = "2026-01-01", end = NULL, lat = 49.28, lon = -123.12),
    "Please provide start, end, lat and lon"
  )
  
  expect_error(
    get_daily_economic_data(start = "2026-01-01", end = "2026-01-02", lat = NULL, lon = -123.12),
    "Please provide start, end, lat and lon"
  )
  
  expect_error(
    get_daily_economic_data(start = "2026-01-01", end = "2026-01-02", lat = 49.28),
    "Please provide start, end, lat and lon"
  )
})

test_that("get_daily_economic_data returns spine with expected columns on failure", {
  # If FRED key is missing or network fails, your function returns a spine + NA cols
  out <- get_daily_economic_data(
    start = "2026-01-01",
    end   = "2026-01-03",
    lat   = 49.28,
    lon   = -123.12
  )
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3)
  expect_true("date" %in% names(out))
  
  expected_cols <- c("interest_rate", "mortgage_rate", "unemployment",
                     "cpi_inflation", "gdp", "public_debt")
  expect_true(all(expected_cols %in% names(out)))
  
  # values can be numeric or NA (depends on whether API works)
  for (cc in expected_cols) {
    expect_true(is.numeric(out[[cc]]) || all(is.na(out[[cc]])))
  }
})

test_that("get_daily_economic_data works with real API (integration)", {
  skip_if_not(Sys.getenv("RUN_INTEGRATION") == "true")
  skip_if_not(Sys.getenv("FRED_API_KEY") != "")
  
  out <- get_daily_economic_data(
    start = "2026-01-01",
    end   = "2026-01-10",
    lat   = 49.28,
    lon   = -123.12
  )
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 10)
  expect_true("date" %in% names(out))
})
