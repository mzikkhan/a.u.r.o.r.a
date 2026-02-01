library(testthat)

test_that("get_daily_weather validates inputs", {
  expect_error(
    get_daily_weather(start = NULL, end = "2026-01-02", lat = 49.28, lon = -123.12),
    "Provide start, end, lat, lon"
  )
  
  expect_error(
    get_daily_weather(start = "2026-01-01", end = NULL, lat = 49.28, lon = -123.12),
    "Provide start, end, lat, lon"
  )
  
  expect_error(
    get_daily_weather(start = "2026-01-01", end = "2026-01-02", lat = NULL, lon = -123.12),
    "Provide start, end, lat, lon"
  )
  
  # lon check uses missing(lon), so omit it entirely
  expect_error(
    get_daily_weather(start = "2026-01-01", end = "2026-01-02", lat = 49.28),
    "Provide start, end, lat, lon"
  )
})

test_that("get_daily_weather returns a spine data.frame even on failure", {
  out <- get_daily_weather(
    start = "2026-01-01",
    end   = "2026-01-03",
    lat   = 9999,
    lon   = 9999
  )
  
  expect_true(is.data.frame(out))
  expect_true(all(c("date", "avg_temp", "max_temp", "tot_rain") %in% names(out)))
  expect_equal(nrow(out), 3)
  expect_equal(out$date, as.Date(c("2026-01-01", "2026-01-02", "2026-01-03")))
  
  # on failure your handler fills NAs
  expect_true(all(is.na(out$avg_temp)))
  expect_true(all(is.na(out$max_temp)))
  expect_true(all(is.na(out$tot_rain)))
})

test_that("get_daily_weather works with real API (integration)", {
  skip_if_not(Sys.getenv("RUN_INTEGRATION") == "true")
  
  out <- get_daily_weather(
    start = "2026-01-01",
    end   = "2026-01-03",
    lat   = 49.2827,
    lon   = -123.1207
  )
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3)
  expect_true(all(c("date", "avg_temp", "max_temp", "tot_rain") %in% names(out)))
})