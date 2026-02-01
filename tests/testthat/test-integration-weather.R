test_that("INTEGRATION: Open-Meteo weather API returns expected daily schema", {
  # Run only when user explicitly enables integration tests
  testthat::skip_if(Sys.getenv("RUN_INTEGRATION") != "true")
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  start <- "2026-01-01"
  end   <- "2026-01-03"
  
  # Vancouver
  out <- get_daily_weather(start, end, lat = 49.2827, lon = -123.1207)
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(nrow(out) == 3)
  
  testthat::expect_true("date" %in% names(out))
  testthat::expect_true(all(c("avg_temp", "max_temp", "tot_rain") %in% names(out)))
  
  testthat::expect_true(inherits(out$date, "Date"))
  testthat::expect_true(is.numeric(out$avg_temp) || all(is.na(out$avg_temp)))
  testthat::expect_true(is.numeric(out$max_temp) || all(is.na(out$max_temp)))
  testthat::expect_true(is.numeric(out$tot_rain) || all(is.na(out$tot_rain)))
})
