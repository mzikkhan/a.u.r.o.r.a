test_that("Integration: get_daily_weather fetches data or returns valid NA spine", {
  skip_on_cran() # Skip on CRAN to avoid network issues flagging check failures
  
  # Use a recent date range
  start <- "2024-01-01"
  end <- "2024-01-05"
  
  res <- get_daily_weather(start, end, 51.5074, -0.1278)
  
  expect_s3_class(res, "data.frame")
  expect_true("date" %in% names(res))
  expect_equal(nrow(res), 5) # 5 days inclusive
  
  # Check if we got data or our NA spine (depending on if API is temporarily down)
  # Either way, format should be consistent.
  expect_true(all(c("avg_temp", "max_temp", "tot_rain") %in% names(res)))
})

test_that("Integration: get_macroeconomic_data returns merged dataframe", {
  skip_on_cran()
  
  start <- "2024-01-01"
  end <- "2024-01-03" # Short range for speed
  
  res <- get_macroeconomic_data(start, end, 51.5074, -0.1278)
  
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 3)
  
  expected_cols <- c("DATE", "avg_temp", "interest_rate", "political_unrest_score")
  expect_true(all(expected_cols %in% names(res)))
})
