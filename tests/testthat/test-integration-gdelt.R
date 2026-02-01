test_that("INTEGRATION: GDELT returns expected timeline schema", {
  testthat::skip_if(Sys.getenv("RUN_INTEGRATION") != "true")
  testthat::skip_if_offline()
  testthat::skip_on_cran()
  
  start <- "2026-01-01"
  end   <- "2026-01-03"
  
  out <- gdelt_timeline_daily(
    query_text = "(protest OR unrest) sourcecountry:CA",
    out_col = "political_unrest_score",
    start = start,
    end = end
  )
  
  testthat::expect_s3_class(out, "data.frame")
  testthat::expect_true(nrow(out) == 3)
  
  testthat::expect_true("date" %in% names(out))
  testthat::expect_true("political_unrest_score" %in% names(out))
  testthat::expect_true(inherits(out$date, "Date"))
  
  # It can be NA if no events or API returns empty, that's fine
  testthat::expect_true(is.numeric(out$political_unrest_score) || all(is.na(out$political_unrest_score)))
})
