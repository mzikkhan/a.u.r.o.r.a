library(testthat)


test_that("gdelt_timeline_daily returns a spine with requested out_col on failure", {
  out <- gdelt_timeline_daily(
    query_text = "nonsense_query_that_returns_nothing_hopefully",
    out_col = "my_score",
    start = "2026-01-01",
    end   = "2026-01-03"
  )
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3)
  expect_true(all(c("date", "my_score") %in% names(out)))
  
  # it may return NA if empty/error
  expect_true(is.numeric(out$my_score) || all(is.na(out$my_score)))
})

test_that("gdelt_timeline_daily works with real API (integration)", {
  skip_if_not(Sys.getenv("RUN_INTEGRATION") == "true")
  
  out <- gdelt_timeline_daily(
    query_text = "protest sourcecountry:CA",
    out_col = "political_unrest_score",
    start = "2026-01-01",
    end   = "2026-01-03"
  )
  
  expect_true(is.data.frame(out))
  expect_equal(nrow(out), 3)
  expect_true(all(c("date", "political_unrest_score") %in% names(out)))
})
