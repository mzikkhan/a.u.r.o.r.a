library(testthat)

test_that("revenue_merge errors if DATE column missing", {
  macro <- data.frame(date = as.Date("2026-01-01"))
  tf <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(Date="2026-01-01", Revenue=100), tf, row.names = FALSE)
  
  expect_error(
    revenue_merge(macro, tf),
    "Input dataframe must contain 'Date' column|Input dataframe must contain 'DATE' column"
  )
})

test_that("revenue_merge errors if CSV Date column missing", {
  macro <- data.frame(DATE = as.Date("2026-01-01"))
  tf <- tempfile(fileext = ".csv")
  utils::write.csv(data.frame(NotDate="2026-01-01", Revenue=100), tf, row.names = FALSE)
  
  expect_error(
    revenue_merge(macro, tf),
    "CSV file must contain 'Date' column"
  )
})

test_that("revenue_merge performs inner join on Date", {
  macro <- data.frame(
    DATE = as.Date(c("2026-01-01","2026-01-02")),
    avg_temp = c(1,2)
  )
  
  tf <- tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(Date = c("2026-01-01","2026-01-03"), Revenue = c(100, 300)),
    tf,
    row.names = FALSE
  )
  
  out <- revenue_merge(macro, tf)
  
  expect_true(is.data.frame(out))
  expect_true("Date" %in% names(out))
  expect_equal(nrow(out), 1)
  expect_equal(out$Date, as.Date("2026-01-01"))
})
