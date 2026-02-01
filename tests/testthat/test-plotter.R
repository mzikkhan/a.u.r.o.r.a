library(testthat)

test_that("plotter errors when required columns are missing", {
  df <- data.frame(x = 1:3)
  expect_error(plotter(df, cols = c("avg_temp")), "Missing date column|Missing revenue column")
})

test_that("plotter returns a patchwork/ggplot object", {
  df <- data.frame(
    Date = as.Date("2026-01-01") + 0:4,
    Revenue = c(10, 11, 9, 13, 12),
    avg_temp = c(1, 2, 3, 4, 5),
    gdp = c(100, 101, 103, 102, 104)
  )
  
  p <- plotter(df, cols = c("avg_temp", "gdp"), drop_na = TRUE)
  
  # patchwork returns a "patchwork" object; sometimes it also inherits gg/ggplot
  expect_true(inherits(p, "patchwork") || inherits(p, "gg") || inherits(p, "ggplot"))
})
