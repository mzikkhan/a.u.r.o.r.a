# ========================= PACKAGES =========================
library(tidyverse)
library(fredr)
library(zoo)

# ========================= INPUTS =========================
lat <- 51.5074
lon <- -0.1278
start <- "2026-01-01"
end   <- "2026-01-15"

# ========================= WEATHER (Open-Meteo) =========================
url <- paste0("https://api.open-meteo.com/v1/forecast?",
              "latitude=", lat, "&longitude=", lon,
              "&start_date=", start, "&end_date=", end,
              "&hourly=temperature_2m,precipitation",
              "&timezone=auto",
              "&format=csv")

weather_data <- read.csv(url, skip = 10, header = FALSE)
colnames(weather_data) <- c("datetime", "temp_c", "precip_mm")

weather_data$datetime <- as.POSIXct(weather_data$datetime, format="%Y-%m-%dT%H:%M")
weather_data$date     <- as.Date(weather_data$datetime)

daily_summary <- aggregate(cbind(temp_c, precip_mm) ~ date,
                           data = weather_data,
                           FUN = function(x) c(mean = mean(x), max = max(x), sum = sum(x)))

weather_daily <- data.frame(
  date      = daily_summary$date,
  avg_temp  = round(daily_summary$temp_c[, "mean"], 1),
  max_temp  = daily_summary$temp_c[, "max"],
  tot_rain  = daily_summary$precip_mm[, "sum"]
)

# ========================= ECON (FRED) =========================
fred_api_key <- "1f52a3bcb927a3a2423a9a2e78bd1261"
fredr_set_key(fred_api_key)

indicators <- c(
  "Interest_Rate"     = "DFF",
  "Mortgage_Rate"     = "MORTGAGE30US",
  "Unemployment"      = "UNRATE",
  "CPI_Inflation"     = "CPIAUCSL",
  "GDP"               = "GDPC1",
  "Public_Debt"       = "GFDEBTN"
)

raw_econ <- map_dfr(
  indicators,
  ~ fredr(series_id = .x,
          observation_start = as.Date(start),
          observation_end   = as.Date(end)),
  .id = "metric"
)

daily_trend <- raw_econ %>%
  select(date, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(date)

full_dates <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))

econ_daily <- full_dates %>%
  left_join(daily_trend, by = "date") %>%
  mutate(across(-date, ~ na.locf(.x, na.rm = FALSE)))

# Rename to your final wanted names
econ_daily_fixed <- final_data %>%
  rename(
    interest_rate = Interest_Rate,
    mortgage_rate = Mortgage_Rate,
    unemployment  = Unemployment,
    inflation     = CPI_Inflation,
    gdp           = GDP,
    public_debt   = Public_Debt
  )

names(final_data)

# ========================= NEWS (GDELT) -> daily tables =========================

gdelt_timeline_daily <- function(query_text, out_col, start, end) {
  
  startdt <- paste0(gsub("-", "", start), "000000")
  enddt   <- paste0(gsub("-", "", end),   "235959")
  
  url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
                "query=", URLencode(query_text),
                "&mode=TimelineTone",
                "&format=CSV",
                "&startdatetime=", startdt,
                "&enddatetime=", enddt)
  
  raw <- try(read.csv(url, check.names = FALSE), silent = TRUE)
  
  # if no data or error -> return spine with NA column
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  
  if (inherits(raw, "try-error") || is.null(raw) || nrow(raw) == 0) {
    spine[[out_col]] <- NA_real_
    return(spine)
  }
  
  # try to detect the date + value cols
  cn <- names(raw)
  date_col <- cn[grepl("date", tolower(cn))][1]
  val_col  <- cn[grepl("value|tone|count", tolower(cn))][1]
  
  if (is.na(date_col) || is.na(val_col)) {
    spine[[out_col]] <- NA_real_
    return(spine)
  }
  
  daily <- raw %>%
    transmute(
      date = as.Date(.data[[date_col]]),
      score = as.numeric(.data[[val_col]])
    ) %>%
    group_by(date) %>%
    summarise(score = mean(score, na.rm = TRUE), .groups = "drop") %>%
    rename(!!out_col := score)
  
  spine %>% left_join(daily, by = "date")
}

political_unrest_daily <- gdelt_timeline_daily(
  query_text = '(protest OR unrest) sourcecountry:Denmark',
  out_col = "political_unrest_score",
  start = start,
  end = end
)

natural_disaster_daily <- gdelt_timeline_daily(
  query_text = '(theme:NATURAL_DISASTER OR "disaster" OR "storm" OR "flood") sourcecountry:DA',
  out_col = "natural_disaster_score",
  start = start,
  end = end
)

# ========================= MERGE ALL =========================
spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))

merged_df <- spine %>%
  left_join(weather_daily,          by = "date") %>%
  left_join(econ_daily_fixed,       by = "date") %>%
  left_join(political_unrest_daily, by = "date") %>%
  left_join(natural_disaster_daily, by = "date") %>%
  select(
    DATE = date,
    avg_temp, max_temp, tot_rain,
    natural_disaster_score, political_unrest_score,
    interest_rate, mortgage_rate, unemployment, inflation, gdp, public_debt
  )

merged_df

