
# load packages and define global variables

library(tidyverse)
library(fredr)
library(zoo)

lat <- 51.5074 # location lattitude
lon <- -0.1278 # location longitude
start <- "2026-01-01" # start date
end   <- "2026-01-25" # end date

## ------------------------------------------  WEATHER API  ---------------------------------------------------

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

weather_daily

## ----------------------------------------  ECONOMIC API  --------------------------------------------------

# Set API key

fred_api_key <- '1f52a3bcb927a3a2423a9a2e78bd1261'
fredr_set_key(fred_api_key)

# 1. Define the parameters and their frequencies
indicators <- c(
  "Interest_Rate"     = "DFF",          # Daily
  "Mortgage_Rate"     = "MORTGAGE30US", # Weekly
  "Unemployment"      = "UNRATE",       # Monthly
  "CPI_Inflation"     = "CPIAUCSL",     # Monthly
  "GDP"               = "GDPC1",        # Quarterly
  "Public_Debt"       = "GFDEBTN"       # Quarterly
)

# 2. Fetch all data at once 
raw_data <- map_dfr(indicators, 
                    ~fredr(series_id = .x, observation_start = as.Date("2020-01-01")), 
                    .id = "metric")

# 3. Pivot to Wide Format 
daily_trend <- raw_data %>%
  select(date, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(date)

# 4. The "Fill" Step (LOCF)
full_dates <- data.frame(date = seq(min(daily_trend$date), max(daily_trend$date), by="day"))

# 5. Keep only daily records
econ_daily_fixed <- full_dates %>%
  left_join(daily_trend, by = "date") %>%
  mutate(across(-date, ~na.locf(., na.rm = FALSE)))

# View the result
head(econ_daily_fixed)
tail(econ_daily_fixed)

# ---------------------------------------------- NEWS API ----------------------------------------------------

# Helper function to call GDELT API
gdelt_timeline_daily <- function(query_text, out_col, start, end) {
  
  # clean the dates format
  startdt <- paste0(gsub("-", "", start), "000000")
  enddt   <- paste0(gsub("-", "", end),   "235959")
  
  # call API
  url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
                "query=", URLencode(query_text),
                "&mode=TimelineTone",
                "&format=CSV",
                "&startdatetime=", startdt,
                "&enddatetime=", enddt)
  
  # retrieve data
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
  
  # search for valid dates and values
  if (is.na(date_col) || is.na(val_col)) {
    spine[[out_col]] <- NA_real_
    return(spine)
  }
  
  # clean data
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

# Get political unrest data
political_unrest_daily <- gdelt_timeline_daily(
  query_text = '(protest OR unrest) sourcecountry:Denmark',
  out_col = "political_unrest_score",
  start = start,
  end = end
)
political_unrest_daily

# Get natural disaster data
natural_disaster_daily <- gdelt_timeline_daily(
  query_text = '(theme:NATURAL_DISASTER OR "disaster" OR "storm" OR "flood") sourcecountry:DA',
  out_col = "natural_disaster_score",
  start = start,
  end = end
)
natural_disaster_daily

# ---------------------------------------------- MERGE -------------------------------------------------------

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

