
# load packages and define global variables

library(tidyverse)
library(fredr)
library(zoo)

lat <- 51.5074 # location lattitude
lon <- -0.1278 # location longitude
start <- "2026-01-01" # start date
end   <- "2026-01-25" # end date

## ------------------------------------------  WEATHER API  ---------------------------------------------------

# 1. CALL API
url <- paste0("https://api.open-meteo.com/v1/forecast?",
              "latitude=", lat, "&longitude=", lon,
              "&start_date=", start, "&end_date=", end,
              "&hourly=temperature_2m,precipitation",
              "&timezone=auto",                        
              "&format=csv")

# 2. Read and Clean Data
weather_data <- read.csv(url, skip = 10, header = FALSE)
colnames(weather_data) <- c("datetime", "temp_c", "precip_mm")
weather_data$datetime <- as.POSIXct(weather_data$datetime, format="%Y-%m-%dT%H:%M")
weather_data$date <- as.Date(weather_data$datetime)

# 4. Calculate stats
daily_summary <- aggregate(cbind(temp_c, precip_mm) ~ date, 
                           data = weather_data, 
                           FUN = function(x) c(mean = mean(x), max = max(x), sum = sum(x)))

# 5. Clean up the messy matrix output from aggregate
daily_summary <- data.frame(
  date      = daily_summary$date,
  avg_temp  = round(daily_summary$temp_c[, "mean"], 1),
  max_temp  = daily_summary$temp_c[, "max"],
  tot_rain  = daily_summary$precip_mm[, "sum"]
)

print(daily_summary)

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

final_data <- full_dates %>%
  left_join(daily_trend, by = "date") %>%
  mutate(across(-date, ~na.locf(., na.rm = FALSE)))

# View the result
head(final_data)
tail(final_data)

## --------------------------------------------  NEWS API  ---------------------------------------------------

# -------------------------------------------  POLITICAL UNREST ----------------------------------------------

# 1. Define QUERY
query_text <- '(protest OR unrest) sourcecountry:Canada'

# 2. Call API
url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
              "query=", URLencode(query_text),
              "&mode=TimelineTone",
              "&timespan=3m",
              "&format=CSV")

# 3. Read the data 
raw_data <- read.csv(url, check.names = FALSE)

raw_data

# -----------------------------------------  NATURAL DISASTERS -----------------------------------------------

# 1. Define QUERY
query_text <- '(theme:NATURAL_DISASTER OR "disaster" OR "storm" OR "flood") sourcecountry:CA'

# 2. Call API
url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
              "query=", URLencode(query_text),
              "&mode=TimelineTone",
              "&timespan=3m",
              "&format=CSV")

# 3. Read the data 
raw_data <- try(read.csv(url, check.names = FALSE), silent = TRUE)

raw_data































