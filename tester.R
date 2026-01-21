
#source("Ballboy.R")

#competition <- "PL"
#season <- 2023

#team_summary <- summarize_team_performance(competition, season = season)
#head(team_summary)

#a = get_competition_matches(competition, season = season)
#head(a)


# MACRO LEVEL ANALYSIS

## --------------------------------------  WEATHER API  ------------------------------------------------

# 1. Define your parameters
lat <- 51.5074
lon <- -0.1278
start <- "2026-01-01"
end   <- "2026-01-15"

# 2. Construct the URL with start/end dates
# Note: We use the 'forecast' endpoint which supports recent history + future
url <- paste0("https://api.open-meteo.com/v1/forecast?",
              "latitude=", lat,
              "&longitude=", lon,
              "&start_date=", start,
              "&end_date=", end,
              "&hourly=temperature_2m",
              "&format=csv")

# 3. Read the data
# We skip 10 rows to get past metadata and the original API header
weather_range <- read.csv(url, skip = 10, header = FALSE)
colnames(weather_range) <- c("datetime", "temp_c")

# 4. Convert strings to usable Date/Time objects
weather_range$datetime <- as.POSIXct(weather_range$datetime, format="%Y-%m-%dT%H:%M")

# View the start and end to verify
head(weather_range, 3)
tail(weather_range, 3)

# -------------------------------------Wrangling Part 1----------------------------------------------

# 1. Create a simple 'date' column (stripping the hourly time)
weather_range$date <- as.Date(weather_range$datetime)

# 2. Calculate the Daily Average
avg_temp <- aggregate(temp_c ~ date, data = weather_range, FUN = mean)
colnames(avg_temp)[2] <- "avg_temp"

# 3. Calculate the Daily Maximum
max_temp <- aggregate(temp_c ~ date, data = weather_range, FUN = max)
colnames(max_temp)[2] <- "max_temp"

# 4. Merge them into one final table
daily_summary <- merge(avg_temp, max_temp, by = "date")

# Optional: Round the average to 2 decimal places for neatness
daily_summary$avg_temp <- round(daily_summary$avg_temp, 2)

# View the result
print(daily_summary)

# ------------------------------------Wrangling Part 2------------------------------------------


# 1. Update URL to include 'precipitation' and 'timezone'
url <- paste0("https://api.open-meteo.com/v1/forecast?",
              "latitude=", lat, "&longitude=", lon,
              "&start_date=", start, "&end_date=", end,
              "&hourly=temperature_2m,precipitation", # Added precipitation
              "&timezone=auto",                        # Ensures local day alignment
              "&format=csv")

# 2. Read and Name
weather_data <- read.csv(url, skip = 10, header = FALSE)
colnames(weather_data) <- c("datetime", "temp_c", "precip_mm")

# 3. Clean Types
weather_data$datetime <- as.POSIXct(weather_data$datetime, format="%Y-%m-%dT%H:%M")
weather_data$date     <- as.Date(weather_data$datetime)

# 4. Calculate everything in one go using 'by' or 'aggregate'
# We'll use a more advanced base R function 'do.call' with 'aggregate' for speed
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

## ---------------------------------  ECONOMIC API  --------------------------------------------------

fred_api_key <- '1f52a3bcb927a3a2423a9a2e78bd1261'

library(tidyverse)
library(fredr)
library(zoo)

fredr_set_key(fred_api_key)

# 1. Define the parameters and their frequencies
# We fetch them all, then align them later.
indicators <- c(
  "Interest_Rate"     = "DFF",          # Daily
  "Mortgage_Rate"     = "MORTGAGE30US", # Weekly
  "Unemployment"      = "UNRATE",       # Monthly
  "CPI_Inflation"     = "CPIAUCSL",     # Monthly
  "GDP"               = "GDPC1",        # Quarterly
  "Public_Debt"       = "GFDEBTN"       # Quarterly
)

# 2. Fetch all data at once using map_dfr
# We start from 2020 to keep the query manageable
raw_data <- map_dfr(indicators, 
                    ~fredr(series_id = .x, observation_start = as.Date("2020-01-01")), 
                    .id = "metric")

# 3. Pivot to Wide Format (One column per metric)
daily_trend <- raw_data %>%
  select(date, metric, value) %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  arrange(date)

# 4. The "Fill" Step (LOCF)
# We create a continuous date sequence to ensure no days are missing entirely
full_dates <- data.frame(date = seq(min(daily_trend$date), max(daily_trend$date), by="day"))

final_data <- full_dates %>%
  left_join(daily_trend, by = "date") %>%
  # Apply Last Observation Carried Forward to all columns except 'date'
  # na.locf will fill NAs with the most recent non-NA value
  mutate(across(-date, ~na.locf(., na.rm = FALSE)))

# View the result
head(final_data)

tail(final_data)

## -----------------------------------  NEWS API  ---------------------------------------------------

library(tidyverse)

# --------------------------------------  WRANGLING 1. ----------------------------------------------

# 1. Define your query
query_text <- '(protest OR unrest) sourcecountry:Denmark'

# 2. Build the URL
url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
              "query=", URLencode(query_text),
              "&mode=TimelineTone",
              "&timespan=3m",
              "&format=CSV")

# 3. Read the data - GDELT often includes a BOM or unusual headers
raw_data <- read.csv(url, check.names = FALSE)

raw_data

# ------------------------------------  WRANGLING 2. -----------------------------------------------

library(tidyverse)

# 1. Define your query
# We use the GDELT AI theme for general natural disasters
# You can change sourcecountry to any FIPS code (e.g., US, PH, GM)
query_text <- '(theme:NATURAL_DISASTER OR "disaster" OR "storm" OR "flood") sourcecountry:DA'

# 2. Build the URL
url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
              "query=", URLencode(query_text),
              "&mode=TimelineTone",
              "&timespan=3m",
              "&format=CSV")
s
# 3. Read the data with a safety check
# We use try() to prevent the "first five rows are empty" error from stopping your script
raw_data <- try(read.csv(url, check.names = FALSE), silent = TRUE)

raw_data































