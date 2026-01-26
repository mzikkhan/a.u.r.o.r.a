# load packages and define global variables

library(tidyverse)
library(fredr)
library(zoo)

## ------------------------------------------  WEATHER API  ---------------------------------------------------

# get_daily_weather: fetches hourly Open-Meteo CSV and returns daily summary
get_daily_weather <- function(start, end, lat, lon) {
  
  # Validate inputs to ensure all required geographic and temporal parameters are present
  if (is.null(lat) || missing(lon) || is.null(start) || is.null(end)) {
    stop("Please provide start, end, lat and lon. Example: get_daily_weather('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)")
  }
  
  # 1. CALL API
  # Construct the API request URL for Open-Meteo.
  # Note: Requesting 'temperature_2m' and 'precipitation' at hourly intervals to allow for custom daily aggregation later.
  # timezone=auto ensures the data aligns with local time rather than UTC, which is crucial for daily aggregation correctness.
  url <- paste0("https://api.open-meteo.com/v1/forecast?",
                "latitude=", lat, "&longitude=", lon,
                "&start_date=", start, "&end_date=", end,
                "&hourly=temperature_2m,precipitation",
                "&timezone=auto",                        
                "&format=csv")
  
  # 2. Read and Clean Data
  # The Open-Meteo CSV format includes metadata headers in the first ~10 lines. 
  # We skip these to reach the actual data table.
  weather_data <- read.csv(url, skip = 10, header = FALSE)
  
  # Manually assign column names since we skipped the header row.
  colnames(weather_data) <- c("datetime", "temp_c", "precip_mm")
  
  # Convert ISO8601 string to POSIXct for time manipulation, then extract just the Date component.
  weather_data$datetime <- as.POSIXct(weather_data$datetime, format="%Y-%m-%dT%H:%M")
  weather_data$date <- as.Date(weather_data$datetime)
  
  # 4. Calculate stats
  # Aggregate hourly rows into single daily rows.
  # We use a custom anonymous function to extract mean, max, and sum in a single pass.
  # This creates a matrix column for each variable in the resulting dataframe.
  daily_summary <- aggregate(cbind(temp_c, precip_mm) ~ date, 
                             data = weather_data, 
                             FUN = function(x) c(mean = mean(x), max = max(x), sum = sum(x)))
  
  # 5. Clean up the messy matrix output from aggregate
  # The 'aggregate' function returns columns of class 'matrix'.
  daily_summary <- data.frame(
    date      = daily_summary$date,
    avg_temp  = round(daily_summary$temp_c[, "mean"], 1),
    max_temp  = daily_summary$temp_c[, "max"],
    tot_rain  = daily_summary$precip_mm[, "sum"]
  )
  
  return(daily_summary)
}

# daily_weather_data <- get_daily_weather('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)
# daily_weather_data

## ----------------------------------------  ECONOMIC API  --------------------------------------------------

# get_daily_economic_data: fetches economic data
get_daily_economic_data <- function(start, end, lat, lon) {
  
  # Input validation (Note: lat/lon aren't used here but kept for signature consistency across fetchers)
  if (is.null(lat) || missing(lon) || is.null(start) || is.null(end)) {
    stop("Please provide start, end, lat and lon. Example: get_daily_economic_data('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)")
  }
  
  # Set API key
  fred_api_key <- '1f52a3bcb927a3a2423a9a2e78bd1261'
  fredr_set_key(fred_api_key)
  
  # 1. Define the parameters and their frequencies
  # We map friendly names (keys) to FRED Series IDs (values).
  # Important: These series have mixed frequencies (Daily, Weekly, Monthly, Quarterly)
  indicators <- c(
    "Interest_Rate"     = "DFF",          # Daily
    "Mortgage_Rate"     = "MORTGAGE30US", # Weekly
    "Unemployment"      = "UNRATE",       # Monthly
    "CPI_Inflation"     = "CPIAUCSL",     # Monthly
    "GDP"               = "GDPC1",        # Quarterly
    "Public_Debt"       = "GFDEBTN"       # Quarterly
  )
  
  # 2. Fetch all data at once 
  # Use map_dfr to iterate over the vector of series IDs, fetch them via fredr(),
  # and bind them into a single long-format dataframe.
  # The .id argument preserves the friendly name in the 'metric' column.
  raw_data <- map_dfr(indicators, 
                      ~fredr(series_id = .x, observation_start = as.Date("2020-01-01")), 
                      .id = "metric")
  
  # 3. Pivot to Wide Format 
  # Transform from Long (Metric, Date, Value) to Wide so each economic indicator has its own column.
  daily_trend <- raw_data %>%
    select(date, metric, value) %>%
    pivot_wider(names_from = metric, values_from = value) %>%
    arrange(date)
  
  # 4. The "Fill" Step (LOCF)
  # Create a complete sequence of daily dates covering the entire range.
  # This ensures we have rows even for days where no data was reported.
  full_dates <- data.frame(date = seq(min(daily_trend$date), max(daily_trend$date), by="day"))
  
  # 5. Keep only daily records
  # Join the actual data to the full date sequence.
  # Use na.locf (Last Observation Carried Forward) to propagate values for lower-frequency data.
  econ_daily_fixed <- full_dates %>%
    left_join(daily_trend, by = "date") %>%
    mutate(across(-date, ~na.locf(., na.rm = FALSE))) %>% 
    rename_with(tolower)
  
  return(econ_daily_fixed)
}

# econ_data <- get_daily_economic_data('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)
# tail(econ_data)

# ---------------------------------------------- NEWS API ----------------------------------------------------

# Helper function to call GDELT API
gdelt_timeline_daily <- function(query_text, out_col, start, end) {
  
  # clean the dates format
  # GDELT requires strict 14-digit datetime strings (YYYYMMDDHHMMSS).
  # We convert standard YYYY-MM-DD inputs to this format.
  startdt <- paste0(gsub("-", "", start), "000000")
  enddt   <- paste0(gsub("-", "", end),   "235959")
  
  # call API
  # Use TimelineTone mode to get sentiment scores over time.
  # URLencode is critical to handle spaces and special chars in the query_text safely.
  url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
                "query=", URLencode(query_text),
                "&mode=TimelineTone",
                "&format=CSV",
                "&startdatetime=", startdt,
                "&enddatetime=", enddt)
  
  # retrieve data
  # Wrap in try() because GDELT often returns 400/500 errors or malformed CSVs if the query finds no hits.
  # silent = TRUE prevents console clutter on failure.
  raw <- try(read.csv(url, check.names = FALSE), silent = TRUE)
  
  # if no data or error -> return spine with NA column
  # We construct a "spine" (empty dataframe with just dates) to ensure the function always returns 
  # a compatible dataframe, even if the API fails, preventing downstream joins from breaking.
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  if (inherits(raw, "try-error") || is.null(raw) || nrow(raw) == 0) {
    spine[[out_col]] <- NA_real_
    return(spine)
  }
  
  # try to detect the date + value cols
  # GDELT column names can be inconsistent
  # Regex matching is used to dynamically identify the correct column indices.
  cn <- names(raw)
  date_col <- cn[grepl("date", tolower(cn))][1]
  val_col  <- cn[grepl("value|tone|count", tolower(cn))][1]
  
  # search for valid dates and values
  # If we cannot identify the columns via regex, fallback to returning NAs.
  if (is.na(date_col) || is.na(val_col)) {
    spine[[out_col]] <- NA_real_
    return(spine)
  }
  
  # clean data
  # Parse the date string, coerce the value to numeric, and aggregate by mean.
  # The aggregation protects against duplicate entries for the same day.
  # Dynamic renaming (!!out_col := score) allows this function to be reused for different metrics.
  daily <- raw %>%
    transmute(
      date = as.Date(.data[[date_col]]),
      score = as.numeric(.data[[val_col]])
    ) %>%
    group_by(date) %>%
    summarise(score = mean(score, na.rm = TRUE), .groups = "drop") %>%
    rename(!!out_col := score)
  
  # Join the calculated data back to the date spine to ensure no days are missing from the sequence.
  spine %>% left_join(daily, by = "date")
}

# ---------------------------------------------- MERGE -------------------------------------------------------

get_macroeconomic_data <- function(start_date, end_date, latitude, longitude) {
  
  # get daily weather data
  weather_daily <- get_daily_weather(start_date, end_date, latitude, longitude)
  
  # get daily economic data
  econ_daily_fixed <- get_daily_economic_data(start_date, end_date, latitude, longitude)
  
  # get daily political unrest data
  political_unrest_daily <- gdelt_timeline_daily(
    query_text = '(protest OR unrest) sourcecountry:Canada',
    out_col = "political_unrest_score",
    start = start_date,
    end = end_date
  )
  
  # get daily natural disaster data
  natural_disaster_daily <- gdelt_timeline_daily(
    query_text = '(theme:NATURAL_DISASTER OR "disaster" OR "storm" OR "flood") sourcecountry:CA',
    out_col = "natural_disaster_score",
    start = start_date,
    end = end_date
  )
  
  # create blank df with daily column
  # Acts as the master timeline to align all diverse data sources
  spine <- data.frame(date = seq(as.Date(start_date), as.Date(end_date), by = "day"))
  
  # merge all the data into one
  # Sequentially joins all datasets. Since all sources were pre-processed to have a 'date' column
  # and daily granularity, this is a clean 1-to-1 join.
  merged_df <- spine %>%
    left_join(weather_daily,          by = "date") %>%
    left_join(econ_daily_fixed,       by = "date") %>%
    left_join(political_unrest_daily, by = "date") %>%
    left_join(natural_disaster_daily, by = "date") %>%
    select(
      DATE = date,
      avg_temp, max_temp, tot_rain,
      natural_disaster_score, political_unrest_score,
      interest_rate, mortgage_rate, unemployment, cpi_inflation, gdp, public_debt
    )
  
  return(merged_df)
  
}

# sample = get_macroeconomic_data('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)
# sample