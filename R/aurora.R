#' Get Daily Weather Data
#'
#' Fetches hourly weather data from Open-Meteo and aggregates it to daily summaries.
#'
#' @param start Character string. Start date in 'YYYY-MM-DD' format.
#' @param end Character string. End date in 'YYYY-MM-DD' format.
#' @param lat Numeric. Latitude.
#' @param lon Numeric. Longitude.
#'
#' @return A data frame with Date, avg_temp, max_temp, and tot_rain.
#' @importFrom utils read.csv
#' @importFrom stats aggregate
#' @importFrom dplyr left_join
#' @export
get_daily_weather <- function(start, end, lat, lon) {
  
  # Validate inputs to ensure all required geographic and temporal parameters are present
  if (is.null(lat) || missing(lon) || is.null(start) || is.null(end)) {
    stop("Please provide start, end, lat and lon. Example: get_daily_weather('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)")
  }
  
  # Create a spine sequence of dates to ensure we can return a valid structure even on failure
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  
  result <- tryCatch({
    # 1. CALL API
    # Construct the API request URL for Open-Meteo.
    url <- paste0("https://api.open-meteo.com/v1/forecast?",
                  "latitude=", lat, "&longitude=", lon,
                  "&start_date=", start, "&end_date=", end,
                  "&hourly=temperature_2m,precipitation",
                  "&timezone=auto",                        
                  "&format=csv")
    
    # 2. Read and Clean Data
    # The Open-Meteo CSV format includes metadata headers in the first ~10 lines. 
    weather_data <- read.csv(url, skip = 10, header = FALSE)
    
    # Manually assign column names since we skipped the header row.
    colnames(weather_data) <- c("datetime", "temp_c", "precip_mm")
    
    # Convert ISO8601 string to POSIXct for time manipulation
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
    
    # Ensure all dates in range are present (though API usually returns full range)
    out <- spine %>% left_join(daily_summary, by = "date")
    return(out)
    
  }, error = function(e) {
    message("Error in get_daily_weather: ", e$message)
    # Return spine with NAs for data columns
    spine$avg_temp <- NA
    spine$max_temp <- NA
    spine$tot_rain <- NA
    return(spine)
  })

  return(result)
}

#' Get Daily Economic Data
#'
#' Fetches economic indicators from FRED and aggregates/fills them to daily frequency.
#'
#' @param start Character string. Start date in 'YYYY-MM-DD' format.
#' @param end Character string. End date in 'YYYY-MM-DD' format.
#' @param lat Numeric. Latitude (not used, kept for API consistency).
#' @param lon Numeric. Longitude (not used, kept for API consistency).
#'
#' @return A data frame with daily economic indicators.
#' @importFrom fredr fredr fredr_set_key
#' @importFrom purrr map_dfr
#' @importFrom tidyr pivot_wider
#' @importFrom zoo na.locf
#' @importFrom dplyr select arrange left_join mutate across rename_with
#' @export
get_daily_economic_data <- function(start, end, lat, lon) {
  
  # Input validation
  if (is.null(lat) || missing(lon) || is.null(start) || is.null(end)) {
    stop("Please provide start, end, lat and lon. Example: get_daily_economic_data('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)")
  }
  
  result <- tryCatch({
    # Set API key
    fred_api_key <- '1f52a3bcb927a3a2423a9a2e78bd1261'
    fredr_set_key(fred_api_key)
    
    # 1. Define the parameters
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
    # Create complete sequence of daily dates covering the requested range (or data range)
    # To be safe, we use the requested range here to ensure consistent return.
    full_dates <- data.frame(date = seq(as.Date(start), as.Date(end), by="day"))
    
    # 5. Keep only daily records
    econ_daily_fixed <- full_dates %>%
      left_join(daily_trend, by = "date") %>%
      mutate(across(-date, ~na.locf(., na.rm = FALSE))) %>% 
      rename_with(tolower)
    
    return(econ_daily_fixed)
    
  }, error = function(e) {
    message("Error in get_daily_economic_data: ", e$message)
    # Return spine with NAs
    full_dates <- data.frame(date = seq(as.Date(start), as.Date(end), by="day"))
    
    # List of expected columns (lowercase) based on indicators
    expected_cols <- c("interest_rate", "mortgage_rate", "unemployment", 
                       "cpi_inflation", "gdp", "public_debt")
    
    for(col in expected_cols) {
      full_dates[[col]] <- NA_real_
    }
    return(full_dates)
  })
  
  return(result)
}

#' Get GDELT Timeline Data
#'
#' Fetches sentiment tone timeline from GDELT API.
#'
#' @param query_text Character string. Query text for GDELT.
#' @param out_col Character string. Name of the output column for the score.
#' @param start Character string. Start date.
#' @param end Character string. End date.
#'
#' @return A data frame with Date and the requested score column.
#' @importFrom utils URLencode read.csv
#' @importFrom dplyr transmute group_by summarise rename left_join
#' @export
gdelt_timeline_daily <- function(query_text, out_col, start, end) {
  
  # clean the dates format
  startdt <- paste0(gsub("-", "", start), "000000")
  enddt   <- paste0(gsub("-", "", end),   "235959")
  
  # Create spine first to ensure safe return
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  
  result <- tryCatch({
    # call API
    url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
                  "query=", URLencode(query_text),
                  "&mode=TimelineTone",
                  "&format=CSV",
                  "&startdatetime=", startdt,
                  "&enddatetime=", enddt)
    
    # retrieve data
    # Use suppressWarnings because GDELT might emit warnings on empty results
    raw <- suppressWarnings(read.csv(url, check.names = FALSE))
    
    if (is.null(raw) || nrow(raw) == 0) {
      stop("GDELT returned empty data")
    }
    
    # try to detect the date + value cols
    cn <- names(raw)
    date_col <- cn[grepl("date", tolower(cn))][1]
    val_col  <- cn[grepl("value|tone|count", tolower(cn))][1]
    
    if (is.na(date_col) || is.na(val_col)) {
      stop("GDELT returned unrecognizable columns")
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
    
    # Join back to spine
    out <- spine %>% left_join(daily, by = "date")
    return(out)
    
  }, error = function(e) {
    message("Warning/Error in gdelt_timeline_daily for query '", query_text, "': ", e$message)
    spine[[out_col]] <- NA_real_
    return(spine)
  })
  
  return(result)
}

#' Get Macroeconomic Data
#'
#' Orchestrates the fetching and merging of weather, economic, and political/disaster data.
#'
#' @param start_date Character string. Start date.
#' @param end_date Character string. End date.
#' @param latitude Numeric. Latitude.
#' @param longitude Numeric. Longitude.
#'
#' @return A merged data frame containing all indicators.
#' @export
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
