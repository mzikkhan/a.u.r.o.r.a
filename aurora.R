# load packages and define global variables

library(tidyverse)
library(fredr)
library(zoo)

## ------------------------------------------  WEATHER API  ---------------------------------------------------

get_daily_weather <- function(start, end, lat, lon) {
  
  if (is.null(lat) || missing(lon) || is.null(start) || is.null(end)) {
    stop("Provide start, end, lat, lon. Example: get_daily_weather('2026-01-01','2026-01-15', 51.5074, -0.1278)")
  }
  
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  
  result <- tryCatch({
    
    # ✅ Use ARCHIVE endpoint for historical ranges
    url <- paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=", lat, "&longitude=", lon,
      "&start_date=", start, "&end_date=", end,
      "&hourly=temperature_2m,precipitation",
      "&timezone=auto",
      "&format=csv"
    )
    
    # Read CSV (no need to guess skip=10; safer: find header row)
    raw <- readLines(url)
    
    # Find the line where the actual CSV header starts (contains "time")
    header_i <- grep("^time,", raw)[1]
    if (is.na(header_i)) stop("Could not find CSV header in Open-Meteo response.")
    
    csv_text <- raw[header_i:length(raw)]
    weather_data <- read.csv(text = paste(csv_text, collapse = "\n"))
    
    # Rename columns to match your code expectations
    # Open-Meteo usually returns columns: time, temperature_2m, precipitation
    names(weather_data) <- c("datetime", "temp_c", "precip_mm")
    
    weather_data$datetime <- as.POSIXct(weather_data$datetime, format = "%Y-%m-%dT%H:%M")
    weather_data$date <- as.Date(weather_data$datetime)
    
    daily_summary <- weather_data %>%
      group_by(date) %>%
      summarise(
        avg_temp = round(mean(temp_c, na.rm = TRUE), 1),
        max_temp = max(temp_c, na.rm = TRUE),
        tot_rain = sum(precip_mm, na.rm = TRUE),
        .groups = "drop"
      )
    
    out <- spine %>% left_join(daily_summary, by = "date")
    out
    
  }, error = function(e) {
    message("Error in get_daily_weather: ", e$message)
    spine$avg_temp <- NA
    spine$max_temp <- NA
    spine$tot_rain <- NA
    spine
  })
  
  result
}

#daily_weather_data <- get_daily_weather("2025-01-01", "2026-01-15", 51.5074, -0.1278)
#daily_weather_data

## ----------------------------------------  ECONOMIC API  --------------------------------------------------

# get_daily_economic_data: fetches economic data
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

# econ_data <- get_daily_economic_data('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)
# tail(econ_data)

# ---------------------------------------------- NEWS API ----------------------------------------------------

# Helper function to call GDELT API
gdelt_timeline_daily <- function(query_text, out_col, start, end) {
  
  # clean the dates format
  startdt <- paste0(gsub("-", "", start), "000000")
  enddt   <- paste0(gsub("-", "", end),   "235959")
  
  # Create spine first to ensure safe return
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  
  # Define success flag
  success <- FALSE
  
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
    # If it's just empty data, it might be expected. Print warning vs error?
    # Requirement is "useful error messages".
    message("Warning/Error in gdelt_timeline_daily for query '", query_text, "': ", e$message)
    spine[[out_col]] <- NA_real_
    return(spine)
  })
  
  return(result)
}

#start_date <- '2026-01-01'
#end_date <- '2026-01-15'

#political_unrest_daily <- gdelt_timeline_daily(
#  query_text = '(protest OR unrest) sourcecountry:CA',
#  out_col = "political_unrest_score",
#  start = start_date,
#  end = end_date
#)

#natural_disaster_daily <- gdelt_timeline_daily(
#  query_text = '(theme:NATURAL_DISASTER OR disaster OR storm OR flood) sourcecountry:CA',
#  out_col = "natural_disaster_score",
#  start = start_date,
#  end = end_date
#)
#head(political_unrest_daily)
#head(natural_disaster_daily)

# ---------------------------------------------- MERGE -------------------------------------------------------

get_macroeconomic_data <- function(start_date, end_date, latitude, longitude) {
  
  # get daily weather data
  weather_daily <- get_daily_weather(start_date, end_date, latitude, longitude)
  
  # get daily economic data
  econ_daily_fixed <- get_daily_economic_data(start_date, end_date, latitude, longitude)
  
  # get daily political unrest data
  political_unrest_daily <- gdelt_timeline_daily(
    query_text = '(protest OR unrest) sourcecountry:CA',
    out_col = "political_unrest_score",
    start = start_date,
    end = end_date
  )
  
  natural_disaster_daily <- gdelt_timeline_daily(
    query_text = '(theme:NATURAL_DISASTER OR disaster OR storm OR flood) sourcecountry:CA',
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

################################################################################################################

################################################################################################################

revenue_merge <- function(input_df, csv_path){
  csv_df <- read.csv(csv_path, stringsAsFactors = FALSE)
  
  # ---- Check Date column exists ----
  if (!"DATE" %in% names(input_df)) {
    stop("Input dataframe must contain 'Date' column")
  }
  
  if (!"Date" %in% names(csv_df)) {
    stop("CSV file must contain 'Date' column")
  }
  
  # ---- Convert Date columns to Date type ----
  input_df$Date <- as.Date(input_df$DATE)
  csv_df$Date <- as.Date(csv_df$Date)
  
  # ---- Perform INNER JOIN ----
  merged_df <- dplyr::inner_join(input_df, csv_df, by = "Date")
  
  return(merged_df)
}

################################################################################################################

################################################################################################################

plotter <- function(df,
                                         cols,
                                         date_col = "Date",
                                         revenue_col = "Revenue",
                                         ncol = NULL,
                                         save_path = NULL,
                                         width = 14, height = 9, dpi = 300,
                                         scale_method = c("zscore", "minmax"),
                                         drop_na = TRUE) {
  
  scale_method <- match.arg(scale_method)
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2: install.packages('ggplot2')")
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("Install patchwork: install.packages('patchwork')")
  
  # Checks
  if (!date_col %in% names(df)) stop(paste0("Missing date column: ", date_col))
  if (!revenue_col %in% names(df)) stop(paste0("Missing revenue column: ", revenue_col))
  
  missing_cols <- setdiff(cols, names(df))
  if (length(missing_cols) > 0) stop(paste("These columns are missing:", paste(missing_cols, collapse = ", ")))
  
  # Date conversion + sort
  df[[date_col]] <- as.Date(df[[date_col]])
  df <- df[order(df[[date_col]]), , drop = FALSE]
  
  # scaler
  scale_vec <- function(x) {
    x <- as.numeric(x)
    if (scale_method == "zscore") {
      s <- stats::sd(x, na.rm = TRUE)
      if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
      (x - mean(x, na.rm = TRUE)) / s
    } else {
      rng <- range(x, na.rm = TRUE)
      if (any(!is.finite(rng)) || rng[1] == rng[2]) return(rep(NA_real_, length(x)))
      (x - rng[1]) / (rng[2] - rng[1])
    }
  }
  
  # Auto ncol
  if (is.null(ncol)) {
    k <- length(cols)
    ncol <- if (k <= 2) 1 else if (k <= 4) 2 else if (k <= 9) 3 else 4
  }
  
  plots <- lapply(cols, function(xcol) {
    tmp <- df[, c(date_col, revenue_col, xcol), drop = FALSE]
    names(tmp) <- c("Date", "Revenue", "X")
    
    # Keep finite numeric (and optionally drop NA)
    tmp$Revenue <- as.numeric(tmp$Revenue)
    tmp$X <- as.numeric(tmp$X)
    
    if (drop_na) {
      tmp <- tmp[is.finite(tmp$Revenue) & is.finite(tmp$X) & !is.na(tmp$Date), , drop = FALSE]
    } else {
      tmp <- tmp[!is.na(tmp$Date), , drop = FALSE]
    }
    
    # Scaled series for overlay comparison
    tmp$Revenue_scaled <- scale_vec(tmp$Revenue)
    tmp$X_scaled <- scale_vec(tmp$X)
    
    # Build long format WITHOUT extra packages
    long_df <- rbind(
      data.frame(Date = tmp$Date, Series = "Revenue", Value = tmp$Revenue_scaled),
      data.frame(Date = tmp$Date, Series = xcol,      Value = tmp$X_scaled)
    )
    
    ggplot2::ggplot(long_df, ggplot2::aes(x = Date, y = Value, group = Series, linetype = Series)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        title = paste0("Revenue vs ", xcol),
        x = date_col,
        y = if (scale_method == "zscore") "z-score (mean=0, sd=1)" else "min-max scaled (0–1)"
      ) +
      ggplot2::theme_minimal()
  })
  
  combined <- patchwork::wrap_plots(plots, ncol = ncol)
  
  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, combined, width = width, height = height, dpi = dpi)
  }
  
  print(combined)
  invisible(combined)
}
