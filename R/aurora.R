## aurora.R
## NOTE: Package-style file: no library() calls here.
## Use package::function everywhere.

## ------------------------------------------  WEATHER API  ---------------------------------------------------

#' Get daily weather summary from Open-Meteo archive API
#'
#' @param start Character. Start date in "YYYY-MM-DD".
#' @param end Character. End date in "YYYY-MM-DD".
#' @param lat Numeric. Latitude.
#' @param lon Numeric. Longitude.
#'
#' @return A data.frame with columns: date, avg_temp, max_temp, tot_rain.
#' @export

get_daily_weather <- function(start, end, lat, lon) {
  
  if (is.null(lat) || missing(lon) || is.null(start) || is.null(end)) {
    stop("Provide start, end, lat, lon. Example: get_daily_weather('2026-01-01','2026-01-15', 51.5074, -0.1278)")
  }
  
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  
  result <- tryCatch({
    
    url <- paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=", lat, "&longitude=", lon,
      "&start_date=", start, "&end_date=", end,
      "&hourly=temperature_2m,precipitation",
      "&timezone=auto",
      "&format=csv"
    )
    
    raw <- suppressWarnings(readLines(url))
    
    header_i <- grep("^time,", raw)[1]
    if (is.na(header_i)) stop("Could not find CSV header in Open-Meteo response.")
    
    csv_text <- raw[header_i:length(raw)]
    weather_data <- utils::read.csv(text = paste(csv_text, collapse = "\n"))
    
    names(weather_data) <- c("datetime", "temp_c", "precip_mm")
    
    weather_data$datetime <- as.POSIXct(weather_data$datetime, format = "%Y-%m-%dT%H:%M")
    weather_data$date <- as.Date(weather_data$datetime)
    
    daily_summary <- dplyr::summarise(
      dplyr::group_by(weather_data, date),
      avg_temp = round(mean(temp_c, na.rm = TRUE), 1),
      max_temp = max(temp_c, na.rm = TRUE),
      tot_rain = sum(precip_mm, na.rm = TRUE),
      .groups = "drop"
    )
    
    dplyr::left_join(spine, daily_summary, by = "date")
    
  }, error = function(e) {
    message("Error in get_daily_weather: ", e$message)
    spine$avg_temp <- NA_real_
    spine$max_temp <- NA_real_
    spine$tot_rain <- NA_real_
    spine
  })
  
  result
}


## ----------------------------------------  ECONOMIC API  --------------------------------------------------

#' Get daily economic indicators from FRED aligned to a daily date spine
#'
#' @param start Character. Start date in "YYYY-MM-DD".
#' @param end Character. End date in "YYYY-MM-DD".
#' @param lat Numeric. Latitude (kept for interface consistency; not used by FRED).
#' @param lon Numeric. Longitude (kept for interface consistency; not used by FRED).
#'
#' @return A data.frame with columns: date and economic indicators
#'   (interest_rate, mortgage_rate, unemployment, cpi_inflation, gdp, public_debt).
#' @export

get_daily_economic_data <- function(start, end, lat, lon) {
  
  if (is.null(lat) || missing(lon) || is.null(start) || is.null(end)) {
    stop("Please provide start, end, lat and lon. Example: get_daily_economic_data('2026-01-01','2026-01-15', lat=51.5074, lon=-0.1278)")
  }
  
  result <- tryCatch({
    
    # Set it as an environment variable: Sys.setenv(FRED_API_KEY="...")
    fred_api_key <- Sys.getenv("FRED_API_KEY")
    if (fred_api_key == "") stop("Missing FRED_API_KEY environment variable.")
    fredr::fredr_set_key(fred_api_key)
    
    indicators <- c(
      "Interest_Rate" = "DFF",          # Daily
      "Mortgage_Rate" = "MORTGAGE30US", # Weekly
      "Unemployment"  = "UNRATE",       # Monthly
      "CPI_Inflation" = "CPIAUCSL",     # Monthly
      "GDP"           = "GDPC1",        # Quarterly
      "Public_Debt"   = "GFDEBTN"       # Quarterly
    )
    
    raw_data <- purrr::map_dfr(
      indicators,
      ~fredr::fredr(series_id = .x, observation_start = as.Date("2020-01-01")),
      .id = "metric"
    )
    
    daily_trend <- raw_data |>
      dplyr::select(date, metric, value) |>
      tidyr::pivot_wider(names_from = metric, values_from = value) |>
      dplyr::arrange(date)
    
    full_dates <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
    
    econ_daily_fixed <- full_dates |>
      dplyr::left_join(daily_trend, by = "date") |>
      dplyr::mutate(dplyr::across(-date, ~zoo::na.locf(., na.rm = FALSE))) |>
      dplyr::rename_with(tolower)
    
    econ_daily_fixed
    
  }, error = function(e) {
    message("Error in get_daily_economic_data: ", e$message)
    
    full_dates <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
    expected_cols <- c("interest_rate", "mortgage_rate", "unemployment",
                       "cpi_inflation", "gdp", "public_debt")
    
    for (col in expected_cols) full_dates[[col]] <- NA_real_
    full_dates
  })
  
  result
}


## ---------------------------------------------- NEWS API ----------------------------------------------------

#' Get a daily GDELT TimelineTone score and align to a date spine
#'
#' @param query_text Character. GDELT query string.
#' @param out_col Character. Name of the score column to create.
#' @param start Character. Start date in "YYYY-MM-DD".
#' @param end Character. End date in "YYYY-MM-DD".
#'
#' @return A data.frame with columns: date and the score column.
#' @export

gdelt_timeline_daily <- function(query_text, out_col, start, end) {
  
  startdt <- paste0(gsub("-", "", start), "000000")
  enddt   <- paste0(gsub("-", "", end),   "235959")
  
  spine <- data.frame(date = seq(as.Date(start), as.Date(end), by = "day"))
  
  result <- tryCatch({
    
    url <- paste0("https://api.gdeltproject.org/api/v2/doc/doc?",
                  "query=", utils::URLencode(query_text),
                  "&mode=TimelineTone",
                  "&format=CSV",
                  "&startdatetime=", startdt,
                  "&enddatetime=", enddt)
    
    raw <- suppressWarnings(utils::read.csv(url, check.names = FALSE))
    
    if (is.null(raw) || nrow(raw) == 0) stop("GDELT returned empty data")
    
    cn <- names(raw)
    date_col <- cn[grepl("date", tolower(cn))][1]
    val_col  <- cn[grepl("value|tone|count", tolower(cn))][1]
    
    if (is.na(date_col) || is.na(val_col)) stop("GDELT returned unrecognizable columns")
    
    daily <- raw |>
      dplyr::transmute(
        date  = as.Date(.data[[date_col]]),
        score = as.numeric(.data[[val_col]])
      ) |>
      dplyr::group_by(date) |>
      dplyr::summarise(score = mean(score, na.rm = TRUE), .groups = "drop")
    
    names(daily)[names(daily) == "score"] <- out_col
    
    dplyr::left_join(spine, daily, by = "date")
    
  }, error = function(e) {
    message("Warning/Error in gdelt_timeline_daily for query '", query_text, "': ", e$message)
    spine[[out_col]] <- NA_real_
    spine
  })
  
  result
}


## ---------------------------------------------- MERGE -------------------------------------------------------

#' Merge weather, economic, and GDELT features into one macro dataset
#'
#' @param start_date Character. Start date in "YYYY-MM-DD".
#' @param end_date Character. End date in "YYYY-MM-DD".
#' @param latitude Numeric. Latitude.
#' @param longitude Numeric. Longitude.
#' @param weather_fun Function. Weather fetcher (default: get_daily_weather).
#' @param econ_fun Function. Economic fetcher (default: get_daily_economic_data).
#' @param gdelt_fun Function. GDELT fetcher (default: gdelt_timeline_daily).
#'
#' @return A data.frame with DATE and merged feature columns.
#' @export

get_macroeconomic_data <- function(start_date, end_date, latitude, longitude,
                                   weather_fun = get_daily_weather,
                                   econ_fun    = get_daily_economic_data,
                                   gdelt_fun   = gdelt_timeline_daily) {
  
  weather_daily <- weather_fun(start_date, end_date, latitude, longitude)
  econ_daily_fixed <- econ_fun(start_date, end_date, latitude, longitude)
  
  political_unrest_daily <- gdelt_fun(
    query_text = '(protest OR unrest) sourcecountry:CA',
    out_col = "political_unrest_score",
    start = start_date,
    end = end_date
  )
  
  natural_disaster_daily <- gdelt_fun(
    query_text = '(theme:NATURAL_DISASTER OR disaster OR storm OR flood) sourcecountry:CA',
    out_col = "natural_disaster_score",
    start = start_date,
    end = end_date
  )
  
  spine <- data.frame(date = seq(as.Date(start_date), as.Date(end_date), by = "day"))
  
  spine |>
    dplyr::left_join(weather_daily,          by = "date") |>
    dplyr::left_join(econ_daily_fixed,       by = "date") |>
    dplyr::left_join(political_unrest_daily, by = "date") |>
    dplyr::left_join(natural_disaster_daily, by = "date") |>
    dplyr::select(
      DATE = date,
      avg_temp, max_temp, tot_rain,
      natural_disaster_score, political_unrest_score,
      interest_rate, mortgage_rate, unemployment, cpi_inflation, gdp, public_debt
    )
}


## ------------------------------------------- REVENUE MERGE --------------------------------------------------

#' Merge macro dataset with revenue CSV by Date
#'
#' @param input_df data.frame. Must contain column DATE.
#' @param csv_path Character. Path to CSV with a Date column.
#'
#' @return A merged data.frame (inner join) on Date.
#' @export

revenue_merge <- function(input_df, csv_path) {
  
  # Input validation
  if (!is.data.frame(input_df)) stop("input_df must be a data frame.")
  if (!file.exists(csv_path)) stop("CSV path does not exist: ", csv_path)
  
  result <- tryCatch({
    
    # Read the revenue CSV
    csv_df <- utils::read.csv(csv_path, stringsAsFactors = FALSE)
    
    # Check for required columns
    if (!"DATE" %in% names(input_df)) stop("Input dataframe must contain 'DATE' column")
    if (!"Date" %in% names(csv_df)) stop("CSV file must contain 'Date' column")
    
    # Standardize Date columns
    input_df$Date <- as.Date(input_df$DATE)
    csv_df$Date   <- as.Date(csv_df$Date)
    
    # Merge datasets (inner join to keep only matching dates)
    dplyr::inner_join(input_df, csv_df, by = "Date")
    
  }, error = function(e) {
    message("Error in revenue_merge: ", e$message)
    return(NULL)
  })
  
  result
}


## ---------------------------------------------- PLOTTER -----------------------------------------------------

#' Plot revenue vs selected variables with scaling
#'
#' @param df data.frame. Input dataset.
#' @param cols Character vector. Columns to plot against revenue.
#' @param date_col Character. Name of date column in df.
#' @param revenue_col Character. Name of revenue column in df.
#' @param ncol Integer. Number of columns in the plot grid.
#' @param save_path Character or NULL. If not NULL, file path to save plot.
#' @param width Numeric. Width for ggsave (inches).
#' @param height Numeric. Height for ggsave (inches).
#' @param dpi Integer. DPI for ggsave.
#' @param scale_method Character. "zscore" or "minmax".
#' @param drop_na Logical. Drop non-finite rows before plotting.
#'
#' @return A patchwork object (invisibly).
#' @export

plotter <- function(df,
                    cols,
                    date_col = "Date",
                    revenue_col = "Revenue",
                    ncol = NULL,
                    save_path = NULL,
                    width = 14, height = 9, dpi = 300,
                    scale_method = c("zscore", "minmax"),
                    drop_na = TRUE) {
  
  # Validate scale method argument
  scale_method <- match.arg(scale_method)
  
  # Check for required packages
  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install ggplot2: install.packages('ggplot2')")
  if (!requireNamespace("patchwork", quietly = TRUE)) stop("Install patchwork: install.packages('patchwork')")
  
  tryCatch({
    
    # Check for required columns in dataframe
    if (!date_col %in% names(df)) stop(paste0("Missing date column: ", date_col))
    if (!revenue_col %in% names(df)) stop(paste0("Missing revenue column: ", revenue_col))
    
    missing_cols <- setdiff(cols, names(df))
    if (length(missing_cols) > 0) stop(paste("These columns are missing:", paste(missing_cols, collapse = ", ")))
    
    # Prepare data: convert date and sort
    df[[date_col]] <- as.Date(df[[date_col]])
    df <- df[order(df[[date_col]]), , drop = FALSE]
    
    # Helper function to scale vectors
    scale_vec <- function(x) {
      x <- as.numeric(x)
      if (scale_method == "zscore") {
        # Z-score standardization
        s <- stats::sd(x, na.rm = TRUE)
        if (is.na(s) || s == 0) return(rep(NA_real_, length(x)))
        (x - mean(x, na.rm = TRUE)) / s
      } else {
        # Min-Max scaling
        rng <- range(x, na.rm = TRUE)
        if (any(!is.finite(rng)) || rng[1] == rng[2]) return(rep(NA_real_, length(x)))
        (x - rng[1]) / (rng[2] - rng[1])
      }
    }
    
    # Determine grid layout
    if (is.null(ncol)) {
      k <- length(cols)
      ncol <- if (k <= 2) 1 else if (k <= 4) 2 else if (k <= 9) 3 else 4
    }
    
    # Generate plots for each column
    plots <- lapply(cols, function(xcol) {
      tmp <- df[, c(date_col, revenue_col, xcol), drop = FALSE]
      names(tmp) <- c("Date", "Revenue", "X")
      
      tmp$Revenue <- as.numeric(tmp$Revenue)
      tmp$X <- as.numeric(tmp$X)
      
      # Filter NAs if requested
      if (drop_na) {
        tmp <- tmp[is.finite(tmp$Revenue) & is.finite(tmp$X) & !is.na(tmp$Date), , drop = FALSE]
      } else {
        tmp <- tmp[!is.na(tmp$Date), , drop = FALSE]
      }
      
      # Apply scaling
      tmp$Revenue_scaled <- scale_vec(tmp$Revenue)
      tmp$X_scaled <- scale_vec(tmp$X)
      
      # Create long format for ggplot
      long_df <- rbind(
        data.frame(Date = tmp$Date, Series = "Revenue", Value = tmp$Revenue_scaled),
        data.frame(Date = tmp$Date, Series = xcol,      Value = tmp$X_scaled)
      )
      
      # Plot
      ggplot2::ggplot(long_df, ggplot2::aes(x = Date, y = Value, group = Series, linetype = Series)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          title = paste0("Revenue vs ", xcol),
          x = date_col,
          y = if (scale_method == "zscore")
            "z-score (mean=0, sd=1)"
          else
            "min-max scaled (0-1)"
        ) +
        ggplot2::theme_minimal()
    })
    
    # Combine plots using patchwork
    combined <- patchwork::wrap_plots(plots, ncol = ncol)
    
    # Save if path provided
    if (!is.null(save_path)) {
      ggplot2::ggsave(save_path, combined, width = width, height = height, dpi = dpi)
    }
    
    print(combined)
    invisible(combined)
    
  }, error = function(e) {
    message("Error in plotter: ", e$message)
    invisible(NULL)
  })
}
