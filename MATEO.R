get_open_meteo_daily <- function(
    latitude,
    longitude,
    start_date,
    end_date,
    daily_vars = c(
      "temperature_2m_max",
      "temperature_2m_min",
      "precipitation_sum",
      "windspeed_10m_max"
    )
) {
  
  # Build API URL
  base_url <- "https://archive-api.open-meteo.com/v1/archive"
  
  response <- GET(
    url = base_url,
    query = list(
      latitude = latitude,
      longitude = longitude,
      start_date = start_date,
      end_date = end_date,
      daily = paste(daily_vars, collapse = ","),
      timezone = "auto"
    )
  )
  
  # Convert JSON → R list
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  
  # Convert daily data to data frame
  daily_df <- data.frame(
    date = as.Date(data$daily$time)
  )
  
  # Add each weather variable as a column
  for (var in daily_vars) {
    daily_df[[var]] <- data$daily[[var]]
  }
  
  return(daily_df)
}

weather_df <- get_open_meteo_daily(
  latitude = 49.8880,     # Kelowna
  longitude = -119.4960,
  start_date = "2022-01-01",
  end_date = "2022-12-31"
)

head(weather_df)