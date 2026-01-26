# Aurora Package

![CI Badge](https://github.com/zaedkhan/aurora/actions/workflows/R-CMD-check.yaml/badge.svg)

**Aurora** is an R package designed to integrate daily weather, economic, and event (GDELT) data into a single, cohesive dataset for macroeconomic analysis.

## Features
- **Weather Data**: Fetches daily temperature and precipitation summaries from Open-Meteo.
- **Economic Indicators**: Retrieves key economic metrics (GDP, Inflation, Unemployment, etc.) from the FRED API.
- **Event Data**: Queries GDELT for sentiment scores related to political unrest and natural disasters.
- **Robust Error Handling**: Gracefully handles API failures by returning missing data (NAs) instead of crashing.

## Installation
You can install the development version from GitHub:

```R
# install.packages("devtools")
devtools::install_github("zaedkhan/aurora")
```

## Usage

```R
library(aurora)

# Fetch data for a specific location and date range
data <- get_macroeconomic_data(
  start_date = "2024-01-01", 
  end_date = "2024-01-31", 
  latitude = 45.4215, 
  longitude = -75.6972
)

head(data)
```

## License
MIT License. See [LICENSE](LICENSE) for details.
