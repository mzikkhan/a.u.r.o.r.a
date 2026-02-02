# Aurora Package

## Motivation

Macroeconomic analysis often requires integrating multiple external data sources such as weather data, economic indicators, and global event sentiment data. These datasets are typically accessed through different APIs and data formats, making the data collection and preprocessing workflow time-consuming and error-prone.

**Aurora** addresses this challenge by providing a unified interface that automatically fetches, processes, and integrates these datasets into a single, analysis-ready macroeconomic dataset. This allows data analysts to focus more on insight generation rather than data collection and preprocessing.

## Data Sources

Aurora integrates data from the following external sources:

- **Open-Meteo API** – Provides daily weather summaries including temperature and precipitation.
- **FRED API** – Provides key economic indicators such as GDP, CPI, inflation rate, and unemployment rate.
- **GDELT Project** – Provides global event data and sentiment indicators related to political unrest, disasters, and global news activity.

## Main Functions

| Function | Description |
|---|---|
| `get_macroeconomic_data()` | Fetches integrated macroeconomic dataset combining weather, economic, and event data |
| `get_daily_weather()` | Retrieves weather data for specified location and date range |
| `get_daily_economic_data()` | Retrieves economic indicators from FRED |
| `gdelt_timeline_daily()` | Retrieves event sentiment data from GDELT |
| `revenue_merge()` | Merges macro dataset with revenue CSV by Date |
| `plotter()` | Plots revenue vs selected variables with scaling |

## Usage

Here is an example of how to use the Aurora package to fetch data, merge it with revenue, and visualize the results.

```r
# 1. Source the package functions
source(file.path("R", "aurora.R"))

# 2. Fetch Macroeconomic Data
# Provide start/end dates and location coordinates (e.g., Copenhagen)
my_analysis_data <- get_macroeconomic_data(
  start_date = "2025-01-01",
  end_date   = "2025-12-01",
  latitude   = 55.6761, 
  longitude  = 12.5683
)

# 3. View the fetched data
head(my_analysis_data)

# 4. Integrate Revenue Data
# Merge with your internal revenue data (must have a 'Date' column)
rev_data <- revenue_merge(my_analysis_data, "Scripts/revenue_data_2025.csv")
head(rev_data)

# 5. Visualize Correlations
# Select columns to analyze against Revenue
plots <- c("avg_temp", "max_temp", "tot_rain", 
           "natural_disaster_score", "political_unrest_score",
           "interest_rate", "mortgage_rate", "unemployment", 
           "cpi_inflation", "gdp", "public_debt")

# Generate scaled plots (z-score)
plotter(rev_data,
        cols = plots,
        date_col = "Date",
        revenue_col = "Revenue",
        ncol = 3,
        scale_method = "zscore")
```

## Error Handling

Aurora is designed with robust error handling to ensure reliability during API communication. If an external data source is temporarily unavailable or returns an error, Aurora gracefully handles the failure by returning missing values (NA) instead of interrupting execution. This ensures downstream workflows remain stable.

## License

This project is licensed under the MIT License.  
See the [LICENSE](LICENSE) file for full details.