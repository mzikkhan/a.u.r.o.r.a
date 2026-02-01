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
| `get_weather_data()` | Retrieves weather data for specified location and date range |
| `get_fred_data()` | Retrieves economic indicators from FRED |
| `get_gdelt_data()` | Retrieves event sentiment data from GDELT |


## Error Handling

Aurora is designed with robust error handling to ensure reliability during API communication. If an external data source is temporarily unavailable or returns an error, Aurora gracefully handles the failure by returning missing values (NA) instead of interrupting execution. This ensures downstream workflows remain stable.

## License

This project is licensed under the MIT License.  
See the [LICENSE](LICENSE) file for full details.