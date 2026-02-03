
# 1. File path
source("R/aurora.R")

Sys.setenv(FRED_API_KEY = "1f52a3bcb927a3a2423a9a2e78bd1261")

# 2. Call the function with your specific inputs
my_analysis_data <- get_macroeconomic_data(
  start_date = "2025-01-01",
  end_date   = "2025-12-01",
  latitude   = 55.6761, 
  longitude  = 12.5683
)

# 3. Use the data!
head(my_analysis_data)

# 4. Add Revenue
rev_data <- revenue_merge(my_analysis_data,"Scripts/revenue_data_2025.csv")
head(rev_data)

# 5. Plotting
plots = c("avg_temp", "max_temp", "tot_rain", "natural_disaster_score", "political_unrest_score","interest_rate","mortgage_rate", "unemployment", "cpi_inflation","gdp", "public_debt")

plotter(rev_data,
          cols = plots,
          date_col = "Date",
          revenue_col = "Revenue",
          ncol = 3,
          scale_method = "zscore")