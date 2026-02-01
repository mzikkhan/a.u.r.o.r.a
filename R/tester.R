
# 1. Load the function from your other file
source("prelim.R")

# 2. Call the function with your specific inputs
my_analysis_data <- get_macroeconomic_data(
  start_date = "2026-01-01",
  end_date   = "2026-01-25",
  latitude   = 55.6761, 
  longitude  = 12.5683
)

# 3. Use the data!
head(my_analysis_data)
