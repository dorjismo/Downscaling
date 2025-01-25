# Calculate anomalies
# set working directory
setwd("C:/Users/s4597131/OneDrive - The University of Queensland/Documents/Doctor of Philosophy UQ/")

# Load required libraries
library(terra)
library(stringr)

# Function to calculate and save anomalies
anomaly <- function(model, variable, scenarios, periods, base_path, output_path) {
  # Historical file pattern
  hist_pattern <- str_c(variable, "_day_", model, "_historical_.*normal.*\\.nc$")
  hist_file <- list.files(base_path, full.names = TRUE, pattern = hist_pattern)
  
  # Check if historical file exists
  if (length(hist_file) == 0) {
    message("No historical file found for model: ", model, ", variable: ", variable)
    return()
  }
  
  # Read the historical raster
  hist_rast <- terra::rast(hist_file)
  
  # Loop through scenarios and periods to calculate anomalies
  for (scenario in scenarios) {
    for (period in periods) {
      # Future file pattern
      fut_pattern <- str_c(variable, "_day_", model, ".*", scenario, ".*", period, ".*normal.*\\.nc$")
      fut_file <- list.files(base_path, full.names = TRUE, pattern = fut_pattern)
      
      # Check if future file exists
      if (length(fut_file) == 0) {
        message("No future file found for model: ", model, ", variable: ", variable, 
                ", scenario: ", scenario, ", period: ", period)
        next
      }
      
      # Read the future raster
      fut_rast <- terra::rast(fut_file)
      
      # Calculate anomaly based on the variable
      if (variable == "pr") {
        anomaly <- (fut_rast - hist_rast) / hist_rast
      } else if (variable %in% c("tasmax", "tasmin")) {
        anomaly <- fut_rast - hist_rast
      }
      
      # Generate output file name with selective period replacement
      raw_file_name <- str_c(model, "_", variable, "_", scenario, "_", period, ".tif")
      cleaned_period <- stringr::str_replace_all(period, "_", "-")  # Replace underscores only in period
      cleaned_file_name <- str_c(model, "_", variable, "_", scenario, "_", cleaned_period, ".tif")
      output_file <- file.path(output_path, cleaned_file_name)
      
      # Save the anomaly raster
      terra::writeRaster(anomaly, filename = output_file, overwrite = TRUE)
      message("Saved anomaly raster: ", output_file)
    }
  }
}

# Define models, variables, scenarios, and periods
models <- c("ACCESS-CM2", "CNRM-CM6-1", "CNRM-ESM2-1", "INM-CM4-8", 
            "INM-CM5-0", "MIROC-ES2L", "MIROC6", "MPI-ESM1-2-LR", "MRI-ESM2-0", "NorESM2-MM")
variables <- c("pr", "tasmax", "tasmin")
scenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")
periods <- c("2021_2050", "2051_2080", "2071_2100")

# Base and output paths
base_path <- "./Delta_downscaling/GCM_Regrid/Regrid_time_slice/"
output_path <- "D:/CMIP6_downscale/"

# Ensure output directory exists
if (!dir.exists(output_path)) dir.create(output_path, recursive = TRUE)

# Loop through all models and variables
for (model in models) {
  for (variable in variables) {
    anomaly(model, variable, scenarios, periods, base_path, output_path)
  }
}
