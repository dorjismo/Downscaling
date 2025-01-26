library(terra)
library(raster)
library(foreach)
library(doParallel)

# Define the generalized downscaling function
downscale <- function(variable, obs_file, future_pattern, output_dir, crs_proj, reorder_indices) {
  # Load observational data
  obs <- terra::rast(obs_file)
  terra::crs(obs) <- crs_proj
  obs <- terra::flip(obs)
  
  # Reorder observation layers if specified
  if (!is.null(reorder_indices)) {
    obs <- obs[[reorder_indices]]
  }
  
  # Read future projection files
  mod_files <- list.files("D:/CMIP6_downscale/TPS/", 
                          pattern = future_pattern, full.names = TRUE)
  mod_rast <- terra::rast(mod_files)
  
  # Perform calculation based on the variable
  if (variable == "pr") {
    downscaled <- obs * (1 + mod_rast)  # Precipitation anomaly
  } else {
    downscaled <- obs + mod_rast  # Temperature anomaly (tasmax, tasmin)
  }
  
  # Assign names to layers
  names(downscaled) <- names(mod_rast)
  
  # Convert SpatRaster to RasterBrick
  downscaled_brick <- raster::brick(downscaled)
  
  # Set up parallel processing
  cl <- makeCluster(10)
  registerDoParallel(cl)
  
  # Process each layer in parallel
  foreach(i = 1:nlayers(downscaled_brick)) %dopar% {
    layer <- raster::raster(downscaled_brick, i)
    output_filename <- paste0(output_dir, "/", gsub("\\.", "-", names(downscaled_brick[[i]])), ".tif")
    raster::writeRaster(layer, filename = output_filename, format = "GTiff", overwrite = TRUE)
  }
  
  stopCluster(cl)
  message("Processing complete for variable: ", variable)
}

# Define parameters for each variable
variables <- list(
  list(
    variable = "pr",
    obs_file = "./Climate Downscaling Bhutan/Delta method/CSIRO Climate Data Bhutan/Main data/precip_3ds_normals_19862015.nc",
    future_pattern = "^pr.*\\.tif$",
    reorder_indices = c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9),
    output_dir = "D:/CMIP6_downscale/Downscale/Precipitation"
  ),
  list(
    variable = "tasmax",
    obs_file = "./Climate Downscaling Bhutan/Delta method/CSIRO Climate Data Bhutan/Main data/tmax_3ds_m_normals_19862015.nc",
    future_pattern = "^tasmax.*\\.tif$",
    reorder_indices = c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9),
    output_dir = "D:/CMIP6_downscale/Downscale/Maximum temperature"
  ),
  list(
    variable = "tasmin",
    obs_file = "./Climate Downscaling Bhutan/Delta method/CSIRO Climate Data Bhutan/Main data/tmin_3ds_tm_normals_19862015.nc",
    future_pattern = "^tasmin.*\\.tif$",
    reorder_indices = c(4, 8, 12, 2, 1, 7, 6, 3, 5, 11, 10, 9),
    output_dir = "D:/CMIP6_downscale/Downscale/Minimum temperature"
  )
)

# CRS projection definition
druk <- "+proj=tmerc +lat_0=0 +lon_0=90 +k=1 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"

# Run the downscaling process for each variable
for (params in variables) {
  downscale(
    variable = params$variable,
    obs_file = params$obs_file,
    future_pattern = params$future_pattern,
    output_dir = params$output_dir,
    crs_proj = druk,
    reorder_indices = params$reorder_indices
  )
}
