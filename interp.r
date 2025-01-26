# Thin Plate Spline interpolation
# Load libraries
library(sp)
library(terra)
library(fields)

druk <- "+proj=tmerc +lat_0=0 +lon_0=90 +k=1 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"

bt <- vect("./Dzongkhag Boundary/Dzongkhag_projected.shp")
# plot(bt, add=T)

#...............................................................................
# Common to all
# Prepare a template for resampling to 250 m (Here precip observation is used)
resamp_tem <- terra::rast("./Climate Downscaling Bhutan/Delta method/CSIRO Climate Data Bhutan/Main data/precip_3ds_normals_19862015.nc") # Just taken pr for resampling. Any variable can be used
terra::crs(resamp_tem) <- "+proj=tmerc +lat_0=0 +lon_0=90 +k=1 +x_0=250000 +y_0=0 +ellps=GRS80 +units=m +no_defs"
resamp_tem <- terra::flip(resamp_tem) # csiro netcdf gets are flipped

# Prepare empty spatraster to store interpolated values. Using DEM as a template for extent and resolution
# Read in DEM and project to 5266 (Bhutan)
dem <- terra::rast("./Climate Downscaling Bhutan/CMIP6_analysis/GCM_Regrid/Elevation/elevation.tif") # Read in dem
dem_proj <- terra::project(dem, druk) # Using "druk" maintains the same as it is except resolution 
#plot(dem_proj)

# Now resample DEM to 250 to match with reference resolution
dem_proj_resamp <- terra::resample(dem_proj, resamp_tem) # resample to 250 m
dem_proj_resamp <- crop(dem_proj_resamp, bt, mask = TRUE)
#plot(dem_proj_resamp)

# Now empty spatraster. "dem_proj_resamp" is assigned to a new object "emp_raster" not to change the original layer
emp_raster <- dem_proj_resamp # Assigning to a new object so that I don't have to keep on running above code
emp_raster [] <- NA # Assign "NAs" to all cells to be replaced by interpolated values
emp_raster <- terra::rast(emp_raster) # Indicate as spatraster although above gives as spatraster. This way works in the code below

#...............................................................................
# Function to perform thin plate spline interpolation for a given model
# first get coordinates (since all gcms have been regridded to same, only one coordinate used)
lf <- list.files("D:/CMIP6_downscale/Anomaly/", 
                 pattern = "tasmax.*\\.tif$",
                 full.names = T)
lf_rast <- rast(lf[1])
gcm_coords <- as.data.frame(crds(lf_rast[[1]]))

# Convert lon lat to xy
library(sf)

# Convert bt_pres to an sf object with the initial CRS
bt_pres <- st_as_sf(gcm_coords, coords = c("x", "y"), crs = 4326) # Replace with actual column names

# Transform to the desired CRS using the EPSG code directly
bt_pres <- st_transform(bt_pres, crs = 5266)

# Convert back to a data frame if needed
bt_pres <- as.data.frame(st_coordinates(bt_pres))

# anomaly list
ano_list <- list.files(anomaly_dir, pattern = paste0(model_name, ".*\\.tif$"), full.names = TRUE)

# Directory setup (modify these paths as needed)
anomaly_dir <- "D:/CMIP6_downscale/Anomaly/"
output_dir <- "D:/CMIP6_downscale/TPS"
crs <- "+init=epsg:5266"

# Give model names
models <- c("ACCESS-CM2", "CNRM-ESM2-1","CNRM-CM6-1","INM-CM4-8", "INM-CM5-0",
            "MIROC-ES2L", "MIROC6","MPI-ESM1-2-LR", "MRI-ESM2-0", "NorESM2-MM")

#........................................
tps_int <- function(model_name, anomaly_dir, output_dir, crs) {
  # List all anomaly files for the model
  ano_list <- list.files(anomaly_dir, pattern = model_name, full.names = TRUE)
  
  # Iterate over each anomaly file
  for (ano_file in ano_list) {
    ano_rast <- rast(ano_file)  # Load the raster
    
    # Check if raster has 12 layers
    if (nlyr(ano_rast) != 12) {
      warning("File ", ano_file, " does not have 12 layers. Skipping.")
      next
    }
    
    # Reproject the raster to the target CRS
    ano_rast_proj <- project(ano_rast, crs)
    
    # Initialize a list to store interpolated layers
    interpolated_layers <- list()
    
    # Loop through each layer (12 layers in the raster)
    for (layer_idx in 1:nlyr(ano_rast_proj)) {
      # Extract the specific layer
      layer <- ano_rast_proj[[layer_idx]]
      
      # Extract raster values for interpolation
      extracted_values <- terra::extract(layer, bt_pres, ID = FALSE)
      
      # Check if extraction returned values
      if (is.null(extracted_values)) {
        message("No extracted values for layer ", layer_idx, " in file: ", ano_file)
        next
      }
      
      # Perform Thin Plate Spline (TPS) interpolation
      tps <- Tps(bt_pres, extracted_values)
      tps_int <- interpolate(emp_raster, tps, xyOnly = TRUE)
      
      # Add the interpolated layer to the list
      interpolated_layers[[layer_idx]] <- tps_int
    }
    
    # Stack the 12 interpolated layers
    interpolated_stack <- rast(interpolated_layers)
    
    # Ensure the names of the stack match the original raster
    if (nlyr(interpolated_stack) == nlyr(ano_rast_proj)) {
      names(interpolated_stack) <- month.name
    } else {
      warning("Mismatch in number of layers for file: ", ano_file)
    }
    
    # Generate output filename and save the stack
    output_filename <- file.path(output_dir, basename(ano_file))
    writeRaster(interpolated_stack, filename = output_filename, overwrite = TRUE)
    message("Saved interpolated stack: ", output_filename)
  }
}

# Apply the function to each model
for (model_name in models) {
  tps_int(model_name, anomaly_dir, output_dir, crs)
}
