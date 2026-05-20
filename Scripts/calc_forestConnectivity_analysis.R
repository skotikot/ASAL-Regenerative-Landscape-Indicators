#' -----------------------------------------------------------------------------
#' Project: LCM Scenario Analysis - Narok County
#' Script: Forest Connectivity Resistance Surfaces & MPG Generation
#' Author: Kotikot et al.
#' -----------------------------------------------------------------------------

# Load Libraries ---------------------------------------------------------------
library(sf)
library(terra)
library(grainscape)
library(dplyr)

#================================================================================================================

export_path <- "../data/Processed"

# 1. Setup & Helper Functions --------------------------------------------------

# Use EPSG codes for clarity
crs_gcs <- "EPSG:4326"  # WGS84
crs_utm <- "EPSG:32737" # UTM 37S

#' Helper function to reclassify rasters based on a threshold vector
#' @param r SpatRaster object
#' @param thresholds Numeric vector of c(min, max, becomes)
#' @param mask_layer SpatRaster to mask the output
reclass_and_mask <- function(r, thresholds, mask_layer) {
  rcl_mat <- matrix(thresholds, ncol = 3, byrow = TRUE)
  r_masked <- terra::mask(r, mask_layer)
  return(terra::classify(r_masked, rcl_mat))
}

#================================================================================================================
# 2. Data Loading & Coordinate Alignment ---------------------------------------

# Vector Data
narok <- st_read("../Data/ComplementaryFiles/narok_county_utm.shp") %>% st_transform(crs_utm)
#narok_1km <- st_read("../data/rawData/shps/narok_county_utm_1kmBff.shp") %>% st_transform(crs_utm)

# Reference Extent
ext_rast <- terra::rast("../Data/ComplementaryFiles/extentRaster.tif")

# 3. Factor Reclassification (Resistance Components) ---------------------------

# Precipitation
precip <- terra::rast("../Data/Processed/prec_annAvg09.tif")
precip_rcl <- reclass_and_mask(precip, c(-Inf, 700, 5, 700, 900, 4, 900, 1100, 3, 1100, 1300, 2, 1300, 3000, 1, 3000, Inf, NA), ext_rast)

# Dist to Pans
dpans <- terra::rast("../Data/Processed/d_pans.tif")
dpans_rcl <- reclass_and_mask(dpans, c(-Inf, 500, 1, 500, 1000, 2, 1000, 5000, 3, 5000, 20000, 4, 20000, 50000, 5, 50000, Inf, NA), ext_rast)

# Dist to Cropland
d_crops <- terra::rast("../Data/Processed/dist_crop2010Lines.tif")
d_crops_rcl <- reclass_and_mask(d_crops, c(-Inf, 200, 1, 200, 500, 2, 500, 5000, 3, 5000, 20000, 4, 20000, 80000, 5, 80000, Inf, NA), ext_rast)

# Land Use (Reclassify Forest/Range/Crop/Urban)
lu10 <- terra::rast("../Data/Processed/lu2010_4cls_90m.tif")
lu_rcl <- reclass_and_mask(lu10, c(-Inf, 1, 0, 1, 2, 1, 2, 3, 2, 3, 4, 3, 4, Inf, NA), ext_rast)

# Dist to River, Forest, and Slope
d_riv <- reclass_and_mask(rast("../Data/Processed/d_rivers.tif"), c(-Inf, 200, 1, 200, 1000, 2, 1000, 2000, 3, 2000, 5000, 4, 5000, 15000, 5, 15000, Inf, NA), ext_rast)
d_for <- reclass_and_mask(rast("../Data/Processed/dist_forest2010.tif"), c(-Inf, 500, 1, 500, 1000, 2, 1000, 2000, 3, 2000, 3000, 4, 3000, 15000, 5, 15000, Inf, NA), ext_rast)
slp   <- reclass_and_mask(rast("../Data/Processed/slp.tif"), c(-Inf, 10, 1, 10, 20, 2, 20, 30, 3, 30, 50, 4, 50, 100, 5, 100, Inf, NA), ext_rast)

#================================================================================================================

# 4. Resistance Surface Assembly -----------------------------------------------

# Sum components and mosaic with forest cover (Forest = 1)
factor_sum <- terra::app(c(precip_rcl, dpans_rcl, d_crops_rcl, d_riv, slp, lu_rcl, d_for), fun = sum)
fcover <- classify(lu10 == 1, matrix(c(-Inf, 0, NA, 0, 1, 1, 1, Inf, NA), ncol = 3, byrow = TRUE)) #forest

# Resistance Surface: Mosaic ensures forest cover (1) overwrites background resistance
res_surface_terra <- terra::mosaic(factor_sum, fcover, fun = "last")

#================================================================================================================
# 5. Grainscape Connectivity Analysis ------------------------------------------

# grainscape currently requires RasterLayer objects (older format)
res_surface_raster <- raster::raster(res_surface_terra)

# Filter for patches (minimum 3 cells)
filtered_patch <- patchFilter(res_surface_raster == 1, cells = 3)
filtered_patch_terra <- ifel(terra::rast(filtered_patch) == 0, NA, 1)
#writeRaster(filtered_patch_terra, "../Data/Processed/filtered_ForPatches.tif")

# Build Minimum Cost Path Graph (MPG)
final_res_raster <- raster::raster(terra::mosaic(factor_sum, filtered_patch_terra, fun = "last"))
forest_mpg <- MPG(cost = final_res_raster, patch = filtered_patch)

# 6. Extraction & Export -------------------------------------------------------

export(forest_mpg, dirname = "forest2010_mpg", path = export_path, overwrite = TRUE)#save
#================================================================================================================

# 7. Post-Processing & Incentive Scenarios -------------------------------------

# Short Links Filter (Links < 500m)
links_lcp <- terra::rast(file.path(export_path, "forest2010_mpg/lcpPerimWeight.tif"))
short_links <- terra::classify(links_lcp, matrix(c(-Inf, 500, 1, 500, Inf, NA), ncol = 3, byrow = TRUE))
#writeRaster(short_links, "../Data/Processed/shrtLnks.tif")

# # Incentive Layers (Distance to short links)
dist_shrt <- terra::rast("../Data/Processed/dist_shrtLnks.tif") #Calculated externally using saved "shrtLnks.tif"
incentive_125 <- terra::classify(dist_shrt, matrix(c(-Inf, 200, 1.25, 200, 20000, 1, 20000, Inf, NA), ncol = 3, byrow = TRUE))
#writeRaster(incentive_125, "../Data/Processed/shrtLnks125_CF_RF.tif", overwrite=TRUE) (LCM input)

#---------------------Nodes filter
ptchs <- patchFilter(final_res_raster == 1, area = 1000000)
m <- c(-Inf,0, 0,  0, 1,1, 1, Inf, NA)
rclmat <- matrix(m, ncol=3, byrow=TRUE)
ptchs1 <- classify(terra::rast(ptchs), rclmat)
ptchs2 <- ifel(ptchs1 == 0, NA, 1)

allPtchs <- terra::classify(terra::rast(filtered_patch), matrix(c(-Inf, 0, 0, 0, Inf, 1), ncol = 3, byrow = TRUE))
smPatchs <- allPtchs - ptchs1
smPatchs1 <- ifel(smPatchs == 0, NA, 1)

#writeRaster(smPatchs1, "../Data/Processed/smPatchs.tif") #small patches
#writeRaster(ptchs2, "../Data/Processed/lgPatchs.tif") #large patches



