
# =============================
# R SCRIPT TEMPLATE: Forest Proportion Calculation with Visualization
# Purpose: Calculate proportion of rangeland that is within 500m of a forest patch 
# across multiple scenario maps and save results to a csv file.
# Author: [Kotikot et al]
# Date: May 2026
# =============================

# Required Packages
library(terra)
library(tools)
library(ggplot2)
library(readr)

# -----------------------------
# USER INPUTS
# -----------------------------
source("Load_scenarioOutputs.R") #this loads all the model projections for all the scenarios. 
#These are grouped into 3 groups[sens1-25% transition adjustment, sens2-50%, sens3-75%]
EXPORT_DIR <- "../Data/Analysis_Outputs"

rangefUN <- function(r, radius){
  
  # Assume forest = 1, other = 0
  forest <- r == 1
  forest <- ifel(forest == 0, 0, 1)
  
  # Create circular weight matrix
  w <- focalMat(forest, radius, type = "circle", fillNA=FALSE)
  w[w > 0] <- 1
  
  # Calculate proportion of forest within radius
  forest_prop <- focal(forest, w, fun = function(x) sum(x, na.rm = TRUE) / length(x))
  plot(forest_prop)
  
  # mask rangeland areas
  range <- r == 2
  range <- ifel(range == 0, NA, 1)
  plot(range)
  
  #mask
  forest_prop_range <- mask(forest_prop, range)
  plot(forest_prop_range)
  
  #get pixels with more than 10% forest cover
  forest_prop_range2 <- forest_prop_range > 0 #any forest cover
  forest_prop_range2 <- ifel(forest_prop_range2 == 0, NA, 1)
  plot(forest_prop_range2)
  
  pixel_counts <- freq(forest_prop_range2)
  cell_area <- 8100
  pixel_counts$nearForArea <- (pixel_counts$count * cell_area)/1000000 #km2
  
  #percent of all range
  pixel_counts2 <- freq(range)
  cell_area <- 8100
  pixel_counts2$rangeArea <- (pixel_counts2$count * cell_area)/1000000 #km2
  
  # Save result raster if desired
  #output_path <- file.path(EXPORT_DIR, paste0(timestep, "_forest_prop_", radius,  ".tif"))
  #writeRaster(forest_prop_range, output_path, overwrite = TRUE)
  
  # Initialize dataframe for summary stats
  stats <- data.frame(TimeStep = character(), MeanProp = numeric(), stringsAsFactors = FALSE)
  
  # Compute mean proportion for trend analysis
  mean_val <- global(forest_prop_range, mean, na.rm = TRUE)[[1]]
  stats <- rbind(stats, data.frame(TimeStep = timestep, MeanProp = mean_val))
  stats$nearForArea1 <- pixel_counts$nearForArea
  stats$rangeArea <- pixel_counts2$rangeArea
  stats$propRange <- stats$nearForArea1/stats$rangeArea
  stats$radii <- radius
  
  return(stats)
  
}

radii = c(500) #Specify other radii for sensitivity tests

statsAll <- data.frame(matrix(ncol = 6, nrow = 0))
names(statsAll) <- c("TimeStep" ,  "MeanProp" ,  "nearForArea1", "rangeArea" ,  "propRange",   "radii")

indata <- lst_sens1 # calculate for lowest transition adjustment (sens1-25%)[sens2-50%, sens3-75%]

for (i in 1:length(indata)){
  
  timestep <- names(indata)[i]
  
  r <- indata[[i]]
  
  for (j in 1:length(radii)){
    
    outStat <- rangefUN(r, radii[j])
    
    statsAll <- rbind(statsAll, outStat)
    
  }
  
}

write_csv(statsAll, paste0(EXPORT_DIR,'/Forest_rangeland_adjacency_sens1', '.csv'))

