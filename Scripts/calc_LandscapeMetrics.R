#' -----------------------------------------------------------------------------
#' Project: Landscape Metrics Analysis for LCM Scenarios
#' Script: Calculate and Format Class/Landscape Metrics
#' Author: Kotikot et al.
#' Date: May 2026
#' -----------------------------------------------------------------------------

# Load Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(landscapemetrics)
library(terra)
library(readr)

# 1. Data Loading & Pre-processing ---------------------------------------------

data_path <- "../Data/LCM_Outputs/"
output_folder <- "../Data/Analysis_Outputs" 
pattern   <- "landcov_predict_stQuo_2020_15\\.rst$|17Nov_Sens[1-3]\\.rst$"

files <- list.files(path = data_path, pattern = pattern, full.names = TRUE)

# Load rasters, classify 0 as NA, and set uniform NA flag for landscapemetrics
raster_list <- lapply(files, function(f) {
  r <- terra::rast(f)
  r <- terra::classify(r, cbind(0, NA))
  terra::NAflag(r) <- 99
  return(r)
})

# Standardize names by removing extension
names(raster_list) <- gsub(".rst", "", basename(files))

# 2. Grouping Sensitivity Scenarios --------------------------------------------

#' Helper function to extract groups containing a baseline and specific sensitivity
extract_sens <- function(sens_pattern, r_list) {
  base_idx <- grep("landcov_predict_stQuo_2020_15$", names(r_list))
  match_indices <- grep(sens_pattern, names(r_list))
  return(r_list[unique(c(base_idx, match_indices))])
}

# Generate sensitivity lists
lst_sens1 <- extract_sens("Sens1", raster_list)
lst_sens2 <- extract_sens("Sens2", raster_list)
lst_sens3 <- extract_sens("Sens3", raster_list)

# Define clean short-names for scenarios 
scenario_names <- c(
  'basel', 'privNS', 'priv_allForConn','priv_allForConn_wpans',
  'priv_allForConn_wpans_nRC', 'priv_LPtc', 'priv_shrtLnks',
  'priv_sPtc', 'priv_wpans', 'tradN', 'trad_allForConn',
  'trad_allForConn_wpans', 'trad_allForConn_wpans_nRC', 
  'trad_LPtc', 'trad_shrtLnks', 'trad_sPtc', 'trad_wpans'
)

# Apply names with suffix to maintain distinction across sensitivity levels
names(lst_sens1) <- paste0(scenario_names, "1")
names(lst_sens2) <- paste0(scenario_names, "2")
names(lst_sens3) <- paste0(scenario_names, "3")

# 3. Metric Calculation --------------------------------------------------------

# Combine into a single master list for batch calculation
narokt <- do.call(c, list(lst_sens1, lst_sens2, lst_sens3))

# Calculate Class-level metrics
#metrics_class <- c("lsm_c_pland", "lsm_c_ed", "lsm_c_contig_mn", "lsm_c_pd", "lsm_c_cohesion")
metrics_class <- c("lsm_c_ed", "lsm_c_pland")
narok.c8 <- calculate_lsm(narokt, what = metrics_class, progress = TRUE, neighbourhood = 8, directions = 8)

# Calculate Landscape-level metrics
#metrics_land <- c("lsm_l_ed", "lsm_l_shdi", "lsm_l_prd", "lsm_l_iji", "lsm_l_cohesion")
metrics_land <- c("lsm_l_ed")
narok.l8 <- calculate_lsm(narokt, what = metrics_land, progress = TRUE, neighbourhood = 8, directions = 8)

# 4. Data Tidying & Formatting -------------------------------------------------

# Define a lookup vector for class names (Professional approach)
class_names <- c(
  "1" = "Forest",
  "2" = "Rangeland",
  "3" = "Cropland",
  "4" = "Urban"
)

# Mapping layer indices to names
layer_map <- data.frame(
  layer = 1:length(narokt),
  layer_name = names(narokt)
)

format_metrics <- function(df, mapping) {
  df %>%
    left_join(mapping, by = "layer") %>%
    # Split "tradN1" into "tradN" and "1"
    separate(layer_name, into = c("policy", "sensit"), sep = "(?<=[A-Za-z])(?=[0-9])", fill = "right") %>%
    mutate(
      across(c(policy, sensit, metric), as.factor),
      # Fix baseline NAs to "0"
      sensit = if_else(is.na(sensit), factor("0"), sensit)
    )
}

# Apply formatting and map the class names
narok.c8_ed <- narok.c8
narok.c8_ed <- format_metrics(narok.c8_ed, layer_map) %>%
  mutate(class = class_names[as.character(class)],# Mapping by index/name vector
         sensit = if_else(policy == "basel", "0", sensit)) 
  
narok.l8_ed <- narok.l8
narok.l8_ed <- format_metrics(narok.l8_ed, layer_map) %>% 
  mutate(sensit = if_else(policy == "basel", "0", sensit))

# 5. Export --------------------------------------------------------------------

if(!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

write_csv(narok.c8_ed, file.path(output_folder, "class_Metrics.csv"))
write_csv(narok.l8_ed, file.path(output_folder, "landscape_Metrics.csv"))

message("Analysis Complete. Files saved to: ", output_folder)
