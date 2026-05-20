library(dplyr)
library(tidyr)
library(terra)

#######################################################################################################
#Load LCM Scenario Outputs
#======================================================================================================
# 1. Define File Paths and Names 
data_path <- "../Data/LCM_Outputs/"

pattern <- "landcov_predict_stQuo_2020_15\\.rst$|17Nov_Sens[1-3]\\.rst$"

# Execute search
files <- list.files(path = data_path, 
                    pattern = pattern, 
                    full.names = TRUE)

# 2. Load all rasters into a list and immediately replace 0 with NA
raster_list <- lapply(files, function(f) {
  r <- terra::rast(f)
  return(terra::classify(r, cbind(0, NA)))
})

#name the files
names(raster_list) <- basename(files) %>% gsub(".rst", "", .)
r_list <- raster_list

# 3. Create Sensitivity Groups 
extract_sens <- function(pattern, r_list) {
  # 1. Identify the baseline key
  # We use a pattern match for the baseline too, in case of extension differences
  base_idx <- grep("landcov_predict_stQuo_2020_15$", names(r_list))
  
  # 2. Identify the sensitivity matches
  match_indices <- grep(pattern, names(r_list))
  
  # 3. Combine indices and subset the list
  combined_indices <- unique(c(base_idx, match_indices))
  
  return(r_list[combined_indices])
}

#Group by sensitivity levels 1-3
lst_sens1 <- extract_sens("Sens1", raster_list)
lst_sens2 <- extract_sens("Sens2", raster_list)
lst_sens3 <- extract_sens("Sens3", raster_list)

names(lst_sens1) <- c('basel', 'privNS1', 'priv_allForConn1','priv_allForConn_wpans1',
                        'priv_allForConn_wpans_nRC1', 'priv_LPtc1', 'priv_shrtLnks1',
                        'priv_sPtc1', 'priv_wpans1',
                        'tradN1', 'trad_allForConn1','trad_allForConn_wpans1',
                        'trad_allForConn_wpans_nRC1', 'trad_LPtc1', 'trad_shrtLnks1',
                        'trad_sPtc1', 'trad_wpans1')

names(lst_sens2) <- c('basel', 'privNS2', 'priv_allForConn2','priv_allForConn_wpans2',
                        'priv_allForConn_wpans_nRC2', 'priv_LPtc2', 'priv_shrtLnks2',
                        'priv_sPtc2', 'priv_wpans2',
                        'tradN2', 'trad_allForConn2','trad_allForConn_wpans2',
                        'trad_allForConn_wpans_nRC2', 'trad_LPtc2', 'trad_shrtLnks2',
                        'trad_sPtc2', 'trad_wpans2')

names(lst_sens3) <- c('basel', 'privNS3', 'priv_allForConn3','priv_allForConn_wpans3',
                        'priv_allForConn_wpans_nRC3', 'priv_LPtc3', 'priv_shrtLnks3',
                        'priv_sPtc3', 'priv_wpans3',
                        'tradN3', 'trad_allForConn3','trad_allForConn_wpans3',
                        'trad_allForConn_wpans_nRC3', 'trad_LPtc3', 'trad_shrtLnks3',
                        'trad_sPtc3', 'trad_wpans3')


#End - Load LCM Scenario Outputs
#======================================================================================================
























