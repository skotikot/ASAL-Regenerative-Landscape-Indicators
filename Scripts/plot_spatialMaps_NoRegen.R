# ==============================================================================
# Script Name: Fig_spatialMaps_NoRegen.R
# Description: Generates a 3-panel horizontal map composite
#              showing predictive land cover changes under different non-regeneration 
#              policy scenarios:
#                 (a) Baseline status quo projection
#                 (b) Traditional Group Ranch (GR) constraints
#                 (c) Privatization framework
# Author: Kotikot et al.
# Date: May 2026
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Load Required Libraries
# ------------------------------------------------------------------------------
library(sf)          # Vector feature processing framework
library(terra)       # Optimized raster manipulation engine 
library(ggplot2)     # Plotting and mapping visualization engine
library(tidyterra)  # Specialized ggplot2 map extensions for SpatRaster/SpatVector data
library(cowplot)     # High-level canvas grouping for multi-panel grids
library(grid)        # Primitive viewport geometry modifiers

# ------------------------------------------------------------------------------
# 1. Environment Configurations & Global Variables
# ------------------------------------------------------------------------------
# File pathways 
DATA_DIR        <- "../Data"
LCM_OUTPUTS_DIR <- "../Data/LCM_Outputs"

# Load essential context boundaries and layout references
extRast <- terra::rast(file.path(DATA_DIR, "ComplementaryFiles/extentRaster.tif"))
narok   <- sf::st_read(file.path(DATA_DIR, "ComplementaryFiles/narok_county_utm.shp"))

# Standard Land Cover Classification mapping table
cls3   <- c("Forest", "Rangeland", "Cropland", "Urban")
cats3  <- data.frame(ID = 1:4, LandCover = cls3)

# Color palette for Land Cover Categories
lc_pal <- c('green4', "yellow3", 'chocolate3', "darkred")

# ------------------------------------------------------------------------------
# 2. Raster Data Loading & Level Factoring
# ------------------------------------------------------------------------------
# Ingest predictive LCM (Land Change Modeler) rst outputs
basel <- terra::rast(file.path(LCM_OUTPUTS_DIR, "landcov_predict_stQuo_2020_15.rst"))
trad  <- terra::rast(file.path(LCM_OUTPUTS_DIR, "landcov_predict_stQuo_2020_15_TrdNoReg_17Nov_Sens1.rst"))
priv  <- terra::rast(file.path(LCM_OUTPUTS_DIR, "landcov_predict_stQuo_2020_15_PrivNoReg_17Nov_Sens1.rst"))

# Explicitly append category factors directly onto the spatial objects
levels(basel) <- cats3
levels(trad)  <- cats3
levels(priv)  <- cats3


# ==============================================================================
# 3. Base Custom Mapping Theme Engine
# ==============================================================================
# Functionalized theme configuration to minimize code repetition
theme_map_pro <- function() {
  theme_void() + 
    theme(
      plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 10)),
      legend.position = "bottom",
      legend.title    = element_text(size = 12, face = "bold"),
      legend.text     = element_text(size = 10),
      plot.margin     = margin(10, 10, 10, 10)
    )
}


# ==============================================================================
# 4. Map Generation Components (ggplot2 maps)
# ==============================================================================

# --- Map A: Baseline Prediction Layout ---
actualMap <- ggplot() +
  geom_spatraster(data = basel) +
  geom_spatvector(data = narok, fill = NA, color = "grey20", linewidth = 0.4) +
  scale_fill_manual(
    values       = lc_pal, 
    guide        = guide_legend(reverse = FALSE, ncol = 1),
    na.translate = FALSE
  ) +
  labs(title = "") +
  theme_map_pro() +
  theme(
    legend.text            = element_text(size = 20),
    legend.position        = "inside",
    legend.position.inside = c(0.85, 0.2), # Standardized inset positioning
    legend.title           = element_blank(),
    legend.key             = element_rect(colour = "transparent", fill = "transparent"),
    legend.background      = element_blank(),
    legend.justification   = "left",
    legend.key.height      = unit(0.25, 'cm'), 
    legend.key.width       = unit(1, 'cm')
  )

# --- Map B: Traditional/Group Ranch Scenario Layout ---
tradMap <- ggplot() +
  geom_spatraster(data = trad) +
  geom_spatvector(data = narok, fill = NA, color = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = lc_pal, name = " ", guide = 'none', na.translate = FALSE) +
  labs(title = "") +
  theme_map_pro()

# --- Map C: Privatization Scenario Layout ---
privMap <- ggplot() +
  geom_spatraster(data = priv) +
  geom_spatvector(data = narok, fill = NA, color = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = lc_pal, name = " ", guide = 'none', na.translate = FALSE) +
  labs(title = "") +
  theme_map_pro()


# ==============================================================================
# 5. Multi-Panel Composite Formatting & Canvas Export
# ==============================================================================

# Safe text alignment wrapper function for clean multi-line titles
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

# String configurations for panel label text blocks
my_label  <- "(a) Baseline prediction (Status quo)"
my_label2 <- "(b) Group ranch (GR) scenario"
my_label3 <- "(c) Privatization scenario"

# Compile individual panels into a unified multi-plot layout framework via cowplot
spatPanel <- plot_grid(actualMap, tradMap, privMap, align = "h", nrow = 1) +
  annotate("text", x = 0.08, y = 0.74, size = 8, label = wrapper(my_label, width = 15)) +
  annotate("text", x = 0.41, y = 0.74, size = 8, label = wrapper(my_label2, width = 20)) +
  annotate("text", x = 0.76, y = 0.74, size = 8, label = wrapper(my_label3, width = 20)) +
  theme(plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"))

# Override inner plot clipping flags to allow annotations to safely render outside panel bounds
gt <- ggplot_gtable(ggplot_build(spatPanel))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

# Save the finalized visualization to high-resolution publication space
ggsave(
  filename    = "Fig_SpatialMaps_NoRegen.tiff", 
  plot        = spatPanel,
  units       = 'px',
  width       = 8500,
  height      = 4000, 
  dpi         = 600,
  compression = 'lzw'
)
