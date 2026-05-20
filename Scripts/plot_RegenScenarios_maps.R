# ==============================================================================
# Script Name: Fig_RegenScenarios_maps.R
# Description: Processes spatial data (rasters and vectors) and generates a 
#              three-panel figure evaluating ecological 
#              regeneration strategies:
#                 (a) Large vs Small Forest Patches
#                 (b) Patch Connectivity (Least Cost Paths)
#                 (c) Water Availability/Suitability
# Author: [Kotikot et al]
# Date: May 2026
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Load Required Libraries
# ------------------------------------------------------------------------------
library(sp)          # Legacy spatial data handling
library(sf)          # Modern Simple Features vector data processing
library(terra)       # Advanced raster and vector data manipulation (replaces raster)
library(tidyterra)  # Provides ggplot2 geoms for terra SpatRaster and SpatVector objects
library(stars)       # Spatiotemporal arrays (used here for alternative raster structures)
library(ggplot2)     # Core data visualization framework
library(cowplot)     # Combining multiple ggplots into structured grids
library(grid)        # Low-level graphical adjustments

# ------------------------------------------------------------------------------
# 1. Environment Configurations & Data Ingestion
# ------------------------------------------------------------------------------
# Path to data
DATA_DIR   <- "../Data"
EXPORT_DIR <- "../Data/Processed"

# Load Narok County boundary shapefile (Vector)
# Note: Ensure 'crs_utm' is explicitly defined or extracted directly from the dataset
narok   <- sf::st_read(file.path(DATA_DIR, "ComplementaryFiles/narok_county_utm.shp"))

# Load the base environmental study extent (Raster)
ext_rast <- terra::rast(file.path(DATA_DIR, "ComplementaryFiles/extentRaster.tif"))
crs_utm <- crs(ext_rast)

# ==============================================================================
# 2. Section A: Forest Patches (Nodes) Classification
# ==============================================================================
# Load filtered base patches and broken down patch size rasters
forPatches <- terra::rast(file.path(DATA_DIR, "Processed/filtered_ForPatches.tif"))
smPatches   <- terra::rast(file.path(DATA_DIR, "Processed/smPatchs.tif"))
lgPatches   <- terra::rast(file.path(DATA_DIR, "Processed/lgPatchs.tif"))

# Reclassify large patches to integer ID = 2 so they don't overlap or conflict 
# with small patches (ID = 1) during the mosaic step
lgPatches <- terra::ifel(lgPatches == 1, 2, lgPatches)

# Combine small and large patch layers into a single categorical SpatRaster
allPtches <- terra::mosaic(smPatches, lgPatches)

# Construct and assign categorical attributes (factoring/levels) for the raster legend
allPtches_clsP   <- c("Small patches (<= 100 Ha)", "Large patches (> 100 Ha)")
allPtches_cats3P <- data.frame(ID = 1:2, LandCover = allPtches_clsP)
levels(allPtches) <- allPtches_cats3P


# ==============================================================================
# 3. Section B: Connectivity Links Classification
# ==============================================================================
# Load Least Cost Path (LCP) weight calculations from the MPG (Spatial Graphs) output
linksReal <- terra::rast(file.path(EXPORT_DIR, "forest2010_mpg/lcpPerimWeight.tif"))
lnkReal   <- sf::st_as_sf(terra::as.lines(linksReal)) # Convert raster lines to vector lines

# Establish a reclassification matrix to group path costs:
# format: c(from, to, new_value)
# Handles ecological constraints (e.g., land incentives vs crop restrictions)
m      <- c(-Inf, 500, 3,  300, 6000, 2,  6000, Inf, NA)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)
lnks1  <- terra::classify(linksReal, rclmat)

# Overlay patches and classified links; fun = "last" ensures links stack cleanly on top
LnksallPtches <- terra::mosaic(forPatches, lnks1, fun = "last")
LnksallPtches <- terra::ifel(LnksallPtches == 0, NA, LnksallPtches) # Convert background zeros to NA

# Define factor levels for the connectivity map categories
LnksallPtches_clsP   <- c("Forest patches", "Low cost connections", "Lower cost connections")
LnksallPtches_cats3P <- data.frame(ID = 1:3, LandCover = LnksallPtches_clsP)
levels(LnksallPtches) <- LnksallPtches_cats3P


# ==============================================================================
# 4. Section C: Water Pans Layout & Suitability Mapping
# ==============================================================================
# Load spatial locations of existing water pans and simulated strategically placed pans
wpans  <- sf::st_read(file.path(DATA_DIR, "Raw/shps/waterPans_utm.shp"))
wpansn <- sf::st_read(file.path(DATA_DIR, "Processed/strategicPans.shp"))

# ==============================================================================
# 5. Build Visualization Components (ggplot2 maps)
# ==============================================================================

# --- Plot C: Water Availability Map ---
wpansFig <- ggplot() +
  geom_sf(data = narok, fill = "transparent", color = "gray30", linewidth = 0.5) +
  geom_sf(data = wpans, aes(shape = "Existing water pans"), color = "#2C7FB8", size = 1, alpha = 0.9) +
  geom_sf(data = wpansn, aes(shape = "New strategically\n placed water pans"), color = "#F28E2B", size = 1, alpha = 0.9) +
  scale_shape_manual(
    name = "",
    values = c("Existing water pans" = 16, "New strategically\n placed water pans" = 16)
  ) +
  coord_sf(datum = NA) + # Drops coordinate system grid labels to keep look clean
  labs(title = "", x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major = element_line(color = "gray90"),
    legend.position  = c(0.2, 0.05),
    legend.text      = element_text(size = 18),
    plot.title       = element_text(face = "bold")
  )

# --- Plot A: Forest Patches Map ---
pches <- ggplot() +
  geom_spatraster(data = allPtches) +
  geom_spatvector(data = narok, fill = NA, color = 'grey30', linewidth = 0.5) +
  scale_fill_manual(
    values       = c('#d95f02', '#a6d854'), 
    name         = "",
    guide        = guide_legend(reverse = FALSE, ncol = 1),
    na.translate = FALSE
  ) +
  theme_bw() +
  theme(
    strip.background      = element_blank(),
    strip.text            = element_text(size = 15, color = "black"),
    panel.grid.major      = element_blank(),
    panel.grid.minor      = element_blank(),
    plot.title            = element_text(face = "bold", color = "black", size = 13, hjust = 0.5),
    legend.text           = element_text(size = 18),
    legend.position       = "inside",
    legend.position.inside = c(-0.1, 0.001), # Inset custom positioning
    legend.title          = element_blank(),
    legend.justification  = "left",
    legend.key.height     = unit(0.25, 'cm'),
    legend.key.width      = unit(1, 'cm'),
    legend.background     = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
    axis.title            = element_blank(),
    axis.text             = element_blank(),
    axis.ticks            = element_blank(),
    panel.border          = element_blank(),
    plot.background       = element_blank(),
    panel.background      = element_blank()
  )

# --- Plot B: Connectivity Links Map ---
lnks <- ggplot() +
  geom_spatraster(data = LnksallPtches) +
  geom_spatvector(data = narok, fill = NA, color = 'grey30', linewidth = 0.5) +
  scale_fill_manual(
    values       = c('#a6d854', '#984ea3', '#d95f02'), 
    name         = "",
    guide        = guide_legend(reverse = FALSE, ncol = 1),
    na.translate = FALSE
  ) +
  theme_bw() +
  theme(
    strip.background      = element_blank(),
    strip.text            = element_text(size = 15, color = "black"),
    panel.grid.major      = element_blank(),
    panel.grid.minor      = element_blank(),
    plot.title            = element_text(face = "bold", color = "black", size = 13, hjust = 0.5),
    legend.text           = element_text(size = 18),
    legend.position       = "inside",
    legend.position.inside = c(-0.1, 0.01),
    legend.title          = element_blank(),
    legend.justification  = "left",
    legend.key.height     = unit(0.25, 'cm'),
    legend.key.width      = unit(1, 'cm'),
    legend.background     = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent", colour = "transparent"),
    axis.title            = element_blank(),
    axis.text             = element_blank(),
    axis.ticks            = element_blank(),
    panel.border          = element_blank(),
    plot.background       = element_blank(),
    panel.background      = element_blank()
  )


# ==============================================================================
# 6. Multi-Panel Composite & Export Layout
# ==============================================================================

# Helper function to prevent long annotations from running off-screen
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

# Figure structural labels
my_label  <- "(a) Large vs small patches"
my_label2 <- "(b) Patch connectivity"
my_label3 <- "(c) Water availability"

# Combine individual plots side-by-side using cowplot::plot_grid
strategies <- plot_grid(pches, lnks, wpansFig, align = "h", nrow = 1) +
  annotate("text", x = 0.08, y = 0.70, size = 8, label = wrapper(my_label, width = 15)) +
  annotate("text", x = 0.41, y = 0.70, size = 8, label = wrapper(my_label2, width = 20)) +
  annotate("text", x = 0.75, y = 0.70, size = 8, label = wrapper(my_label3, width = 20)) +
  theme(plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"))

# Override clipping settings on the grid panel objects to make sure text 
# annotations aren't cropped during export 
gt <- ggplot_gtable(ggplot_build(strategies))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

# Export high-resolution TIFF file 
ggsave(
  filename    = "Fig_RegenScenarios_maps.tiff", 
  plot        = strategies,
  units       = 'px', 
  width       = 6500, 
  height      = 4500, 
  dpi         = 600, 
  compression = 'lzw' # LZW compression keeps files highly detailed but light
)