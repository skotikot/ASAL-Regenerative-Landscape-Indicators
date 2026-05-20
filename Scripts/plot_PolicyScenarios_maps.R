# ==============================================================================
# Script Name: Fig_PolicyScenarios_maps.R
# Description: Processes policy spatial files and generates a 3-panel 
#               figure showing land policy scenario variants:
#                 (a) Baseline Policy (Status Quo)
#                 (b) Group Ranch (GR) Scenario
#                 (c) Privatization Scenario
# Author: Kotikot et al. 
# Date: May 2026
# ==============================================================================

# ------------------------------------------------------------------------------
# 0. Load Required Libraries
# ------------------------------------------------------------------------------
library(sp)          # Legacy spatial data frameworks
library(sf)          # Modern vector data processing (Simple Features)
library(terra)       # Advanced engine for raster operations
library(tidyterra)  # Seamless ggplot2 integration for terra spatial objects
library(stars)       # Used for rasterizing sf vectors directly
library(ggplot2)     # Core plotting visualization engine
library(cowplot)     # Grid layout composition tool
library(grid)        # Primitive graphical objects adjustments

# ------------------------------------------------------------------------------
# 1. Environment Configurations & Data Ingestion
# ------------------------------------------------------------------------------
# Paths structured cleanly for easy portability across environments
DATA_DIR   <- "../Data"
EXPORT_DIR <- "../Data/Processed"

# Load Narok County boundary shapefile (Vector framework)
narok   <- sf::st_read(file.path(DATA_DIR, "ComplementaryFiles/narok_county_utm.shp"))
crs_utm <- sf::st_crs(narok) 

# Load standard environment extent reference raster
ext_rast <- terra::rast(file.path(DATA_DIR, "ComplementaryFiles/extentRaster.tif"))


# ==============================================================================
# 2. Base Policy Shapefile Processing & Alignment
# ==============================================================================
# Load policy-specific zone layout vectors
pols <- sf::st_read(file.path(DATA_DIR, "Raw/shps/policyShapeFile.shp"))

# Transform coordinate system to match the base environmental raster extent
pols <- sf::st_transform(pols, terra::crs(ext_rast))

# Isolate internal structural ID attribute 
pols <- pols[, c("id2")]

# Rasterize vector layer using stars package infrastructure
pols_r <- stars::st_rasterize(pols)

# Align boundaries, matching origin and pixel layout via extend and resample steps
polsex  <- terra::extend(terra::rast(pols_r), terra::ext(ext_rast))
polsres <- terra::resample(polsex, ext_rast, method = 'near') # Use 'near' for categorical indices

# Mask raster values to match exact boundary constraint lines of the study extent
polcy <- terra::mask(polsres, ext_rast)

# Establish and attach factor attribute mapping tables to the core policy raster
cls_polcy  <- c("Private", "Group ranch (current)", "Group ranch (Historical)", 
                "Conservancy", "Mau Forest (PA)", "Game reserve (PA)")
cats_polcy <- data.frame(ID = 1:6, LandCover = cls_polcy)
levels(polcy) <- cats_polcy


# ==============================================================================
# 3. Scenario Matrix Logic & Classification
# ==============================================================================

# --- Scenario 1: Traditional Layout (Status quo, no extra strategies) ---
# Format: c(from, to, value)
m      <- c(-Inf, 1, 1,   1, 4, 2,   4, 5, 3,   5, 6, 4,   6, Inf, NA)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)
Tpol1  <- terra::classify(polcy, rclmat)

clsT   <- c("Private", "Group ranch", "Mau Forest (PA)", "Game reserve (PA)")
cats3T <- data.frame(ID = 1:4, LandCover = clsT)
levels(Tpol1) <- cats3T

# --- Scenario 2: Privatization Focus ---
m      <- c(-Inf, 4, 1,   4, 5, 2,   5, 6, 3,   6, Inf, NA)
rclmat <- matrix(m, ncol = 3, byrow = TRUE)
Ppol1  <- terra::classify(polcy, rclmat)

clsP   <- c("Private", "Mau Forest (PA)", "Game reserve (PA)")
cats3P <- data.frame(ID = 1:3, LandCover = clsP)
levels(Ppol1) <- cats3P


# ==============================================================================
# 4. Map Generation Components (ggplot2 maps)
# ==============================================================================

# --- Map A: Baseline Policy Framework ---
actualPol <- ggplot() +
  geom_spatraster(data = polcy) +
  geom_spatvector(data = narok, fill = NA, color = 'grey30', linewidth = 0.5) +
  scale_fill_manual(
    values       = c('grey90', 'grey40', 'gray70', '#66a61e', '#a6761d', '#e6ab02'), 
    name         = "",
    guide        = guide_legend(reverse = FALSE, ncol = 1),
    na.translate = FALSE
  ) +
  theme_bw() +
  theme(
    strip.background       = element_blank(),
    strip.text             = element_text(size = 15, color = "black"),
    panel.grid.major       = element_blank(),
    panel.grid.minor       = element_blank(),
    plot.title             = element_text(face = "bold", color = "black", size = 13, hjust = 0.5),
    legend.text            = element_text(size = 20),
    legend.position        = "inside",
    legend.position.inside = c(0.8, 0.1), # Baseline layout retains legend visibility
    legend.title           = element_blank(),
    legend.justification   = "left",
    legend.background      = element_rect(fill = "transparent"),
    legend.box.background  = element_rect(fill = "transparent", colour = "transparent"),
    axis.title             = element_blank(),
    axis.text              = element_blank(),
    axis.ticks             = element_blank(),
    legend.key.height      = unit(0.25, 'cm'), 
    legend.key.width       = unit(1, 'cm'), 
    legend.spacing.x       = unit(1, 'cm'),
    panel.border           = element_blank(),
    plot.background        = element_blank(),
    panel.background       = element_blank()
  )

# --- Map B: Traditional/Group Ranch Layout ---
tradPol <- ggplot() +
  geom_spatraster(data = Tpol1) +
  geom_spatvector(data = narok, fill = NA, color = 'grey30', linewidth = 0.5) +
  scale_fill_manual(
    values       = c('grey90', 'grey70', '#a6761d', '#e6ab02'), 
    name         = "",
    guide        = "none", # Legend suppressed to keep composite visual balanced
    na.translate = FALSE
  ) +
  theme_bw() +
  theme(
    strip.background       = element_blank(),
    strip.text             = element_text(size = 15, color = "black"),
    panel.grid.major       = element_blank(),
    panel.grid.minor       = element_blank(),
    plot.title             = element_text(face = "bold", color = "black", size = 13, hjust = 0.5),
    legend.text            = element_text(size = 20),
    legend.position        = "inside",
    legend.position.inside = c(0.1, 0.2),
    legend.title           = element_blank(),
    legend.justification   = "left",
    axis.title             = element_blank(),
    axis.text              = element_blank(),
    axis.ticks             = element_blank(),
    legend.key.height      = unit(0.25, 'cm'), 
    legend.key.width       = unit(1, 'cm'), 
    panel.border           = element_blank(),
    plot.background        = element_blank(),
    panel.background       = element_blank()
  )

# --- Map C: Privatization Layout ---
privPol <- ggplot() +
  geom_spatraster(data = Ppol1) +
  geom_spatvector(data = narok, fill = NA, color = 'grey30', linewidth = 0.5) +
  scale_fill_manual(
    values       = c('grey90', '#a6761d', '#e6ab02'), 
    name         = "",
    guide        = "none", # Legend suppressed
    na.translate = FALSE
  ) +
  theme_bw() +
  theme(
    strip.background       = element_blank(),
    strip.text             = element_text(size = 15, color = "black"),
    panel.grid.major       = element_blank(),
    panel.grid.minor       = element_blank(),
    plot.title             = element_text(face = "bold", color = "black", size = 13, hjust = 0.5),
    legend.text            = element_text(size = 20),
    legend.position        = "inside",
    legend.position.inside = c(0.1, 0.2),
    legend.title           = element_blank(),
    legend.justification   = "left",
    axis.title             = element_blank(),
    axis.text              = element_blank(),
    axis.ticks             = element_blank(),
    legend.key.height      = unit(0.25, 'cm'), 
    legend.key.width       = unit(1, 'cm'), 
    panel.border           = element_blank(),
    plot.background        = element_blank(),
    panel.background       = element_blank()
  )


# ==============================================================================
# 5. Panel Grid Array Compilation & Export
# ==============================================================================

# Safe text alignment wrapper function
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

# String configurations for panel label text blocks
my_label  <- "(a) Baseline policy (Status quo)"
my_label2 <- "(b) Group Ranch (GR) scenario"
my_label3 <- "(c) Privatization scenario"

# Form cross-sectional horizontal grid array
polss <- plot_grid(actualPol, tradPol, privPol, align = "h", nrow = 1) +
  annotate("text", x = 0.09, y = 0.74, size = 8, label = wrapper(my_label, width = 20)) +
  annotate("text", x = 0.41, y = 0.74, size = 8, label = wrapper(my_label2, width = 20)) +
  annotate("text", x = 0.76, y = 0.74, size = 8, label = wrapper(my_label3, width = 20)) +
  theme(plot.margin = unit(c(0.4, 0, 0.4, 0), "cm"))

# Override inner bounding box clips to prevent textual edge masking
gt <- ggplot_gtable(ggplot_build(polss))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

# Write out optimized high-resolution TIFF file structure
ggsave(
  filename    = "Fig_PolicyScenarions_maps.tiff", 
  plot        = polss,
  units       = 'px',
  width       = 8500,
  height      = 4000, 
  dpi         = 600,
  compression = 'lzw'
)
