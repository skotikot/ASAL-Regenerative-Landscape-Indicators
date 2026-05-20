#' -----------------------------------------------------------------------------
#' Project: LCM Scenario Analysis
#' Script: Multi-panel Sensitivity Comparison Plot (sens1, sens2, sens3)
#' Author: Kotikot et al.
#' Date: May 2026
#' -----------------------------------------------------------------------------

# Load Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)

# 1. Load Data -----------------------------------------------------------------
DATA_PATH <- "../data/Analysis_Outputs"
sens1 <- read_csv(paste0(DATA_PATH, "/sens1Indicators.csv"), show_col_types = FALSE)
sens2 <- read_csv(paste0(DATA_PATH, "/sens2Indicators.csv"), show_col_types = FALSE)
sens3 <- read_csv(paste0(DATA_PATH, "/sens3Indicators.csv"), show_col_types = FALSE)

# 2. Combine & Reshape Data ----------------------------------------------------
df_long <- bind_rows(sens1, sens2, sens3) %>%
  # Select only the target columns requested for the plot
  dplyr::select(Scenario, Policy, sensit, Fragmentation, AgFores, RangFor) %>%
  
  # CONVERT TO PERCENT: Multiply fractional indicators by 100
  mutate(
    AgFores = AgFores * 100,
    RangFor = RangFor * 100,
    
    # Explicitly map old levels to new labels
    Policy = factor(Policy, 
                    levels = c("GR", "PR"), 
                    labels = c("Group ranch (GR)", "Privatization (PR)"))
  ) %>%
  
  # Pivot wide metrics columns into a unified long-format structure for faceting
  pivot_longer(
    cols = c(Fragmentation, AgFores, RangFor),
    names_to = "Indicator",
    values_to = "Value"
  ) %>%
  mutate(
    # Force the display order of the panels to match your requirements
    Indicator = factor(Indicator, levels = c("Fragmentation", "AgFores", "RangFor")),
    sensit    = as.numeric(sensit)
  )

# 3. Plot Aesthetics Configurations --------------------------------------------
# Reusing the exact palette from your radar chart for visual continuity
scenario_colors <- c(
  "Baseline"          = "#333333", 
  "No strategies"     = "#f781bf", 
  "Small patches"     = "#377EB8", 
  "Large patches"     = "#4DAF4A", 
  "Connections"       = "#984EA3", 
  "Water pans"        = "#FF7F00", 
  "Forest strategies" = "cyan", 
  "All strategies"    = "#A65628"
)

# Apply explicit scenario ordering factor
scenario_order <- c("Baseline", "No strategies", "Small patches", "Large patches", 
                    "Connections", "Water pans", "Forest strategies", "All strategies")
df_long$Scenario <- factor(df_long$Scenario, levels = scenario_order)


# 4. Multi-Panel Visualization Generation --------------------------------------
# Create a named vector to map old column names to new pretty panel titles
raw_panel_names <- c(
  "Fragmentation" = "a) Landscape Fragmentation Index",
  "AgFores"       = "b) Cropland Adjacency to Forest Patches (%)",
  "RangFor"       = "c) Rangeland Adjacency to Forest Patches (%)"
)

# WRAP PANEL TITLES: This automatically inserts line breaks (\n) every ~20 characters
panel_names_wrapped <- stringr::str_wrap(raw_panel_names, width = 20)
names(panel_names_wrapped) <- names(raw_panel_names) # Re-assign original names for matching

sens_plot <- ggplot(df_long, aes(
  x = sensit, 
  y = Value, 
  color = Scenario, 
  linetype = Policy, 
  group = interaction(Scenario, Policy)  # Ensures R links 1 -> 2 -> 3 for each combination
)) +
  geom_line(linewidth = 1.5, alpha = 0.8) +
  geom_point(size = 2.5) +
  
  # Split into 3 horizontal panels with independent y-axes
  facet_wrap(~Indicator, scales = "free_y", nrow = 1,
             labeller = as_labeller(panel_names_wrapped)) +
  
  # Clear and discrete x-axis stepping labels
  scale_x_continuous(breaks = c(1, 2, 3),
                     labels = c("25%", "50%", "75%")) +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("Group ranch (GR)"   = "solid", 
                                   "Privatization (PR)" = "dashed")) +
  
  labs(
    x = "Sensitivity Level",
    y = "Indicator Value",
    color = "Scenario",
    linetype = "Policy"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "Cambria", face = "plain"),
    strip.text = element_text(size = 20, face = "plain", color = "black"), # Style for panel titles
    panel.grid.minor = element_blank(),
    panel.spacing = unit(1.8, "lines"), # Adds clean spacing between panel borders
    legend.position = "right",
    legend.key.width = unit(2.5, "line")
  )

# 5. Display and Save ---------------------------------------------------------
print(sens_plot)

# Saves a clean widescreen layout
ggsave(file="Fig_Sensitivity_Analysis_Comparison.tiff", sens_plot,
       units='px',width=6000,height=2000, dpi=400,compression='lzw')
# 
# # Saves a clean widescreen layout 
# ggsave("Fig_Sensitivity_Analysis_Comparison.png", plot = sens_plot, width = 15, height = 5, dpi = 300)
