#' -----------------------------------------------------------------------------
#' Project: LCM Scenario Analysis
#' Script: Data Preparation and Comparative Indicator Radar Chart Plotting
#' Author: Kotikot et al.
#' Date: May 2026
#' -----------------------------------------------------------------------------

# Load Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(scales)
library(stringr)

# 1. Global Configurations & Constants -----------------------------------------
DATA_PATH <- "../data/Analysis_Outputs"

# Technical to Publication String Lookups
GROUP_LOOKUP <- c(
  "tradN"                     = "a", "privNS"                    = "a",
  "trad_sPtc"                 = "b", "priv_sPtc"                 = "b",
  "trad_LPtc"                 = "c", "priv_LPtc"                 = "c",
  "trad_shrtLnks"             = "d", "priv_shrtLnks"             = "d",
  "trad_wpans"                = "e", "priv_wpans"                = "e",
  "trad_allForConn"           = "f", "priv_allForConn"           = "f",
  "trad_allForConn_wpans"     = "g", "priv_allForConn_wpans"     = "g",
  "trad_allForConn_wpans_nRC" = "h", "priv_allForConn_wpans_nRC" = "h"
)

GROUP_MAPPING_AGRO <- c(
  "tradN1"                     = "a", "privNS1"                    = "a",
  "trad_sPtc1"                 = "b", "priv_sPtc1"                 = "b",
  "trad_LPtc1"                 = "c", "priv_LPtc1"                 = "c",
  "trad_shrtLnks1"             = "d", "priv_shrtLnks1"             = "d",
  "trad_wpans1"                = "e", "priv_wpans1"                = "e",
  "trad_allForConn1"           = "f", "priv_allForConn1"           = "f",
  "trad_allForConn_wpans1"     = "g", "priv_allForConn_wpans1"     = "g",
  "trad_allForConn_wpans_nRC1" = "h", "priv_allForConn_wpans_nRC1" = "h"
)

SCENARIO_MAP <- c(
  "trad-Baseline"             = "Baseline",          "priv-Baseline" = "Baseline",
  "tradN"                     = "No strategies",     "privNS"        = "No strategies",
  "trad_sPtc"                 = "Small patches",     "priv_sPtc"     = "Small patches",
  "trad_LPtc"                 = "Large patches",     "priv_LPtc"     = "Large patches",
  "trad_shrtLnks"             = "Connections",       "priv_shrtLnks" = "Connections",
  "trad_wpans"                = "Water pans",        "priv_wpans"    = "Water pans",
  "trad_allForConn"           = "Forest strategies", "priv_allForConn" = "Forest strategies",
  "trad_allForConn_wpans_nRC" = "All strategies",    "priv_allForConn_wpans_nRC" = "All strategies"
)

# Target constraints
EXCLUDED_POLICIES <- c('trad-FWpans', 'priv-FWpans')
TARGET_CLASSES    <- c("Forest", "Rangeland")
TARGET_METRIC     <- "ed"
RADIUS_AGRO       <- 500
RADIUS_RANGE      <- 500

# Helper function to dynamically modify vector mapping keys/values
replace_3_with_1 <- function(vec) {
  updated_values <- stringr::str_replace_all(vec, "3", "1")
  names(updated_values) <- stringr::str_replace_all(names(vec), "3", "1")
  return(updated_values)
}
GROUP_MAPPING_RANGE <- replace_3_with_1(GROUP_MAPPING_AGRO)


# 2. Data Ingestion & Processing Pipelines -------------------------------------

## Pipeline A: Forest & Rangeland Class Metrics ----
class_metrics_raw <- read_csv(file.path(DATA_PATH, "class_Metrics.csv"), show_col_types = FALSE)

class_baselines <- class_metrics_raw %>%
  filter(metric == TARGET_METRIC, sensit == 0, class %in% TARGET_CLASSES) %>%
  group_by(class) %>%
  summarize(baseline_value = first(value), .groups = "drop")

scenarios_clean <- class_metrics_raw %>%
  filter(metric == TARGET_METRIC, sensit == 1, class %in% TARGET_CLASSES, !policy %in% EXCLUDED_POLICIES) %>%
  dplyr::select(policy, class, value)

connectivity_final <- class_baselines %>%
  reframe(policy = c("trad-Baseline", "priv-Baseline"), value = baseline_value, .by = class) %>%
  bind_rows(scenarios_clean) %>%
  arrange(class, desc(grepl("Baseline", policy)), policy)

forest_conn2 <- connectivity_final %>% 
  filter(class == "Forest", policy != "priv_allForConn_wpans", policy != "trad_allForConn_wpans") %>% 
  mutate(Scenario = policy)

## Pipeline B: Landscape Metrics ----
landscape_metrics_raw <- read_csv(file.path(DATA_PATH, "landscape_Metrics.csv"), show_col_types = FALSE)

ed_baseline_val <- landscape_metrics_raw %>%
  filter(metric == "ed", sensit == 0) %>%
  pull(value) %>%
  first()

edge_scenarios <- landscape_metrics_raw %>%
  filter(metric == "ed", sensit == 1) %>%
  mutate(
    governance = if_else(grepl("^trad", policy), "trad", "priv"),
    grp = GROUP_LOOKUP[policy]
  ) %>%
  filter(grp != "g") %>%
  dplyr::select(Scenario = policy, Value = value, Policy = governance, Group = grp)

edge_final <- tibble(
  Scenario = c("trad-Baseline", "priv-Baseline"),
  Value    = ed_baseline_val,
  Policy   = c("trad", "priv"),
  Group    = "aa"
) %>%
  bind_rows(edge_scenarios) %>%
  arrange(Policy, Group)

## Pipeline C: Forest-Cropland Adjacency ----
agro_raw <- read_csv(file.path(DATA_PATH, "Forest_cropland_adjacency_sens1.csv"), show_col_types = FALSE)

agro_baseline_val <- agro_raw %>%
  filter(TimeStep == "basel", radii == RADIUS_AGRO) %>%
  pull(propCrop)

agro_final <- agro_raw %>%
  filter(TimeStep != "basel") %>%
  mutate(
    policy = if_else(grepl("^trad", TimeStep), "trad", "priv"),
    grp    = GROUP_MAPPING_AGRO[TimeStep],
    grp2   = case_match(as.character(radii), "500" ~ "b", "300" ~ "a", "1000" ~ "c", .default = as.character(radii))
  ) %>%
  filter(grp2 == "b", grp != "g") %>%
  dplyr::select(Scenario = TimeStep, Value = propCrop, Policy = policy, Group = grp) %>%
  bind_rows(tibble(Scenario = c("trad-Baseline", "priv-Baseline"), Value = agro_baseline_val, Policy = c("trad", "priv"), Group = "aa")) %>%
  mutate(Scenario = stringr::str_remove(Scenario, "1$")) %>%
  arrange(Policy, Group)

## Pipeline D: Forest-Rangeland Adjacency ----
range_raw <- read_csv(file.path(DATA_PATH, "Forest_rangeland_adjacency_sens1.csv"), show_col_types = FALSE)

range_baseline_val <- range_raw %>%
  filter(TimeStep == "basel", radii == RADIUS_RANGE) %>%
  pull(propRange) %>%
  first()

r_range_final <- range_raw %>%
  filter(TimeStep != "basel") %>%
  mutate(
    policy = if_else(grepl("^trad", TimeStep), "trad", "priv"),
    grp    = GROUP_MAPPING_RANGE[TimeStep],
    grp2   = case_match(as.character(radii), "500" ~ "a", "1000" ~ "b", .default = as.character(radii))
  ) %>%
  filter(grp2 == "a", grp != "g") %>%
  dplyr::select(Scenario = TimeStep, Value = propRange, Policy = policy, Group = grp) %>%
  bind_rows(tibble(Scenario = c("trad-Baseline", "priv-Baseline"), Value = range_baseline_val, Policy = c("trad", "priv"), Group = "aa")) %>%
  mutate(Scenario = stringr::str_remove(Scenario, "1$")) %>%
  arrange(Policy, Group)


# 3. Functional Join Workflow -------------------------------------------------
master_dat <- edge_final %>%
  rename(Fragmentation = Value) %>%
  mutate(
    Policy = if_else(Policy == "trad", "GR", "PR"),
    Scenario_clean = SCENARIO_MAP[Scenario]
  ) %>%
  left_join(dplyr::select(agro_final, Scenario, AgFores = Value), by = "Scenario") %>%
  left_join(dplyr::select(r_range_final, Scenario, RangFor = Value), by = "Scenario") %>%
  left_join(dplyr::select(forest_conn2, Scenario, ForFrag = value), by = "Scenario") %>%
  dplyr::select(Scenario = Scenario_clean, Policy, Fragmentation, AgFores, RangFor, ForFrag)


# 4. Radar Plot Transformation & Visualization --------------------------------

# Function for conversion to straight segment radar boundaries
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CoordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction), is_linear = function() TRUE)
}

# Variable configuration
metrics <- c("Fragmentation", "AgFores", "RangFor", "ForFrag")
scenario_order <- c("Baseline", "No strategies", "Small patches", "Large patches", 
                    "Connections", "Water pans", "Forest strategies", "All strategies")

# Rescale using explicit namespace to avoid package conflicts
dat_rescaled <- master_dat %>%
  mutate(across(all_of(metrics), ~ scales::rescale(.x, to = c(0.1, 1))))

df_long <- dat_rescaled %>%
  mutate(Scenario = factor(Scenario, levels = scenario_order)) %>%
  pivot_longer(cols = all_of(metrics), names_to = "Metric", values_to = "Score")

df_long$Metric_ID <- as.numeric(factor(df_long$Metric, levels = metrics))

# Close loop geometry for radar chart paths
df_closed <- df_long %>%
  group_by(Policy, Scenario) %>%
  arrange(Metric_ID) %>%
  do(rbind(., .[1, ] %>% mutate(Metric_ID = 5))) %>%
  ungroup()

# Aesthetics Setup
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

label_data <- data.frame(
  Metric_ID = 1:4, 
  Score = 1.25,
  Metric_Label = stringr::str_wrap(c(
    "Landscape level fragmentation (Spatial heterogeneity)", 
    "Cropland adjacency to forest patches [Edge benefits]", 
    "Rangeland adjacency to forest patches (Refugia benefits)", 
    "Forest fragmentation (Edge benefits)"
  ), width = 15)
)

# Plotting Generation
radds <- ggplot(df_closed, aes(x = Metric_ID, y = Score, color = Scenario, linetype = Policy)) +
  geom_path(linewidth = 1) +
  geom_point(size = 2, alpha = 1) +
  geom_text(data = label_data, aes(x = Metric_ID, y = Score, label = Metric_Label),
            size = 4.5, lineheight = 0.9, fontface = "plain", color = "black", inherit.aes = FALSE) + 
  coord_radar() +
  scale_color_manual(values = scenario_colors) +
  scale_linetype_manual(values = c("GR" = "solid", "PR" = "dashed")) +
  scale_x_continuous(breaks = 1:4, labels = metrics) +
  scale_y_continuous(limits = c(0, 1.25), breaks = c(0.25, 0.5, 0.75, 1)) + 
  theme_minimal() +
  theme(
    text = element_text(family = "Cambria", face = "bold", size = 20),
    axis.title = element_blank(),
    axis.text.x = element_blank(), 
    axis.text.y = element_blank(),
    panel.grid.major = element_line(color = "gray70"),
    panel.grid.minor = element_line(color = "gray70"),
    legend.position = "right",
    legend.key.width = unit(2, "line"),
    legend.text = element_text(size = 14, face = 'plain')
  ) +
  labs(color = "Scenario", linetype = "Policy")

# Print plot
print(radds)

ggsave(file="Fig_RadarChart_multistrategy.tiff", radds,
       units='px',width=5000,height=3000, dpi=400,compression='lzw')
