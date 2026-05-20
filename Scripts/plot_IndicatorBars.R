#' -----------------------------------------------------------------------------
#' Project: LCM Scenario Analysis
#' Script: Visualizing Configuration Metrics - bar chart for sensitivity 25% adjustment scenarios
#' Author: Kotikot et al.
#' Date: May 2026
#' -----------------------------------------------------------------------------

# Load Libraries ---------------------------------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(stringr)
library(extrafont)

# 1. Load and Clean Data -------------------------------------------------------
data_path <- "../Data/Analysis_Outputs" 
narok_metrics <-  read_csv(paste0(data_path,'/landscape_Metrics.csv'))#narok_l8_processed.csv

# Define Scenario Label Mapping 
scenario_labels <- c(
  "basel"                     = "Baseline prediction (Status quo)",
  "tradN"                     = "No regenerative strategies",
  "privNS"                    = "No regenerative strategies",
  "trad_sPtc"                 = "Small forest patches",
  "priv_sPtc"                 = "Small forest patches",
  "trad_LPtc"                 = "Large forest patches",
  "priv_LPtc"                 = "Large forest patches",
  "trad_shrtLnks"             = "Forest connections",
  "priv_shrtLnks"             = "Forest connections",
  "trad_wpans"                = "Strategic water pans",
  "priv_wpans"                = "Strategic water pans",
  "trad_allForConn"           = "All forest strategies",
  "priv_allForConn"           = "All forest strategies",
  "trad_allForConn_wpans_nRC" = "All strategies combined",
  "priv_allForConn_wpans_nRC" = "All strategies combined"
)

# 2. Data Processing -----------------------------------------------------------

# Extract dynamic baseline for the plot indicator
ed_baseline <- narok_metrics %>%
  filter(metric == "ed", policy == 'basel') %>%
  pull(value) %>%
  first()

# Filter and format for plotting
plot_df <- narok_metrics %>%
  filter(metric == "ed", sensit == 1) %>%
  mutate(
    # Classify Governance (Automated string detection)
    governance = case_when(
      grepl("^trad", policy) ~ "Group Ranch",
      grepl("^priv", policy) ~ "Privatization",
      TRUE ~ "Baseline"
    ),
    # Map technical policy names to descriptive labels
    clean_label = scenario_labels[policy]
  ) %>%
  # Filter out specific scenarios if necessary (as in original script)
  filter(!grepl("allForConn_wpans$", policy)) %>%
  filter(!grepl("basel$", policy)) %>%
  # Convert to factor to ensure specific plot order
  mutate(clean_label = factor(clean_label, levels = unique(scenario_labels)))
  #mutate(clean_label = factor(clean_label, levels = rev(unique(scenario_labels))))

lgndDf <- plot_df
# 3. Visualization ------------------------------------------------------------

p_ed <- ggplot(plot_df, aes(x = clean_label, y = value, fill = governance)) +
  # Use position_dodge to show side-by-side comparison
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Add the baseline dashed line
  geom_hline(yintercept = ed_baseline, linetype = "dashed", color = "grey20", linewidth = 1.2) +
  # Annotate the baseline line
  annotate("text", x = 4.0, y = ed_baseline + 1, label = "Baseline LULC", 
           angle = 90, size = 6, fontface = "plain") +
  coord_flip(ylim = c(40, 55)) +
  scale_fill_manual(values = c('Privatization' = '#E1BE6A', 'Group Ranch' = '#40B0A6')) +
  labs(
    x = NULL,
    y = "Edge Density (m/ha)",
    fill = ""
  ) +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "serif"),
    axis.text = element_text(face = "plain", size=20, color='black',),
    axis.text.y = element_text(face = "plain", size=20, color='black'),
    axis.title.y = element_text(size = 20, face = "bold", color = "black"),
    axis.text.x = element_text(face = "plain", size=20, color='black', angle = 0, vjust = 0.5, hjust = 0),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    legend.position = "none",
    legend.text=element_text(size=16)
  )

##########################################################################################################
#Forest-Cropland adjacency
#---------------------------------------------------------------------------------------------------------

# Optional: Font setup (Run once if Cambria is not available)
# font_import(prompt = FALSE)
# loadfonts(device = "win")

# 1. Load Data -----------------------------------------------------------------
# Using the standard naming convention from directory
file_name <- "Forest_cropland_adjacency_sens1.csv"

raw_data <- read_csv(file.path(data_path, file_name))

# 2. Configuration & Mapping ---------------------------------------------------

# Define the display labels for scenarios
scenario_labels <- c(
  "tradN1"                     = "No regenerative strategies",
  "privNS1"                    = "No regenerative strategies",
  "trad_sPtc1"                 = "Small forest patches",
  "priv_sPtc1"                 = "Small forest patches",
  "trad_LPtc1"                 = "Large forest patches",
  "priv_LPtc1"                 = "Large forest patches",
  "trad_shrtLnks1"             = "Forest connections",
  "priv_shrtLnks1"             = "Forest connections",
  "trad_wpans1"                = "Strategic water pans",
  "priv_wpans1"                = "Strategic water pans",
  "trad_allForConn1"           = "All forest strategies",
  "priv_allForConn1"           = "All forest strategies",
  "trad_allForConn_wpans1"     = "All forest & water strategies",
  "priv_allForConn_wpans1"     = "All forest & water strategies",
  "trad_allForConn_wpans_nRC1" = "All strategies combined",
  "priv_allForConn_wpans_nRC1" = "All strategies combined"
)

replace_3_with_1 <- function(vec) {
  # 1. Replace '3' with '1' in the vector's values
  updated_values <- gsub("1", "1", vec)
  # 2. Replace '3' with '1' in the vector's names
  names(updated_values) <- gsub("1", "1", names(vec))
  return(updated_values)
}

scenario_labels <- replace_3_with_1(scenario_labels)

# 3. Data Processing -----------------------------------------------------------

# Extract baseline value (Status Quo) for the reference line
# Multiplied by 100 to match the percentage scale in the plot
baseline_val <- raw_data %>%
  filter(TimeStep == "basel") %>%
  pull(propCrop) %>%
  first() * 100

# Process the scenarios for plotting
agro_plot_df <- raw_data %>%
  # Filter out baseline and specific unwanted scenarios
  filter(TimeStep != "basel",
         !grepl("allForConn_wpans1$", TimeStep)) %>% 
  mutate(
    # Detect Governance via string matching (replaces manual indexing)
    policy = if_else(grepl("^trad", TimeStep), "Group Ranch", "Privatization"),
    
    # Apply clean labels
    clean_label = scenario_labels[TimeStep],
    
    # Convert propCrop to percentage
    perc_crop = as.numeric(propCrop) * 100
  ) %>%
  # Filter by radius (Adjust "500" to "600" or "1000" as needed for your specific analysis)
  filter(radii == 500) %>%
  # Set factor levels to control the vertical order of the bars
  mutate(clean_label = factor(clean_label, levels = unique(scenario_labels)))

# 4. Visualization ------------------------------------------------------------

p_agro <- ggplot(agro_plot_df, aes(x = clean_label, y = perc_crop, fill = policy)) +
  # Use position_dodge to show policy comparisons side-by-side
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Add the baseline dashed line
  geom_hline(yintercept = baseline_val, linetype = "dashed", color = "grey20", linewidth = 1.2) +
  # Annotation for the baseline
  annotate("text", x = 4, y = baseline_val + 1.5, label = "Baseline LULC", 
           angle = 90, size = 6, color = "black", fontface = "plain") +
  coord_flip(ylim = c(10, 32)) +
  scale_fill_manual(values = c('Privatization' = '#E1BE6A', 'Group Ranch' = '#40B0A6')) +
  scale_x_discrete(name ="",
                   breaks=c(""),
                   labels=c(""))+
  labs(
    y = str_wrap("% Cropland with >10% forest cover within 500m", width = 20),
    x = NULL,
    fill = "Governance Scenario"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "serif"), # "Cambria" or "serif"
    axis.text = element_text(color = "black", size = 20),
    axis.title.x = element_text(size = 20, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.position = "none" # Set to "bottom" if you want to show the legend
  )

##########################################################################################################
#Forest-Rangeland adjacensy
#---------------------------------------------------------------------------------------------------------

# 1. Setup & Data Loading ------------------------------------------------------
file_name <- "Forest_rangeland_adjacency_sens1.csv"
raw_data <- read_csv(file.path(data_path, file_name))

# 2. Configuration & Label Mapping ---------------------------------------------

# Define clean labels for the scenarios
scenario_labels <- c(
  "tradN1"                     = "No regenerative strategies",
  "privNS1"                    = "No regenerative strategies",
  "trad_sPtc1"                 = "Small forest patches",
  "priv_sPtc1"                 = "Small forest patches",
  "trad_LPtc1"                 = "Large forest patches",
  "priv_LPtc1"                 = "Large forest patches",
  "trad_shrtLnks1"             = "Forest connections",
  "priv_shrtLnks1"             = "Forest connections",
  "trad_wpans1"                = "Strategic water pans",
  "priv_wpans1"                = "Strategic water pans",
  "trad_allForConn1"           = "All forest strategies",
  "priv_allForConn1"           = "All forest strategies",
  "trad_allForConn_wpans1"     = "All forest & water strategies",
  "priv_allForConn_wpans1"     = "All forest & water strategies",
  "trad_allForConn_wpans_nRC1" = "All strategies combined",
  "priv_allForConn_wpans_nRC1" = "All strategies combined"
)

# 3. Data Processing -----------------------------------------------------------

# Extract Baseline for the dashed line (Status Quo)
# We filter for 'basel' and the 500m radius specifically
baseline_val <- raw_data %>%
  filter(TimeStep == "basel", radii == 500) %>%
  pull(propRange) %>%
  first() * 100

# Prepare data for plotting
plot_df <- raw_data %>%
  # Remove baseline and the 'All forest & water' intermediate scenario (grp 'g')
  filter(TimeStep != "basel", 
         !grepl("_allForConn_wpans1$", TimeStep)) %>%
  # Filter for the 500m radius
  filter(radii == 500) %>%
  mutate(
    # Classify Policy Type automatically (Dynamic string detection)
    policy = if_else(grepl("^trad", TimeStep), "Group Ranch", "Privatization"),
    
    # Map raw TimeStep names to descriptive labels
    clean_label = scenario_labels[TimeStep],
    
    # Convert proportion to percentage for the Y-axis
    perc_range = as.numeric(propRange) * 100
  ) %>%
  # Set factor levels to control the vertical order in the plot
  mutate(clean_label = factor(clean_label, levels = unique(scenario_labels)))

# 4. Visualization ------------------------------------------------------------

p_range <- ggplot(plot_df, aes(x = clean_label, y = perc_range, fill = policy)) +
  # Bar chart with side-by-side comparison
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  # Reference line for baseline
  geom_hline(yintercept = baseline_val, linetype = "dashed", color = "grey20", linewidth = 1.2) +
  # Annotation for the baseline line
  annotate("text", x = 4, y = baseline_val + 1, label = "Baseline LULC", 
           angle = 90, size = 6, color = "black", fontface = "plain") +
  coord_flip(ylim = c(35, 50)) +
  scale_fill_manual(values = c('Privatization' = '#E1BE6A', 'Group Ranch' = '#40B0A6')) +
  scale_x_discrete(name ="",
                   breaks=c(""),#c('a','b','c','d','e','f','h'),
                   labels=c(""))+
  labs(
    y = str_wrap("% of rangeland within 500m of a forest patch", width = 20),
    x = NULL,
    fill = "Governance Scenario"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    text = element_text(family = "serif"), # Usually "Cambria" on Windows
    axis.text = element_text(color = "black", size = 20),
    axis.title.x = element_text(size = 20, margin = margin(t = 10)),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
    legend.position = "none" # Hide legend if plotting in a grid; else set to "bottom"
  )

##########################################################################################################
#Dummy plot for legend
#---------------------------------------------------------------------------------------------------------
p_lgnd <- ggplot(lgndDf, aes(x = clean_label, y = value, fill = governance)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.7) +
  coord_flip(ylim = c(40, 55)) +
  scale_fill_manual(values = c('Privatization' = '#E1BE6A', 'Group Ranch' = '#40B0A6')) +
  labs(
    fill = ""
  ) +
  theme_minimal(base_size = 20) +
  theme(
    legend.position = "top",
    legend.text=element_text(size=16)
  )

#=================================================================================
#---------------------------
library(cowplot)
library(ggplot2)
library(grid)

grobs2 <- ggplotGrob(p_lgnd)$grobs
legend2 <- grobs2[[which(sapply(grobs2, function(x) x$name) == "guide-box")]]

IndPanel1=plot_grid(p_ed, p_agro, p_range, align = "h", nrow = 1, rel_widths = c(2,1,1))

# build grid without legends
IndPanel=plot_grid(legend2, IndPanel1, align = "v", nrow = 2, rel_heights = c(1.7,25))+
  
  # # Add some space around the edges  
  theme(plot.margin = unit(c(0.4,0,0.4,0), "cm"))

# # Have to turn off clipping
gt <- ggplot_gtable(ggplot_build(IndPanel))
gt$layout$clip[gt$layout$name == "panel"] <- "off"
# # need to draw it with the new clip settings
IndPanel

ggsave(file="Fig_IndicatorBars_plots.tiff", IndPanel,
       units='px',width=7500,height=3500, dpi=600,compression='lzw')

