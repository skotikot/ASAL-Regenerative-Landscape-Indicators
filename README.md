# Modeling Regenerative Pathways for Multifunctional Landscapes Under Increasing Environmental Stressors

This repository contains the replication code, spatial analysis workflows, and data visualization scripts associated with the manuscript:

> **Indicators of Regenerative Landscape Pathways Under Contrasting Land Tenure**
> *Susan M. Kotikot, Erica A.H. Smithwick, Sarah Gergel, Jedidah Nankaya, Romulus Abila*
> Correspondence: [susan.kotikot@uconn.edu](mailto:susan.kotikot@uconn.edu)

---

## Project Overview

Multifunctional arid and semi-arid landscapes (ASALs) face intensifying pressures from climate variability, shifting land tenure systems, and land-use change. This project implements a spatially explicit scenario framework using a calibrated **Land Change Modeler (LCM)** execution to simulate future landscape patterns under two contrasting land tenure regimes in **Narok County, Kenya**:
1. **Group Ranch (GR) Governance** (Communal/Traditional arrangements)
2. **Privatization (PR)** (Subdivided, individually managed parcels)

Across these tenure regimes, various **regenerative management strategies** are modeled at distinct baseline transition adjustments (Sensitivity levels: **Sens1 = 25%**, **Sens2 = 50%**, and **Sens3 = 75%**). These scripts evaluate landscape configuration indicators—specifically landscape heterogeneity, forest fragmentation, and functional landscape adjacency (cropland-forest and rangeland-forest interfaces)—to act as diagnostic signals of regenerative landscape capacity.

---

## Repository Structure & Core Scripts

The repository is organized into data processing workflows (`calc_*.R`), global data configurations (`Load_*.R`), and visualization scripts (`plot_*.R`).

### 1. Data Ingestion & Setup
* **`Load_scenarioOutputs.R`**
    * *Purpose:* Standardizes environment setups and auto-loads raw raster projections (`.rst` format) from LCM simulations.
    * *Functions:* Converts zero/background pixels uniformly to `NA`, applies consistency flags for spatial computations, and divides outputs into respective sensitivity group lists (`lst_sens1`, `lst_sens2`, `lst_sens3`) matching the baseline and intervention runs.

### 2. Spatial Indicator & Metric Computations
* **`calc_LandscapeMetrics.R`**
    * *Purpose:* Uses the `landscapemetrics` and `terra` packages to derive patch- and landscape-level configuration metrics.
    * *Key Output:* Measures class and landscape-level spatial parameters (e.g., Edge Density) across land cover types (Forest, Rangeland, Cropland, Urban) across all scenarios.
* **`calc_Forest_Rangeland_adjacency.R`**
    * *Purpose:* Quantifies the functional proximity/refugia benefits of dryland mosaics. 
    * *Methodology:* Uses a circular focal window matrix (`terra::focal`) to calculate the exact proportion of communal/private rangeland situated within a critical `500m` buffer zone of a forest patch.
* **`calc_Forest_Cropland_adjacency.R`**
    * *Purpose:* Computes cross-habitat spillover indicators (e.g., ecosystem services, insect pollination, and pest regulation potentials).
    * *Methodology:* Identifies individual cropland pixels and evaluates adjacent forest canopy density within predetermined neighborhood scales.
* **`calc_forestConnectivity_analysis.R`**
    * *Purpose:* Generates landscape resistance surfaces and Minimum Cost Path Networks.
    * *Methodology:* Utilizes the `grainscape`, `sf`, and `terra` libraries to calculate structural connectivity, build Minimum Path Graphs (MPG), and isolate structural link layers (e.g., short habitat links $< 500\text{m}$) used to configure incentive-based land target layers.

### 3. Data Visualization & Figures
* **`plot_PolicyScenarios_maps.R`**
    * *Purpose:* Processes vector boundaries and policy zoning data to map out the baseline and projected structural boundaries of the two primary land tenure regimes. 
    * *Key Output:* Generates maps comparing the geographic layouts of:
        1. **Baseline Policy (Status Quo)**
        2. **Group Ranch (GR) Scenario** (Communal/Traditional arrangements)
        3. **Privatization Scenario** (Subdivided, individually managed parcels)
* **`plot_RegenScenarios_maps.R`**
    * *Purpose:* Ingests spatial simple features (`sf`), spatiotemporal arrays (`stars`), and `tidyterra` geometries and generates a three-panel figure evaluating ecological regeneration strategies (modeled interventions):
        1. Large vs. Small patches distribution
        2. Least Cost Paths (LCP) patch connectivity
        3. Strategic water-pan availability 
* **`plot_spatialMaps_NoRegen.R`**
    * *Purpose:* Visualizes predicted land-use and land-cover (LULC) changes across the landscape under baseline, group ranch and privatization configurations where no active regenerative management interventions are applied.
    * *Key Output:* Generates a three-panel horizontal map array displaying projected landscape patterns under:
        1. **Baseline Prediction (Status Quo)**
        2. **Group Ranch (GR) Scenario** (Traditional governance constraints without regenerative intervention)
        3. **Privatization Scenario** (Subdivided framework without regenerative interventio)
    * *Methodology:* Ingests simulated scenario rasters using the `terra` engine, aligns spatial layers with vector boundary shapefiles using `sf`, and maps the surface classifications via `tidyterra` and `ggplot2`. 
* **`plot_IndicatorBars.R`**
    * *Purpose:* Code to generate composite figure comparisons of configuration outputs (Edge Density, Agro-Forestry interfaces, Rangeland adjacency metrics). Uses `cowplot::plot_grid` to stitch multiple ggplot objects into a clean, horizontal multi-panel array.
* **`plot_RadarChart_multistrategy.R`**
    * *Purpose:* Implements a custom `coord_radar()` extension over `ggplot2` to evaluate multi-component tradeoffs across policy bundles. 
* **`plot_IndicatorSensitivity.R`**
    * *Purpose:* Evaluates the sensitivity of spatial indicators across stepping thresholds (25%, 50%, and 75% transition adjustments).
---

## Directory Schema Requirement

To execute the scripts without directory breaking, maintain the following parent-folder orientation:

```text
├── Data/
│   ├── LCM_Outputs/             # Contains raw simulated .rst raster mosaics
│   ├── Processed/               # Intermediate resistance surfaces & distance files
│   ├── Analysis_Outputs/        # Saved tabular summaries (.csv) of indicators
│   └── ComplementaryFiles/      # Narok County vector boundary layers (.shp)
└── Scripts/
    ├── Load_scenarioOutputs.R   # Run this first to load predicted LULC maps
    ├── calc_...                 # Processing and structural scripts
    └── plot_...                 # Visualization and layout scripts

---```

## Recommended Script Execution Order

To reproduce the analysis and figures, the scripts must be run in a specific sequence. This ensures that environmental spatial layers are generated first, followed by tabular indicator calculations, and finally the plots.

### Phase 1: Environment & Base Connectivity Setup
1. **`Load_scenarioOutputs.R`** *Run first.* This loads all the predicted LULC maps.

### Phase 2: Spatial Indicator Calculations
Run these scripts to parse the Land Change Modeler (LCM) scenario outputs and calculate the diagnostic landscape metrics. Each script automatically writes its respective summarized dataset (`.csv`) to `../Data/Analysis_Outputs`.
2. **`calc_forestConnectivity_analysis.R`** 
3. **`calc_LandscapeMetrics.R`** (Outputs: `landscape_Metrics.csv`)
4. **`calc_Forest_Rangeland_adjacency.R`** (Outputs: `Forest_rangeland_adjacency_sens1.csv`)
5. **`calc_Forest_Cropland_adjacency.R`** (Outputs: `Forest_cropland_adjacency_sens1.csv`)

### Phase 3: Data Visualization & Figure Generation
Once all `.csv` summaries and spatial arrays are generated in the steps above, you can execute the plotting scripts independently to reproduce the paper's final figures:

Input data for  * `plot_IndicatorSensitivity.R` is provided in the "Analysis_Outputs" folder (`sens1Indicators.csv`, `sens2Indicators.csv`, `sens3Indicators.csv`). These are similar to those generated (and used) by `plot_RadarChart_multistrategy.R`, and were generated by running the script multiple time, each time for 25%, 50%, and 75% transition adjustments (sensitivities)
