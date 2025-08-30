# Load required libraries
library(sf)
library(lwgeom)
library(spatstat)
library(viridis)
library(spdep)
library(truncnorm)
library(tmvtnorm)
library(patchwork)
library(data.table)
library(MASS)  
library(tidyverse)

# Set working directory 
setwd('~/Dropbox/RPV/Code/Simulation/')

rm(list = ls())

# Source auxiliary functions and North Carolina precinct-level demographic data
source('R/auxiliary.R')

# ---------- Run TX maps (neutral, R, D) ----------
draw_tx_maps = function() {
  
  N_PLANS          = 1000
  ENSEMBLE_SIZE    = 10
  BURST_LENGTH     = 10
  NUM_BURSTS       = 10
  PATIENCE_BURSTS  = 5
  SOFT_K           = 60
  SIMPLIFY_TOL     = NA
  RANDOM_SEED      = 123
  OUTPUT_BASE_DIR  = 'Output/TEXAS2/'
  DEV_MODE         = TRUE
  
  # Paths
  SHAPEFILE_IN        = "data/States/TX/out/tx_24.shp"
  SHAPEFILE_FOR_PIPE  = file.path(OUTPUT_BASE_DIR, "state_map.shp")
  OUTPUT_DIR          = OUTPUT_BASE_DIR
  
  # Fixed problem parameters
  NUM_STEPS      = N_PLANS
  POP_DEVIATION  = 0.01
  NUM_DISTRICTS  = 38
  
  # EI hyperparameters (env-exposed)
  EI_MEANS = c(
    white = 0.33,
    black = 0.73,
    hisp  = 0.49
  )
  EI_SDS = c(
    white = 0.14,
    black = 0.26,
    hisp  = 31
  )
  EI_CORR = list(
    rho_wb = 0.25,
    rho_wh = 0.25,
    rho_bh = 0.25
  )
  
  # 1) Build shapefile with EI-imposed group-specific votes
  prepare_tx_for_redistricting(
    in_shapefile        = SHAPEFILE_IN,
    out_shapefile       = SHAPEFILE_FOR_PIPE,
    ei_means            = EI_MEANS,
    ei_sds              = EI_SDS,
    ei_corr             = EI_CORR,
    seed                = RANDOM_SEED,
    simplify_tolerance  = SIMPLIFY_TOL
  )
  
  # 2) Run redistricting driver (Python bursts + ensembles)
  res = run_redistricting(
    shapefile_path       = SHAPEFILE_FOR_PIPE,
    output_dir           = OUTPUT_DIR,
    num_steps            = NUM_STEPS,
    ensemble_size        = ENSEMBLE_SIZE,
    pop_deviation        = POP_DEVIATION,
    num_districts        = NUM_DISTRICTS,
    dev_mode             = DEV_MODE,
    burst_length         = BURST_LENGTH,
    num_bursts           = NUM_BURSTS,
    soft_k               = SOFT_K,
    simplify_tolerance   = SIMPLIFY_TOL
  )
  
  # 3) Post-process summaries and make figures
  map_data = sf::read_sf(SHAPEFILE_FOR_PIPE) %>%
    dplyr::rename(
      precinct_id = pct_id, population  = pop,
      n_minority  = n_min,  n_majority  = n_maj,
      dem_votes   = dem_v,  rep_votes   = rep_v,
      dem_votes_minority = dem_v_min,   rep_votes_minority = rep_v_min,
      dem_votes_majority = dem_v_maj,   rep_votes_majority = rep_v_maj,
      per_minority = pct_min, dem_voteshare = dem_vsh,
      dem_voteshare_minority = dem_vsh_1, dem_voteshare_majority = dem_vsh_0
    )
  
  summaries = process_redistricting_results(
    map_data = map_data,
    redistricting_results = res,
    current_output_dir = OUTPUT_DIR
  )
  
  # Neutral plans file (if present) for the "representative neutral plan" plot
  CD_plans_neutral = NULL
  neutral_file = file.path(OUTPUT_DIR, "neutral/CD_plans.csv")
  if (file.exists(neutral_file)) CD_plans_neutral = data.table::fread(neutral_file)
  
  create_redistricting_visualizations(
    map_data              = map_data,
    summaries             = summaries,
    CD_plans_neutral      = CD_plans_neutral,
    output_dir            = OUTPUT_DIR,
    segregation_level     = "texas"
  )
  
  list(summaries = summaries, output_dir = OUTPUT_DIR)
}

# Run
tx_plans = draw_tx_maps()

