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
# nc_pop = read_csv('~/Dropbox/RPV/Data/Clean/NC_precinct_votes_and_pop_by_race.csv')


# ===================================================================
# ==================== SIMULATION PARAMETERS ========================
# ===================================================================

# Set parameters
run_sim = function() {
  N_PRECINCTS        = 830
  N_CENTERS          = 2
  INPUT_DATA         = NULL
  POP_MEAN           = 2300
  POP_SD             = 1500
  MINORITY_MEAN      = 0.25
  MINORITY_SD        = 0.30
  N_DISTRICTS        = 14
  
  # Voting parameters
  VOTING_MODEL       = "ei"
  PROB_MINORITY_DEM  = 0.81
  PROB_MAJORITY_DEM  = 0.37
  SD_MINORITY        = 0.1
  SD_MAJORITY        = 0.3
  RHO                = 0.2
  B_MIN_CONTEXT      = 0.1
  B_MAJ_CONTEXT      = 0.3
  SIGMA_PRECINCT     = 0.2
  FIELD_RHO          = 0.8
  SHARED_FIELD_WT    = 0.4
  SHARED_FIELD_RHO   = 0.85
  
  # Grid parameters
  DECAY_RATE         = 10        
  BASE_DENSITY       = 0.1      
  PEAK_MULTIPLIER    = 10.0   
  DECAY_POWER        = 1          
  CENTER_RADIUS      = 10     
  RANDOM_SEED        = 14
  
  # Redistricting parameters
  N_PLANS            = 100 # Size of neutral map ensemble
  ENSEMBLE_SIZE      = 10 # Size of biased map ensemble  
  POP_DEVIATION      = 0.01
  
  # ENSEMBLE_SIZE = 25–30
  # BURST_LENGTH = 400
  # NUM_BURSTS = 24 # planned steps = 9,600
  # PATIENCE_BURSTS = 6
  
  # Short burst optimizer / dev parameters
  DEV_MODE            = TRUE
  BURST_LENGTH        = 5     # steps per burst
  NUM_BURSTS          = 5    # number of bursts (iterations)
  PATIENCE_BURSTS     = 5     # optional early stop after N bad bursts
  SOFT_K              = 60    # (unchanged) steepness for soft seats
  SIMPLIFY_TOL        = NA    # numeric or NA
  
  SCORE_MODELS        = FALSE
  EX_POST_VOTE_MODELS  = NULL
  
  
  
  # Output directory
  OUTPUT_BASE_DIR    = "Output/Tests/"
  
  # Run the analysis
  results = analyze_redistricting_impact(
    output_base_dir    = OUTPUT_BASE_DIR,
    n_plans            = N_PLANS,
    ensemble_size      = ENSEMBLE_SIZE,
    n_districts        = N_DISTRICTS,
    n_precincts        = N_PRECINCTS,
    n_centers          = N_CENTERS,
    input_data         = INPUT_DATA,
    pop_mean           = POP_MEAN,
    pop_sd             = POP_SD,
    minority_mean      = MINORITY_MEAN,
    minority_sd        = MINORITY_SD,
    voting_model       = VOTING_MODEL,
    prob_minority_dem  = PROB_MINORITY_DEM,
    prob_majority_dem  = PROB_MAJORITY_DEM,
    sd_minority        = SD_MINORITY,
    sd_majority        = SD_MAJORITY,
    rho                = RHO,
    b_min_context      = B_MIN_CONTEXT,
    b_maj_context      = B_MAJ_CONTEXT,
    sigma_precinct     = SIGMA_PRECINCT,
    field_rho          = FIELD_RHO,
    shared_field_wt    = SHARED_FIELD_WT,   
    shared_field_rho   = SHARED_FIELD_RHO,
    decay_rate         = DECAY_RATE,
    base_density       = BASE_DENSITY,
    peak_multiplier    = PEAK_MULTIPLIER,
    decay_power        = DECAY_POWER,
    center_radius      = CENTER_RADIUS,
    random_seed        = RANDOM_SEED,
    pop_deviation      = POP_DEVIATION,
    dev_mode           = DEV_MODE,
    burst_length       = BURST_LENGTH,
    num_bursts         = NUM_BURSTS,
    patience_bursts    = PATIENCE_BURSTS,
    soft_k             = SOFT_K,
    simplify_tolerance = SIMPLIFY_TOL,
    score_models       = SCORE_MODELS,
    ex_post_vote_models = EX_POST_VOTE_MODELS
  )
  
  # Verify consistency
  verify_fixed_population_consistency(results$simulation_results)
  
  # Summarize results
  if (!is.null(results$results)) {
    summary = summarize_segregation_results(results$results)
  }
  
  return(results)
}


# Run 
results = run_sim()









# ---------- Run TX maps (neutral, R, D) ----------
draw_tx_maps = function() {
  # Paths & params
  SHAPEFILE_IN        = 'data/States/TX/out/tx_24.shp'
  SHAPEFILE_FOR_PIPE  = 'Output/TEXAS/state_map.shp'  # what run_redistricting will read
  OUTPUT_DIR          = 'Output/TEXAS/'
  
  NUM_STEPS           = 1000
  ENSEMBLE_SIZE       = 10
  POP_DEVIATION       = 0.01
  NUM_DISTRICTS       = 38
  DEV_MODE            = TRUE
  BURST_LENGTH        = 10 # 400   # larger for real data
  NUM_BURSTS          = 10 # 24
  PATIENCE_BURSTS     = 6
  SOFT_K              = 60
  SIMPLIFY_TOLERANCE  = 0.5   # geometry simplify for speed; set NA to skip
  
  # EI hyperparameters (tweak as needed)
  EI_MEANS = c(white = 0.35, black = 0.90, hisp = 0.65)
  EI_SDS   = c(white = 0.08, black = 0.10, hisp = 0.12)
  EI_CORR  = list(rho_wb = 0.5, rho_wh = 0.5, rho_bh = 0.5)  # modest positive co-movement
  
  # 1) Build shapefile with EI-imposed group-specific votes
  prepare_tx_for_redistricting(
    in_shapefile        = SHAPEFILE_IN,
    out_shapefile       = SHAPEFILE_FOR_PIPE,
    ei_means            = EI_MEANS,
    ei_sds              = EI_SDS,
    ei_corr             = EI_CORR,
    seed                = 123,
    simplify_tolerance  = SIMPLIFY_TOLERANCE
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
    patience_bursts      = PATIENCE_BURSTS,
    soft_k               = SOFT_K,
    simplify_tolerance   = SIMPLIFY_TOLERANCE
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
  
  # Neutral plans file (if present) for the “representative neutral plan” plot
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

tx_plans = draw_tx_maps()


  
  
