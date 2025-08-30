# Check for required packages
required_packages = c("sf", "spatstat", "viridis", "spdep", 
                       "truncnorm", "tmvtnorm", "patchwork", "data.table", 
                        "tidyverse", "reticulate")

missing_packages = required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "),
             "\nRun install_r_packages.sh to install them"))
}

# Load required libraries
library(sf)
library(spatstat)
library(viridis)
library(spdep)
library(truncnorm)
library(tmvtnorm)
library(patchwork)
library(data.table)
library(tidyverse)

# Set working directory 
base_dir = Sys.getenv("BASE_DIR", getwd())
setwd(base_dir)

# Source auxiliary functions and North Carolina precinct-level demographic data
source('R/auxiliary.R')

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
  # --- env var defaults (strings), safe on fresh R sessions ---
  POP_DEVIATION      = 0.01
  N_PLANS            = as.integer(Sys.getenv("N_PLANS",         "100"))
  ENSEMBLE_SIZE      = as.integer(Sys.getenv("ENSEMBLE_SIZE",   "20"))
  BURST_LENGTH       = as.integer(Sys.getenv("BURST_LENGTH",    "150"))
  NUM_BURSTS         = as.integer(Sys.getenv("NUM_BURSTS",      "15"))
  PATIENCE_BURSTS    = as.integer(Sys.getenv("PATIENCE_BURSTS", "6"))
  SOFT_K             = as.numeric(Sys.getenv("SOFT_K",          "60"))
  SIMPLIFY_TOL       = if (nzchar(Sys.getenv("SIMPLIFY_TOL",""))) as.numeric(Sys.getenv("SIMPLIFY_TOL")) else NA
  RANDOM_SEED        = as.integer(Sys.getenv("RANDOM_SEED",     "123"))
  OUTPUT_BASE_DIR    = Sys.getenv("OUTPUT_BASE_DIR", "Output/")

  # Short burst optimizer / dev parameters
  DEV_MODE           = tolower(Sys.getenv("DEV_MODE", "true")) %in% c("1","true","yes","t")
  
  # ITERATIVE SAVING
  
  SAVE_EVERY_STEPS   = as.integer(Sys.getenv("SAVE_EVERY_STEPS", "50"))
  RESUME             = TRUE 
  SCORE_MODELS       = FALSE
  
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
    save_every_steps   = SAVE_EVERY_STEPS,
    resume             = RESUME
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
