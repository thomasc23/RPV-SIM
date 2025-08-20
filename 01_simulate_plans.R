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
nc_pop = read_csv('~/Dropbox/RPV/Data/Clean/NC_precinct_votes_and_pop_by_race.csv')


# ===================================================================
# ==================== SIMULATION PARAMETERS ========================
# ===================================================================

# Set parameters
run_sim = function() {
  N_PRECINCTS = 830
  N_CENTERS = 2
  INPUT_DATA = NULL
  POP_MEAN = 2300
  POP_SD = 1500
  MINORITY_MEAN = 0.25
  MINORITY_SD = 0.30
  N_DISTRICTS = 14
  
  # Voting parameters
  VOTING_MODEL = "ei"
  PROB_MINORITY_DEM = 0.81
  PROB_MAJORITY_DEM = 0.37
  SD_MINORITY = 0.1
  SD_MAJORITY = 0.3
  RHO = 0.2
  B_MIN_CONTEXT = 0.1
  B_MAJ_CONTEXT = 0.3
  SIGMA_PRECINCT = 0.05
  VOTING_WEIGHT = 0.7
  
  # Grid parameters
  DECAY_RATE = 10        
  BASE_DENSITY = 0.1      
  PEAK_MULTIPLIER = 10.0   
  DECAY_POWER = 1          
  CENTER_RADIUS = 10     
  RANDOM_SEED = 14
  
  # Redistricting parameters
  N_PLANS = 1000
  ENSEMBLE_SIZE = 1000
  POP_DEVIATION = 0.01
  
  # Output directory
  OUTPUT_BASE_DIR = "Output/Tests/"
  
  # Run the analysis
  results = analyze_redistricting_impact(
    output_base_dir = OUTPUT_BASE_DIR,
    n_plans = N_PLANS,
    ensemble_size = ENSEMBLE_SIZE,
    n_districts = N_DISTRICTS,
    n_precincts = N_PRECINCTS,
    n_centers = N_CENTERS,
    input_data = INPUT_DATA,
    pop_mean = POP_MEAN,
    pop_sd = POP_SD,
    minority_mean = MINORITY_MEAN,
    minority_sd = MINORITY_SD,
    voting_model = VOTING_MODEL,
    prob_minority_dem = PROB_MINORITY_DEM,
    prob_majority_dem = PROB_MAJORITY_DEM,
    sd_minority = SD_MINORITY,
    sd_majority = SD_MAJORITY,
    rho = RHO,
    b_min_context = B_MIN_CONTEXT,
    b_maj_context = B_MAJ_CONTEXT,
    sigma_precinct = SIGMA_PRECINCT,
    voting_weight = VOTING_WEIGHT,
    decay_rate = DECAY_RATE,
    base_density = BASE_DENSITY,
    peak_multiplier = PEAK_MULTIPLIER,
    decay_power = DECAY_POWER,
    center_radius = CENTER_RADIUS,
    random_seed = RANDOM_SEED,
    pop_deviation = POP_DEVIATION
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
