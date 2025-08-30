# Check for required packages
required_packages = c("sf", "spatstat", "viridis", "spdep", "lwgeom",
                      "truncnorm", "tmvtnorm", "patchwork", "data.table",
                      "tidyverse", "reticulate", "MASS")

missing_packages = required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(paste("Missing required packages:", paste(missing_packages, collapse = ", "),
             "\nRun install_r_packages.sh to install them"))
}

# Load libraries
library(sf); library(lwgeom); library(spatstat); library(viridis)
library(spdep); library(truncnorm); library(tmvtnorm); library(patchwork)
library(data.table); library(MASS); library(tidyverse)

# Working directory
base_dir = Sys.getenv("BASE_DIR", getwd())
setwd(base_dir)

# Helpers
source('R/auxiliary.R')
get_num_env = function(var, default) {
  val = Sys.getenv(var, "")
  if (nzchar(val)) as.numeric(val) else default
}

# ---------- Run TX maps (neutral, R, D) ----------
draw_tx_maps = function() {
  
  # Core knobs (env first, with sensible defaults)
  N_PLANS          = as.integer(Sys.getenv("N_PLANS",         "1000"))
  ENSEMBLE_SIZE    = as.integer(Sys.getenv("ENSEMBLE_SIZE",   "100"))
  BURST_LENGTH     = as.integer(Sys.getenv("BURST_LENGTH",    "500"))
  NUM_BURSTS       = as.integer(Sys.getenv("NUM_BURSTS",      "30"))
  PATIENCE_BURSTS  = as.integer(Sys.getenv("PATIENCE_BURSTS", "10"))
  SOFT_K           = as.numeric(Sys.getenv("SOFT_K",          "60"))
  SIMPLIFY_TOL     = if (nzchar(Sys.getenv("SIMPLIFY_TOL",""))) as.numeric(Sys.getenv("SIMPLIFY_TOL")) else NA_real_
  RANDOM_SEED      = as.integer(Sys.getenv("RANDOM_SEED",     "123"))
  OUTPUT_BASE_DIR  = Sys.getenv("OUTPUT_BASE_DIR", "TX/Output")
  DEV_MODE         = tolower(Sys.getenv("DEV_MODE", "false")) %in% c("1","true","yes","t")
  SAVE_EVERY_STEPS = as.integer(Sys.getenv("SAVE_EVERY_STEPS", "50"))
  
  # Paths
  SHAPEFILE_IN        = Sys.getenv("SHAPEFILE_IN", "data/tx_24.shp")
  SHAPEFILE_FOR_PIPE  = Sys.getenv("SHAPEFILE_FOR_PIPE", file.path(OUTPUT_BASE_DIR, "state_map.shp"))
  OUTPUT_DIR          = OUTPUT_BASE_DIR
  
  # Fixed problem parameters
  NUM_STEPS      = N_PLANS
  POP_DEVIATION  = 0.01
  NUM_DISTRICTS  = 38
  
  # EI hyperparameters (env-exposed)
  EI_MEANS = c(
    white = get_num_env("EI_MEAN_WHITE", 0.35),
    black = get_num_env("EI_MEAN_BLACK", 0.90),
    hisp  = get_num_env("EI_MEAN_HISP",  0.65)
  )
  EI_SDS = c(
    white = get_num_env("EI_SD_WHITE", 0.08),
    black = get_num_env("EI_SD_BLACK", 0.10),
    hisp  = get_num_env("EI_SD_HISP",  0.12)
  )
  EI_CORR = list(
    rho_wb = get_num_env("EI_RHO_WB", 0.5),
    rho_wh = get_num_env("EI_RHO_WH", 0.5),
    rho_bh = get_num_env("EI_RHO_BH", 0.5)
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
    num_bursts          = NUM_BURSTS,
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