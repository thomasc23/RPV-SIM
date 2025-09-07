################################################################################################################################################################################################################################################
# This File Prepares Redistricing Plans for EI/ER runs
# Last Updated: 09/01/25
################################################################################################################################################################################################################################################

################################################################################################################################################################################################################################################
# Setup
################################################################################################################################################################################################################################################

# Packages
require(mvtnorm)
require(eiCompare)
require(ei)
require(sf)
require(tidyverse)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

select = dplyr::select

# Suppress dplyr messages
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)

set.seed(14618)


# -------------------- CONFIG --------------------
# Point at Texas outputs + precinct data
root_dir       = "Output/BLUEHIVE MAPS"               # where the three plan dirs live
neutral_dir    = file.path(root_dir, "neutral")
republican_dir = file.path(root_dir, "republican")
democratic_dir = file.path(root_dir, "democratic")

precincts_path = file.path(root_dir, "state_map.shp")        # must contain columns used by EI/ER
out_dir        = paste0(root_dir, "/EI_ER_prepared")              
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# -------------------- HELPERS --------------------
stop_missing = function(df, req, label) {
  miss = setdiff(req, names(df))
  if (length(miss)) {
    stop("Missing required columns in ", label, ": ", paste(miss, collapse = ", "))
  }
}

# Natural sort plan columns by trailing integer
natural_sort_plan_cols = function(cols) {
  ord = order(as.integer(gsub(".*_(\\d+)$", "\\1", cols)))
  cols[ord]
}

load_plan_file = function(csv_path, plan_type) {
  if (!file.exists(csv_path)) stop("File not found: ", csv_path)
  df = readr::read_csv(csv_path, show_col_types = FALSE)
  
  # minimal ID columns we care about
  stopifnot("precinct_id" %in% names(df))
  id_cols = intersect(c("precinct_id", "orig_id", "init_CD"), names(df))
  
  # discover plan columns by type
  pat = switch(
    tolower(plan_type),
    "neutral"     = "^step_\\d+$",
    "republican"  = "^republican_map_\\d+$",
    "democratic"  = "^democratic_map_\\d+$",
    stop("Unknown plan_type: ", plan_type)
  )
  plan_cols = grep(pat, names(df), value = TRUE)
  if (!length(plan_cols)) {
    stop("No plan columns matching ", pat, " in ", csv_path,
         "\nFound columns: ", paste(head(names(df), 20), collapse = ", "), " ...")
  }
  
  # ensure numerical order (... _2 comes before ..._10)
  plan_cols = natural_sort_plan_cols(plan_cols)
  
  # melt to long
  long = df %>%
    select(all_of(c(id_cols, plan_cols))) %>%
    pivot_longer(
      cols = all_of(plan_cols),
      names_to = "plan_name",
      values_to = "district_id"
    ) %>%
    mutate(plan_type = tolower(plan_type))
  
  # attach per-type 1..N map_id, stable over re-runs (by numeric suffix of plan_name)
  idx = tibble(
    plan_type = tolower(plan_type),
    plan_name = plan_cols,
    map_id    = seq_along(plan_cols)  # 1..N for that plan_type
  )
  
  long = long %>% left_join(idx, by = c("plan_type", "plan_name"))
  
  list(assignments_long = long, index = idx)
}

# Validate precincts contain everything your ER/EI code uses
validate_precincts = function(df) {
  # Ground truth functions in your script use these:
  req = c(
    "precinct_id", "population",
    "n_minority", "n_majority",
    "dem_votes", "rep_votes",
    "dem_votes_minority", "dem_votes_majority"
  )
  stop_missing(df, req, "precincts.csv")
  
  # sanity types
  num_cols = setdiff(req, "precinct_id")
  df = df %>%
    mutate(across(all_of(num_cols), as.numeric)) %>%
    mutate(precinct_id = as.integer(precinct_id))
  df
}


# -------------------- RUN --------------------
message("Reading plan CSVs ...")
neutral_plans    = load_plan_file(file.path(neutral_dir, "CD_plans.csv"),    "neutral")
republican_plans = load_plan_file(file.path(republican_dir, "CD_plans.csv"), "republican")
democratic_plans = load_plan_file(file.path(democratic_dir, "CD_plans.csv"), "democratic")

message("Binding assignments ...")
assignments_long = bind_rows(
  neutral_plans$assignments_long,
  republican_plans$assignments_long,
  democratic_plans$assignments_long
)

map_index = bind_rows(
  neutral_plans$index,
  republican_plans$index,
  democratic_plans$index
)

# Write tidy plan inputs
readr::write_csv(assignments_long, file.path(out_dir, "assignments_long.csv"))
readr::write_csv(map_index,        file.path(out_dir, "map_index.csv"))

message("Validating precincts ...")

precincts = read_sf(precincts_path) %>%
  st_drop_geometry() %>%
  mutate(
    precinct_id = pct_id,
    population  = pop, 
    n_minority  = n_min,
    n_majority  = n_maj,
    dem_votes   = dem_v, 
    rep_votes   = rep_v,
    dem_votes_minority = dem_v_min,
    dem_votes_majority = dem_v_maj
  ) %>%
  select(precinct_id, orig_id, population, n_minority, n_majority,
         dem_votes, rep_votes, dem_votes_minority, dem_votes_majority)
precincts = validate_precincts(precincts)

# Keep a clean copy alongside the prepared inputs
readr::write_csv(precincts, file.path(out_dir, "precincts.csv"))

message("\nPrepared inputs written to: ", out_dir,
        "\n  - assignments_long.csv (precinct_id, plan_type, plan_name, map_id, district_id)",
        "\n  - map_index.csv        (plan_type, plan_name, map_id)",
        "\n  - precincts.csv        (validated copy)")









