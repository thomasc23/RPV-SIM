####################################################################################################
# Prepare Redistricting Plans for EI/ER
# Last Updated: 2025-09-01
####################################################################################################

suppressPackageStartupMessages({
  library(sf)
  library(readr)
  library(dplyr)
  library(tidyr)
  library(stringr)
})

rm(list = ls())
set.seed(14618)

# -------------------- CONFIG --------------------
# Use the same base as your other jobs (env wins; fallback to TX2/Output)
root_dir       = Sys.getenv("OUTPUT_BASE_DIR", unset = "TX2/Output")
neutral_dir    = file.path(root_dir, "neutral")
republican_dir = file.path(root_dir, "republican")
democratic_dir = file.path(root_dir, "democratic")

# precincts shapefile copied by your earlier pipeline into Output/
precincts_path = file.path(root_dir, "state_map.shp")

# write here (under Output/, so we don't touch earlier products)
out_dir = file.path(root_dir, "EI_ER_prepared")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# -------------------- HELPERS --------------------
stop_missing = function(df, req, label) {
  miss = setdiff(req, names(df))
  if (length(miss)) stop("Missing required columns in ", label, ": ", paste(miss, collapse = ", "))
}

natural_sort_plan_cols = function(cols) {
  ord = order(as.integer(gsub(".*_(\\d+)$", "\\1", cols)))
  cols[ord]
}

load_plan_file = function(csv_path, plan_type) {
  if (!file.exists(csv_path)) stop("File not found: ", csv_path)
  df = readr::read_csv(csv_path, show_col_types = FALSE)
  
  stopifnot("precinct_id" %in% names(df))
  id_cols = intersect(c("precinct_id", "orig_id", "init_CD"), names(df))
  
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
  
  plan_cols = natural_sort_plan_cols(plan_cols)
  
  long = df %>%
    select(any_of(c(id_cols, plan_cols))) %>%
    pivot_longer(
      cols = all_of(plan_cols),
      names_to = "plan_name",
      values_to = "district_id"
    ) %>%
    mutate(plan_type = tolower(plan_type))
  
  # stable 1..N index per plan_type (by numeric suffix order)
  idx = tibble(
    plan_type = tolower(plan_type),
    plan_name = plan_cols,
    map_id    = seq_along(plan_cols)
  )
  
  long = long %>% left_join(idx, by = c("plan_type", "plan_name"))
  
  list(assignments_long = long, index = idx)
}

validate_precincts = function(df) {
  req = c(
    "precinct_id", "population",
    "n_minority", "n_majority",
    "dem_votes", "rep_votes",
    "dem_votes_minority", "dem_votes_majority"
  )
  stop_missing(df, req, "precincts.csv")
  num_cols = setdiff(req, "precinct_id")
  df %>%
    mutate(across(all_of(num_cols), as.numeric),
           precinct_id = as.integer(precinct_id))
}

# -------------------- RUN --------------------
message("Reading plan CSVs ...")
neutral_plans    = load_plan_file(file.path(neutral_dir,    "CD_plans.csv"), "neutral")
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

readr::write_csv(assignments_long, file.path(out_dir, "assignments_long.csv"))
readr::write_csv(map_index,        file.path(out_dir, "map_index.csv"))

message("Validating precincts ...")
if (!file.exists(precincts_path)) stop("Missing shapefile: ", precincts_path)

precincts = sf::read_sf(precincts_path) %>%
  sf::st_drop_geometry() %>%
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
  select(any_of(c("precinct_id", "orig_id", "population", "n_minority", "n_majority",
                  "dem_votes", "rep_votes", "dem_votes_minority", "dem_votes_majority")))
precincts = validate_precincts(precincts)

readr::write_csv(precincts, file.path(out_dir, "precincts.csv"))

message("\nPrepared inputs written to: ", out_dir,
        "\n  - assignments_long.csv (precinct_id, plan_type, plan_name, map_id, district_id)",
        "\n  - map_index.csv        (plan_type, plan_name, map_id)",
        "\n  - precincts.csv        (validated copy)")
