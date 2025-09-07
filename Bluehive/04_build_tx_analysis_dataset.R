# 04_build_tx_analysis_dataset.R
# Build a unified district-level dataset for ER/EI evaluation (orientation-aware RPV)

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr); library(purrr)
})

rm(list = ls())

# ---------- CONFIG ----------
prep_dir      = "Output/BLUEHIVE MAPS/TEXAS/EI_ER_prepared"

er_path       = file.path(prep_dir, "ER_results_all_maps.csv")
ei_path       = file.path(prep_dir, "EI_results_all_maps.csv")
asgn_path     = file.path(prep_dir, "assignments_long.csv")
pct_path      = file.path(prep_dir, "precincts.csv")

out_panel_rds = file.path(prep_dir, "district_panel.rds")
out_panel_csv = file.path(prep_dir, "district_panel.csv")
out_map_csv   = file.path(prep_dir, "map_level_truth.csv")

# ---------- LOAD ----------
stopifnot(file.exists(er_path), file.exists(ei_path), file.exists(asgn_path), file.exists(pct_path))

er               = readr::read_csv(er_path, show_col_types = FALSE)
ei               = readr::read_csv(ei_path, show_col_types = FALSE)
assignments_long = readr::read_csv(asgn_path, show_col_types = FALSE)
precincts        = readr::read_csv(pct_path,  show_col_types = FALSE)

# Sanity: columns in EI/ER 
need_er = c("plan_type","map_id","district_id",
             "er_minority_dem_share","er_majority_dem_share","er_minority_se","er_majority_se","er_rpv",
             "true_minority_dem_share","true_majority_dem_share","true_rpv",
             "er_minority_resid","er_majority_resid","er_rpv_error","n_precincts","total_population")
need_ei = c("plan_type","map_id","district_id",
             "ei_minority_dem_share","ei_majority_dem_share","ei_minority_sd","ei_majority_sd","ei_rpv",
             "true_minority_dem_share","true_majority_dem_share","true_rpv",
             "ei_minority_resid","ei_majority_resid","total_population","n_precincts",
             "prop_minority","prop_dem")

miss_er = setdiff(need_er, names(er))
miss_ei = setdiff(need_ei, names(ei))
if (length(miss_er)) stop("ER missing cols: ", paste(miss_er, collapse=", "))
if (length(miss_ei)) stop("EI missing cols: ", paste(miss_ei, collapse=", "))

# ---------- MERGE ER + EI (no recompute of truth) ----------
key = c("plan_type","map_id","district_id")

# Keep only EI columns that don't clash with ER's truth/totals
ei_keep = ei %>%
  select(all_of(key),
         starts_with("ei_"),
         prop_minority, prop_dem)  # ER doesn't have these

panel = er %>%
  left_join(ei_keep, by = key)

# ---------- Oriented RPV helpers ----------
# Return orientation string if on opposite sides of 0.5, else NA:
#  - "min>50_maj<50" when minority > 0.5 & majority < 0.5
#  - "min<50_maj>50" when minority < 0.5 & majority > 0.5
get_orient = function(min_share, maj_share) {
  out = ifelse(is.na(min_share) | is.na(maj_share), NA_character_,
                ifelse(min_share > 0.5 & maj_share < 0.5, "min>50_maj<50",
                       ifelse(min_share < 0.5 & maj_share > 0.5, "min<50_maj>50", NA_character_)))
  out
}

# Given true and predicted orientations, build strict confusion labels where:
#  - TP: predicted orientation == true orientation (both show RPV in same direction)
#  - TN: both show no RPV (both orientations NA)
#  - FP: predicted shows RPV (any orientation) but truth shows none
#  - FN: truth shows RPV but predicted shows none OR shows the wrong direction
confusion_oriented = function(true_min, true_maj, pred_min, pred_maj) {
  true_or   = get_orient(true_min, true_maj)
  pred_or   = get_orient(pred_min, pred_maj)
  
  case = dplyr::case_when(
    is.na(true_or) & is.na(pred_or)            ~ "TN",
    is.na(true_or) & !is.na(pred_or)           ~ "FP",
    !is.na(true_or) & is.na(pred_or)           ~ "FN",
    !is.na(true_or) & !is.na(pred_or) & (true_or == pred_or) ~ "TP",
    !is.na(true_or) & !is.na(pred_or) & (true_or != pred_or) ~ "FN",  # wrong direction counts as FN
    TRUE ~ NA_character_
  )
  tibble::tibble(
    case        = case,
    correct     = as.numeric(case %in% c("TP","TN")),
    tp          = as.numeric(case == "TP"),
    tn          = as.numeric(case == "TN"),
    fp          = as.numeric(case == "FP"),
    fn          = as.numeric(case == "FN"),
    true_orient = true_or,
    pred_orient = pred_or
  )
}

# ---------- CONFUSION: orientation-aware (STRICT) ----------
# ER
er_conf = confusion_oriented(
  true_min = panel$true_minority_dem_share,
  true_maj = panel$true_majority_dem_share,
  pred_min = panel$er_minority_dem_share,
  pred_maj = panel$er_majority_dem_share
) %>%
  rename(
    er_rpv_case_strict    = case,
    er_rpv_correct_strict = correct,
    er_tp_strict          = tp,
    er_tn_strict          = tn,
    er_fp_strict          = fp,
    er_fn_strict          = fn,
    true_orient           = true_orient,  # keep canonical "true" once
    er_orient             = pred_orient
  )

# EI
ei_conf = confusion_oriented(
  true_min = panel$true_minority_dem_share,
  true_maj = panel$true_majority_dem_share,
  pred_min = panel$ei_minority_dem_share,
  pred_maj = panel$ei_majority_dem_share
) %>%
  select(-true_orient) %>%  # already present from ER block
  rename(
    ei_rpv_case_strict    = case,
    ei_rpv_correct_strict = correct,
    ei_tp_strict          = tp,
    ei_tn_strict          = tn,
    ei_fp_strict          = fp,
    ei_fn_strict          = fn,
    ei_orient             = pred_orient
  )

panel = bind_cols(panel, er_conf, ei_conf)

# (Optional) also keep simple "binary RPV" correctness if you want a lenient metric
panel = panel %>%
  mutate(
    true_rpv_binary = as.numeric(!is.na(true_orient)),
    er_rpv_binary   = as.numeric(!is.na(er_orient)),
    ei_rpv_binary   = as.numeric(!is.na(ei_orient)),
    er_rpv_case_loose = dplyr::case_when(
      is.na(true_rpv_binary) | is.na(er_rpv_binary) ~ NA_character_,
      true_rpv_binary == 1 & er_rpv_binary == 1     ~ "TP",
      true_rpv_binary == 0 & er_rpv_binary == 0     ~ "TN",
      true_rpv_binary == 0 & er_rpv_binary == 1     ~ "FP",
      true_rpv_binary == 1 & er_rpv_binary == 0     ~ "FN"
    ),
    er_rpv_correct_loose = as.numeric(er_rpv_case_loose %in% c("TP","TN")),
    ei_rpv_case_loose = dplyr::case_when(
      is.na(true_rpv_binary) | is.na(ei_rpv_binary) ~ NA_character_,
      true_rpv_binary == 1 & ei_rpv_binary == 1     ~ "TP",
      true_rpv_binary == 0 & ei_rpv_binary == 0     ~ "TN",
      true_rpv_binary == 0 & ei_rpv_binary == 1     ~ "FP",
      true_rpv_binary == 1 & ei_rpv_binary == 0     ~ "FN"
    ),
    ei_rpv_correct_loose = as.numeric(ei_rpv_case_loose %in% c("TP","TN"))
  )

# ---------- WASTED VOTES (packed / cracked) ----------
# Compute from precincts + assignments; do NOT alter ER/EI truth already in panel
req_pct = c("precinct_id","population","n_minority","n_majority",
             "dem_votes","rep_votes","dem_votes_minority","dem_votes_majority")
miss_pct = setdiff(req_pct, names(precincts))
if (length(miss_pct)) stop("precincts.csv missing: ", paste(miss_pct, collapse=", "))

wasted_by_district = assignments_long %>%
  select(plan_type, map_id, precinct_id, district_id) %>%
  inner_join(precincts, by = "precinct_id") %>%
  group_by(plan_type, map_id, district_id) %>%
  summarise(
    total_population        = sum(population),
    total_minority          = sum(n_minority),
    total_majority          = sum(n_majority),
    total_dem_votes         = sum(dem_votes),
    total_rep_votes         = sum(rep_votes),
    true_minority_dem_votes = sum(dem_votes_minority),
    true_majority_dem_votes = sum(dem_votes_majority),
    .groups = "drop"
  ) %>%
  mutate(
    prop_dem      = dplyr::if_else(total_dem_votes + total_rep_votes > 0,
                                   total_dem_votes / (total_dem_votes + total_rep_votes), NA_real_),
    prop_minority = dplyr::if_else(total_population > 0,
                                   total_minority / total_population, NA_real_),
    dem_district  = prop_dem > 0.5,
    votes_to_win  = floor(total_population / 2) + 1,
    
    true_minority_rep_votes = pmax(0, total_minority  - true_minority_dem_votes),
    true_majority_rep_votes = pmax(0, total_majority  - true_majority_dem_votes),
    
    # packed
    minority_dem_packed = ifelse(dem_district,
                                 pmax(0, true_minority_dem_votes - (votes_to_win * true_minority_dem_votes / total_dem_votes)),
                                 0),
    minority_rep_packed = ifelse(!dem_district,
                                 pmax(0, true_minority_rep_votes - (votes_to_win * true_minority_rep_votes / total_rep_votes)),
                                 0),
    majority_dem_packed = ifelse(dem_district,
                                 pmax(0, true_majority_dem_votes - (votes_to_win * true_majority_dem_votes / total_dem_votes)),
                                 0),
    majority_rep_packed = ifelse(!dem_district,
                                 pmax(0, true_majority_rep_votes - (votes_to_win * true_majority_rep_votes / total_rep_votes)),
                                 0),
    
    # cracked (all of losing party’s votes)
    minority_dem_cracked = ifelse(!dem_district, true_minority_dem_votes, 0),
    minority_rep_cracked = ifelse( dem_district, true_minority_rep_votes, 0),
    majority_dem_cracked = ifelse(!dem_district, true_majority_dem_votes, 0),
    majority_rep_cracked = ifelse( dem_district, true_majority_rep_votes, 0),
    
    # totals
    packed_min_votes   = minority_dem_packed + minority_rep_packed,
    cracked_min_votes  = minority_dem_cracked + minority_rep_cracked,
    packed_maj_votes   = majority_dem_packed + majority_rep_packed,
    cracked_maj_votes  = majority_dem_cracked + majority_rep_cracked,
    
    minority_wasted_total = packed_min_votes + cracked_min_votes,
    majority_wasted_total = packed_maj_votes + cracked_maj_votes,
    
    minority_waste_rate = dplyr::if_else(total_minority > 0, minority_wasted_total / total_minority, NA_real_),
    majority_waste_rate = dplyr::if_else(total_majority > 0, majority_wasted_total / total_majority, NA_real_),
    waste_disparity     = minority_waste_rate - majority_waste_rate,
    
    packed_votes  = packed_min_votes  + packed_maj_votes,
    cracked_votes = cracked_min_votes + cracked_maj_votes,
    total_wasted  = packed_votes + cracked_votes
  )

# ---------- MERGE WASTED INTO PANEL (no overwrite of ER/EI truth cols) ----------
panel = panel %>%
  left_join(
    wasted_by_district %>%
      select(plan_type, map_id, district_id,
             # add these metrics
             prop_minority, prop_dem, dem_district, votes_to_win,
             true_minority_dem_votes, true_majority_dem_votes,
             packed_votes, cracked_votes, total_wasted,
             packed_min_votes, cracked_min_votes,
             minority_wasted_total, majority_wasted_total,
             minority_waste_rate, majority_waste_rate, waste_disparity,
             total_minority, total_majority, total_dem_votes, total_rep_votes),
    by = key,
    suffix = c("", "_waste")
  ) %>%
  mutate(
    # If props already present (from EI), keep them; otherwise, use merged totals
    prop_minority = dplyr::coalesce(prop_minority, prop_minority_waste),
    prop_dem      = dplyr::coalesce(prop_dem,      prop_dem_waste)
  ) %>%
  select(-ends_with("_waste"))

# ---------- MAP-LEVEL SEATS ----------
map_level = panel %>%
  group_by(plan_type, map_id) %>%
  summarise(
    n_districts  = dplyr::n(),
    n_dem_seats  = sum(!is.na(prop_dem) & prop_dem > 0.5),
    n_rep_seats  = sum(!is.na(prop_dem) & prop_dem < 0.5),
    n_tied_seats = sum(!is.na(prop_dem) & prop_dem == 0.5),
    .groups = "drop"
  )

# ---------- SAVE ----------
saveRDS(panel, out_panel_rds)
readr::write_csv(panel, out_panel_csv)
readr::write_csv(map_level, out_map_csv)

cat("Saved:\n - ", out_panel_rds, "\n - ", out_panel_csv, "\n - ", out_map_csv, "\n", sep = "")

