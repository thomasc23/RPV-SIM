# Functions in this file read redistricting plans from data/ and ensure standard column naming and formatting. 

#' @keywords internal
#' @importFrom readr read_csv
#' @importFrom dplyr mutate select rename across everything coalesce
#' @importFrom purrr map_dfr quietly
#' @importFrom tibble tibble
#' @importFrom stringr str_detect str_replace
#' @importFrom fs dir_exists dir_ls path path_file
#' @importFrom glue glue
NULL

#' Safe CSV reader (return empty tibble on failure)
#' @param path character
#' @return tibble
safe_read_csv = function(path) {
  if (!file.exists(path)) return(tibble())
  out = try(readr::read_csv(path, show_col_types = FALSE, progress = FALSE), silent = TRUE)
  if (inherits(out, "try-error")) tibble() else out
}

short_mp_mk = function(mp_mk_type) {
  stopifnot(mp_mk_type %in% c("democratic", "neutral", "republican"))
  
  dplyr::case_when(
    mp_mk_type == "democratic" ~ "dem",
    mp_mk_type == "neutral" ~ "netural",
    mp_mk_type == "republican" ~ "rep",
    TRUE ~ NA_character_
  )
}

#' Find scenarios present under root path (data/)
#' Looks for low/medium/high + Summaries & Plans folders, plus (NEW) per-map CSVS under .../maps/
#' REQUIRED structure:
#' data/
#'   precincts.csv      (used for truth-only summaries)
#'   low|medium|high/
#'     democratic|neutral|republican/   
#'       maps/
#'         <anything>.csv  (columns: precinct, district)
#' @param data_root char
#' @return tibble with agg_level, mm_type (available subsets), and existing files
find_scenarios = function(data_root = "data/Simulated Plans") {
  stopifnot(fs::dir_exists(data_root))
  
  agg_levels = c("low", "medium", "high")
  mp_makers  = c("democratic", "neutral", "republican")
  
  rows = list()
  for (agg in agg_levels) {
    agg_dir = fs::path(data_root, agg)
    if (!fs::dir_exists(agg_dir)) next
  
    for (mp_mk_type in mp_makers) {
      mp_mk_dir = fs::path(agg_dir, mp_mk_type)
      maps_dir = fs::path(mp_mk_dir, "maps")
      
      rows[[length(rows) + 1]] = tibble(
        agg_level   = agg, 
        mp_mk_dir   = mp_mk_dir,
        mp_mk_type  = short_mp_mk(mp_mk_type),
        maps_dir    = maps_dir,
        n_maps      = if (fs::dir_exists(maps_dir)) length(fs::dir_ls(maps_dir, type="file", glob="*.csv")) else 0L,
        exists_maps = fs::dir_exists(maps_dir)
      )
    }
  }
  
  out = bind_rows(rows) %>%
    mutate(
      precincts = fs::path(data_root, "precincts.csv"),
      exists_precincts = fs::file_exists(precincts),
      map_summary = fs::path(mp_mk_dir, "Summaries", glue::glue("map_summaries_{ifelse(mp_mk_type=='neutral','neutral', ifelse(mp_mk_type=='rep','rep','dem'))}.csv")),
      district_summary = fs::path(mp_mk_dir, "Summaries", glue::glue("district_summaries_{ifelse(mp_mk_type=='neutral','neutral', ifelse(mp_mk_type=='rep','rep','dem'))}.csv")),
      plans = fs::path(mp_mk_dir, "Plans", "CD_plans.csv")
    ) %>%
    arrange(match(agg_level, c("low","medium","high")),
            match(mp_mk_type, c("dem","neutral","rep")))
  out
  
}

#' @param file
#' @param agg_level
#' @param mp_mk_type
#' @return 
read_map_assignment = function(file, agg_level, mp_mk_type) {
  
  df = safe_read_csv(file)
  stopifnot(nrow(df) > 0)
  
  map_id = suppressWarnings(as.integer(stringr::str_match(fs::path_file(file), "([0-9]+)")[, 2]))
  df %>%
    mutate(
      map_id = map_id,
      agg_level = agg_level,
      mp_mk_type  = mp_mk_type
    ) %>%
    select(agg_level, mp_mk_type, map_id, precinct, district)
}


#' Read *all* maps for one scenario row from find_scenarios()
#' @param scn_row
read_maps_for_scenario = function(scn_row) {
  stopifnot(scn_row$exists_maps)
  files = fs::dir_ls(scn_row$maps_dir, type = "file", glob = "*.csv")
  if (!length(files)) return(tibble())
  df = purrr::map_dfr(files, read_map_assignment, agg_level = scn_row$agg_level, mp_mk_type = scn_row$mp_mk_type)
  precincts = safe_read_csv('data/precincts.csv')
  df = df %>%
    left_join(precincts, by = c('precinct' = 'precinct_id')) %>%
    mutate(precinct_id = precinct, 
           district_id = district) %>%
    select(-precinct, -district)
  
  normalize_precincts(df)
}


#' Standardize precinct-level column names
#' @param df tibble of precincts
#' @return tibble with canonical column names
normalize_precincts = function(df) {
  if (nrow(df) == 0) return(tibble())
  
  # Catch bad column names (REMOVE EVENTUALLY BECAUSE THESE SHOULD BE SET IN SIM)
  nm = names(df)
  rename_map = c(
    precinct_id        = if ("precinct_id" %in% nm) "precinct_id" else if ("precinct" %in% nm) "precinct" else NA,
    population         = if ("population" %in% nm) "population" else if ("pop" %in% nm) "pop" else NA,
    per_minority       = if ("per_minority" %in% nm) "per_minority" else if ("minority_share" %in% nm) "minority_share" else NA,
    dem_voteshare      = if ("dem_voteshare" %in% nm) "dem_voteshare" else if ("dem_share" %in% nm) "dem_share" else NA,
    dem_votes          = if ("dem_votes" %in% nm) "dem_votes" else NA,
    rep_votes          = if ("rep_votes" %in% nm) "rep_votes" else NA,
    n_minority         = if ("n_minority" %in% nm) "n_minority" else NA,
    n_majority         = if ("n_majority" %in% nm) "n_majority" else NA,
    dem_votes_minority = if ("dem_votes_minority" %in% nm) "dem_votes_minority" else NA,
    dem_votes_majority = if ("dem_votes_majority" %in% nm) "dem_votes_majority" else NA
  )
  
  # Apply renames
  renamers = stats::na.omit(rename_map)
  names(renamers) = names(rename_map)[!is.na(rename_map)]
  out = dplyr::rename(df, dplyr::all_of(renamers))
  
  # If true group-level votes are missing, keep NA (truth_metrics handles NA safely)
  if (!("dem_votes_minority" %in% names(out))) out$dem_votes_minority = NA_real_
  if (!("dem_votes_majority" %in% names(out))) out$dem_votes_majority = NA_real_
  
  # Ensure types
  out %>%
    dplyr::mutate(
      precinct_id = as.integer(.data$precinct_id),
      population  = as.integer(.data$population),
      n_minority  = as.integer(.data$n_minority),
      n_majority  = as.integer(.data$n_majority),
      dem_votes   = as.integer(.data$dem_votes),
      rep_votes   = as.integer(.data$rep_votes)
    )
}


#' Normalize district summaries to cannonical names (helper)
normalize_district_summary = function(df, agg_level, mm_type) {
  if (nrow(df) == 0) return(tibble())
  
  if (!"map_id" %in% names(df) && "plan" %in% names(df)) df = dplyr::rename(df, map_id = .data$plan)
  df$agg_level = agg_level
  df$mm_type   = mm_type
  df
}


#' Load all data pieces for a given (agg_level, mm_type) that exist
#' @param scenario_row row from discover_scenarios()
#' @return list(precincts, plans, district_summary, map_summary)
load_scenario_parts = function(scenario_row) {
  pr = safe_read_csv(scenario_row$precincts) %>% normalize_precincts()
  pl = safe_read_csv(scenario_row$plans)
  ds = safe_read_csv(scenario_row$district_summary) %>% normalize_district_summary(scenario_row$agg_level, scenario_row$mm_type)
  ms = safe_read_csv(scenario_row$map_summary) %>% normalize_map_summary(scenario_row$agg_level, scenario_row$mm_type)
  list(precincts = pr, plans = pl, district_summary = ds, map_summary = ms)
}








