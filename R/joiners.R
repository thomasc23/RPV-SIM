#' Joiners: plans --> district assignment, precinct stats builder
#' @keywords internal
#' @importFrom dplyr left_join select mutate
#' @importFrom tibble tibble
NULL

#' Extract precinct --> district for a given map_id from a CD_plans.csv
#' @param plan_df tibble with precinct_id and step_<map_id> columns
#' @param map_id integer map id (column suffix in plans matrix)
#' @return tibble(precinct_id, district_id)
get_district_assignments = function(plan_df, map_id) {
  if (nrow(plan_df) == 0) return(tibble())
  
  col = paste0("step_", map_id)
  if (!col %in% names(plan_df)) return(tibble())  # partial availability
  
  tibble::tibble(
    precinct_id = as.integer(plan_df$precinct_id),
    district_id = as.integer(plan_df[[col]])
  )
}

#' Build precinct-level dataset for a (map_id, agg_level, mm_type)
#' @param plan_df CD_plans
#' @param precincts normalized precincts
#' @return tibble with canonical columns + tags
build_precinct_stats = function(plan_df, precincts, map_id, agg_level, mm_type) {
  assign = get_district_assignments(plan_df, map_id)
  if (nrow(assign) == 0 || nrow(precincts) == 0) return(tibble())
  out = dplyr::left_join(assign, precincts, by = "precinct_id") %>%
    dplyr::mutate(
      map_id = as.integer(map_id),
      agg_level = agg_level,
      mm_type = mm_type,
      # VAP convenience for EI
      minority_vap = dplyr::coalesce(.data$n_minority, 0L),
      majority_vap = dplyr::coalesce(.data$n_majority, 0L)
    )
  out
}

#' List available map ids from a map_summary tibble (fallback to plan_df columns)
#' @return integer vector
list_available_map_ids = function(map_summary, plan_df) {
  mids = integer()
  if (nrow(map_summary) > 0 && "map_id" %in% names(map_summary)) {
    mids = sort(unique(as.integer(map_summary$map_id)))
  } else if (nrow(plan_df) > 0) {
    # Plans: step_<id> columns
    step_cols = grep("^step_[0-9]+$", names(plan_df), value = TRUE)
    mids = as.integer(gsub("^step_", "", step_cols))
  }
  mids
}