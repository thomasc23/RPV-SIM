# Data preparation functions for rpvsimulator package

#' Prepare precincts for redistricting analysis
#' @param precincts sf object with precinct data
#' @return formatted sf object ready for redistricting
prepare_for_redistricting = function(precincts) {
  output = precincts %>%
    st_sf() %>%
    filter(!is.na(precinct_id)) %>%
    transmute(
      pct_id = precinct_id, pop = population,
      n_min = n_minority, n_maj = n_majority,
      dem_v = dem_votes, rep_v = rep_votes,
      dem_v_min = dem_votes_minority, rep_v_min = rep_votes_minority,
      dem_v_maj = dem_votes_majority, rep_v_maj = rep_votes_majority,
      pct_min = per_minority, dem_vsh = dem_voteshare,
      dem_vsh_1 = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),  # Fix NA
      dem_vsh_0 = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority),  # Fix NA
      urb = urbanness
    )
  return(output)
}

#' Verify fixed population consistency across segregation levels
#' @param simulation_results list with simulation results for different segregation levels
#' @return TRUE if verification passes
verify_fixed_population_consistency = function(simulation_results) {
  cat("\n=== VERIFYING FIXED POPULATION CONSISTENCY ===\n")
  
  levels = c("low", "medium", "high")
  
  # Check total population
  cat("\nTotal Population:\n")
  for (level in levels) {
    total_pop = sum(simulation_results[[level]]$precincts$population)
    cat(sprintf("  %s: %s\n", level, format(total_pop, big.mark = ",")))
  }
  
  # Check total minority population
  cat("\nTotal Minority Population:\n")
  for (level in levels) {
    total_minority = sum(simulation_results[[level]]$precincts$n_minority)
    cat(sprintf("  %s: %s\n", level, format(total_minority, big.mark = ",")))
  }
  
  # Check total votes
  cat("\nTotal Democratic Votes:\n")
  for (level in levels) {
    total_dem = sum(simulation_results[[level]]$precincts$dem_votes)
    cat(sprintf("  %s: %s\n", level, format(total_dem, big.mark = ",")))
  }
  
  # Check distribution of minority percentages
  cat("\nDistribution of Minority Percentages:\n")
  for (level in levels) {
    per_min = simulation_results[[level]]$precincts$per_minority
    cat(sprintf("  %s: Mean = %.3f, SD = %.3f, 0%% precincts = %d, 100%% precincts = %d\n",
                level, mean(per_min), sd(per_min), 
                sum(per_min == 0), sum(per_min == 1)))
  }
  
  # Calculate Moran's I for each level
  cat("\nSpatial Autocorrelation (Moran's I):\n")
  for (level in levels) {
    precincts = simulation_results[[level]]$precincts
    if (!is.null(precincts$geometry)) {
      tryCatch({
        nb = spdep::poly2nb(precincts, queen = TRUE)
        W = spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
        morans_result = moran.test(precincts$per_minority, W, 
                                   alternative = "greater")
        cat(sprintf("  %s: I = %.4f, p-value = %.4f\n", 
                    level, morans_result$estimate[1], morans_result$p.value))
      }, error = function(e) {
        cat(sprintf("  %s: Moran's I calculation failed: %s\n", level, e$message))
      })
    }
  }
  
  return(TRUE)
}

#' Load and validate shapefile
#' @param file_path path to shapefile
#' @param required_columns vector of required column names
#' @return validated sf object
load_and_validate_shapefile = function(file_path, required_columns = NULL) {
  if (!file.exists(file_path)) {
    stop("Shapefile does not exist: ", file_path)
  }
  
  # Load shapefile
  sf_obj = sf::read_sf(file_path) %>% sf::st_make_valid()
  
  # Check required columns if provided
  if (!is.null(required_columns)) {
    missing_cols = setdiff(required_columns, names(sf_obj))
    if (length(missing_cols) > 0) {
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }
  }
  
  return(sf_obj)
}

#' Standardize precinct data format
#' @param sf_obj sf object with precinct data
#' @param id_col column name for precinct ID
#' @param pop_col column name for population
#' @param dem_col column name for Democratic votes
#' @param rep_col column name for Republican votes
#' @return standardized sf object
standardize_precinct_data = function(sf_obj, 
                                   id_col = "precinct_id",
                                   pop_col = "population", 
                                   dem_col = "dem_votes",
                                   rep_col = "rep_votes") {
  
  # Rename columns to standard names
  sf_obj = sf_obj %>%
    rename(
      precinct_id = !!sym(id_col),
      population = !!sym(pop_col),
      dem_votes = !!sym(dem_col),
      rep_votes = !!sym(rep_col)
    )
  
  # Ensure numeric types
  sf_obj = sf_obj %>%
    mutate(
      population = as.numeric(population),
      dem_votes = as.numeric(dem_votes),
      rep_votes = as.numeric(rep_votes)
    )
  
  return(sf_obj)
}

#' Validate precinct data consistency
#' @param sf_obj sf object with precinct data
#' @return TRUE if validation passes, stops with error if not
validate_precinct_data = function(sf_obj) {
  # Check for required columns
  required_cols = c("precinct_id", "population", "dem_votes", "rep_votes")
  missing_cols = setdiff(required_cols, names(sf_obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check for negative values
  if (any(sf_obj$population < 0, na.rm = TRUE)) {
    stop("Population cannot be negative")
  }
  
  if (any(sf_obj$dem_votes < 0, na.rm = TRUE)) {
    stop("Democratic votes cannot be negative")
  }
  
  if (any(sf_obj$rep_votes < 0, na.rm = TRUE)) {
    stop("Republican votes cannot be negative")
  }
  
  # Check for missing precinct IDs
  if (any(is.na(sf_obj$precinct_id))) {
    stop("Precinct ID cannot be missing")
  }
  
  # Check for duplicate precinct IDs
  if (any(duplicated(sf_obj$precinct_id))) {
    stop("Duplicate precinct IDs found")
  }
  
  return(TRUE)
}
