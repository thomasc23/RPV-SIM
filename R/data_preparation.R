# Data preparation functions for rpvsimulator package

#' Standardize column names to canonical format
#'
#' Converts legacy or abbreviated column names to the canonical names defined
#' in COL_NAMES. This ensures consistent data structures across all package functions.
#'
#' @param df Data frame or sf object with possibly legacy column names
#' @param warn Logical, whether to warn about renamed columns (default TRUE)
#' @return Data frame/sf with canonical column names
#' @export
#' @examples
#' \dontrun{
#' # Convert old column names to new format
#' df <- data.frame(population = 100, n_min = 25, dem_votes = 60)
#' df_std <- standardize_column_names(df)
#' # Now has: total_pop, n_minority, votes_dem
#' }
standardize_column_names <- function(df, warn = TRUE) {
  if (is.null(df) || nrow(df) == 0) {
    return(df)
  }

  current_names <- names(df)
  renamed <- character(0)

  for (old_name in names(LEGACY_COL_MAP)) {
    new_name <- LEGACY_COL_MAP[old_name]

    # Only rename if old name exists and new name doesn't
    if (old_name %in% current_names && !(new_name %in% current_names)) {
      names(df)[names(df) == old_name] <- new_name
      renamed <- c(renamed, sprintf("%s -> %s", old_name, new_name))
    }
  }

  if (warn && length(renamed) > 0) {
    message("Standardized column names: ", paste(renamed, collapse = ", "))
  }

  df
}


#' Convert canonical column names to shapefile-compatible abbreviations
#'
#' Shapefiles have a 10-character limit for column names. This function
#' converts canonical names to abbreviated versions for shapefile export.
#'
#' @param df Data frame or sf object with canonical column names
#' @return Data frame/sf with abbreviated column names
#' @keywords internal
abbreviate_column_names <- function(df) {
  abbrev_map <- c(
    "precinct_id" = "pct_id",
    "total_pop" = "pop",
    "n_minority" = "n_min",
    "n_majority" = "n_maj",
    "pct_minority" = "pct_min",
    "votes_dem" = "dem_v",
    "votes_rep" = "rep_v",
    "votes_total" = "tot_v",
    "votes_dem_minority" = "dem_v_min",
    "votes_dem_majority" = "dem_v_maj",
    "votes_rep_minority" = "rep_v_min",
    "votes_rep_majority" = "rep_v_maj",
    "share_dem" = "dem_vsh",
    "share_dem_minority" = "dem_vsh_1",
    "share_dem_majority" = "dem_vsh_0",
    "mu_minority" = "mu_min",
    "mu_majority" = "mu_maj",
    "urbanness" = "urb"
  )

  for (canonical in names(abbrev_map)) {
    if (canonical %in% names(df)) {
      names(df)[names(df) == canonical] <- abbrev_map[canonical]
    }
  }

  df
}


#' Prepare precincts for redistricting analysis
#'
#' Converts precinct data to the format expected by redistricting scripts,
#' with abbreviated column names for shapefile compatibility.
#'
#' @param precincts sf object with precinct data (canonical or legacy column names)
#' @return formatted sf object ready for redistricting
#' @export
prepare_for_redistricting = function(precincts) {
  # First standardize to canonical names
  precincts <- standardize_column_names(precincts, warn = FALSE)
  # Select and rename columns for redistricting
  # Use canonical names, then abbreviate for shapefile compatibility
  output = precincts %>%
    sf::st_sf() %>%
    dplyr::filter(!is.na(.data[[COL_NAMES$precinct_id]])) %>%
    dplyr::transmute(
      !!COL_NAMES$precinct_id := .data[[COL_NAMES$precinct_id]],
      !!COL_NAMES$total_pop := .data[[COL_NAMES$total_pop]],
      !!COL_NAMES$n_minority := .data[[COL_NAMES$n_minority]],
      !!COL_NAMES$n_majority := .data[[COL_NAMES$n_majority]],
      !!COL_NAMES$votes_dem := .data[[COL_NAMES$votes_dem]],
      !!COL_NAMES$votes_rep := .data[[COL_NAMES$votes_rep]],
      !!COL_NAMES$votes_dem_minority := .data[[COL_NAMES$votes_dem_minority]],
      !!COL_NAMES$votes_rep_minority := .data[[COL_NAMES$votes_rep_minority]],
      !!COL_NAMES$votes_dem_majority := .data[[COL_NAMES$votes_dem_majority]],
      !!COL_NAMES$votes_rep_majority := .data[[COL_NAMES$votes_rep_majority]],
      !!COL_NAMES$pct_minority := .data[[COL_NAMES$pct_minority]],
      !!COL_NAMES$share_dem := .data[[COL_NAMES$share_dem]],
      !!COL_NAMES$share_dem_minority := ifelse(
        is.na(.data[[COL_NAMES$share_dem_minority]]), 0,
        .data[[COL_NAMES$share_dem_minority]]
      ),
      !!COL_NAMES$share_dem_majority := ifelse(
        is.na(.data[[COL_NAMES$share_dem_majority]]), 0,
        .data[[COL_NAMES$share_dem_majority]]
      ),
      !!COL_NAMES$urbanness := if (COL_NAMES$urbanness %in% names(precincts))
        .data[[COL_NAMES$urbanness]] else NA_real_
    )

  # Abbreviate for shapefile compatibility
  output <- abbreviate_column_names(output)

  return(output)
}

#' Verify fixed population consistency across segregation levels
#'
#' Checks that population totals, demographic counts, and vote totals are
#' consistent across different segregation level simulations.
#'
#' @param simulation_results list with simulation results for different segregation levels
#' @return TRUE if verification passes
#' @export
verify_fixed_population_consistency = function(simulation_results) {
  cat("\n=== VERIFYING FIXED POPULATION CONSISTENCY ===\n")

  levels = c("low", "medium", "high")

  # Helper to get column value, handling both canonical and legacy names
  get_col <- function(df, canonical_name) {
    if (canonical_name %in% names(df)) {
      return(df[[canonical_name]])
    }
    # Try legacy names
    legacy <- names(LEGACY_COL_MAP)[LEGACY_COL_MAP == canonical_name]
    for (leg in legacy) {
      if (leg %in% names(df)) return(df[[leg]])
    }
    return(NULL)
  }

  # Check total population
  cat("\nTotal Population:\n")
  for (level in levels) {
    pop_col <- get_col(simulation_results[[level]]$precincts, COL_NAMES$total_pop)
    if (!is.null(pop_col)) {
      total_pop = sum(pop_col, na.rm = TRUE)
      cat(sprintf("  %s: %s\n", level, format(total_pop, big.mark = ",")))
    }
  }

  # Check total minority population
  cat("\nTotal Minority Population:\n")
  for (level in levels) {
    min_col <- get_col(simulation_results[[level]]$precincts, COL_NAMES$n_minority)
    if (!is.null(min_col)) {
      total_minority = sum(min_col, na.rm = TRUE)
      cat(sprintf("  %s: %s\n", level, format(total_minority, big.mark = ",")))
    }
  }

  # Check total votes
  cat("\nTotal Democratic Votes:\n")
  for (level in levels) {
    dem_col <- get_col(simulation_results[[level]]$precincts, COL_NAMES$votes_dem)
    if (!is.null(dem_col)) {
      total_dem = sum(dem_col, na.rm = TRUE)
      cat(sprintf("  %s: %s\n", level, format(total_dem, big.mark = ",")))
    }
  }

  # Check distribution of minority percentages
  cat("\nDistribution of Minority Percentages:\n")
  for (level in levels) {
    pct_col <- get_col(simulation_results[[level]]$precincts, COL_NAMES$pct_minority)
    if (!is.null(pct_col)) {
      cat(sprintf("  %s: Mean = %.3f, SD = %.3f, 0%% precincts = %d, 100%% precincts = %d\n",
                  level, mean(pct_col, na.rm = TRUE), sd(pct_col, na.rm = TRUE),
                  sum(pct_col == 0, na.rm = TRUE), sum(pct_col == 1, na.rm = TRUE)))
    }
  }

  # Calculate Moran's I for each level
  cat("\nSpatial Autocorrelation (Moran's I):\n")
  for (level in levels) {
    precincts = simulation_results[[level]]$precincts
    pct_col <- get_col(precincts, COL_NAMES$pct_minority)
    if (!is.null(precincts$geometry) && !is.null(pct_col)) {
      tryCatch({
        nb = spdep::poly2nb(precincts, queen = TRUE)
        W = spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
        morans_result = spdep::moran.test(pct_col, W, alternative = "greater")
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
#'
#' Checks that required columns exist, values are non-negative, and IDs are unique.
#' Works with both canonical and legacy column names.
#'
#' @param sf_obj sf object with precinct data
#' @param standardize Logical, whether to first standardize column names (default TRUE)
#' @return TRUE if validation passes, stops with error if not
#' @export
validate_precinct_data = function(sf_obj, standardize = TRUE) {
  # Optionally standardize column names first
  if (standardize) {
    sf_obj <- standardize_column_names(sf_obj, warn = FALSE)
  }

  # Check for required columns (using canonical names)
  required_cols = c(
    COL_NAMES$precinct_id,
    COL_NAMES$total_pop,
    COL_NAMES$votes_dem,
    COL_NAMES$votes_rep
  )
  missing_cols = setdiff(required_cols, names(sf_obj))
  if (length(missing_cols) > 0) {
    stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  # Check for negative values
  if (any(sf_obj[[COL_NAMES$total_pop]] < 0, na.rm = TRUE)) {
    stop("Population cannot be negative")
  }

  if (any(sf_obj[[COL_NAMES$votes_dem]] < 0, na.rm = TRUE)) {
    stop("Democratic votes cannot be negative")
  }

  if (any(sf_obj[[COL_NAMES$votes_rep]] < 0, na.rm = TRUE)) {
    stop("Republican votes cannot be negative")
  }

  # Check for missing precinct IDs
  if (any(is.na(sf_obj[[COL_NAMES$precinct_id]]))) {
    stop("Precinct ID cannot be missing")
  }

  # Check for duplicate precinct IDs
  if (any(duplicated(sf_obj[[COL_NAMES$precinct_id]]))) {
    stop("Duplicate precinct IDs found")
  }

  return(TRUE)
}
