# Packages
library(tidyverse)
library(broom)

rm(list = ls())

setwd('~/Dropbox/RPV/Code/Simulation/Workflow/')

select = dplyr::select

# Suppress dplyr messages
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)

# Load the original results
res = readRDS('OLD/POPULATION_DENSITY/ER_EI_MRP/partial_results_20250706_114336.rds')

# Load the precinct data
precincts = read_csv('OLD/POPULATION_DENSITY/precincts.csv', show_col_types = FALSE)

# Load plan data
plan_data = list(
  low = read_csv('OLD/POPULATION_DENSITY/low/Plans/CD_plans.csv', show_col_types = FALSE),
  medium = read_csv('OLD/POPULATION_DENSITY/medium/Plans/CD_plans.csv', show_col_types = FALSE),
  high = read_csv('OLD/POPULATION_DENSITY/high/Plans/CD_plans.csv', show_col_types = FALSE)
)

# First, let's check the structure of the data
cat("Checking data structure...\n")
cat("ER columns:", names(res[[1]][[1]]$er), "\n")
cat("EI columns:", names(res[[1]][[1]]$ei), "\n\n")

# Function to process all maps for a given seg_level and redist_level at once
process_scenario = function(seg_level, redist_level, er_df, ei_df, plan_df, precincts) {
  
  cat("Processing", seg_level, "-", redist_level, "\n")
  
  # Add scenario identifiers
  er_df$seg_level = seg_level
  er_df$redist_level = redist_level
  ei_df$seg_level = seg_level
  ei_df$redist_level = redist_level
  
  # Get all map columns
  map_cols = names(plan_df)[grepl("^step_", names(plan_df))]
  
  # Create a long dataframe with all precinct-district-map assignments
  all_assignments = data.frame()
  
  for(col in map_cols) {
    map_id = as.numeric(sub("step_", "", col))
    
    # Only process maps that exist in the ER/EI results
    if(map_id %in% er_df$map_id) {
      temp_df = data.frame(
        precinct_id = plan_df$precinct_id,
        district_id = plan_df[[col]],
        map_id = map_id
      )
      all_assignments = rbind(all_assignments, temp_df)
    }
  }
  
  cat("  Total precinct-district-map combinations:", nrow(all_assignments), "\n")
  
  # Merge with precinct data
  precinct_map = merge(all_assignments, precincts, by = "precinct_id")
  
  # Prepare ER data for merging (select relevant columns)
  er_merge = er_df %>%
    select(map_id, district_id, seg_level, redist_level,
           n_precincts, total_population,
           er_minority_dem_share, er_majority_dem_share,
           er_minority_se, er_majority_se, er_rpv,
           er_minority_resid, er_majority_resid,
           true_minority_dem_share, true_majority_dem_share, true_rpv)
  
  # Prepare EI data for merging
  ei_merge = ei_df %>%
    select(map_id, district_id,
           ei_minority_dem_share, ei_majority_dem_share,
           ei_minority_sd, ei_majority_sd, ei_rpv,
           ei_minority_resid, ei_majority_resid,
           prop_minority, prop_dem)
  
  # Merge with ER estimates
  precinct_map = merge(precinct_map, er_merge, 
                       by = c("map_id", "district_id"), all.x = TRUE)
  
  # Merge with EI estimates
  precinct_map = merge(precinct_map, ei_merge, 
                       by = c("map_id", "district_id"), all.x = TRUE)
  
  # Calculate all precinct-level metrics
  precinct_map = precinct_map %>%
    mutate(
      # True vote shares at precinct level
      true_minority_dem_share_precinct = ifelse(n_minority > 0, dem_votes_minority / n_minority, NA),
      true_majority_dem_share_precinct = ifelse(n_majority > 0, dem_votes_majority / n_majority, NA),
      true_overall_dem_share_precinct = dem_votes / population,
      
      # True RPV at precinct level
      true_rpv_precinct = as.numeric(
        (true_minority_dem_share_precinct > 0.5 & true_majority_dem_share_precinct < 0.5) |
          (true_minority_dem_share_precinct < 0.5 & true_majority_dem_share_precinct > 0.5)
      ),
      
      # Predictions at precinct level (using district-level coefficients)
      er_pred_minority_share_precinct = er_minority_dem_share,
      er_pred_majority_share_precinct = er_majority_dem_share,
      ei_pred_minority_share_precinct = ei_minority_dem_share,
      ei_pred_majority_share_precinct = ei_majority_dem_share,
      
      # Predicted RPV at precinct level
      er_rpv_precinct = as.numeric(
        (er_pred_minority_share_precinct > 0.5 & er_pred_majority_share_precinct < 0.5) |
          (er_pred_minority_share_precinct < 0.5 & er_pred_majority_share_precinct > 0.5)
      ),
      ei_rpv_precinct = as.numeric(
        (ei_pred_minority_share_precinct > 0.5 & ei_pred_majority_share_precinct < 0.5) |
          (ei_pred_minority_share_precinct < 0.5 & ei_pred_majority_share_precinct > 0.5)
      ),
      
      # RPV correctness at precinct level
      er_rpv_correct_precinct = (er_rpv_precinct == true_rpv_precinct) * 1,
      ei_rpv_correct_precinct = (ei_rpv_precinct == true_rpv_precinct) * 1,
      
      # Errors at precinct level
      er_minority_error_precinct = abs(er_pred_minority_share_precinct - true_minority_dem_share_precinct),
      er_majority_error_precinct = abs(er_pred_majority_share_precinct - true_majority_dem_share_precinct),
      ei_minority_error_precinct = abs(ei_pred_minority_share_precinct - true_minority_dem_share_precinct),
      ei_majority_error_precinct = abs(ei_pred_majority_share_precinct - true_majority_dem_share_precinct),
      
      # Wasted votes calculations at precinct level
      precinct_dem_win = (dem_votes > rep_votes),
      precinct_votes_to_win = floor((dem_votes + rep_votes) / 2) + 1,
      
      # Democratic wasted votes
      precinct_dem_wasted_surplus = ifelse(precinct_dem_win & dem_votes > precinct_votes_to_win, 
                                           dem_votes - precinct_votes_to_win, 0),
      precinct_dem_wasted_losing = ifelse(!precinct_dem_win, dem_votes, 0),
      precinct_dem_wasted = precinct_dem_wasted_surplus + precinct_dem_wasted_losing,
      
      # Republican wasted votes
      precinct_rep_wasted_surplus = ifelse(!precinct_dem_win & rep_votes > precinct_votes_to_win, 
                                           rep_votes - precinct_votes_to_win, 0),
      precinct_rep_wasted_losing = ifelse(precinct_dem_win, rep_votes, 0),
      precinct_rep_wasted = precinct_rep_wasted_surplus + precinct_rep_wasted_losing,
      
      # Total wasted votes
      precinct_total_wasted = precinct_dem_wasted + precinct_rep_wasted,
      
      # Minority wasted votes (proportional allocation)
      minority_dem_wasted_surplus_precinct = ifelse(precinct_dem_win & dem_votes > 0,
                                                    dem_votes_minority * precinct_dem_wasted_surplus / dem_votes, 0),
      minority_dem_wasted_losing_precinct = ifelse(!precinct_dem_win & dem_votes > 0,
                                                   dem_votes_minority * precinct_dem_wasted_losing / dem_votes, 0),
      minority_dem_wasted_precinct = minority_dem_wasted_surplus_precinct + minority_dem_wasted_losing_precinct,
      
      minority_rep_wasted_surplus_precinct = ifelse(!precinct_dem_win & rep_votes > 0,
                                                    (n_minority - dem_votes_minority) * precinct_rep_wasted_surplus / rep_votes, 0),
      minority_rep_wasted_losing_precinct = ifelse(precinct_dem_win & rep_votes > 0,
                                                   (n_minority - dem_votes_minority) * precinct_rep_wasted_losing / rep_votes, 0),
      minority_rep_wasted_precinct = minority_rep_wasted_surplus_precinct + minority_rep_wasted_losing_precinct,
      
      minority_wasted_total_precinct = minority_dem_wasted_precinct + minority_rep_wasted_precinct,
      
      # Majority wasted votes (proportional allocation)
      majority_dem_wasted_surplus_precinct = ifelse(precinct_dem_win & dem_votes > 0,
                                                    dem_votes_majority * precinct_dem_wasted_surplus / dem_votes, 0),
      majority_dem_wasted_losing_precinct = ifelse(!precinct_dem_win & dem_votes > 0,
                                                   dem_votes_majority * precinct_dem_wasted_losing / dem_votes, 0),
      majority_dem_wasted_precinct = majority_dem_wasted_surplus_precinct + majority_dem_wasted_losing_precinct,
      
      majority_rep_wasted_surplus_precinct = ifelse(!precinct_dem_win & rep_votes > 0,
                                                    (n_majority - dem_votes_majority) * precinct_rep_wasted_surplus / rep_votes, 0),
      majority_rep_wasted_losing_precinct = ifelse(precinct_dem_win & rep_votes > 0,
                                                   (n_majority - dem_votes_majority) * precinct_rep_wasted_losing / rep_votes, 0),
      majority_rep_wasted_precinct = majority_rep_wasted_surplus_precinct + majority_rep_wasted_losing_precinct,
      
      majority_wasted_total_precinct = majority_dem_wasted_precinct + majority_rep_wasted_precinct,
      
      # Summary by type at precinct level
      minority_wasted_surplus_precinct = minority_dem_wasted_surplus_precinct + minority_rep_wasted_surplus_precinct,
      minority_wasted_losing_precinct = minority_dem_wasted_losing_precinct + minority_rep_wasted_losing_precinct,
      majority_wasted_surplus_precinct = majority_dem_wasted_surplus_precinct + majority_rep_wasted_surplus_precinct,
      majority_wasted_losing_precinct = majority_dem_wasted_losing_precinct + majority_rep_wasted_losing_precinct,
      total_wasted_surplus_precinct = precinct_dem_wasted_surplus + precinct_rep_wasted_surplus,
      total_wasted_losing_precinct = precinct_dem_wasted_losing + precinct_rep_wasted_losing,
      
      # Additional columns
      prop_minority_precinct = n_minority / population,
      prop_dem_precinct = dem_votes / (dem_votes + rep_votes),
      
      # Create unique identifier
      unique_id = paste(map_id, district_id, precinct_id, seg_level, redist_level, sep = "_")
    )
  
  return(precinct_map)
}

# Process all scenarios
all_precinct_data = data.frame()

for (seg_level in names(res)) {
  for (redist_level in names(res[[seg_level]])) {
    
    # Extract data for this scenario
    er_df = res[[seg_level]][[redist_level]]$er
    ei_df = res[[seg_level]][[redist_level]]$ei
    plan_df = plan_data[[seg_level]]
    
    # Process this scenario
    scenario_data = process_scenario(seg_level, redist_level, er_df, ei_df, plan_df, precincts)
    
    # Add to combined dataframe
    all_precinct_data = rbind(all_precinct_data, scenario_data)
  }
}

cat("\nTotal rows processed:", nrow(all_precinct_data), "\n")

# Add map-level statistics
all_precinct_data = all_precinct_data %>%
  group_by(map_id, seg_level, redist_level) %>%
  mutate(
    n_dem_seats = n_distinct(district_id[prop_dem > 0.5]),
    n_rep_seats = n_distinct(district_id[prop_dem <= 0.5]),
    n_maj_min = n_distinct(district_id[prop_minority > 0.5])
  ) %>%
  ungroup()

# Save the precinct-level dataset
saveRDS(all_precinct_data, 'OLD/POPULATION_DENSITY/ER_EI_precinct_results.rds')

# Print summary statistics
cat("\nSummary Statistics:\n")
cat("Total precinct-map-district combinations:", nrow(all_precinct_data), "\n")
cat("Unique precincts:", n_distinct(all_precinct_data$precinct_id), "\n")
cat("Unique maps:", n_distinct(all_precinct_data$map_id), "\n")
cat("Unique districts:", n_distinct(paste(all_precinct_data$map_id, all_precinct_data$district_id)), "\n")

# Check for missing values in key columns
key_cols = c("true_minority_dem_share_precinct", "er_pred_minority_share_precinct", 
             "ei_pred_minority_share_precinct", "precinct_dem_wasted")
missing_summary = sapply(all_precinct_data[key_cols], function(x) sum(is.na(x)))
cat("\nMissing values in key columns:\n")
print(missing_summary)

# Clean up duplicates if necessary (following the same logic as district-level)
# Identify maps assigned to multiple redist_levels
duplicate_maps = all_precinct_data %>%
  select(seg_level, map_id, redist_level) %>%
  distinct() %>%
  group_by(seg_level, map_id) %>%
  summarise(n_redist_levels = n_distinct(redist_level), 
            redist_levels = paste(sort(unique(redist_level)), collapse = ", ")) %>%
  filter(n_redist_levels > 1)

if(nrow(duplicate_maps) > 0) {
  cat("\nNumber of maps with multiple redist_levels:", nrow(duplicate_maps), "\n")
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Create a lookup table with one redist_level per (seg_level, map_id)
  map_assignments = all_precinct_data %>%
    select(seg_level, map_id, redist_level) %>%
    distinct() %>%
    group_by(seg_level, map_id) %>%
    sample_n(1) %>%  # Randomly pick one redist_level per map to keep
    ungroup()
  
  # Keep only the rows that match the selected assignments
  all_precinct_data_cleaned = all_precinct_data %>%
    semi_join(map_assignments, by = c("seg_level", "map_id", "redist_level"))
  
  # Save cleaned data
  saveRDS(all_precinct_data_cleaned, 'OLD/POPULATION_DENSITY/ER_EI_precinct_results_cleaned.rds')
  
  cat("Original rows:", nrow(all_precinct_data), "\n")
  cat("Cleaned rows:", nrow(all_precinct_data_cleaned), "\n")
  cat("Rows removed:", nrow(all_precinct_data) - nrow(all_precinct_data_cleaned), "\n")
} else {
  cat("\nNo duplicate maps found. Saving original data as cleaned version.\n")
  saveRDS(all_precinct_data, 'OLD/POPULATION_DENSITY/ER_EI_precinct_results_cleaned.rds')
}
