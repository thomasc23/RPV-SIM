# # Packages
# library(tidyverse)
# library(broom)
# 
# # library(profvis)
# 
# rm(list = ls())
# 
# setwd('~/Dropbox/RPV/Code/Simulation/Workflow/')
# 
# select = dplyr::select
# 
# # Suppress dplyr messages
# library(dplyr, warn.conflicts = FALSE)
# options(dplyr.summarise.inform = FALSE)
# 
# 
# res = readRDS('OLD/POPULATION_DENSITY/ER_EI_MRP/partial_results_20250706_114336.rds')
# head(res)
# 
# 
# 
# # Extract all data 
# all_data = data.frame()
# 
# for (seg_level in names(res)) {
#   for (redist_level in names(res[[seg_level]])) {
#     
#     # Extract ER data
#     er_df = res[[seg_level]][[redist_level]]$er
#     rownames(er_df) = NULL
#     
#     er_df$seg_level = seg_level
#     er_df$redist_level = redist_level
#     
#     # Extract EI data
#     ei_df = res[[seg_level]][[redist_level]]$ei
#     rownames(ei_df) = NULL
#     
#     ei_df$seg_level = seg_level
#     ei_df$redist_level = redist_level
#     
#     # Create unique identifier for merging
#     er_df$unique_id = paste(er_df$map_id, er_df$district_id, er_df$seg_level, er_df$redist_level, sep = "_")
#     ei_df$unique_id = paste(ei_df$map_id, ei_df$district_id, ei_df$seg_level, ei_df$redist_level, sep = "_")
#     
#     # Select columns from ER (including ground truth)
#     er_selected = er_df %>%
#       select(unique_id, map_id, district_id, seg_level, redist_level, 
#              n_precincts, total_population,
#              true_minority_dem_share, true_majority_dem_share, true_rpv,
#              er_minority_dem_share, er_majority_dem_share, 
#              er_minority_se, er_majority_se, er_rpv, 
#              er_minority_resid, er_majority_resid)
#     
#     # Select columns from EI
#     ei_selected = ei_df %>%
#       select(unique_id, prop_minority, prop_dem,
#              ei_minority_dem_share, ei_majority_dem_share, 
#              ei_minority_sd, ei_majority_sd, ei_rpv, 
#              ei_minority_resid, ei_majority_resid) %>%
#       rename(ei_minority_se = ei_minority_sd,
#              ei_majority_se = ei_majority_sd)
#     
#     # Merge on unique_id
#     scenario_data = merge(er_selected, ei_selected, 
#                           by = "unique_id", 
#                           all.x = TRUE)
#     
#     # Remove the unique_id column
#     # scenario_data$unique_id = NULL
#     
#     # Add RPV correctness indicators
#     scenario_data$er_rpv_correct = ifelse(
#       (scenario_data$true_minority_dem_share > 0.5 & 
#          scenario_data$true_majority_dem_share < 0.5 &
#          scenario_data$er_minority_dem_share > 0.5 & 
#          scenario_data$er_majority_dem_share < 0.5) |
#         (scenario_data$true_minority_dem_share < 0.5 & 
#            scenario_data$true_majority_dem_share > 0.5 &
#            scenario_data$er_minority_dem_share < 0.5 & 
#            scenario_data$er_majority_dem_share > 0.5),
#       1, 0
#     )
#     
#     scenario_data$ei_rpv_correct = ifelse(
#       (scenario_data$true_minority_dem_share > 0.5 & 
#          scenario_data$true_majority_dem_share < 0.5 &
#          scenario_data$ei_minority_dem_share > 0.5 & 
#          scenario_data$ei_majority_dem_share < 0.5) |
#         (scenario_data$true_minority_dem_share < 0.5 & 
#            scenario_data$true_majority_dem_share > 0.5 &
#            scenario_data$ei_minority_dem_share < 0.5 & 
#            scenario_data$ei_majority_dem_share > 0.5),
#       1, 0
#     )
#     
#     # Add to all_data
#     all_data = rbind(all_data, scenario_data)
#   }
# }
# 
# all_data
# 
# 
# 
# 
# 
# saveRDS(all_data, 'OLD/POPULATION_DENSITY/ER_EI_results.rds')
# 
# 
# 
# 
# 
# 
# # Add ground truth info at district level
# 
# # Set working directory
# setwd('~/Dropbox/RPV/Code/Simulation/Workflow/')
# subdir = 'OLD/POPULATION_DENSITY/'
# all_data = readRDS('OLD/POPULATION_DENSITY/ER_EI_results.rds')
# 
# # Load the precinct data
# precincts = read_csv(paste0(subdir, '/precincts.csv'), show_col_types = FALSE)
# 
# # Load plan data
# plan_data = list(
#   low = read_csv(paste0(subdir, '/low/Plans/CD_plans.csv'), show_col_types = FALSE),
#   medium = read_csv(paste0(subdir, '/medium/Plans/CD_plans.csv'), show_col_types = FALSE),
#   high = read_csv(paste0(subdir, '/high/Plans/CD_plans.csv'), show_col_types = FALSE)
# )
# 
# # Helper functions
# get_district_assignments = function(plan_df, map_id) {
#   col_name = paste0("step_", map_id)
#   if(!(col_name %in% names(plan_df))) {
#     stop(paste("Map ID", map_id, "not found in plan data"))
#   }
#   data.frame(
#     precinct_id = plan_df$precinct_id,
#     district_id = plan_df[[col_name]]
#   )
# }
# 
# calculate_precinct_stats = function(map_id, plan_df, precinct_data, agg_level, partisan) {
#   assignments = get_district_assignments(plan_df, map_id)
#   precinct_stats = merge(precinct_data, assignments, by = "precinct_id")
#   precinct_stats$agg_level = agg_level
#   precinct_stats$partisan = partisan
#   precinct_stats$map_id = map_id
#   if (!"minority_vap" %in% names(precinct_stats)) {
#     precinct_stats$minority_vap = precinct_stats$n_minority
#     precinct_stats$majority_vap = precinct_stats$n_majority
#   }
#   return(precinct_stats)
# }
# 
# # Combined function to calculate ground truth and wasted votes
# calculate_ground_truth_and_wasted_votes = function(precinct_stats) {
#   district_truth = precinct_stats %>%
#     group_by(map_id, district_id, agg_level, partisan) %>%
#     summarise(
#       # Basic counts
#       total_population = sum(population),
#       total_minority = sum(n_minority),
#       total_majority = sum(n_majority),
#       total_dem_votes = sum(dem_votes),
#       total_rep_votes = sum(rep_votes),
#       true_minority_dem_votes = sum(dem_votes_minority),
#       true_majority_dem_votes = sum(dem_votes_majority),
#       true_minority_dem_share = sum(dem_votes_minority) / sum(n_minority),
#       true_majority_dem_share = sum(dem_votes_majority) / sum(n_majority),
#       true_overall_dem_share = sum(dem_votes) / sum(population),
#       n_precincts = n(),
#       .groups = 'drop'
#     ) %>%
#     mutate(
#       # Calculate RPV
#       true_rpv = as.numeric(
#         (true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) |
#           (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5)
#       ),
#       
#       # Calculate winner and votes needed
#       prop_dem = total_dem_votes / (total_dem_votes + total_rep_votes),
#       dem_win = prop_dem > 0.5,
#       votes_to_win = floor((total_dem_votes + total_rep_votes) / 2) + 1,
#       
#       # Calculate minority votes by party
#       true_minority_rep_votes = total_minority - true_minority_dem_votes,
#       true_majority_rep_votes = total_majority - true_majority_dem_votes,
#       
#       # FIRST: Calculate all the detailed breakdowns
#       
#       # Overall surplus and losing wasted votes
#       dem_wasted_surplus = ifelse(dem_win, pmax(0, total_dem_votes - votes_to_win), 0),
#       dem_wasted_losing = ifelse(!dem_win, total_dem_votes, 0),
#       rep_wasted_surplus = ifelse(!dem_win, pmax(0, total_rep_votes - votes_to_win), 0),
#       rep_wasted_losing = ifelse(dem_win, total_rep_votes, 0),
#       
#       # Minority surplus wasted votes (proportional allocation when party wins)
#       minority_dem_wasted_surplus = ifelse(dem_win & total_dem_votes > 0,
#                                            pmax(0, true_minority_dem_votes * (total_dem_votes - votes_to_win) / total_dem_votes),
#                                            0),
#       minority_dem_wasted_losing = ifelse(!dem_win, true_minority_dem_votes, 0),
#       
#       minority_rep_wasted_surplus = ifelse(!dem_win & total_rep_votes > 0,
#                                            pmax(0, true_minority_rep_votes * (total_rep_votes - votes_to_win) / total_rep_votes),
#                                            0),
#       minority_rep_wasted_losing = ifelse(dem_win, true_minority_rep_votes, 0),
#       
#       # Majority surplus wasted votes (proportional allocation when party wins)
#       majority_dem_wasted_surplus = ifelse(dem_win & total_dem_votes > 0,
#                                            pmax(0, true_majority_dem_votes * (total_dem_votes - votes_to_win) / total_dem_votes),
#                                            0),
#       majority_dem_wasted_losing = ifelse(!dem_win, true_majority_dem_votes, 0),
#       
#       majority_rep_wasted_surplus = ifelse(!dem_win & total_rep_votes > 0,
#                                            pmax(0, true_majority_rep_votes * (total_rep_votes - votes_to_win) / total_rep_votes),
#                                            0),
#       majority_rep_wasted_losing = ifelse(dem_win, true_majority_rep_votes, 0),
#       
#       # SECOND: Calculate the original columns as sums of the breakdowns
#       
#       # Overall wasted votes (sum of surplus and losing)
#       dem_wasted = dem_wasted_surplus + dem_wasted_losing,
#       rep_wasted = rep_wasted_surplus + rep_wasted_losing,
#       total_wasted = dem_wasted + rep_wasted,
#       
#       # Minority wasted votes (sum of surplus and losing)
#       minority_dem_wasted = minority_dem_wasted_surplus + minority_dem_wasted_losing,
#       minority_rep_wasted = minority_rep_wasted_surplus + minority_rep_wasted_losing,
#       minority_wasted_total = minority_dem_wasted + minority_rep_wasted,
#       
#       # Majority wasted votes (sum of surplus and losing)
#       majority_dem_wasted = majority_dem_wasted_surplus + majority_dem_wasted_losing,
#       majority_rep_wasted = majority_rep_wasted_surplus + majority_rep_wasted_losing,
#       majority_wasted_total = majority_dem_wasted + majority_rep_wasted,
#       
#       # Summary totals by type
#       minority_wasted_surplus = minority_dem_wasted_surplus + minority_rep_wasted_surplus,
#       minority_wasted_losing = minority_dem_wasted_losing + minority_rep_wasted_losing,
#       majority_wasted_surplus = majority_dem_wasted_surplus + majority_rep_wasted_surplus,
#       majority_wasted_losing = majority_dem_wasted_losing + majority_rep_wasted_losing,
#       total_wasted_surplus = dem_wasted_surplus + rep_wasted_surplus,
#       total_wasted_losing = dem_wasted_losing + rep_wasted_losing
#     )
#   
#   return(district_truth)
# }
# 
# # Get unique combinations of seg_level, redist_level, and map_id from existing data
# unique_scenarios = all_data %>%
#   select(seg_level, redist_level, map_id) %>%
#   distinct()
# 
# # Initialize dataframe to store all results
# all_results = data.frame()
# 
# # Process each unique scenario
# pb = txtProgressBar(min = 0, max = nrow(unique_scenarios), style = 3)
# 
# for(i in 1:nrow(unique_scenarios)) {
#   scenario = unique_scenarios[i,]
#   
#   # Get the plan data for this segregation level
#   plan_df = plan_data[[scenario$seg_level]]
#   
#   # Calculate precinct stats
#   precinct_stats = calculate_precinct_stats(
#     map_id = scenario$map_id,
#     plan_df = plan_df,
#     precinct_data = precincts,
#     agg_level = scenario$seg_level,
#     partisan = scenario$redist_level
#   )
#   
#   # Calculate ground truth AND wasted votes in one go
#   results = calculate_ground_truth_and_wasted_votes(precinct_stats)
#   
#   # Rename columns to match what we need
#   results = results %>%
#     rename(seg_level = agg_level, redist_level = partisan)
#   
#   # Add to combined dataframe
#   all_results = rbind(all_results, results)
#   
#   setTxtProgressBar(pb, i)
# }
# 
# close(pb)
# 
# # Create unique ID for merging
# all_results$unique_id = paste(
#   all_results$map_id,
#   all_results$district_id,
#   all_results$seg_level,
#   all_results$redist_level,
#   sep = "_"
# )
# 
# # Select columns to add to existing data
# results_to_add = all_results %>%
#   select(unique_id, 
#          # Ground truth columns
#          total_minority, total_majority, 
#          total_dem_votes, total_rep_votes,
#          true_minority_dem_votes, true_majority_dem_votes,
#          true_overall_dem_share,
#          # Wasted votes columns
#          dem_win, votes_to_win,
#          dem_wasted, rep_wasted, total_wasted,
#          # Breakdowns by group
#          minority_dem_wasted, minority_rep_wasted, minority_wasted_total,
#          majority_dem_wasted, majority_rep_wasted, majority_wasted_total,
#          # Breakdowns by type
#          minority_dem_wasted_surplus, minority_dem_wasted_losing,
#          minority_rep_wasted_surplus, minority_rep_wasted_losing,
#          majority_dem_wasted_surplus, majority_dem_wasted_losing,
#          majority_rep_wasted_surplus, majority_rep_wasted_losing,
#          minority_wasted_surplus, minority_wasted_losing, 
#          majority_wasted_surplus, majority_wasted_losing,
#          total_wasted_surplus, total_wasted_losing
#          )
# 
# # Merge with existing data
# all_data_enhanced = all_data %>%
#   left_join(results_to_add, by = "unique_id")
# 
# # Check the merge worked
# cat("\nOriginal data rows:", nrow(all_data), "\n")
# cat("Enhanced data rows:", nrow(all_data_enhanced), "\n")
# cat("Rows with missing data:", sum(is.na(all_data_enhanced$true_minority_dem_votes)), "\n")
# 
# # Verify the existing ground truth matches what we calculated
# sample_check = all_data_enhanced %>%
#   filter(!is.na(true_minority_dem_votes)) %>%
#   slice_sample(n = min(100, nrow(.))) %>%
#   mutate(
#     minority_share_diff = abs(true_minority_dem_share - (true_minority_dem_votes / total_minority)),
#     majority_share_diff = abs(true_majority_dem_share - (true_majority_dem_votes / total_majority))
#   )
# 
# cat("\nVerification - max difference in minority share:", max(sample_check$minority_share_diff, na.rm = TRUE), "\n")
# cat("Verification - max difference in majority share:", max(sample_check$majority_share_diff, na.rm = TRUE), "\n")
# 
# # Save the enhanced dataset
# saveRDS(all_data_enhanced, 'OLD/POPULATION_DENSITY/ER_EI_results_with_wasted_votes.rds')
# 
# 
# 
# 
# 
# 
# # After loading all_data_enhanced
# all_data_enhanced = readRDS('OLD/POPULATION_DENSITY/ER_EI_results_with_wasted_votes.rds')
# 
# # Identify maps assigned to multiple redist_levels
# duplicate_maps = all_data_enhanced %>%
#   select(seg_level, map_id, redist_level) %>%
#   distinct() %>%
#   group_by(seg_level, map_id) %>%
#   summarise(n_redist_levels = n_distinct(redist_level), 
#             redist_levels = paste(sort(unique(redist_level)), collapse = ", ")) %>%
#   filter(n_redist_levels > 1)
# 
# cat("Number of maps with multiple redist_levels:", nrow(duplicate_maps), "\n")
# print(duplicate_maps)
# 
# # Check the distribution before cleaning
# cat("\nOriginal distribution:\n")
# all_data_enhanced %>%
#   select(seg_level, map_id, redist_level) %>%
#   distinct() %>%
#   count(seg_level, redist_level) %>%
#   print()
# 
# # Set seed for reproducibility
# set.seed(123)
# 
# # Create a lookup table with one redist_level per (seg_level, map_id)
# # This determines which version to KEEP
# map_assignments = all_data_enhanced %>%
#   select(seg_level, map_id, redist_level) %>%
#   distinct() %>%
#   group_by(seg_level, map_id) %>%
#   sample_n(1) %>%  # Randomly pick one redist_level per map to keep
#   ungroup()
# 
# # Keep only the rows that match the selected assignments
# all_data_cleaned = all_data_enhanced %>%
#   semi_join(map_assignments, by = c("seg_level", "map_id", "redist_level"))
# 
# # Verify the fix
# duplicate_check = all_data_cleaned %>%
#   select(seg_level, map_id, redist_level) %>%
#   distinct() %>%
#   group_by(seg_level, map_id) %>%
#   summarise(n_redist_levels = n_distinct(redist_level)) %>%
#   filter(n_redist_levels > 1)
# 
# cat("\nAfter cleaning - maps with multiple redist_levels:", nrow(duplicate_check), "\n")
# 
# # Check the distribution after cleaning
# cat("\nCleaned distribution:\n")
# all_data_cleaned %>%
#   select(seg_level, map_id, redist_level) %>%
#   distinct() %>%
#   count(seg_level, redist_level) %>%
#   print()
# 
# # Check how many rows were removed
# cat("\nOriginal rows:", nrow(all_data_enhanced), "\n")
# cat("Cleaned rows:", nrow(all_data_cleaned), "\n")
# cat("Rows removed:", nrow(all_data_enhanced) - nrow(all_data_cleaned), "\n")
# 
# # Calculate expected removal
# n_duplicate_maps = nrow(duplicate_maps)
# expected_rows_removed = n_duplicate_maps * 14  # Each duplicate map has 14 extra rows
# cat("Expected rows removed (", n_duplicate_maps, "maps × 14 districts):", expected_rows_removed, "\n")
# 
# # Save cleaned data
# saveRDS(all_data_cleaned, 'OLD/POPULATION_DENSITY/ER_EI_results_cleaned.rds')
# 


# Packages
library(tidyverse)
library(broom)

rm(list = ls())

setwd('~/Dropbox/RPV/Code/Simulation/Workflow/')

select = dplyr::select

# Suppress dplyr messages
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)

# Load initial results
res = readRDS('OLD/POPULATION_DENSITY/ER_EI_MRP/partial_results_20250706_114336.rds')

# Extract all data 
all_data = data.frame()

for (seg_level in names(res)) {
  for (redist_level in names(res[[seg_level]])) {
    
    # Extract ER data
    er_df = res[[seg_level]][[redist_level]]$er
    rownames(er_df) = NULL
    er_df$seg_level = seg_level
    er_df$redist_level = redist_level
    
    # Extract EI data
    ei_df = res[[seg_level]][[redist_level]]$ei
    rownames(ei_df) = NULL
    ei_df$seg_level = seg_level
    ei_df$redist_level = redist_level
    
    # Create unique identifier for merging
    er_df$unique_id = paste(er_df$map_id, er_df$district_id, er_df$seg_level, er_df$redist_level, sep = "_")
    ei_df$unique_id = paste(ei_df$map_id, ei_df$district_id, ei_df$seg_level, ei_df$redist_level, sep = "_")
    
    # Select columns from ER (including ground truth)
    er_selected = er_df %>%
      select(unique_id, map_id, district_id, seg_level, redist_level, 
             n_precincts, total_population,
             true_minority_dem_share, true_majority_dem_share, true_rpv,
             er_minority_dem_share, er_majority_dem_share, 
             er_minority_se, er_majority_se, er_rpv, 
             er_minority_resid, er_majority_resid)
    
    # Select columns from EI
    ei_selected = ei_df %>%
      select(unique_id, prop_minority, prop_dem,
             ei_minority_dem_share, ei_majority_dem_share, 
             ei_minority_sd, ei_majority_sd, ei_rpv, 
             ei_minority_resid, ei_majority_resid) %>%
      rename(ei_minority_se = ei_minority_sd,
             ei_majority_se = ei_majority_sd)
    
    # Merge on unique_id
    scenario_data = merge(er_selected, ei_selected, 
                          by = "unique_id", 
                          all.x = TRUE)
    
    # Add RPV correctness indicators
    scenario_data$er_rpv_correct = ifelse(
      (scenario_data$true_minority_dem_share > 0.5 & 
         scenario_data$true_majority_dem_share < 0.5 &
         scenario_data$er_minority_dem_share > 0.5 & 
         scenario_data$er_majority_dem_share < 0.5) |
        (scenario_data$true_minority_dem_share < 0.5 & 
           scenario_data$true_majority_dem_share > 0.5 &
           scenario_data$er_minority_dem_share < 0.5 & 
           scenario_data$er_majority_dem_share > 0.5),
      1, 0
    )
    
    scenario_data$ei_rpv_correct = ifelse(
      (scenario_data$true_minority_dem_share > 0.5 & 
         scenario_data$true_majority_dem_share < 0.5 &
         scenario_data$ei_minority_dem_share > 0.5 & 
         scenario_data$ei_majority_dem_share < 0.5) |
        (scenario_data$true_minority_dem_share < 0.5 & 
           scenario_data$true_majority_dem_share > 0.5 &
           scenario_data$ei_minority_dem_share < 0.5 & 
           scenario_data$ei_majority_dem_share > 0.5),
      1, 0
    )
    
    # Add to all_data
    all_data = rbind(all_data, scenario_data)
  }
}

# Add ground truth info at district level
subdir = 'OLD/POPULATION_DENSITY/'

# Load the precinct data
precincts = read_csv(paste0(subdir, '/precincts.csv'), show_col_types = FALSE)

# Load plan data
plan_data = list(
  low = read_csv(paste0(subdir, '/low/Plans/CD_plans.csv'), show_col_types = FALSE),
  medium = read_csv(paste0(subdir, '/medium/Plans/CD_plans.csv'), show_col_types = FALSE),
  high = read_csv(paste0(subdir, '/high/Plans/CD_plans.csv'), show_col_types = FALSE)
)

# Helper functions
get_district_assignments = function(plan_df, map_id) {
  col_name = paste0("step_", map_id)
  if(!(col_name %in% names(plan_df))) {
    stop(paste("Map ID", map_id, "not found in plan data"))
  }
  data.frame(
    precinct_id = plan_df$precinct_id,
    district_id = plan_df[[col_name]]
  )
}

calculate_precinct_stats = function(map_id, plan_df, precinct_data, agg_level, partisan) {
  assignments = get_district_assignments(plan_df, map_id)
  precinct_stats = merge(precinct_data, assignments, by = "precinct_id")
  precinct_stats$agg_level = agg_level
  precinct_stats$partisan = partisan
  precinct_stats$map_id = map_id
  if (!"minority_vap" %in% names(precinct_stats)) {
    precinct_stats$minority_vap = precinct_stats$n_minority
    precinct_stats$majority_vap = precinct_stats$n_majority
  }
  return(precinct_stats)
}

# calculate_ground_truth_and_wasted_votes = function(precinct_stats) {
#   district_truth = precinct_stats %>%
#     group_by(map_id, agg_level, partisan) %>%
#     summarise(
#       # Basic counts
#       total_population = sum(population),
#       total_minority = sum(n_minority),
#       total_majority = sum(n_majority),
#       total_dem_votes = sum(dem_votes),
#       total_rep_votes = sum(rep_votes),
#       true_minority_dem_votes = sum(dem_votes_minority),
#       true_majority_dem_votes = sum(dem_votes_majority),
#       true_minority_dem_share = sum(dem_votes_minority) / sum(n_minority),
#       true_majority_dem_share = sum(dem_votes_majority) / sum(n_majority),
#       true_overall_dem_share = sum(dem_votes) / sum(population),
#       n_precincts = n(),
#       .groups = 'drop'
#     ) %>%
#     mutate(
#       # Calculate RPV
#       true_rpv = as.numeric(
#         (true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) |
#           (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5)
#       ),
#       
#       # Calculate winner and votes
#       prop_dem = total_dem_votes / (total_dem_votes + total_rep_votes),
#       dem_district = as.numeric(prop_dem > 0.5),
#       
#       # Calculate minority and majority Republican votes
#       true_minority_rep_votes = total_minority - true_minority_dem_votes,
#       true_majority_rep_votes = total_majority - true_majority_dem_votes,
#       
#       # --- Packed votes (surplus votes for winning party)
#       packed_votes = dem_district * (total_dem_votes - 0.5 * total_population) +
#         (1 - dem_district) * (0.5 * total_population - total_dem_votes),
#       
#       # Cracked votes (all votes for losing party)
#       cracked_votes = dem_district * total_rep_votes + (1 - dem_district) * total_dem_votes,
#       
#       # Total wasted votes (should equal 50% of population)
#       total_wasted = packed_votes + cracked_votes,
#       
#       # Allocate packed votes by group (proportional to group's share of winning party)
#       packed_min_votes = packed_votes * (
#         dem_district * (true_minority_dem_votes / total_dem_votes) +
#           (1 - dem_district) * (true_minority_rep_votes / total_rep_votes)
#       ),
#       
#       # Allocate cracked votes by group (proportional to group's share of losing party)
#       cracked_min_votes = cracked_votes * (
#         dem_district * (true_minority_rep_votes / total_rep_votes) +
#           (1 - dem_district) * (true_minority_dem_votes / total_dem_votes)
#       ),
#       
#       # Total wasted votes by group
#       minority_wasted_total = packed_min_votes + cracked_min_votes,
#       majority_wasted_total = total_wasted - minority_wasted_total,
#       
#       # Wasted vote rates
#       minority_waste_rate = minority_wasted_total / total_minority,
#       majority_waste_rate = majority_wasted_total / total_majority,
#       
#       # Disparity in wasted votes
#       waste_disparity = minority_waste_rate - majority_waste_rate
#     )
#   
#   return(district_truth)
# }

# New function using packed-cracked method
calculate_ground_truth_and_wasted_votes = function(precinct_stats) {
  district_truth = precinct_stats %>%
    group_by(map_id, district_id, agg_level, partisan) %>%
    summarise(
      # Basic counts
      total_population = sum(population),
      total_minority = sum(n_minority),
      total_majority = sum(n_majority),
      total_dem_votes = sum(dem_votes),
      total_rep_votes = sum(rep_votes),
      true_minority_dem_votes = sum(dem_votes_minority),
      true_majority_dem_votes = sum(dem_votes_majority),
      true_minority_dem_share = sum(dem_votes_minority) / sum(n_minority),
      true_majority_dem_share = sum(dem_votes_majority) / sum(n_majority),
      true_overall_dem_share = sum(dem_votes) / sum(population),
      n_precincts = n(),
      .groups = 'drop'
    ) %>%
    mutate(
      # Calculate RPV
      true_rpv = as.numeric(
        (true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) |
          (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5)
      ),
      
      # Calculate winner and votes
      prop_dem = total_dem_votes / (total_dem_votes + total_rep_votes),
      dem_district = prop_dem > 0.5,
      votes_to_win = floor(total_population / 2) + 1,
      
      # Calculate minority and majority Republican votes
      true_minority_rep_votes = total_minority - true_minority_dem_votes,
      true_majority_rep_votes = total_majority - true_majority_dem_votes,
      
      # Direct calculation of packed votes (surplus for winning party)
      minority_dem_packed = ifelse(dem_district, pmax(0, true_minority_dem_votes - (votes_to_win * true_minority_dem_votes / total_dem_votes)), 0),
      minority_rep_packed = ifelse(!dem_district, pmax(0, true_minority_rep_votes - (votes_to_win * true_minority_rep_votes / total_rep_votes)), 0),
      majority_dem_packed = ifelse(dem_district, pmax(0, true_majority_dem_votes - (votes_to_win * true_majority_dem_votes / total_dem_votes)), 0),
      majority_rep_packed = ifelse(!dem_district, pmax(0, true_majority_rep_votes - (votes_to_win * true_majority_rep_votes / total_rep_votes)), 0),
      
      # Direct calculation of cracked votes (all votes for losing party)
      minority_dem_cracked = ifelse(!dem_district, true_minority_dem_votes, 0),
      minority_rep_cracked = ifelse(dem_district, true_minority_rep_votes, 0),
      majority_dem_cracked = ifelse(!dem_district, true_majority_dem_votes, 0),
      majority_rep_cracked = ifelse(dem_district, true_majority_rep_votes, 0),
      
      # Sum to get group totals
      packed_min_votes = minority_dem_packed + minority_rep_packed,
      cracked_min_votes = minority_dem_cracked + minority_rep_cracked,
      packed_maj_votes = majority_dem_packed + majority_rep_packed,
      cracked_maj_votes = majority_dem_cracked + majority_rep_cracked,
      
      # Total wasted by group
      minority_wasted_total = packed_min_votes + cracked_min_votes,
      majority_wasted_total = packed_maj_votes + cracked_maj_votes,
      
      # Wasted vote rates
      minority_waste_rate = minority_wasted_total / total_minority,
      majority_waste_rate = majority_wasted_total / total_majority,
      
      # Disparity in wasted votes
      waste_disparity = minority_waste_rate - majority_waste_rate,
      
      # Verify totals
      packed_votes = packed_min_votes + packed_maj_votes,
      cracked_votes = cracked_min_votes + cracked_maj_votes,
      total_wasted = packed_votes + cracked_votes
    )
}

# Get unique combinations of seg_level, redist_level, and map_id from existing data
unique_scenarios = all_data %>%
  select(seg_level, redist_level, map_id) %>%
  distinct()

# Initialize dataframe to store all results
all_results = data.frame()

# Process each unique scenario
pb = txtProgressBar(min = 0, max = nrow(unique_scenarios), style = 3)

for(i in 1:nrow(unique_scenarios)) {
  scenario = unique_scenarios[i,]
  
  # Get the plan data for this segregation level
  plan_df = plan_data[[scenario$seg_level]]
  
  # Calculate precinct stats
  precinct_stats = calculate_precinct_stats(
    map_id = scenario$map_id,
    plan_df = plan_df,
    precinct_data = precincts,
    agg_level = scenario$seg_level,
    partisan = scenario$redist_level
  )
  
  # Calculate ground truth AND wasted votes using packed-cracked method
  results = calculate_ground_truth_and_wasted_votes(precinct_stats)
  
  # Rename columns to match what we need
  results = results %>%
    rename(seg_level = agg_level, redist_level = partisan)
  
  # Add to combined dataframe
  all_results = rbind(all_results, results)
  
  setTxtProgressBar(pb, i)
}

close(pb)

# Create unique ID for merging
all_results$unique_id = paste(
  all_results$map_id,
  all_results$district_id,
  all_results$seg_level,
  all_results$redist_level,
  sep = "_"
)

# Select columns to add to existing data
results_to_add = all_results %>%
  select(unique_id, 
         # Ground truth columns
         total_minority, total_majority, 
         total_dem_votes, total_rep_votes,
         true_minority_dem_votes, true_majority_dem_votes,
         true_overall_dem_share,
         # Winner info
         dem_district,
         # Wasted votes columns (packed-cracked method)
         packed_votes, cracked_votes, total_wasted,
         packed_min_votes, cracked_min_votes,
         minority_wasted_total, majority_wasted_total,
         minority_waste_rate, majority_waste_rate,
         waste_disparity)

# Merge with existing data
all_data_enhanced = all_data %>%
  left_join(results_to_add, by = "unique_id")

# Check the merge worked
cat("\nOriginal data rows:", nrow(all_data), "\n")
cat("Enhanced data rows:", nrow(all_data_enhanced), "\n")
cat("Rows with missing data:", sum(is.na(all_data_enhanced$true_minority_dem_votes)), "\n")

# Verify wasted votes calculation
sample_check = all_data_enhanced %>%
  filter(!is.na(total_wasted)) %>%
  slice_sample(n = min(100, nrow(.))) %>%
  mutate(
    wasted_pct = total_wasted / total_population,
    minority_share_diff = abs(true_minority_dem_share - (true_minority_dem_votes / total_minority)),
    majority_share_diff = abs(true_majority_dem_share - (true_majority_dem_votes / total_majority))
  )

cat("\nVerification - wasted votes percentage (should be ~0.5):", 
    mean(sample_check$wasted_pct, na.rm = TRUE), "\n")
cat("Verification - max difference in minority share:", 
    max(sample_check$minority_share_diff, na.rm = TRUE), "\n")
cat("Verification - max difference in majority share:", 
    max(sample_check$majority_share_diff, na.rm = TRUE), "\n")

# Handle duplicate maps (maps assigned to multiple redist_levels)
# Identify duplicates
duplicate_maps = all_data_enhanced %>%
  select(seg_level, map_id, redist_level) %>%
  distinct() %>%
  group_by(seg_level, map_id) %>%
  summarise(n_redist_levels = n_distinct(redist_level), 
            redist_levels = paste(sort(unique(redist_level)), collapse = ", "),
            .groups = 'drop') %>%
  filter(n_redist_levels > 1)

cat("\nNumber of maps with multiple redist_levels:", nrow(duplicate_maps), "\n")

# Set seed for reproducibility
set.seed(123)

# Create a lookup table with one redist_level per (seg_level, map_id)
map_assignments = all_data_enhanced %>%
  select(seg_level, map_id, redist_level) %>%
  distinct() %>%
  group_by(seg_level, map_id) %>%
  sample_n(1) %>%  # Randomly pick one redist_level per map to keep
  ungroup()

# Keep only the rows that match the selected assignments
all_data_cleaned = all_data_enhanced %>%
  semi_join(map_assignments, by = c("seg_level", "map_id", "redist_level"))

# Final verification
cat("\nFinal dataset rows:", nrow(all_data_cleaned), "\n")
cat("Maps per segregation level:\n")
all_data_cleaned %>%
  select(seg_level, map_id, redist_level) %>%
  distinct() %>%
  count(seg_level, redist_level) %>%
  print()

# Save final cleaned data
saveRDS(all_data_cleaned, 'OLD/POPULATION_DENSITY/ER_EI_results_final.rds')
























# Check the distribution of minority population across districts
minority_distribution = all_data_enhanced %>%
  mutate(
    minority_pct = total_minority / total_population,
    is_majority_minority = minority_pct > 0.5
  ) %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    n_districts = n(),
    n_maj_min = sum(is_majority_minority),
    pct_maj_min = mean(is_majority_minority) * 100,
    # Where do minorities live?
    minorities_in_maj_min = sum(total_minority[is_majority_minority]),
    minorities_in_other = sum(total_minority[!is_majority_minority]),
    pct_minorities_in_maj_min = minorities_in_maj_min / (minorities_in_maj_min + minorities_in_other) * 100,
    # Average minority percentage by district type
    avg_min_pct_in_maj_min = mean(minority_pct[is_majority_minority]),
    avg_min_pct_in_other = mean(minority_pct[!is_majority_minority]),
    .groups = 'drop'
  )

print(minority_distribution)



# Analyze waste patterns by district type
waste_analysis = all_data_enhanced %>%
  mutate(
    minority_pct = total_minority / total_population,
    is_majority_minority = minority_pct > 0.5,
    dem_won = dem_district == 1
  ) %>%
  group_by(seg_level, redist_level, is_majority_minority) %>%
  summarise(
    n_districts = n(),
    total_minority_pop = sum(total_minority),
    total_majority_pop = sum(total_majority),
    # Packed vs cracked
    minority_packed = sum(packed_min_votes),
    minority_cracked = sum(cracked_min_votes),
    majority_packed = sum(packed_votes - packed_min_votes),
    majority_cracked = sum(cracked_votes - cracked_min_votes),
    # Waste rates
    minority_waste_rate = (minority_packed + minority_cracked) / total_minority_pop,
    majority_waste_rate = (majority_packed + majority_cracked) / total_majority_pop,
    # Proportion of waste that is packed vs cracked
    minority_pct_packed = minority_packed / (minority_packed + minority_cracked),
    .groups = 'drop'
  )
