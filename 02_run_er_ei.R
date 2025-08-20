################################################################################################################################################################################################################################################
# This File Runs MRP, ER, and EI on Simulated Districts and Builds Infrastructure for Diagnostics
# Last Updated: 06/27/25
################################################################################################################################################################################################################################################

################################################################################################################################################################################################################################################
# Setup
################################################################################################################################################################################################################################################

# Packages
require(mvtnorm)
require(eiCompare)
require(ei)
require(tidyverse)

# library(profvis)

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

select = dplyr::select

# Suppress dplyr messages
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)

subdir = 'OLD/POPULATION_DENSITY/'

set.seed(14618)


# # ===================================================================
# # =============== DATA LOADING HELPER FUNCTIONS =====================
# # ===================================================================
# 
# # Function to load all data from a simulation run
# load_simulation_data = function(simulation_dir) {
#   
#   # Check if directory exists
#   if (!dir.exists(simulation_dir)) {
#     stop(paste("Directory does not exist:", simulation_dir))
#   }
#   
#   # Initialize result list
#   data = list()
#   
#   # Load fixed population data
#   precincts_file = paste0(simulation_dir, "/precincts.csv")
#   if (file.exists(precincts_file)) {
#     data$precincts = read_csv(precincts_file, show_col_types = FALSE)
#     cat("Loaded precincts data:", nrow(data$precincts), "rows\n")
#   }
#   
#   # Load data for each segregation level
#   segregation_levels = c("low", "medium", "high")
#   redistricting_types = c("neutral", "republican", "democratic")
#   
#   for (seg_level in segregation_levels) {
#     data[[seg_level]] = list()
#     
#     for (redis_type in redistricting_types) {
#       base_path = paste0(simulation_dir, "/", seg_level, "/", redis_type, "/")
#       
#       if (dir.exists(base_path)) {
#         # Load CD plans
#         cd_plans_file = paste0(base_path, "CD_plans.csv")
#         if (file.exists(cd_plans_file)) {
#           data[[seg_level]][[redis_type]]$plans = read_csv(cd_plans_file, show_col_types = FALSE)
#         }
#         
#         # Load district summaries
#         district_file = paste0(base_path, "district_summaries.csv")
#         if (file.exists(district_file)) {
#           data[[seg_level]][[redis_type]]$districts = read_csv(district_file, show_col_types = FALSE)
#         }
#         
#         # Load map summaries
#         map_file = paste0(base_path, "map_summaries.csv")
#         if (file.exists(map_file)) {
#           data[[seg_level]][[redis_type]]$maps = read_csv(map_file, show_col_types = FALSE)
#         }
#         
#         # Load plan scores (for ensembles)
#         scores_file = paste0(base_path, "plan_scores.csv")
#         if (file.exists(scores_file)) {
#           data[[seg_level]][[redis_type]]$scores = read_csv(scores_file, show_col_types = FALSE)
#         }
#       }
#     }
#   }
#   
#   # Load comparison data
#   comparison_file = paste0(simulation_dir, "/comparisons/summary_table.csv")
#   if (file.exists(comparison_file)) {
#     data$comparison_summary = read_csv(comparison_file, show_col_types = FALSE)
#   }
#   
#   # Load all results RDS if available
#   rds_file = paste0(simulation_dir, "/all_results.rds")
#   if (file.exists(rds_file)) {
#     data$all_results = readRDS(rds_file)
#   }
#   
#   return(data)
# }
# 
# # Function to load data matching the old format expectations
# load_simulation_data_legacy = function(simulation_dir) {
#   
#   data = list()
#   
#   # Load precincts
#   data$precincts = read_csv(paste0(simulation_dir, '/precincts.csv'), show_col_types = FALSE)
#   
#   # Map new names to old names for compatibility
#   name_mapping = list(
#     "republican" = "top_rep",
#     "neutral" = "middle", 
#     "democratic" = "top_dem"
#   )
#   
#   # Load district and map level data
#   for (seg_level in c("low", "medium", "high")) {
#     for (new_name in names(name_mapping)) {
#       old_name = name_mapping[[new_name]]
#       
#       # District data
#       district_file = paste0(simulation_dir, "/", seg_level, "/", new_name, "/district_summaries.csv")
#       if (file.exists(district_file)) {
#         var_name = paste0(seg_level, "_", old_name, "_district")
#         data[[var_name]] = read_csv(district_file, show_col_types = FALSE)
#       }
#       
#       # Map data
#       map_file = paste0(simulation_dir, "/", seg_level, "/", new_name, "/map_summaries.csv")
#       if (file.exists(map_file)) {
#         var_name = paste0(seg_level, "_", old_name, "_map")
#         data[[var_name]] = read_csv(map_file, show_col_types = FALSE)
#       }
#     }
#     
#     # Plans data
#     plans_file = paste0(simulation_dir, "/", seg_level, "/neutral/CD_plans.csv")
#     if (file.exists(plans_file)) {
#       var_name = paste0(seg_level, "_plans")
#       data[[var_name]] = read_csv(plans_file, show_col_types = FALSE)
#     }
#   }
#   
#   return(data)
# }
# 
# example_data_loading = function() {
#   
#   # Load all data
#   data = load_simulation_data("Output/Tests/test_20241223_143022")
#   
#   # Access precincts
#   precincts = data$precincts
#   
#   # Access specific redistricting results
#   low_neutral_maps = data$low$neutral$maps
#   medium_republican_districts = data$medium$republican$districts
#   high_democratic_plans = data$high$democratic$plans
#   
#   # use the legacy loader for backward compatibility
#   legacy_data = load_simulation_data_legacy("Output/Tests/test_20241223_143022")
#   
#   # gives variables named like:
#   # legacy_data$low_top_rep_district
#   # legacy_data$medium_middle_map
#   # legacy_data$high_top_dem_district
#   # etc.
# }

################################################################################################################################################################################################################################################
# Load Data
################################################################################################################################################################################################################################################

# --- Load individual voter and precinct data
# voters = read_csv(paste0(subdir, '/voters.csv'), show_col_types = FALSE)
precincts = read_csv(paste0(subdir, '/precincts.csv'), show_col_types = FALSE)
# precincts = read_csv(paste0(subdir, '/fixed_population.csv'), show_col_types = FALSE)

# --- 3x3 Aggregation bias and mapmaker incentive scenarios

# - District level data

# Low aggregation bias
low_rep_district = read_csv(paste0(subdir, '/low/Summaries/district_summaries_top_rep.csv'), show_col_types = FALSE)
low_middle_district = read_csv(paste0(subdir, '/low/Summaries/district_summaries_middle.csv'), show_col_types = FALSE)
low_dem_district = read_csv(paste0(subdir, '/low/Summaries/district_summaries_top_dem.csv'), show_col_types = FALSE)

# Medium aggregation bias
medium_rep_district = read_csv(paste0(subdir, '/medium/Summaries/district_summaries_top_rep.csv'), show_col_types = FALSE)
medium_middle_district = read_csv(paste0(subdir, '/medium/Summaries/district_summaries_middle.csv'), show_col_types = FALSE)
medium_dem_district = read_csv(paste0(subdir, '/medium/Summaries/district_summaries_top_dem.csv'), show_col_types = FALSE)

# High aggregation bias
high_rep_district = read_csv(paste0(subdir, '/high/Summaries/district_summaries_top_rep.csv'), show_col_types = FALSE)
high_middle_district = read_csv(paste0(subdir, '/high/Summaries/district_summaries_middle.csv'), show_col_types = FALSE)
high_dem_district = read_csv(paste0(subdir, '/high/Summaries/district_summaries_top_dem.csv'), show_col_types = FALSE)

# - Map level data

# Low aggregation bias
low_rep_map = read_csv(paste0(subdir, '/low/Summaries/map_summaries_top_rep.csv'), show_col_types = FALSE)
low_middle_map = read_csv(paste0(subdir, '/low/Summaries/map_summaries_middle.csv'), show_col_types = FALSE)
low_dem_map = read_csv(paste0(subdir, '/low/Summaries/map_summaries_top_dem.csv'), show_col_types = FALSE)

# Medium aggregation bias
medium_rep_map = read_csv(paste0(subdir, '/medium/Summaries/map_summaries_top_rep.csv'), show_col_types = FALSE)
medium_middle_map = read_csv(paste0(subdir, '/medium/Summaries/map_summaries_middle.csv'), show_col_types = FALSE)
medium_dem_map = read_csv(paste0(subdir, '/medium/Summaries/map_summaries_top_dem.csv'), show_col_types = FALSE)

# High aggregation bias
high_rep_map = read_csv(paste0(subdir, '/high/Summaries/map_summaries_top_rep.csv'), show_col_types = FALSE)
high_middle_map = read_csv(paste0(subdir, '/high/Summaries/map_summaries_middle.csv'), show_col_types = FALSE)
high_dem_map = read_csv(paste0(subdir, '/high/Summaries/map_summaries_top_dem.csv'), show_col_types = FALSE)


# --- Read plan data (maps precincts to district IDs for different maps)
low_plans = read_csv(paste0(subdir, '/low/Plans/CD_plans.csv'), show_col_types = FALSE)
medium_plans = read_csv(paste0(subdir, '/medium/Plans/CD_plans.csv'), show_col_types = FALSE)
high_plans = read_csv(paste0(subdir, '/high/Plans/CD_plans.csv'), show_col_types = FALSE)


################################################################################################################################################################################################################################################
# Organize Data
################################################################################################################################################################################################################################################

# By aggregation level and mapmaker objective
district_data = list(
  low = list(rep = low_rep_district, middle = low_middle_district, dem = low_dem_district),
  medium = list(rep = medium_rep_district, middle = medium_middle_district, dem = medium_dem_district),
  high = list(rep = high_rep_district, middle = high_middle_district, dem = high_dem_district)
)

map_data = list(
  low = list(rep = low_rep_map, middle = low_middle_map, dem = low_dem_map),
  medium = list(rep = medium_rep_map, middle = medium_middle_map, dem = medium_dem_map),
  high = list(rep = high_rep_map, middle = high_middle_map, dem = high_dem_map)
)

plan_data = list(
  low = low_plans,
  medium = medium_plans,
  high = high_plans
)

# Get unique map IDs for each scenario
map_ids = list()
for(agg_level in c("low", "medium", "high")) {
  map_ids[[agg_level]] = list()
  for(partisan in c("rep", "middle", "dem")) {
    map_ids[[agg_level]][[partisan]] = unique(map_data[[agg_level]][[partisan]]$map_id)
  }
}

# Number of maps per scenario
cat("Number of maps per scenario:\n")
for(agg_level in c("low", "medium", "high")) {
  for(partisan in c("rep", "middle", "dem")) {
    n_maps = length(map_ids[[agg_level]][[partisan]])
    cat(sprintf("%s-%s: %d maps\n", agg_level, partisan, n_maps))
  }
}

# List to store results
results = list()
for(agg_level in c("low", "medium", "high")) {
  results[[agg_level]] = list()
  for(partisan in c("rep", "middle", "dem")) {
    results[[agg_level]][[partisan]] = list(
      er = data.frame(),
      ei = data.frame()
    )
  }
}

# Function to extract district assignments for a specific map
get_district_assignments = function(plan_df, map_id) {
  
  # The column name in CD_plans is step_X where X is the map_id
  col_name = paste0("step_", map_id)
  
  if(!(col_name %in% names(plan_df))) {
    stop(paste("Map ID", map_id, "not found in plan data (looking for column", col_name, ")"))
  }
  
  # Return a data frame with precinct_id and district_id
  data.frame(
    precinct_id = plan_df$precinct_id,
    district_id = plan_df[[col_name]]
  )
}


# # Test with one map
# test_assignments = get_district_assignments(plan_data$high, map_ids$high$rep[1])
# cat("\nTest district assignments (first 5 rows):\n")
# print(head(test_assignments, 5))
# 
# cat(
#   "\nNumber of precincts in plan data vs precinct data:\n",
#   "Plan data (high):", nrow(plan_data$high), "\n",
#   "Precinct data:", nrow(precincts), "\n"
# )


################################################################################################################################################################################################################################################
# Process Precinct Data
################################################################################################################################################################################################################################################

# Calculate precinct-level statistics for a given map
calculate_precinct_stats = function(map_id,
                                    plan_df,
                                    precinct_data,
                                    agg_level,
                                    partisan) {
  
  # Get district assignments for this map
  assignments = get_district_assignments(plan_df, map_id)
  
  # Merge with precinct data
  precinct_stats = merge(precinct_data, assignments, by = "precinct_id")
  
  # Add aggregation level and partisan scenario
  precinct_stats$agg_level = agg_level
  precinct_stats$partisan = partisan
  precinct_stats$map_id = map_id
  
  # Add VAP columns if not present (for EI compatibility)
  if (!"minority_vap" %in% names(precinct_stats)) {
    precinct_stats$minority_vap = precinct_stats$n_minority
    precinct_stats$majority_vap = precinct_stats$n_majority
  }
  
  return(precinct_stats)
}

# # Test with one map
# test_precinct_stats = calculate_precinct_stats(
#   map_id = map_ids$high$rep[10],
#   plan_df = plan_data$high,
#   precinct_data = precincts,
#   agg_level = "high",
#   partisan = "rep"
# )
# 
# cat("\nTest precinct statistics (first 5 rows):\n")
# print(head(test_precinct_stats[, c("precinct_id", "district_id", "population", "per_minority",
#                                    "dem_voteshare", "n_minority", "n_majority", "dem_votes", "rep_votes")], 5))
# 
# # Check that calculations make sense
# cat("\nSample calculations check:\n",
#     "Precinct 1: population =", test_precinct_stats$population[1], "\n",
#     "per_minority =", test_precinct_stats$per_minority[1], "\n",
#     "n_minority =", test_precinct_stats$n_minority[1],
#     "(should be ~", round(test_precinct_stats$population[1] * test_precinct_stats$per_minority[1]), ")\n",
#     "dem_voteshare =", test_precinct_stats$dem_voteshare[1], "\n",
#     "dem_votes =", test_precinct_stats$dem_votes[1],
#     "(should be ~", round(test_precinct_stats$population[1] * test_precinct_stats$dem_voteshare[1]), ")\n")

################################################################################################################################################################################################################################################
# Ground Truth RPV
################################################################################################################################################################################################################################################

# Calculate true RPV at district level
calculate_ground_truth = function(precinct_stats) {
  
  # Aggregate to district level
  district_truth = precinct_stats %>%
    group_by(map_id, district_id, agg_level, partisan) %>%
    summarise(
      # Population totals
      total_population = sum(population),
      total_minority = sum(n_minority),
      total_majority = sum(n_majority),
      
      # Vote totals
      total_dem_votes = sum(dem_votes),
      total_rep_votes = sum(rep_votes),
      
      # Calculate true vote counts by group
      true_minority_dem_votes = sum(dem_votes_minority),
      true_majority_dem_votes = sum(dem_votes_majority),
      
      # Calculate true vote shares by group - handle zero denominators
      true_minority_dem_share = ifelse(
        sum(n_minority) > 0,
        sum(dem_votes_minority) / sum(n_minority),
        NA_real_  # Use NA_real_ for numeric NA
      ),
      true_majority_dem_share = ifelse(
        sum(n_majority) > 0,
        sum(dem_votes_majority) / sum(n_majority),
        NA_real_
      ),
      
      # Overall dem share
      true_overall_dem_share = sum(dem_votes) / sum(population),
      
      # Number of precincts
      n_precincts = n(),
      
      .groups = 'drop'
    ) %>%
    mutate(
      # Calculate RPV indicator - handle NAs
      true_rpv = case_when(
        is.na(true_minority_dem_share) | is.na(true_majority_dem_share) ~ NA_real_,
        (true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) ~ 1,
        (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5) ~ 1,
        TRUE ~ 0
      )
    )
  
  return(district_truth)
}

# # Test with precinct stats
# test_ground_truth = calculate_ground_truth(test_precinct_stats)
# 
# cat("\nGround truth for test map (first 5 districts):\n")
# print(head(test_ground_truth[, c("district_id", "total_population", "true_minority_dem_share",
#                                  "true_majority_dem_share", "true_overall_dem_share", "true_rpv")], 5))
# 
# # Verify RPV calculation with an example
# cat("\nRPV calculation example:\n",
#     "District", test_ground_truth$district_id[1], ":\n",
#     "  Minority dem share:", round(test_ground_truth$true_minority_dem_share[1], 3), "\n",
#     "  Majority dem share:", round(test_ground_truth$true_majority_dem_share[1], 3), "\n",
#     "  RPV:", test_ground_truth$true_rpv[1], "\n",
#     "  (Should be 1 if one is > 0.5 and other is < 0.5)\n")
# 
# # Check distribution of RPV across districts
# cat("\nDistribution of RPV across districts in test map:\n")
# print(table(test_ground_truth$true_rpv))


################################################################################################################################################################################################################################################
# Ecological Regression (ER) 
################################################################################################################################################################################################################################################

# Run ER for a map
run_ecological_regression = function(precinct_stats, ground_truth) {
  
  # Get unique map info
  map_id = unique(precinct_stats$map_id)
  agg_level = unique(precinct_stats$agg_level)
  partisan = unique(precinct_stats$partisan)
  
  # Initialize results dataframe
  er_results = data.frame()
  
  # Get unique districts
  unique_districts = unique(precinct_stats$district_id)
  
  # Run ER for each district separately
  for(dist_id in unique_districts) {
    
    # Get precincts in this district
    district_precincts = precinct_stats[precinct_stats$district_id == dist_id, ]
    
    # Skip if too few precincts for regression
    if(nrow(district_precincts) < 3) {
      next
    }
    
    # Prepare precinct-level data for regression
    precinct_data = district_precincts %>%
      mutate(
        # Calculate precinct-level shares
        minority_share = n_minority / population,
        majority_share = n_majority / population,
        dem_voteshare = dem_votes / population
      ) %>%
      # filter(population > 0) %>%  # Remove precincts with zero population
      as.data.frame()
    
    # Skip if too few valid precincts remain
    if(nrow(precinct_data) < 3) {
      next
    }
    
    # Run ER at precinct level for this district
    tryCatch({
      er_model = lm(dem_voteshare ~ minority_share + majority_share - 1, 
                    data = precinct_data, weights = population)
      
      # Extract coefficients (these are the district-specific estimates)
      coef_minority = coef(er_model)["minority_share"]
      coef_majority = coef(er_model)["majority_share"]
      
      # Get standard errors
      se_minority = summary(er_model)$coefficients["minority_share", "Std. Error"]
      se_majority = summary(er_model)$coefficients["majority_share", "Std. Error"]
      
      # Handle NA coefficients
      if(is.na(coef_minority) || is.na(coef_majority)) {
        next
      }
      
      # Get ground truth for this district
      truth = ground_truth[ground_truth$district_id == dist_id, ]
      
      # Calculate RPV prediction for this district
      pred_rpv = as.numeric(
        (coef_minority > 0.5 & coef_majority < 0.5) |
          (coef_minority < 0.5 & coef_majority > 0.5)
      )
      
      # Store results for this district
      er_results = rbind(er_results, data.frame(
        map_id = map_id,
        agg_level = agg_level,
        partisan = partisan,
        district_id = dist_id,
        
        # ER estimates (district-specific)
        er_minority_dem_share = coef_minority,
        er_majority_dem_share = coef_majority,
        er_minority_se = se_minority,
        er_majority_se = se_majority,
        er_rpv = pred_rpv,
        
        # Ground truth
        true_minority_dem_share = truth$true_minority_dem_share,
        true_majority_dem_share = truth$true_majority_dem_share,
        true_rpv = truth$true_rpv,
        
        # Residuals
        er_minority_resid = ifelse(
          is.na(truth$true_minority_dem_share),
          NA,
          coef_minority - truth$true_minority_dem_share
        ), 
        er_majority_resid = ifelse(
          is.na(truth$true_majority_dem_share),
          NA,
          coef_majority - truth$true_majority_dem_share
        ), 
        er_rpv_error = ifelse(is.na(truth$true_rpv), NA, pred_rpv - truth$true_rpv),
        
        # Additional info
        n_precincts = nrow(precinct_data),
        total_population = truth$total_population
      ))
      
    }, error = function(e) {
      cat("Error in ER for district", dist_id, ":", e$message, "\n")
    })
  }
  
  return(er_results)
}

# # Test ER on test data
# test_er_results = run_ecological_regression(test_precinct_stats, test_ground_truth)
# 
# cat("\nER results for test map (first 5 districts):\n")
# print(head(test_er_results[, c("district_id", "er_minority_dem_share", "er_majority_dem_share",
#                                "true_minority_dem_share", "true_majority_dem_share",
#                                "er_rpv", "true_rpv", "er_rpv_error")], 5))
# 
# # Check accuracy
# cat("\nER performance summary:\n",
#     "Mean absolute error (minority):", round(mean(abs(test_er_results$er_minority_resid)), 4), "\n",
#     "Mean absolute error (majority):", round(mean(abs(test_er_results$er_majority_resid)), 4), "\n",
#     "RPV classification accuracy:", round(mean(test_er_results$er_rpv == test_er_results$true_rpv), 4), "\n")


################################################################################################################################################################################################################################################
# Ecological Inference (EI) 
################################################################################################################################################################################################################################################

run_ecological_inference = function(precinct_stats, ground_truth) {
  # Get unique map info
  map_id = unique(precinct_stats$map_id)
  agg_level = unique(precinct_stats$agg_level)
  partisan = unique(precinct_stats$partisan)
  
  # Prepare data for EI at precinct level 
  precinct_data = precinct_stats %>%
    mutate(
      minority_vap = n_minority,
      majority_vap = n_majority,
      dem_votes = dem_votes,
      rep_votes = rep_votes
    ) %>%
    arrange(district_id, precinct_id) %>%  # Ensure consistent ordering
    as.data.frame()
  
  # Run EI 
  tryCatch({
    
    formula = cbind(dem_votes, rep_votes) ~ cbind(minority_vap, majority_vap)
    
    # ei_model = ei(formula,
    #               data = precinct_data,
    #               burnin = 100,  # Reduced
    #               sample = 200,  # Reduced
    #               verbose = FALSE)
    
    # to supress ei() messages
    capture.output({
      ei_model = ei(formula,
                    data = precinct_data,
                    burnin = 1000,  # Reduced
                    sample = 2000,  # Reduced
                    verbose = FALSE)
    }, file = nullfile())
    
    # Process results if model ran succesfully
    if (!is.null(ei_model) && !is.null(ei_model$draws) && !is.null(ei_model$draws$Beta)) {
      
      beta_draws = ei_model$draws$Beta
      n_draws = nrow(beta_draws)
      n_precincts = nrow(precinct_data)
      
      ei_results = data.frame()
      
      unique_districts = unique(precinct_data$district_id)
      
      # Calculate district-level predictions
      for(dist_id in unique_districts) {
        
        # Get indices 
        district_indices = which(precinct_data$district_id == dist_id)
        district_precincts = precinct_data[district_indices, ]
        
        # Get total VAP
        total_minority_vap = sum(district_precincts$minority_vap)
        total_majority_vap = sum(district_precincts$majority_vap)
        
        # Store district-level draws
        district_minority_dem_draws = numeric(n_draws)
        district_majority_dem_draws = numeric(n_draws)
        
        # For each draw, aggregate precinct estimates to district level
        for(draw in 1:n_draws) {
          total_minority_dem = 0
          total_majority_dem = 0
          
          # Sum across all precincts in the district
          for(i in seq_along(district_indices)) {
            precinct_idx = district_indices[i]
            
            minority_col = paste0("beta.minority_vap.dem_votes.", precinct_idx)
            majority_col = paste0("beta.majority_vap.dem_votes.", precinct_idx)
            
            
            # Get the proportion estimates for this precinct and draw
            minority_prop = beta_draws[draw, minority_col]
            majority_prop = beta_draws[draw, majority_col]
            
            # Weight by precinct population to get votes
            total_minority_dem = total_minority_dem + 
              minority_prop * district_precincts$minority_vap[i]
            total_majority_dem = total_majority_dem + 
              majority_prop * district_precincts$majority_vap[i]
            
          }
          
          # Convert to proportions at district level
          district_minority_dem_draws[draw] = total_minority_dem / total_minority_vap
          district_majority_dem_draws[draw] = total_majority_dem / total_majority_vap
        }
        
        # Calculate posterior mean and SD for this district
        ei_minority_dem = mean(district_minority_dem_draws)
        ei_majority_dem = mean(district_majority_dem_draws)
        ei_minority_sd = sd(district_minority_dem_draws)
        ei_majority_sd = sd(district_majority_dem_draws)
        
        # Get ground truth for this district
        truth = ground_truth[ground_truth$district_id == dist_id, ]
        
        # Calculate RPV prediction
        pred_rpv = as.numeric(
          (ei_minority_dem > 0.5 & ei_majority_dem < 0.5) |
            (ei_minority_dem < 0.5 & ei_majority_dem > 0.5)
        )
        
        # Store results for this district
        ei_results = rbind(ei_results, data.frame(
          map_id = map_id,
          agg_level = agg_level,
          partisan = partisan,
          district_id = dist_id,
          
          # EI estimates
          ei_minority_dem_share = ei_minority_dem,
          ei_majority_dem_share = ei_majority_dem,
          ei_minority_sd = ei_minority_sd,
          ei_majority_sd = ei_majority_sd,
          ei_rpv = pred_rpv,
          
          # Ground truth
          true_minority_dem_share = truth$true_minority_dem_share,
          true_majority_dem_share = truth$true_majority_dem_share,
          true_rpv = truth$true_rpv,
          
          # Residuals
          ei_minority_resid = ifelse(
            is.na(truth$true_minority_dem_share),
            NA, 
            ei_minority_dem - truth$true_minority_dem_share
            ),
          ei_majority_resid = ifelse(
            is.na(truth$true_majority_dem_share),
            NA, 
            ei_majority_dem - truth$true_majority_dem_share
          ),
          ei_rpv_error = ifelse(is.na(truth$true_rpv), NA, pred_rpv - truth$true_rpv),
          
          # Additional info
          total_population = truth$total_population,
          n_precincts = truth$n_precincts,
          prop_minority = truth$total_minority / truth$total_population,
          prop_dem = truth$total_dem_votes / (truth$total_dem_votes + truth$total_rep_votes)
        ))
      }
      
      return(ei_results)
      
    } else {
      warning(paste("EI model failed for map", map_id))
      return(data.frame())
    }
    
  }, error = function(e) {
    warning(paste("EI error for map", map_id, ":", e$message))
    return(data.frame())
  })
}

run_ecological_inference_by_district = function(precinct_stats, ground_truth, 
                                                burnin = 5000, sample = 10000,
                                                min_precincts = 3,
                                                parallel = FALSE, n_cores = NULL) {
  # Get unique map info
  map_id = unique(precinct_stats$map_id)
  agg_level = unique(precinct_stats$agg_level)
  partisan = unique(precinct_stats$partisan)
  
  # Prepare data for EI at precinct level 
  precinct_data = precinct_stats %>%
    mutate(
      minority_vap = n_minority,
      majority_vap = n_majority,
      dem_votes = dem_votes,
      rep_votes = rep_votes
    ) %>%
    arrange(district_id, precinct_id) %>%  # Ensure consistent ordering
    as.data.frame()
  
  # Get unique districts
  unique_districts = unique(precinct_data$district_id)
  
  # Function to run EI for a single district
  run_district_ei = function(dist_id) {
    # Get district data
    district_precincts = precinct_data[precinct_data$district_id == dist_id, ]
    
    # Skip if too few precincts
    if(nrow(district_precincts) < min_precincts) {
      return(NULL)
    }
    
    # Run EI for this district
    tryCatch({
      formula = cbind(dem_votes, rep_votes) ~ cbind(minority_vap, majority_vap)
      
      # Suppress ei() messages
      capture.output({
        ei_model = ei(formula,
                      data = district_precincts,
                      burnin = burnin,
                      sample = sample,
                      verbose = FALSE)
      }, file = nullfile())
      
      # Process results if model ran successfully
      if (!is.null(ei_model) && !is.null(ei_model$draws) && !is.null(ei_model$draws$Beta)) {
        
        beta_draws = ei_model$draws$Beta
        n_draws = nrow(beta_draws)
        
        # Get total VAP for district
        total_minority_vap = sum(district_precincts$minority_vap)
        total_majority_vap = sum(district_precincts$majority_vap)
        
        # Store district-level draws
        district_minority_dem_draws = numeric(n_draws)
        district_majority_dem_draws = numeric(n_draws)
        
        # For each draw, aggregate precinct estimates to district level
        for(draw in 1:n_draws) {
          total_minority_dem = 0
          total_majority_dem = 0
          
          # Sum across all precincts in the district
          for(i in 1:nrow(district_precincts)) {
            minority_col = paste0("beta.minority_vap.dem_votes.", i)
            majority_col = paste0("beta.majority_vap.dem_votes.", i)
            
            # Get the proportion estimates for this precinct and draw
            minority_prop = beta_draws[draw, minority_col]
            majority_prop = beta_draws[draw, majority_col]
            
            # Weight by precinct population to get votes
            total_minority_dem = total_minority_dem + 
              minority_prop * district_precincts$minority_vap[i]
            total_majority_dem = total_majority_dem + 
              majority_prop * district_precincts$majority_vap[i]
          }
          
          # Convert to proportions at district level
          district_minority_dem_draws[draw] = total_minority_dem / total_minority_vap
          district_majority_dem_draws[draw] = total_majority_dem / total_majority_vap
        }
        
        browser()
        
        # Calculate posterior mean and SD for this district
        ei_minority_dem = mean(district_minority_dem_draws)
        ei_majority_dem = mean(district_majority_dem_draws)
        ei_minority_sd = sd(district_minority_dem_draws)
        ei_majority_sd = sd(district_majority_dem_draws)
        
        # Get ground truth for this district
        truth = ground_truth[ground_truth$district_id == dist_id, ]
        
        # Calculate RPV prediction
        pred_rpv = as.numeric(
          (ei_minority_dem > 0.5 & ei_majority_dem < 0.5) |
            (ei_minority_dem < 0.5 & ei_majority_dem > 0.5)
        )
        
        # Return results for this district
        return(data.frame(
          map_id = map_id,
          agg_level = agg_level,
          partisan = partisan,
          district_id = dist_id,
          
          # EI estimates
          ei_minority_dem_share = ei_minority_dem,
          ei_majority_dem_share = ei_majority_dem,
          ei_minority_sd = ei_minority_sd,
          ei_majority_sd = ei_majority_sd,
          ei_rpv = pred_rpv,
          
          # Ground truth
          true_minority_dem_share = truth$true_minority_dem_share,
          true_majority_dem_share = truth$true_majority_dem_share,
          true_rpv = truth$true_rpv,
          
          # Residuals
          ei_minority_resid = ifelse(
            is.na(truth$true_minority_dem_share),
            NA, 
            ei_minority_dem - truth$true_minority_dem_share
          ),
          ei_majority_resid = ifelse(
            is.na(truth$true_majority_dem_share),
            NA, 
            ei_majority_dem - truth$true_majority_dem_share
          ),
          ei_rpv_error = ifelse(is.na(truth$true_rpv), NA, pred_rpv - truth$true_rpv),
          
          # Additional info
          total_population = truth$total_population,
          n_precincts = truth$n_precincts,
          prop_minority = truth$total_minority / truth$total_population,
          prop_dem = truth$total_dem_votes / (truth$total_dem_votes + truth$total_rep_votes)
        ))
        
      } else {
        warning(paste("EI model failed for district", dist_id, "in map", map_id))
        return(NULL)
      }
      
    }, error = function(e) {
      warning(paste("EI error for district", dist_id, "in map", map_id, ":", e$message))
      return(NULL)
    })
  }
  
  # Run EI for all districts
  if(parallel && requireNamespace("parallel", quietly = TRUE)) {
    # Parallel execution
    if(is.null(n_cores)) {
      n_cores = parallel::detectCores() - 1
    }
    
    cl = parallel::makeCluster(n_cores)
    
    # Export necessary objects to cluster
    parallel::clusterExport(cl, c("precinct_data", "ground_truth", "map_id", 
                                  "agg_level", "partisan", "burnin", "sample", 
                                  "min_precincts"), 
                            envir = environment())
    
    # Load required packages on each worker
    parallel::clusterEvalQ(cl, {
      library(ei)
      library(dplyr)
    })
    
    # Run in parallel
    results_list = parallel::parLapply(cl, unique_districts, run_district_ei)
    
    parallel::stopCluster(cl)
    
  } else {
    # Sequential execution with progress bar
    cat("Running EI for", length(unique_districts), "districts...\n")
    pb = txtProgressBar(min = 0, max = length(unique_districts), style = 3)
    
    results_list = list()
    for(i in seq_along(unique_districts)) {
      results_list[[i]] = run_district_ei(unique_districts[i])
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  
  # Remove NULL results and combine
  results_list = results_list[!sapply(results_list, is.null)]
  
  if(length(results_list) == 0) {
    warning(paste("No districts successfully processed for map", map_id))
    return(data.frame())
  }
  
  # Combine all district results
  ei_results = do.call(rbind, results_list)
  
  # Print summary
  cat("\nEI Summary for map", map_id, ":\n")
  cat("  Districts processed:", nrow(ei_results), "/", length(unique_districts), "\n")
  cat("  Mean iterations per district:", burnin + sample, "\n")
  cat("  MAE minority:", round(mean(abs(ei_results$ei_minority_resid)), 4), "\n")
  cat("  MAE majority:", round(mean(abs(ei_results$ei_majority_resid)), 4), "\n")
  
  return(ei_results)
}

# Wrapper function that matches the original interface
run_ecological_inference = function(precinct_stats, ground_truth) {
  # Default to district-by-district with reasonable iterations
  return(run_ecological_inference_by_district(
    precinct_stats = precinct_stats,
    ground_truth = ground_truth,
    burnin = 5000,
    sample = 10000,
    min_precincts = 3,
    parallel = TRUE,
    n_cores = parallel::detectCores() - 1  # Use all but one core
  ))
}


# # Fast testing (lower iterations)
# ei_results_fast = run_ecological_inference_by_district(
#   test_precinct_stats, test_ground_truth,
#   burnin = 1000, sample = 2000
# )
# 
# # High quality (more iterations)
# ei_results_hq = run_ecological_inference_by_district(
#   test_precinct_stats, test_ground_truth,
#   burnin = 10000, sample = 20000
# )
# 
# # Parallel processing (much faster on multi-core machines)
# ei_results_parallel = run_ecological_inference_by_district(
#   test_precinct_stats, test_ground_truth,
#   burnin = 5000, sample = 10000,
#   parallel = TRUE, n_cores = 8
# )

# # Test EI
# test_ei_results = run_ecological_inference(test_precinct_stats, test_ground_truth)
# 
# cat("\nEI results for test map (first 5 districts):\n")
# print(head(test_ei_results[, c("district_id", "ei_minority_dem_share", "ei_majority_dem_share",
#                                "true_minority_dem_share", "true_majority_dem_share",
#                                "ei_rpv", "true_rpv", "ei_rpv_error")], 5))
# 
# # Check accuracy
# cat("\nEI performance summary:\n",
#     "Mean absolute error (minority):", round(mean(abs(test_ei_results$ei_minority_resid)), 4), "\n",
#     "Mean absolute error (majority):", round(mean(abs(test_ei_results$ei_majority_resid)), 4), "\n",
#     "RPV classification accuracy:", round(mean(test_ei_results$ei_rpv == test_ei_results$true_rpv), 4), "\n")
# 
# # Compare ER vs EI performance
# cat("\n\nComparison of ER vs EI:\n",
#     "ER - MAE minority:", round(mean(abs(test_er_results$er_minority_resid)), 4), "\n",
#     "EI - MAE minority:", round(mean(abs(test_ei_results$ei_minority_resid)), 4), "\n",
#     "ER - MAE majority:", round(mean(abs(test_er_results$er_majority_resid)), 4), "\n",
#     "EI - MAE majority:", round(mean(abs(test_ei_results$ei_majority_resid)), 4), "\n",
#     "ER - RPV accuracy:", round(mean(test_er_results$er_rpv == test_er_results$true_rpv), 4), "\n",
#     "EI - RPV accuracy:", round(mean(test_ei_results$ei_rpv == test_ei_results$true_rpv), 4), "\n")


################################################################################################################################################################################################################################################
# Functions for incremental map processing and saving
################################################################################################################################################################################################################################################

# Save progress for a single map
save_map_results = function(map_id, agg_level, partisan, er_results, ei_results, progress_dir) {
  # Create a unique filename for this map
  filename = paste0(progress_dir, "/map_", map_id, "_", agg_level, "_", partisan, ".rds")
  
  # Save both ER and EI results for this map
  map_data = list(
    map_id = map_id,
    agg_level = agg_level,
    partisan = partisan,
    er = er_results,
    ei = ei_results
  )
  
  saveRDS(map_data, file = filename)
}

# Check if a map has already been processed
is_map_processed = function(map_id, agg_level, partisan, progress_dir) {
  filename = paste0(progress_dir, "/map_", map_id, "_", agg_level, "_", partisan, ".rds")
  return(file.exists(filename))
}

# Load all saved results for a scenario
load_scenario_results = function(agg_level, partisan, progress_dir) {
  # All files for scenario
  pattern = paste0("map_.*_", agg_level, "_", partisan, "\\.rds$")
  files = list.files(progress_dir, pattern = pattern, full.names = TRUE)
  
  if(length(files) == 0) {
    return(list(er = data.frame(), ei = data.frame()))
  }
  
  # Load and combine results
  all_er = data.frame()
  all_ei = data.frame()
  
  for(file in files) {
    map_data = readRDS(file)
    all_er = rbind(all_er, map_data$er)
    all_ei = rbind(all_ei, map_data$ei)
  }
  
  return(list(er = all_er, ei = all_ei))
}

# Get progress status
get_progress_status = function(agg_level, partisan, map_ids, progress_dir) {
  processed = 0
  for(map_id in map_ids) {
    if(is_map_processed(map_id, agg_level, partisan, progress_dir)) {
      processed = processed + 1
    }
  }
  return(list(
    total = length(map_ids),
    processed = processed,
    remaining = length(map_ids) - processed
  ))
}

# Modified process_scenario function with incremental saving
process_scenario_incremental = function(agg_level, partisan, progress_dir, max_maps = NULL) {
  
  cat("\n\nProcessing", agg_level, "-", partisan, "scenario...\n")
  
  # Create progress directory if it doesn't exist
  if(!dir.exists(progress_dir)) {
    dir.create(progress_dir, recursive = TRUE)
  }
  
  # Get data for this scenario
  maps = map_ids[[agg_level]][[partisan]]
  plan_df = plan_data[[agg_level]]
  
  # Limit number of maps if specified (for testing)
  if(!is.null(max_maps)) {
    maps = maps[1:min(max_maps, length(maps))]
  }
  
  # Check progress
  status = get_progress_status(agg_level, partisan, maps, progress_dir)
  cat("Progress:", status$processed, "/", status$total, "maps already processed\n")
  
  if(status$remaining == 0) {
    cat("All maps already processed for this scenario!\n")
    # Load and store results
    scenario_results = load_scenario_results(agg_level, partisan, progress_dir)
    results[[agg_level]][[partisan]][["er"]] <<- scenario_results$er
    results[[agg_level]][[partisan]][["ei"]] <<- scenario_results$ei
    return()
  }
  
  # Progress counter - only for remaining maps
  pb = txtProgressBar(min = 0, max = status$remaining, style = 3)
  progress_count = 0
  
  for(i in 1:length(maps)) {
    map_id = maps[i]
    
    # Skip if already processed
    if(is_map_processed(map_id, agg_level, partisan, progress_dir)) {
      next
    }
    
    tryCatch({
      # Calculate precinct stats
      precinct_stats = calculate_precinct_stats(
        map_id = map_id,
        plan_df = plan_df,
        precinct_data = precincts,
        agg_level = agg_level,
        partisan = partisan
      )
      
      # Calculate ground truth
      ground_truth = calculate_ground_truth(precinct_stats)
      
      # Run ER
      er_results = run_ecological_regression(precinct_stats, ground_truth)
      
      # Run EI
      ei_results = run_ecological_inference(precinct_stats, ground_truth)
      
      # Save results immediately
      save_map_results(map_id, agg_level, partisan, er_results, ei_results, progress_dir)
      
      progress_count = progress_count + 1
      setTxtProgressBar(pb, progress_count)
      
    }, error = function(e) {
      cat("\nError processing map", map_id, ":", e$message, "\n")
      # Save empty results to mark as attempted
      save_map_results(map_id, agg_level, partisan, data.frame(), data.frame(), progress_dir)
      progress_count = progress_count + 1
      setTxtProgressBar(pb, progress_count)
    })
  }
  close(pb)
  
  # Load all results for this scenario
  scenario_results = load_scenario_results(agg_level, partisan, progress_dir)
  results[[agg_level]][[partisan]][["er"]] <<- scenario_results$er
  results[[agg_level]][[partisan]][["ei"]] <<- scenario_results$ei
  
  # Print summary
  cat("\nScenario summary:\n",
      "Maps processed in this run:", progress_count, "\n",
      "Total maps processed:", status$processed + progress_count, "\n",
      "Total districts:", nrow(scenario_results$er), "\n")
}

# Clean up progress files after successful completion
cleanup_progress_files = function(progress_dir, backup = TRUE) {
  if(backup) {
    # Create backup directory
    backup_dir = paste0(progress_dir, "_backup_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    dir.create(backup_dir, recursive = TRUE)
    
    # Copy all files to backup
    files = list.files(progress_dir, full.names = TRUE)
    file.copy(files, backup_dir)
    cat("Progress files backed up to:", backup_dir, "\n")
  }
  
  # Remove progress files
  unlink(progress_dir, recursive = TRUE)
  cat("Progress files cleaned up\n")
}

# Print overall progress across all scenarios
print_overall_progress = function(progress_dir) {
  cat("\n=== OVERALL PROGRESS ===\n")
  total_maps = 0
  processed_maps = 0
  
  for(agg in c("low", "medium", "high")) {
    for(part in c("rep", "middle", "dem")) {
      maps = map_ids[[agg]][[part]]
      status = get_progress_status(agg, part, maps, progress_dir)
      total_maps = total_maps + status$total
      processed_maps = processed_maps + status$processed
      
      cat(sprintf("%s-%s: %d/%d (%.1f%%)\n", 
                  agg, part, status$processed, status$total, 
                  100 * status$processed / status$total))
    }
  }
  
  cat(sprintf("\nTOTAL: %d/%d maps processed (%.1f%%)\n", 
              processed_maps, total_maps, 100 * processed_maps / total_maps))
  
  if(processed_maps < total_maps) {
    cat("\nTo resume processing, simply run the processing loop again.\n")
  }
}


################################################################################################################################################################################################################################################
# Testing
################################################################################################################################################################################################################################################

# Set up progress directory
progress_dir = paste0(subdir, "ER_EI_MRP/progress_test")  # Use a test directory

# Check current progress before starting
print_overall_progress(progress_dir)

# # Test with just one scenario first (e.g., high-rep with 10 maps)
# cat("\n=== TESTING WITH HIGH-REP SCENARIO (10 maps) ===\n")
# process_scenario_incremental("high", "rep", progress_dir, max_maps = 10)
# 
# # Check results
# cat("\nTest results check:\n",
#     "ER results rows:", nrow(results$high$rep$er), "\n",
#     "EI results rows:", nrow(results$high$rep$ei), "\n")

test_all_scenarios = TRUE 

if(test_all_scenarios) {
  cat("\n=== TESTING ALL SCENARIOS (5 maps each) ===\n")
  
  start_time = Sys.time()
  
  for(agg in c("low", "medium", "high")) {
    for(part in c("rep", "middle", "dem")) {
      process_scenario_incremental(agg, part, progress_dir, max_maps = 5)
    }
  }
  
  end_time = Sys.time()
  run_time = end_time - start_time
  cat("\nTest run time:", run_time, "\n")
  
  # Check final progress
  print_overall_progress(progress_dir)
  
  # Save test results
  saveRDS(results, file = paste0(subdir, "/ER_EI_MRP/test_results_10maps.rds"))
}

# You can also test the resume capability:
test_resume = TRUE  # Change to TRUE to test resume

if(test_resume) {
  cat("\n=== TESTING RESUME CAPABILITY ===\n")
  cat("Processing first 5 maps of medium-dem...\n")
  process_scenario_incremental("medium", "dem", progress_dir, max_maps = 5)
  
  cat("\nNow processing all 10 maps (should skip first 5)...\n")
  process_scenario_incremental("medium", "dem", progress_dir, max_maps = 10)
}


################################################################################################################################################################################################################################################
# Run analysis across maps
################################################################################################################################################################################################################################################

# Set up progress directory
progress_dir = paste0(subdir, "ER_EI_MRP/progress") # for full run

# Check current progress before starting
print_overall_progress(progress_dir)

# Process all scenarios with incremental saving
{
  start_time = Sys.time()
  
  for(agg in c("low", "medium", "high")) {
    for(part in c("rep", "middle", "dem")) {
      process_scenario_incremental(agg, part, progress_dir)
    }
  }
  
  end_time = Sys.time()
}

run_time = end_time - start_time
cat("\nTotal run time:", run_time, "\n")

# Check final progress
print_overall_progress(progress_dir)

# Save final combined results
saveRDS(results, file = paste0(subdir, "/ER_EI_MRP/raw_results.rds"))

# Optional: Clean up progress files after successful completion
# Uncomment the next line if you want to remove progress files after everything is done
# cleanup_progress_files(progress_dir, backup = TRUE)


################################################################################################################################################################################################################################################
# Functions to Analyze Partial Results
################################################################################################################################################################################################################################################

# Function to load all available results from progress directory
load_all_available_results = function(progress_dir) {
  cat("\n=== LOADING AVAILABLE RESULTS ===\n")
  
  # Initialize results structure
  partial_results = list()
  for(agg_level in c("low", "medium", "high")) {
    partial_results[[agg_level]] = list()
    for(partisan in c("rep", "middle", "dem")) {
      partial_results[[agg_level]][[partisan]] = list(
        er = data.frame(),
        ei = data.frame()
      )
    }
  }
  
  # Load results for each scenario
  total_maps_loaded = 0
  for(agg in c("low", "medium", "high")) {
    for(part in c("rep", "middle", "dem")) {
      scenario_results = load_scenario_results(agg, part, progress_dir)
      partial_results[[agg]][[part]][["er"]] = scenario_results$er
      partial_results[[agg]][[part]][["ei"]] = scenario_results$ei
      
      n_maps = length(unique(scenario_results$er$map_id))
      total_maps_loaded = total_maps_loaded + n_maps
      
      if(n_maps > 0) {
        cat(sprintf("%s-%s: %d maps loaded\n", agg, part, n_maps))
      }
    }
  }
  
  cat(sprintf("\nTotal maps loaded: %d\n", total_maps_loaded))
  return(partial_results)
}

# Function to create summary with confidence intervals based on available data
create_partial_summary_with_ci = function(partial_results) {
  map_summaries = list()
  
  for(agg in c("low", "medium", "high")) {
    for(part in c("rep", "middle", "dem")) {
      # Skip if no data
      if(nrow(partial_results[[agg]][[part]]$er) == 0) {
        next
      }
      
      # ER by map
      er_data = partial_results[[agg]][[part]]$er
      
      # Calculate strict RPV correctness ex-post for ER
      er_data = er_data %>%
        mutate(
          er_rpv_correct = as.numeric(
            (true_minority_dem_share > 0.5 & 
               true_majority_dem_share < 0.5 &
               er_minority_dem_share > 0.5 & 
               er_majority_dem_share < 0.5) |
              (true_minority_dem_share < 0.5 & 
                 true_majority_dem_share > 0.5 &
                 er_minority_dem_share < 0.5 & 
                 er_majority_dem_share > 0.5)
          )
        )
      
      er_by_map = er_data %>%
        group_by(map_id) %>%
        summarise(
          mae_minority = mean(abs(er_minority_resid), na.rm = TRUE),
          mae_majority = mean(abs(er_majority_resid), na.rm = TRUE),
          rpv_accuracy = mean(er_rpv_correct, na.rm = TRUE),  # Use the strict test
          bias_minority = mean(er_minority_resid, na.rm = TRUE),
          bias_majority = mean(er_majority_resid, na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(agg_level = agg, partisan = part, method = "ER")
      
      # EI by map (only if available)
      if(nrow(partial_results[[agg]][[part]]$ei) > 0) {
        ei_data = partial_results[[agg]][[part]]$ei
        
        # Calculate strict RPV correctness ex-post for EI
        ei_data = ei_data %>%
          mutate(
            ei_rpv_correct = as.numeric(
              (true_minority_dem_share > 0.5 & 
                 true_majority_dem_share < 0.5 &
                 ei_minority_dem_share > 0.5 & 
                 ei_majority_dem_share < 0.5) |
                (true_minority_dem_share < 0.5 & 
                   true_majority_dem_share > 0.5 &
                   ei_minority_dem_share < 0.5 & 
                   ei_majority_dem_share > 0.5)
            )
          )
        
        ei_by_map = ei_data %>%
          group_by(map_id) %>%
          summarise(
            mae_minority = mean(abs(ei_minority_resid), na.rm = TRUE),
            mae_majority = mean(abs(ei_majority_resid), na.rm = TRUE),
            rpv_accuracy = mean(ei_rpv_correct, na.rm = TRUE),  # Use the strict test
            bias_minority = mean(ei_minority_resid, na.rm = TRUE),
            bias_majority = mean(ei_majority_resid, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          mutate(agg_level = agg, partisan = part, method = "EI")
        
        map_summaries[[paste(agg, part, sep = "_")]] = rbind(er_by_map, ei_by_map)
      } else {
        map_summaries[[paste(agg, part, sep = "_")]] = er_by_map
      }
    }
  }
  
  if(length(map_summaries) == 0) {
    cat("No results available yet!\n")
    return(NULL)
  }
  
  map_summary = do.call(rbind, map_summaries)
  
  # Calculate means and CIs with available data
  summary_with_ci = map_summary %>%
    group_by(agg_level, partisan, method) %>%
    summarise(
      n_maps = n(),  # Add count of maps
      
      # MAE minority
      mae_minority_mean = mean(mae_minority),
      mae_minority_lower = quantile(mae_minority, 0.025),
      mae_minority_upper = quantile(mae_minority, 0.975),
      
      # MAE majority  
      mae_majority_mean = mean(mae_majority),
      mae_majority_lower = quantile(mae_majority, 0.025),
      mae_majority_upper = quantile(mae_majority, 0.975),
      
      # RPV accuracy
      rpv_accuracy_mean = mean(rpv_accuracy),
      rpv_accuracy_lower = quantile(rpv_accuracy, 0.025),
      rpv_accuracy_upper = quantile(rpv_accuracy, 0.975),
      
      # Bias minority
      bias_minority_mean = mean(bias_minority),
      bias_minority_lower = quantile(bias_minority, 0.025),
      bias_minority_upper = quantile(bias_minority, 0.975),
      
      # Bias majority
      bias_majority_mean = mean(bias_majority),
      bias_majority_lower = quantile(bias_majority, 0.025),
      bias_majority_upper = quantile(bias_majority, 0.975),
      
      .groups = 'drop'
    )
  
  return(list(map_summary = map_summary, summary_with_ci = summary_with_ci))
}

# Function to create plots with partial data
create_partial_plots = function(summary_with_ci, output_dir) {
  
  if(is.null(summary_with_ci)) {
    cat("No data available for plotting.\n")
    return()
  }
  
  # Add sample size to plot titles
  plot_title_suffix = paste0("\n(Based on ", sum(summary_with_ci$n_maps) / 2, " maps processed)")
  
  # Ensure factors are properly set
  summary_with_ci = summary_with_ci %>%
    mutate(
      agg_level = factor(agg_level, levels = c('low', 'medium', 'high')),
      partisan = factor(partisan, levels = c('dem', 'middle', 'rep'))
    )
  
  # Figure 1: MAE 
  mae_plot_data = summary_with_ci %>%
    select(agg_level, partisan, method, n_maps, contains("mae")) %>%
    pivot_longer(cols = c(mae_minority_mean, mae_majority_mean),
                 names_to = "group",
                 values_to = "mean") %>%
    mutate(
      group = ifelse(grepl("minority", group), "Minority", "Majority"),
      lower = ifelse(group == "Minority", mae_minority_lower, mae_majority_lower),
      upper = ifelse(group == "Minority", mae_minority_upper, mae_majority_upper)
    )
  
  p1 = ggplot(mae_plot_data, aes(x = agg_level, y = mean, color = method)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = lower, ymax = upper), 
                  position = position_dodge(0.3), 
                  width = 0.2) +
    geom_text(aes(label = n_maps), 
              position = position_dodge(0.3), 
              vjust = -1.5, 
              size = 3) +
    facet_grid(group ~ partisan) + #, scales = "free_y") +
    labs(title = paste0("Vote Share Estimation Error by Aggregation Level", plot_title_suffix),
         x = "Aggregation Bias Level",
         y = "Mean Absolute Error",
         color = "Method") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p1)
  ggsave(paste0(output_dir, "/partial_mae_comparison.pdf"), p1, width = 12, height = 10)
  
  # Figure 2: RPV Classification 
  rpv_plot_data = summary_with_ci %>%
    select(agg_level, partisan, method, n_maps, starts_with("rpv_accuracy"))
  
  p2 = ggplot(rpv_plot_data, aes(x = agg_level, y = rpv_accuracy_mean, color = method)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
    geom_point(position = position_dodge(0.3), size = 3) +
    geom_errorbar(aes(ymin = rpv_accuracy_lower, ymax = rpv_accuracy_upper), 
                  position = position_dodge(0.3), 
                  width = 0.2) +
    geom_text(aes(label = n_maps), 
              position = position_dodge(0.3), 
              vjust = -1.5, 
              size = 3) +
    facet_wrap(~ partisan) +
    labs(title = paste0("RPV Classification Accuracy", plot_title_suffix),
         x = "Aggregation Bias Level", 
         y = "Accuracy",
         color = "Method") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylim(0, 1)
  
  print(p2)
  ggsave(paste0(output_dir, "/partial_rpv_classification.pdf"), p2, width = 12, height = 8)
  
  # Print summary table
  cat("\n=== PARTIAL RESULTS SUMMARY ===\n")
  summary_table = summary_with_ci %>%
    select(agg_level, partisan, method, n_maps, 
           mae_minority_mean, mae_majority_mean, rpv_accuracy_mean) %>%
    arrange(agg_level, partisan, method)
  
  print(summary_table, n = Inf)
}

# Create aggregate scatter plots with partial data
create_partial_scatter_plots = function(partial_results, output_dir) {
  # Combine available results
  all_er = list()
  all_ei = list()
  
  for(agg in c("low", "medium", "high")) {
    for(part in c("rep", "middle", "dem")) {
      if(nrow(partial_results[[agg]][[part]]$er) > 0) {
        all_er[[paste(agg, part, sep = "_")]] = partial_results[[agg]][[part]]$er
      }
      if(nrow(partial_results[[agg]][[part]]$ei) > 0) {
        all_ei[[paste(agg, part, sep = "_")]] = partial_results[[agg]][[part]]$ei
      }
    }
  }
  
  if(length(all_er) == 0) {
    cat("No ER results available for scatter plots.\n")
    return()
  }
  
  all_er_results = do.call(rbind, all_er)
  all_ei_results = if(length(all_ei) > 0) do.call(rbind, all_ei) else data.frame()
  
  # Prepare data for plotting
  er_plot_data <- all_er_results %>%
    select(agg_level, partisan, true_minority_dem_share, true_majority_dem_share, er_minority_dem_share, er_majority_dem_share) %>%
    rename(estimate_minority = er_minority_dem_share, estimate_majority = er_majority_dem_share) %>%
    mutate(method = "ER")
  
  ei_plot_data <- all_ei_results %>%
    select(agg_level, partisan, true_minority_dem_share, true_majority_dem_share, ei_minority_dem_share, ei_majority_dem_share) %>%
    rename(estimate_minority = ei_minority_dem_share, estimate_majority = ei_majority_dem_share) %>%
    mutate(method = "EI")
  
  combined_plot_data <- rbind(er_plot_data, ei_plot_data) %>%
    mutate(
      agg_level = factor(agg_level, levels = c('low', 'medium', 'high')),
      partisan = factor(partisan, levels = c('dem', 'middle', 'rep'))
    )
  
  # --- Plot 1: Minority Vote Share Estimates vs. Truth ---
  p_minority_agg <- ggplot(combined_plot_data, aes(x = true_minority_dem_share, y = estimate_minority, color = method)) +
    geom_point(alpha = 0.2, size = 1.5) + # Use alpha for overplotting
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    facet_grid(partisan ~ agg_level) +
    labs(
      title = "Minority Democratic Vote Share: Estimates vs. Truth (PARTIAL)",
      x = "True Minority Democratic Share",
      y = "Estimated Minority Democratic Share",
      color = "Method"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1))
  
  ggsave(paste0(subdir, "/ER_EI_MRP/partial_aggregate_estimates_minority.pdf"), p_minority_agg, width = 10, height = 8)
  
  # --- Plot 2: Majority Vote Share Estimates vs. Truth ---
  p_majority_agg <- ggplot(combined_plot_data, aes(x = true_majority_dem_share, y = estimate_majority, color = method)) +
    geom_point(alpha = 0.2, size = 1.5) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
    facet_grid(partisan ~ agg_level) +
    labs(
      title = "Majority Democratic Vote Share: Estimates vs. Truth (PARTIAL)",
      x = "True Majority Democratic Share",
      y = "Estimated Majority Democratic Share",
      color = "Method"
    ) +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    coord_fixed(xlim = c(0, 1), ylim = c(0, 1))
  
  print(p_majority_agg)
  ggsave(paste0(subdir, "/ER_EI_MRP/partial_aggregate_estimates_majority.pdf"), p_majority_agg, width = 10, height = 8)
  
  cat("Scatter plots created with", nrow(all_er_results), "ER districts and", 
      nrow(all_ei_results), "EI districts.\n")
}

################################################################################################################################################################################################################################################
# Run Partial Analysis
################################################################################################################################################################################################################################################

# Set directories
progress_dir = paste0(subdir, "ER_EI_MRP/progress")
output_dir = paste0(subdir, "ER_EI_MRP")

# Check what's available
print_overall_progress(progress_dir)

# Load all available results
partial_results = load_all_available_results(progress_dir)

# Create summaries
summaries = create_partial_summary_with_ci(partial_results)

# Create plots if we have data
if(!is.null(summaries)) {
  create_partial_plots(summaries$summary_with_ci, output_dir)
  
  # Save partial results for later use
  saveRDS(partial_results, file = paste0(output_dir, "/partial_results_", 
                                         format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
  
  # Also save the summary
  saveRDS(summaries, file = paste0(output_dir, "/partial_summary_", 
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".rds"))
}


# Create scatter plots if desired
create_partial_scatter_plots(partial_results, output_dir)






















