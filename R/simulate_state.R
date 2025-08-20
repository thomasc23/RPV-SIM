################################################################################
### Function to generate a single state (i.e. voters and assigned precincts) ###

### Function Arguments ###

# target_population:
# desired number of voters in a state. Defaults to 1,000,000

# state_type: 
# low (<10% minority): one of MT, OR, UT, VT, WY
# medium (10-25% minority): one of FL, IL, MI, NY, PA, TX
# high (25-40% minority): one of AL, GA, LA, MD, MS, SC
# default is medium

# seed_distributions:
# a list of dataframes, each representing a small geographic area that will proxy a precinct. 
# Must include numeric population and per_minority columns

# prob_minority_dem_vote:
# Pr(voter i votes for Democrat | voter i is member of minority group)
# prob_majority_dem_vote: 
# Pr(voter i votes for Democrat | voter i is member of majority group)

# voting_prob_assignment:
# Controls whether 
# a) individuals get same probability across precincts ('constant')
# b) whether precincts have specific mean conditional vote choice probabilities 
#    that are centered at each race's mean ('precinct')
# c) voters get a precinct-type specific shock to their vote choice. In precincts 
#    with high minority voter shares, white voters are probabilistically assigned 
#    a shock that nudges their vote choice toward or away from the Democratic 
#    candidate ('context') (NOT YET IMPLEMENTED)

# context_threshold:
# when voting_prob_assignment is 'context' this threshold [0, 1] determines the minority population
# share at the precinct level above which majority voters get a positive shock to their probability 
# of voting for the democratic candidate, and below which they get a negative shock

# prob_shock_pos / prob_shock_neg
# probability a voter in a precinct above (below) the context_threshold receives a positive (negative)
# shock to their vote choice probability

# shock_positive / shock_negative
# magnitude of positive (negative) shock

# sd_ind_noise:
# how much noise is added to vote choice probabilities

# sd_precinct_minority:
# controls variation in conditional vote choice probabilities around the mean across minority precincts

# sd_precinct_majority:
# controls variation in conditional vote choice probabilities around the mean across majority precincts

################################################################################


simulate_voters = function(target_population = 1000000, 
                           state_type = 'medium',
                           seed_distributions = emp_pop_min_joint_dists,
                           prob_minority_dem_vote = 0.84,
                           prob_majority_dem_vote = 0.43,
                           sd_minority = 0.1,
                           sd_majority = 0.3,
                           voting_prob_assignment = 'hier',
                           rho = 0.2
                           ) {
  
  
  #########################################
  ### Step 1: Check arguments are valid ###
  #########################################
  
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  
  ##################################################################################
  ### Step 2: Define target minority percentage and beta distribution parameters ###
  ##################################################################################
  
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  # Select seed states for the given state_type
  selected_seed_states = seed_states[[state_type]]
  
  # Combine the precinct data from selected seed states
  census_blocks = data.frame(
    population = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$population)),
    per_minority = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$per_minority))
  )
  
  # Remove precincts with missing or invalid data
  census_blocks = census_blocks %>%
    filter(!is.na(population), !is.na(per_minority), population > 0, per_minority >= 0, per_minority <= 1)
  
  # Ensure per_minority values are within (0,1)
  census_blocks$per_minority = pmin(pmax(census_blocks$per_minority, 0), 1)
  
  
  #############################################
  ### Step 3: Adjust sampling probabilities ###
  #############################################
  
  # Set target mean minority percentage
  if (state_type == 'low') {
    target_per_min = 0.05   # 5%
  } else if (state_type == 'medium') {
    target_per_min = 0.175  # 17.5%
  } else if (state_type == 'high') {
    target_per_min = 0.325  # 32.5%
  }
  
  # Assign weights to precincts inversely proportional 
  # to their deviation from target minority percentage
  # Makes precincts closer to target more likely to be sampled
  census_blocks = census_blocks %>%
    mutate(weight = 1 / (abs(per_minority - target_per_min) + 0.001)) # Small number to avoid division by zero
  
  # Normalize weights
  census_blocks$weight = census_blocks$weight / sum(census_blocks$weight)
  
  # Add small random perturbations to per_minority values when sampling precincts
  census_blocks = census_blocks %>%
    mutate(per_minority = per_minority + runif(n(), -0.02, 0.02))
  
  # Ensure values stay within bounds
  census_blocks$per_minority = pmin(pmax(census_blocks$per_minority, 0.001), 0.999)
  

  ############################################################
  ### Step 4: Sample precincts with adjusted probabilities ###
  ############################################################
  
  # Initialize precincts data frame
  precincts = data.frame(
    precinct_id = numeric(),
    population = numeric(),
    per_minority = numeric()
  )
  
  # Initialize total population counter
  N = 0
  
  while (N < target_population) {
    # Sample a batch of precincts with adjusted probabilities
    sample_size = 1000  # Adjust as needed
    
    # Sample row numbers from census block data weighted by weights calculated above
    sampled_indices = sample(
      x = 1:nrow(census_blocks), 
      size = sample_size, 
      replace = TRUE, 
      prob = census_blocks$weight
    )
    
    sampled_precincts = census_blocks[sampled_indices, ]
    sampled_precincts$precinct_id = seq(nrow(precincts) + 1, nrow(precincts) + sample_size)
    
    # Add sampled precincts to the precincts data frame
    precincts = bind_rows(precincts, sampled_precincts)
    
    # Update total population
    N = sum(precincts$population)
  }
  
  precincts = precincts %>%
    mutate(
      # Add random perturbation to per_minority to create more natural spread
      per_minority = per_minority + rnorm(n(), 0, 0.03),
      # Ensure values stay within bounds
      per_minority = pmin(pmax(per_minority, 0.001), 0.999)
    )
  
  # Truncate precincts to match the target population exactly
  precincts = precincts %>%
    mutate(cum_pop = cumsum(population)) %>%
    filter(cum_pop <= target_population)
  
  # Remove cumulative population column and rownames
  precincts$cum_pop = NULL
  rownames(precincts) = NULL
  
  # Recalculate total population
  N = sum(precincts$population)
  
  # Calculate overall percent minority
  overall_per_min = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  
  
  ##########################################
  ### Step 5: Assign voters to precincts ###
  ##########################################
  
  assign_voters_to_precincts = function(precinct_data) {
    
    # Calculate number of minority and majority voters per precinct
    precinct_data = precinct_data %>%
      mutate(
        population = round(population),  # Ensure population is integer
        num_min = round(population * per_minority),
        num_maj = population - num_min
      )
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
      
    } else if (voting_prob_assignment == 'ei') {
      
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      # Using rejection sampling for truncation to [0,1] x [0,1]
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } 
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  ###############################
  ### Step 6: Generate voters ###
  ###############################
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Function returns:
  # Precinct-level data
  # Individual-level data
  # List of parameters used to seed the simulation for a state
  return(list(precinct_data = precincts, 
              voter_data = electorate, 
              seed_parameters = list(target_population = target_population, 
                                     state_type = state_type,
                                     prob_minority_dem_vote = prob_minority_dem_vote,
                                     prob_majority_dem_vote = prob_majority_dem_vote,
                                     voting_prob_assignment = voting_prob_assignment,
                                     sd_minority = sd_minority,
                                     sd_majority = sd_majority,
                                     rho = rho)))
  
}



sim_state = simulate_voters(
  target_population = 1000000,
  state_type = 'high',
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = 0.84,
  prob_majority_dem_vote = 0.43,
  sd_minority = 0.1,
  sd_majority = 0.3,
  voting_prob_assignment = 'ei'
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) + 
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data",
       x = "Percentage Minority",
       y = "Democratic Voteshare")










# Alternative sampling methods that preserve variation while hitting targets

# Method 1: Stratified Sampling with Reweighting
sample_precincts_stratified = function(census_blocks, target_population, target_per_min) {
  # Stratify census blocks by minority percentage
  census_blocks = census_blocks %>%
    mutate(
      minority_bin = cut(per_minority, 
                         breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0),
                         include.lowest = TRUE,
                         labels = FALSE)
    )
  
  # Calculate current distribution
  current_dist = census_blocks %>%
    group_by(minority_bin) %>%
    summarise(
      n = n(),
      total_pop = sum(population),
      avg_minority = weighted.mean(per_minority, population),
      .groups = 'drop'
    ) %>%
    mutate(prop = n / sum(n))
  
  # Sample from each stratum
  precincts = data.frame()
  current_pop = 0
  current_minority_pop = 0
  
  while(current_pop < target_population) {
    # Sample one precinct from each stratum
    for(bin in unique(census_blocks$minority_bin)) {
      bin_blocks = census_blocks[census_blocks$minority_bin == bin, ]
      if(nrow(bin_blocks) > 0) {
        # Sample proportional to population size
        sampled = bin_blocks[sample(nrow(bin_blocks), 1, prob = bin_blocks$population), ]
        sampled$precinct_id = nrow(precincts) + 1
        precincts = bind_rows(precincts, sampled)
        
        current_pop = current_pop + sampled$population
        current_minority_pop = current_minority_pop + sampled$population * sampled$per_minority
      }
    }
  }
  
  # Reweight to achieve target minority percentage
  current_per_min = current_minority_pop / current_pop
  adjustment_factor = target_per_min / current_per_min
  
  # Apply soft adjustment to minority percentages
  precincts = precincts %>%
    mutate(
      # Gentle adjustment toward target
      per_minority_adj = per_minority * (0.7 + 0.3 * adjustment_factor),
      per_minority_adj = pmin(pmax(per_minority_adj, 0.001), 0.999)
    ) %>%
    select(-per_minority) %>%
    rename(per_minority = per_minority_adj)
  
  return(precincts)
}

# Method 2: Accept-Reject Sampling
sample_precincts_accept_reject = function(census_blocks, target_population, target_per_min, tolerance = 0.02) {
  max_attempts = 1000
  best_sample = NULL
  best_deviation = Inf
  
  for(attempt in 1:max_attempts) {
    # Random sample weighted by population
    n_precincts = ceiling(target_population / mean(census_blocks$population))
    sampled_indices = sample(nrow(census_blocks), n_precincts, replace = TRUE, prob = census_blocks$population)
    sampled = census_blocks[sampled_indices, ]
    
    # Calculate total population and minority percentage
    total_pop = sum(sampled$population)
    total_minority = sum(sampled$population * sampled$per_minority)
    achieved_per_min = total_minority / total_pop
    
    deviation = abs(achieved_per_min - target_per_min)
    
    # Accept if within tolerance
    if(deviation < tolerance) {
      sampled$precinct_id = 1:nrow(sampled)
      return(sampled)
    }
    
    # Keep best attempt
    if(deviation < best_deviation) {
      best_deviation = deviation
      best_sample = sampled
    }
  }
  
  # Return best attempt if we couldn't get within tolerance
  cat("Warning: Could not achieve exact target. Best deviation:", best_deviation, "\n")
  best_sample$precinct_id = 1:nrow(best_sample)
  return(best_sample)
}

# Method 3: IPF (Iterative Proportional Fitting) Approach
sample_precincts_ipf = function(census_blocks, target_population, target_per_min) {
  # Start with a diverse sample
  n_precincts = ceiling(target_population / mean(census_blocks$population))
  
  # Initial sample with mild preference for diversity
  diversity_weights = census_blocks %>%
    mutate(
      # Prefer precincts away from 50% minority for more variation
      diversity_score = abs(per_minority - 0.5),
      weight = population * (1 + diversity_score)
    ) %>%
    pull(weight)
  
  sampled_indices = sample(nrow(census_blocks), n_precincts, replace = TRUE, prob = diversity_weights)
  precincts = census_blocks[sampled_indices, ]
  precincts$precinct_id = 1:nrow(precincts)
  precincts$weight = 1  # Initial weight
  
  # Iteratively adjust weights to match targets
  for(iter in 1:20) {
    # Current statistics
    total_pop = sum(precincts$population * precincts$weight)
    total_minority = sum(precincts$population * precincts$per_minority * precincts$weight)
    current_per_min = total_minority / total_pop
    
    # Adjust weights
    if(abs(current_per_min - target_per_min) < 0.001) break
    
    adjustment = target_per_min / current_per_min
    precincts = precincts %>%
      mutate(
        # Adjust weights more for precincts far from target
        weight_adjustment = 1 + (adjustment - 1) * abs(per_minority - target_per_min),
        weight = weight * weight_adjustment,
        weight = pmax(0.1, pmin(weight, 10))  # Bound weights
      )
  }
  
  # Apply final weights by resampling
  final_sample_indices = sample(nrow(precincts), nrow(precincts), replace = TRUE, prob = precincts$weight)
  final_precincts = precincts[final_sample_indices, ]
  final_precincts$precinct_id = 1:nrow(final_precincts)
  
  return(final_precincts %>% select(-weight, -weight_adjustment))
}

# Method 4: Preserve Original Distribution with Post-hoc Adjustment
sample_precincts_preserve_distribution = function(census_blocks, target_population, target_per_min, state_type) {
  # Get the empirical distribution from the state type
  state_blocks = census_blocks %>%
    mutate(
      minority_decile = ntile(per_minority, 10)
    )
  
  # Calculate the natural distribution
  natural_dist = state_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(state_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = state_blocks[state_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
  
  if(abs(current_minority - target_per_min) > 0.02) {
    # Only adjust if we're far off
    adjustment = (target_per_min - current_minority) / 2  # Gentle adjustment
    precincts = precincts %>%
      mutate(
        per_minority = pmin(pmax(per_minority + adjustment, 0.001), 0.999)
      )
  }
  
  return(precincts %>% select(-minority_decile))
}

# Comparison function
compare_sampling_methods = function(census_blocks, target_population = 100000, target_per_min = 0.325) {
  library(ggplot2)
  library(patchwork)
  
  # Current method (for comparison)
  current_method = function(census_blocks, target_population, target_per_min) {
    census_blocks = census_blocks %>%
      mutate(weight = 1 / (abs(per_minority - target_per_min) + 0.001))
    census_blocks$weight = census_blocks$weight / sum(census_blocks$weight)
    
    n_precincts = ceiling(target_population / mean(census_blocks$population))
    sampled_indices = sample(nrow(census_blocks), n_precincts, replace = TRUE, prob = census_blocks$weight)
    sampled = census_blocks[sampled_indices, ]
    sampled$precinct_id = 1:nrow(sampled)
    return(sampled)
  }
  
  # Apply each method
  methods = list(
    "Current (Importance)" = current_method(census_blocks, target_population, target_per_min),
    "Stratified" = sample_precincts_stratified(census_blocks, target_population, target_per_min),
    "Accept-Reject" = sample_precincts_accept_reject(census_blocks, target_population, target_per_min),
    "IPF" = sample_precincts_ipf(census_blocks, target_population, target_per_min),
    "Preserve Distribution" = sample_precincts_preserve_distribution(census_blocks, target_population, target_per_min, "high")
  )
  
  # Calculate statistics for each method
  stats = lapply(names(methods), function(method_name) {
    data = methods[[method_name]]
    total_pop = sum(data$population)
    achieved_minority = sum(data$population * data$per_minority) / total_pop
    
    data.frame(
      method = method_name,
      n_precincts = nrow(data),
      total_population = total_pop,
      achieved_minority = achieved_minority,
      minority_sd = sd(data$per_minority),
      minority_range = max(data$per_minority) - min(data$per_minority),
      prop_below_10pct = mean(data$per_minority < 0.1),
      prop_above_50pct = mean(data$per_minority > 0.5)
    )
  }) %>% bind_rows()
  
  # Create comparison plots
  plot_data = lapply(names(methods), function(method_name) {
    methods[[method_name]] %>%
      mutate(method = method_name)
  }) %>% bind_rows()
  
  p1 = ggplot(plot_data, aes(x = per_minority, fill = method)) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~method, ncol = 2, scales = "free_y") +
    geom_vline(xintercept = target_per_min, linetype = "dashed", color = "red") +
    labs(title = "Distribution of Precinct Minority Percentages by Sampling Method",
         x = "Percent Minority",
         y = "Count") +
    theme_minimal() +
    theme(legend.position = "none")
  
  p2 = ggplot(plot_data, aes(x = method, y = per_minority)) +
    geom_boxplot(aes(fill = method)) +
    geom_hline(yintercept = target_per_min, linetype = "dashed", color = "red") +
    labs(title = "Variation in Minority Percentage by Method",
         x = "Sampling Method",
         y = "Percent Minority") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")
  
  return(list(
    stats = stats,
    plots = list(distributions = p1, boxplots = p2),
    samples = methods
  ))
}

# Modified simulate_voters function using better sampling
simulate_voters_improved = function(
    target_population = 1000000,
    state_type = 'medium',
    seed_distributions = emp_pop_min_joint_dists,
    prob_minority_dem_vote = 0.84,
    prob_majority_dem_vote = 0.43,
    sd_minority = 0.1,
    sd_majority = 0.3,
    voting_prob_assignment = 'hier',
    rho = 0.2,
    sampling_method = "preserve_distribution"  # New parameter
) {
  
  # [Previous validation code...]
  
  # Get census blocks
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  selected_seed_states = seed_states[[state_type]]
  
  census_blocks = data.frame(
    population = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$population)),
    per_minority = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$per_minority))
  ) %>%
    filter(!is.na(population), !is.na(per_minority), population > 0, per_minority >= 0, per_minority <= 1)
  
  # Set target
  target_per_min = case_when(
    state_type == 'low' ~ 0.05,
    state_type == 'medium' ~ 0.175,
    state_type == 'high' ~ 0.325
  )
  
  # Apply selected sampling method
  if(sampling_method == "stratified") {
    precincts = sample_precincts_stratified(census_blocks, target_population, target_per_min)
  } else if(sampling_method == "accept_reject") {
    precincts = sample_precincts_accept_reject(census_blocks, target_population, target_per_min)
  } else if(sampling_method == "ipf") {
    precincts = sample_precincts_ipf(census_blocks, target_population, target_per_min)
  } else if(sampling_method == "preserve_distribution") {
    precincts = sample_precincts_preserve_distribution(census_blocks, target_population, target_per_min, state_type)
  } else {
    stop("Unknown sampling method")
  }
  
  # Add final perturbation for realism
  precincts = precincts %>%
    mutate(
      per_minority = per_minority + rnorm(n(), 0, 0.02),
      per_minority = pmin(pmax(per_minority, 0.001), 0.999)
    )
  
  # Truncate to target population
  precincts = precincts %>%
    mutate(cum_pop = cumsum(population)) %>%
    filter(cum_pop <= target_population) %>%
    select(-cum_pop)
  
  rownames(precincts) = NULL
  N = sum(precincts$population)
  
  overall_per_min = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  
  # [Rest of function continues with voter assignment as before...]
  
  # The assign_voters_to_precincts function remains the same
  assign_voters_to_precincts = function(precinct_data) {
    # [Previous code unchanged...]
    precinct_data = precinct_data %>%
      mutate(
        population = round(population),
        num_min = round(population * per_minority),
        num_maj = population - num_min
      )
    
    N = sum(precinct_data$population)
    voter_id = 1:N
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
    } else if (voting_prob_assignment == 'ei') {
      Sigma = matrix(c(
        sd_minority^2,
        rho * sd_minority * sd_majority,
        rho * sd_minority * sd_majority,
        sd_majority^2
      ), nrow = 2)
      
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
    }
    
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  # Generate voters
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate final statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  return(list(
    precinct_data = precincts,
    voter_data = electorate,
    seed_parameters = list(
      target_population = target_population,
      state_type = state_type,
      prob_minority_dem_vote = prob_minority_dem_vote,
      prob_majority_dem_vote = prob_majority_dem_vote,
      voting_prob_assignment = voting_prob_assignment,
      sd_minority = sd_minority,
      sd_majority = sd_majority,
      rho = rho,
      sampling_method = sampling_method
    )
  ))
}



sim_state = simulate_voters(
  target_population = 1000000,
  state_type = 'high',
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = 0.84,
  prob_majority_dem_vote = 0.43,
  sd_minority = 0.1,
  sd_majority = 0.3,
  voting_prob_assignment = 'ei'
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) + 
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data",
       x = "Percentage Minority",
       y = "Democratic Voteshare")


# Load your census block data first
# Assuming emp_pop_min_joint_dists is loaded

# Compare sampling methods
comparison = compare_sampling_methods(
  census_blocks = bind_rows(emp_pop_min_joint_dists[c('al','ga','ms','sc')]),
  target_population = 100000,
  target_per_min = 0.325
)

# View statistics
print(comparison$stats)

# View plots
print(comparison$plots$distributions)
print(comparison$plots$boxplots)

# Test with improved sampling
sim_improved = simulate_voters_improved(
  target_population = 1000000,
  state_type = "high",
  prob_minority_dem_vote = 0.85,
  prob_majority_dem_vote = 0.35,
  sd_minority = 0.20,
  sd_majority = 0.40,
  voting_prob_assignment = "ei",
  rho = 0.1,
  sampling_method = "preserve_distribution"  # This maintains natural variation
)

# Compare variance
cat("\nVariance in democratic voteshare:", var(sim_improved$precinct_data$dem_voteshare), "\n")
cat("Range of minority %:", range(sim_improved$precinct_data$per_minority), "\n")















# Test version with multiplicative scaling
sample_precincts_multiplicative = function(census_blocks, target_population, target_per_min, state_type) {
  # Get the empirical distribution from the state type
  state_blocks = census_blocks %>%
    mutate(
      minority_decile = ntile(per_minority, 10)
    )
  
  # Calculate the natural distribution
  natural_dist = state_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(state_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = state_blocks[state_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Calculate current minority percentage
  current_minority = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
  
  cat('\nMethod: Multiplicative Scaling\n')
  cat('Natural distribution before adjustment:\n')
  cat('  Mean:', round(mean(precincts$per_minority), 3), '\n')
  cat('  SD:', round(sd(precincts$per_minority), 3), '\n')
  cat('  Range:', round(range(precincts$per_minority), 3), '\n')
  cat('  Population-weighted mean:', round(current_minority, 3), '\n')
  
  # Apply multiplicative adjustment if needed
  if(abs(current_minority - target_per_min) > 0.02) {
    # Scale all minority percentages proportionally
    scale_factor = target_per_min / current_minority
    cat('Target minority %:', round(target_per_min * 100, 2), '%\n')
    cat('Scaling all minority percentages by factor of', round(scale_factor, 3), '\n')
    
    precincts = precincts %>%
      mutate(
        # Multiplicative adjustment preserves relative differences and zeros
        per_minority = per_minority * scale_factor,
        # Apply bounds
        per_minority = pmin(pmax(per_minority, 0.0), 1.0)
      )
    
    # Check final result
    final_minority = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
    cat('Final achieved minority %:', round(final_minority * 100, 2), '%\n')
    cat('Final distribution:\n')
    cat('  Mean:', round(mean(precincts$per_minority), 3), '\n')
    cat('  SD:', round(sd(precincts$per_minority), 3), '\n')
    cat('  Range:', round(range(precincts$per_minority), 3), '\n')
  }
  
  return(precincts %>% select(-minority_decile))
}

# Enhanced comparison function that includes both methods
compare_adjustment_methods = function(census_blocks, target_population = 100000, target_per_min = 0.325) {
  library(ggplot2)
  library(patchwork)
  
  # Get natural sample first (no adjustment)
  natural_sample = sample_precincts_preserve_distribution(census_blocks, target_population, target_per_min, "high")
  natural_minority = sum(natural_sample$population * natural_sample$per_minority) / sum(natural_sample$population)
  
  cat("Natural sample (before any adjustment):\n")
  cat("  Achieved minority %:", round(natural_minority * 100, 2), "%\n")
  cat("  Target minority %:", round(target_per_min * 100, 2), "%\n")
  cat("  Difference:", round((natural_minority - target_per_min) * 100, 2), "percentage points\n\n")
  
  # Test different adjustment methods
  methods = list(
    "Natural (No Adjustment)" = natural_sample,
    "Additive Adjustment" = sample_precincts_preserve_distribution(census_blocks, target_population, target_per_min, "high"),
    "Multiplicative Scaling" = sample_precincts_multiplicative(census_blocks, target_population, target_per_min, "high")
  )
  
  # Calculate statistics for each method
  stats = lapply(names(methods), function(method_name) {
    data = methods[[method_name]]
    total_pop = sum(data$population)
    achieved_minority = sum(data$population * data$per_minority) / total_pop
    
    # Count how many precincts are at the bounds
    at_lower_bound = sum(data$per_minority <= 0.001)
    at_upper_bound = sum(data$per_minority >= 0.999)
    
    data.frame(
      method = method_name,
      n_precincts = nrow(data),
      total_population = total_pop,
      target_minority = target_per_min,
      achieved_minority = achieved_minority,
      error_from_target = achieved_minority - target_per_min,
      minority_mean = mean(data$per_minority),
      minority_sd = sd(data$per_minority),
      minority_min = min(data$per_minority),
      minority_max = max(data$per_minority),
      minority_range = max(data$per_minority) - min(data$per_minority),
      prop_below_10pct = mean(data$per_minority < 0.1),
      prop_above_50pct = mean(data$per_minority > 0.5),
      prop_above_90pct = mean(data$per_minority > 0.9),
      precincts_at_bounds = at_lower_bound + at_upper_bound
    )
  }) %>% bind_rows()
  
  # Create comparison plots
  plot_data = lapply(names(methods), function(method_name) {
    methods[[method_name]] %>%
      mutate(method = method_name)
  }) %>% bind_rows()
  
  # Distribution plots
  p1 = ggplot(plot_data, aes(x = per_minority)) +
    geom_histogram(bins = 50, fill = "steelblue", color = "white") +
    facet_wrap(~method, ncol = 1, scales = "free_y") +
    geom_vline(xintercept = target_per_min, linetype = "dashed", color = "red", linewidth = 1) +
    labs(title = "Distribution of Minority % by Adjustment Method",
         subtitle = paste("Target:", round(target_per_min * 100, 1), "%"),
         x = "Percent Minority",
         y = "Count") +
    theme_minimal()
  
  # Scatter plot to show the adjustment effect
  p2 = ggplot(plot_data, aes(x = method, y = per_minority)) +
    geom_jitter(alpha = 0.3, width = 0.2) +
    geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    geom_hline(yintercept = target_per_min, linetype = "dashed", color = "red", size = 1) +
    labs(title = "Minority % Distribution by Method",
         x = "Adjustment Method",
         y = "Percent Minority") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Before/after comparison for adjustment methods
  if(natural_minority != target_per_min) {
    adjustment_comparison = data.frame(
      precinct_id = 1:nrow(natural_sample),
      natural = natural_sample$per_minority,
      additive = methods[["Additive Adjustment"]]$per_minority,
      multiplicative = methods[["Multiplicative Scaling"]]$per_minority
    ) %>%
      tidyr::pivot_longer(cols = -precinct_id, names_to = "method", values_to = "per_minority")
    
    p3 = ggplot(adjustment_comparison, aes(x = per_minority, fill = method)) +
      geom_histogram(alpha = 0.6, position = "identity", bins = 50) +
      scale_fill_manual(values = c("natural" = "gray50", 
                                   "additive" = "red", 
                                   "multiplicative" = "blue")) +
      labs(title = "Comparison of Adjustment Effects",
           x = "Percent Minority",
           y = "Count") +
      theme_minimal()
  } else {
    p3 = NULL
  }
  
  return(list(
    stats = stats,
    plots = list(
      distributions = p1, 
      boxplots = p2,
      adjustment_comparison = p3
    ),
    samples = methods,
    natural_minority = natural_minority
  ))
}

# Test specifically for high minority states
test_high_minority_state = function() {
  # Load census blocks from high minority states
  high_state_blocks = bind_rows(
    emp_pop_min_joint_dists[c('al', 'ga', 'la', 'md', 'ms', 'sc')]
  )
  
  # Test with the exact parameters from your simulation
  cat("Testing adjustment methods for HIGH minority state (target 32.5%):\n")
  cat(rep("=", 60), "\n\n")
  
  comparison = compare_adjustment_methods(
    census_blocks = high_state_blocks,
    target_population = 100000,  # Smaller for testing
    target_per_min = 0.325
  )
  
  # Print detailed statistics
  cat("\n\nDETAILED STATISTICS:\n")
  print(comparison$stats %>% 
          select(method, achieved_minority, minority_mean, minority_sd, 
                 minority_min, minority_max, precincts_at_bounds) %>%
          mutate(across(where(is.numeric), ~round(., 4))))
  
  # Show plots
  print(comparison$plots$distributions)
  print(comparison$plots$boxplots)
  if(!is.null(comparison$plots$adjustment_comparison)) {
    print(comparison$plots$adjustment_comparison)
  }
  
  return(comparison)
}

# Run the test
test_results = test_high_minority_state()

# Additional diagnostic: Show what happens to specific percentiles
show_percentile_changes = function(comparison) {
  natural = comparison$samples[["Natural (No Adjustment)"]]$per_minority
  additive = comparison$samples[["Additive Adjustment"]]$per_minority
  multiplicative = comparison$samples[["Multiplicative Scaling"]]$per_minority
  
  percentiles = c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
  
  percentile_df = data.frame(
    percentile = percentiles * 100,
    natural = quantile(natural, percentiles),
    additive = quantile(additive, percentiles),
    multiplicative = quantile(multiplicative, percentiles)
  )
  
  cat("\n\nPERCENTILE CHANGES:\n")
  print(percentile_df)
  
  # Show specific examples
  cat("\n\nEXAMPLE TRANSFORMATIONS:\n")
  examples = c(0.0, 0.05, 0.10, 0.32, 0.50, 0.90, 1.0)
  for(ex in examples) {
    nat_idx = which.min(abs(natural - ex))
    cat(sprintf("Natural %.1f%% -> Additive %.1f%% | Multiplicative %.1f%%\n",
                natural[nat_idx] * 100,
                additive[nat_idx] * 100,
                multiplicative[nat_idx] * 100))
  }
}

# Show percentile changes
show_percentile_changes(test_results)



























sample_precincts_preserve_distribution = function(census_blocks, target_population, target_per_min, state_type) {
  # Get the empirical distribution from the state type
  state_blocks = census_blocks %>%
    mutate(
      minority_decile = ntile(per_minority, 10)
    )
  
  # Calculate the natural distribution
  natural_dist = state_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(state_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = state_blocks[state_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
  
  if(abs(current_minority - target_per_min) > 0.02) {
    # Only adjust if we're far off
    adjustment = (target_per_min - current_minority) / 2  # Gentle adjustment
    precincts = precincts %>%
      mutate(
        per_minority = pmin(pmax(per_minority + adjustment, 0.001), 0.999)
      )
  }
  
  return(precincts %>% select(-minority_decile))
}







simulate_voters = function(target_population = 1000000, 
                           state_type = 'medium',
                           seed_distributions = emp_pop_min_joint_dists,
                           prob_minority_dem_vote = 0.84,
                           prob_majority_dem_vote = 0.43,
                           sd_minority = 0.1,
                           sd_majority = 0.3,
                           voting_prob_assignment = 'hier',
                           rho = 0.2
) {
  
  
  #########################################
  ### Step 1: Check arguments are valid ###
  #########################################
  
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  
  ##################################################################################
  ### Step 2: Define target minority percentage and beta distribution parameters ###
  ##################################################################################
  
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('nc') # c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  # Select seed states for the given state_type
  selected_seed_states = seed_states[[state_type]]
  
  # Combine the precinct data from selected seed states
  census_blocks = data.frame(
    population = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$population)),
    per_minority = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$per_minority))
  )
  
  # Remove precincts with missing or invalid data
  census_blocks = census_blocks %>%
    filter(!is.na(population), !is.na(per_minority), population > 0, per_minority >= 0, per_minority <= 1)
  
  # Ensure per_minority values are within (0,1)
  census_blocks$per_minority = pmin(pmax(census_blocks$per_minority, 0), 1)
  
  
  ###################################################################
  ### Step 3: Preserve natural distribution with minimal adjustment ###
  ###################################################################
  
  # Set target mean minority percentage
  if (state_type == 'low') {
    target_per_min = 0.05   # 5%
  } else if (state_type == 'medium') {
    target_per_min = 0.175  # 17.5%
  } else if (state_type == 'high') {
    target_per_min = 0.325  # 32.5%
  }
  
  # Calculate natural distribution by deciles
  census_blocks = census_blocks %>%
    mutate(minority_decile = ntile(per_minority, 10))
  
  natural_dist = census_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(census_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  
  ##################################################################
  ### Step 4: Sample precincts maintaining natural distribution ###
  ##################################################################
  
  # Initialize precincts data frame
  precincts = data.frame()
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = census_blocks[census_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
  
  # if(abs(current_minority - target_per_min) > 0.02) {
  #   if(current_minority > target_per_min) {
  #     # Soft truncate high values
  #     threshold = quantile(precincts$per_minority, 0.8)
  #     high_indices = which(precincts$per_minority > threshold)
  #     
  #     # Randomly keep only some high minority precincts
  #     keep_prop = target_per_min / current_minority
  #     keep_indices = sample(high_indices, round(length(high_indices) * keep_prop))
  #     drop_indices = setdiff(high_indices, keep_indices)
  #     
  #     # Replace dropped with middle-range precincts
  #     middle_candidates = census_blocks %>% 
  #       filter(minority_decile %in% 4:6)
  #     replacements = middle_candidates[sample(nrow(middle_candidates), length(drop_indices), replace = TRUE), ]
  #     
  #     precincts[drop_indices, c("population", "per_minority")] = replacements[, c("population", "per_minority")]
  #   }
  # }
  
  # Remove the minority_decile column if it exists
  if ("minority_decile" %in% names(precincts)) {
    precincts = precincts %>% select(-minority_decile)
  }
  
  # Recalculate total population
  N = sum(precincts$population)
  
  # Calculate and report final statistics
  overall_per_min = sum(precincts$population * precincts$per_minority) / sum(precincts$population)
  cat('Overall population:', sum(precincts$population), '%\n')
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  cat('Number of precincts:', nrow(precincts), '\n')
  
  
  ##########################################
  ### Step 5: Assign voters to precincts ###
  ##########################################
  
  assign_voters_to_precincts = function(precinct_data) {
    
    # Calculate number of minority and majority voters per precinct
    precinct_data = precinct_data %>%
      mutate(
        population = round(population),  # Ensure population is integer
        num_min = round(population * per_minority),
        num_maj = population - num_min
      )
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
      
    } else if (voting_prob_assignment == 'ei') {
      
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      # Using rejection sampling for truncation to [0,1] x [0,1]
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } 
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  ###############################
  ### Step 6: Generate voters ###
  ###############################
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Function returns:
  # Precinct-level data
  # Individual-level data
  # List of parameters used to seed the simulation for a state
  return(list(precinct_data = precincts, 
              voter_data = electorate, 
              seed_parameters = list(target_population = target_population, 
                                     state_type = state_type,
                                     prob_minority_dem_vote = prob_minority_dem_vote,
                                     prob_majority_dem_vote = prob_majority_dem_vote,
                                     voting_prob_assignment = voting_prob_assignment,
                                     sd_minority = sd_minority,
                                     sd_majority = sd_majority,
                                     rho = rho)))
  
}



sim_state = simulate_voters(
  target_population = 1000000,
  state_type = 'high',
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = 0.84,
  prob_majority_dem_vote = 0.43,
  sd_minority = 0.1,
  sd_majority = 0.3,
  voting_prob_assignment = 'ei'
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) + 
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data",
       x = "Percentage Minority",
       y = "Democratic Voteshare")























# Sample population counts instead of percent minority
simulate_voters = function(target_population = 1000000, 
                           state_type = 'medium',
                           seed_distributions = emp_pop_min_joint_dists,
                           prob_minority_dem_vote = 0.84,
                           prob_majority_dem_vote = 0.43,
                           sd_minority = 0.1,
                           sd_majority = 0.3,
                           voting_prob_assignment = 'hier',
                           rho = 0.2,
                           b_min_context = 0.1,
                           b_maj_context = 0.2
) {
  
  
  #########################################
  ### Step 1: Check arguments are valid ###
  #########################################
  
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei', 'contextual'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  
  ##################################################################################
  ### Step 2: Define target minority percentage and beta distribution parameters ###
  ##################################################################################
  
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('nc') # c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  # Select seed states for the given state_type
  selected_seed_states = seed_states[[state_type]]
  
  # Combine the precinct data from selected seed states
  census_blocks = data.frame(
    cvap_maj = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_maj)),
    cvap_min = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_min))
  )
  
  # Calculate derived values
  census_blocks = census_blocks %>%
    mutate(
      population = cvap_maj + cvap_min,  # Total population is sum of maj and min
      per_minority = cvap_min / population
    )
  
  # Remove precincts with missing or invalid data
  census_blocks = census_blocks %>%
    filter(!is.na(cvap_maj), !is.na(cvap_min), 
           cvap_maj >= 0, cvap_min >= 0, 
           population > 0)
  
  
  ###################################################################
  ### Step 3: Preserve natural distribution with minimal adjustment ###
  ###################################################################
  
  # Set target mean minority percentage
  if (state_type == 'low') {
    target_per_min = 0.05   # 5%
  } else if (state_type == 'medium') {
    target_per_min = 0.175  # 17.5%
  } else if (state_type == 'high') {
    target_per_min = 0.325  # 32.5%
  }
  
  # Calculate natural distribution by deciles
  census_blocks = census_blocks %>%
    mutate(minority_decile = ntile(per_minority, 10))
  
  natural_dist = census_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(census_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  
  ##################################################################
  ### Step 4: Sample precincts maintaining natural distribution ###
  ##################################################################
  
  # Initialize precincts data frame
  precincts = data.frame()
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = census_blocks[census_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$cvap_min) / sum(precincts$population)
  
  # Remove the minority_decile column if it exists
  if ("minority_decile" %in% names(precincts)) {
    precincts = precincts %>% select(-minority_decile)
  }
  
  # Recalculate total population
  N = sum(precincts$population)
  
  # Calculate and report final statistics
  overall_per_min = sum(precincts$cvap_min) / sum(precincts$population)
  cat('Overall population:', sum(precincts$population), '\n')
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  cat('Number of precincts:', nrow(precincts), '\n')
  
  
  ##########################################
  ### Step 5: Assign voters to precincts ###
  ##########################################
  
  assign_voters_to_precincts = function(precinct_data) {
    
    # Now we directly use cvap_maj and cvap_min - no rounding needed!
    precinct_data = precinct_data %>%
      mutate(
        num_min = cvap_min,
        num_maj = cvap_maj
      )
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
      
    } else if (voting_prob_assignment == 'ei') {
      
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } else if (voting_prob_assignment == 'contextual') {
      
      # Same as 'ei' but with added linear term for precinct-level minority share so that behavior is correlated with geography
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      per_minority_centered = precinct_data$per_minority - mean(precinct_data$per_minority)
      
      # Apply contextual effects 
      prob_minority_dem_vote_vec = beta[, 1] + b_min_context * per_minority_centered
      prob_majority_dem_vote_vec = beta[, 2] + b_maj_context * per_minority_centered
      
      # Ensure probabilities stay in [0,1]
      prob_minority_dem_vote_vec = pmax(0, pmin(1, prob_minority_dem_vote_vec))
      prob_majority_dem_vote_vec = pmax(0, pmin(1, prob_majority_dem_vote_vec))
      
    }
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  ###############################
  ### Step 6: Generate voters ###
  ###############################
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      n_minority = sum(race == 1),
      n_majority = sum(race == 0),
      dem_votes = sum(vote == 1),
      rep_votes = sum(vote == 0),
      dem_votes_minority = sum(vote[race == 1] == 1), 
      rep_votes_minority = sum(vote[race == 1] == 0), 
      dem_votes_majority = sum(vote[race == 0] == 1), 
      rep_votes_majority = sum(vote[race == 0] == 0),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Function returns:
  # Precinct-level data
  # Individual-level data
  # List of parameters used to seed the simulation for a state
  return(list(precinct_data = precincts, 
              voter_data = electorate, 
              seed_parameters = list(target_population = target_population, 
                                     state_type = state_type,
                                     prob_minority_dem_vote = prob_minority_dem_vote,
                                     prob_majority_dem_vote = prob_majority_dem_vote,
                                     voting_prob_assignment = voting_prob_assignment,
                                     sd_minority = sd_minority,
                                     sd_majority = sd_majority,
                                     rho = rho,
                                     b_min_context = b_min_context,
                                     b_maj_context = b_maj_context)))
  
}

sim_state = simulate_voters(
  target_population = TARGET_POPULATION,
  state_type = STATE_TYPE,
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = PROB_MINORITY_DEM_VOTE,
  prob_majority_dem_vote = PROB_MAJORITY_DEM_VOTE,
  sd_minority = SD_MINORITY,
  sd_majority = SD_MAJORITY,
  voting_prob_assignment = VOTING_PROB_ASSIGNMENT,
  rho = RHO,
  b_min_context = B_MIN_CONTEXT,
  b_maj_context = B_MAJ_CONTEXT
  # context_threshold = CONTEXT_THRESHOLD,
  # prob_shock_pos = PROB_SHOCK_POS,
  # shock_positive = PROB_SHOCK_NEG,
  # prob_shock_neg = SHOCK_NEGATIVE,
  # shock_negative = SHOCK_POSITIVE,
  # sd_ind_noise = SD_IND_NOISE,
  # sd_precinct_minority = SD_PRECINCT_MINORITY,
  # sd_precinct_majority = SD_PRECINCT_MAJORITY
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) +
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data", x = "Percentage Minority", y = "Democratic Voteshare")







simulate_voters = function(target_population = 1000000, 
                           state_type = 'medium',
                           seed_distributions = emp_pop_min_joint_dists,
                           prob_minority_dem_vote = 0.84,
                           prob_majority_dem_vote = 0.43,
                           sd_minority = 0.1,
                           sd_majority = 0.3,
                           voting_prob_assignment = 'hier',
                           rho = 0.2,
                           b_min_context = 0.1,
                           b_maj_context = 0.2
) {
  
  
  #########################################
  ### Step 1: Check arguments are valid ###
  #########################################
  
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei', 'contextual'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  
  ##################################################################################
  ### Step 2: Define target minority percentage and beta distribution parameters ###
  ##################################################################################
  
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('nc') # c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  # Select seed states for the given state_type
  selected_seed_states = seed_states[[state_type]]
  
  # Combine the precinct data from selected seed states
  census_blocks = data.frame(
    cvap_maj = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_maj)),
    cvap_min = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_min))
  )
  
  # Calculate derived values
  census_blocks = census_blocks %>%
    mutate(
      population = cvap_maj + cvap_min,  # Total population is sum of maj and min
      per_minority = cvap_min / population
    )
  
  # Remove precincts with missing or invalid data
  census_blocks = census_blocks %>%
    filter(!is.na(cvap_maj), !is.na(cvap_min), 
           cvap_maj >= 0, cvap_min >= 0, 
           population > 0)
  
  # Aggregate census blocks into precinct-sized units
  # Target precinct size varies by density (smaller blocks = denser areas = larger precincts)
  census_blocks = census_blocks %>%
    arrange(runif(n())) %>%  # Random shuffle to mix geographies
    mutate(
      # Smaller blocks (urban) aggregate into larger precincts
      # Larger blocks (rural) need fewer blocks per precinct
      target_precinct_pop = case_when(
        population < 50 ~ 2000,    # Very small blocks (urban) -> larger precincts
        population < 200 ~ 1500,   # Small blocks -> medium precincts  
        population < 500 ~ 1000,   # Medium blocks -> smaller precincts
        TRUE ~ 800                 # Large blocks (rural) -> smallest precincts
      ),
      cumsum_pop = cumsum(population),
      precinct_group = ceiling(cumsum_pop / mean(target_precinct_pop))
    ) %>%
    group_by(precinct_group) %>%
    summarise(
      cvap_maj = sum(cvap_maj),
      cvap_min = sum(cvap_min),
      population = sum(population),
      per_minority = sum(cvap_min) / sum(population),
      n_blocks_aggregated = n(),
      .groups = 'drop'
    ) %>%
    select(-precinct_group)
  
  
  ###################################################################
  ### Step 3: Preserve natural distribution with minimal adjustment ###
  ###################################################################
  
  # Set target mean minority percentage
  if (state_type == 'low') {
    target_per_min = 0.05   # 5%
  } else if (state_type == 'medium') {
    target_per_min = 0.175  # 17.5%
  } else if (state_type == 'high') {
    target_per_min = 0.325  # 32.5%
  }
  
  # Calculate natural distribution by deciles
  census_blocks = census_blocks %>%
    mutate(minority_decile = ntile(per_minority, 10))
  
  natural_dist = census_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(census_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  
  ##################################################################
  ### Step 4: Sample precincts maintaining natural distribution ###
  ##################################################################
  
  # Initialize precincts data frame
  precincts = data.frame()
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = census_blocks[census_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$cvap_min) / sum(precincts$population)
  
  # Remove the minority_decile column if it exists
  if ("minority_decile" %in% names(precincts)) {
    precincts = precincts %>% select(-minority_decile)
  }
  
  # Recalculate total population
  N = sum(precincts$population)
  
  # Calculate and report final statistics
  overall_per_min = sum(precincts$cvap_min) / sum(precincts$population)
  cat('Overall population:', sum(precincts$population), '\n')
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  cat('Number of precincts:', nrow(precincts), '\n')
  
  
  ##########################################
  ### Step 5: Assign voters to precincts ###
  ##########################################
  
  assign_voters_to_precincts = function(precinct_data) {
    
    # Now we directly use cvap_maj and cvap_min - no rounding needed!
    precinct_data = precinct_data %>%
      mutate(
        num_min = cvap_min,
        num_maj = cvap_maj
      )
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
      
    } else if (voting_prob_assignment == 'ei') {
      
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } else if (voting_prob_assignment == 'contextual') {
      
      # Same as 'ei' but with added linear term for precinct-level minority share so that behavior is correlated with geography
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      per_minority_centered = precinct_data$per_minority - mean(precinct_data$per_minority)
      
      # Apply contextual effects 
      prob_minority_dem_vote_vec = beta[, 1] + b_min_context * per_minority_centered
      prob_majority_dem_vote_vec = beta[, 2] + b_maj_context * per_minority_centered
      
      # Ensure probabilities stay in [0,1]
      prob_minority_dem_vote_vec = pmax(0, pmin(1, prob_minority_dem_vote_vec))
      prob_majority_dem_vote_vec = pmax(0, pmin(1, prob_majority_dem_vote_vec))
      
    }
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  ###############################
  ### Step 6: Generate voters ###
  ###############################
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      n_minority = sum(race == 1),
      n_majority = sum(race == 0),
      dem_votes = sum(vote == 1),
      rep_votes = sum(vote == 0),
      dem_votes_minority = sum(vote[race == 1] == 1), 
      rep_votes_minority = sum(vote[race == 1] == 0), 
      dem_votes_majority = sum(vote[race == 0] == 1), 
      rep_votes_majority = sum(vote[race == 0] == 0),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Function returns:
  # Precinct-level data
  # Individual-level data
  # List of parameters used to seed the simulation for a state
  return(list(precinct_data = precincts, 
              voter_data = electorate, 
              seed_parameters = list(target_population = target_population, 
                                     state_type = state_type,
                                     prob_minority_dem_vote = prob_minority_dem_vote,
                                     prob_majority_dem_vote = prob_majority_dem_vote,
                                     voting_prob_assignment = voting_prob_assignment,
                                     sd_minority = sd_minority,
                                     sd_majority = sd_majority,
                                     rho = rho,
                                     b_min_context = b_min_context,
                                     b_maj_context = b_maj_context)))
  
}


sim_state = simulate_voters(
  target_population = TARGET_POPULATION,
  state_type = STATE_TYPE,
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = PROB_MINORITY_DEM_VOTE,
  prob_majority_dem_vote = PROB_MAJORITY_DEM_VOTE,
  sd_minority = SD_MINORITY,
  sd_majority = SD_MAJORITY,
  voting_prob_assignment = VOTING_PROB_ASSIGNMENT,
  rho = RHO,
  b_min_context = B_MIN_CONTEXT,
  b_maj_context = B_MAJ_CONTEXT
  # context_threshold = CONTEXT_THRESHOLD,
  # prob_shock_pos = PROB_SHOCK_POS,
  # shock_positive = PROB_SHOCK_NEG,
  # prob_shock_neg = SHOCK_NEGATIVE,
  # shock_negative = SHOCK_POSITIVE,
  # sd_ind_noise = SD_IND_NOISE,
  # sd_precinct_minority = SD_PRECINCT_MINORITY,
  # sd_precinct_majority = SD_PRECINCT_MAJORITY
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) +
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data", x = "Percentage Minority", y = "Democratic Voteshare")



simulate_voters = function(target_population = 1000000, 
                           state_type = 'medium',
                           seed_distributions = emp_pop_min_joint_dists,
                           prob_minority_dem_vote = 0.84,
                           prob_majority_dem_vote = 0.43,
                           sd_minority = 0.1,
                           sd_majority = 0.3,
                           voting_prob_assignment = 'hier',
                           rho = 0.2,
                           b_min_context = 0.1,
                           b_maj_context = 0.2
) {
  
  
  #########################################
  ### Step 1: Check arguments are valid ###
  #########################################
  
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei', 'contextual'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  
  ##################################################################################
  ### Step 2: Define target minority percentage and beta distribution parameters ###
  ##################################################################################
  
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('nc') # c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  # Select seed states for the given state_type
  selected_seed_states = seed_states[[state_type]]
  
  # Combine the precinct data from selected seed states
  census_blocks = data.frame(
    cvap_maj = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_maj)),
    cvap_min = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_min))
  )
  
  # Calculate derived values
  census_blocks = census_blocks %>%
    mutate(
      population = cvap_maj + cvap_min,  # Total population is sum of maj and min
      per_minority = cvap_min / population
    )
  
  # Remove precincts with missing or invalid data
  census_blocks = census_blocks %>%
    filter(!is.na(cvap_maj), !is.na(cvap_min), 
           cvap_maj >= 0, cvap_min >= 0, 
           population > 0)
  
  # Aggregate census blocks into precinct-sized units by grouping similar blocks
  # This preserves segregation patterns and extreme values
  
  # Target ~4200 voters per precinct for NC (11M voters / 2600 precincts)
  target_voters_per_precinct = target_population / 2600
  
  # Sort blocks by minority percentage to group similar blocks together
  census_blocks = census_blocks %>%
    arrange(per_minority, population) %>%
    mutate(
      # Create groups of similar blocks
      cumsum_pop = cumsum(population),
      precinct_group = ceiling(cumsum_pop / target_voters_per_precinct)
    ) %>%
    group_by(precinct_group) %>%
    summarise(
      cvap_maj = sum(cvap_maj),
      cvap_min = sum(cvap_min),
      population = sum(population),
      per_minority = sum(cvap_min) / sum(population),
      n_blocks_aggregated = n(),
      .groups = 'drop'
    ) %>%
    select(-precinct_group)
  
  # Add some controlled mixing to avoid perfect sorting while preserving extremes
  # Split into homogeneous (0-5% or 95-100%) and mixed (5-95%) groups
  homogeneous_blocks = census_blocks %>%
    filter(per_minority <= 0.05 | per_minority >= 0.95)
  
  mixed_blocks = census_blocks %>%
    filter(per_minority > 0.05 & per_minority < 0.95) %>%
    # Add small random noise to create some variation in the middle
    mutate(
      sort_key = per_minority + runif(n(), -0.05, 0.05),
      sort_key = pmax(0.05, pmin(0.95, sort_key))  # Keep within bounds
    ) %>%
    arrange(sort_key) %>%
    select(-sort_key)
  
  # Recombine preserving the extremes
  census_blocks = bind_rows(homogeneous_blocks, mixed_blocks) %>%
    arrange(per_minority)
  
  
  ###################################################################
  ### Step 3: Preserve natural distribution with minimal adjustment ###
  ###################################################################
  
  # Set target mean minority percentage
  if (state_type == 'low') {
    target_per_min = 0.05   # 5%
  } else if (state_type == 'medium') {
    target_per_min = 0.175  # 17.5%
  } else if (state_type == 'high') {
    target_per_min = 0.325  # 32.5%
  }
  
  # Calculate natural distribution by deciles
  census_blocks = census_blocks %>%
    mutate(minority_decile = ntile(per_minority, 10))
  
  natural_dist = census_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(census_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  
  ##################################################################
  ### Step 4: Sample precincts maintaining natural distribution ###
  ##################################################################
  
  # Initialize precincts data frame
  precincts = data.frame()
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = census_blocks[census_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$cvap_min) / sum(precincts$population)
  
  # Remove the minority_decile column if it exists
  if ("minority_decile" %in% names(precincts)) {
    precincts = precincts %>% select(-minority_decile)
  }
  
  # Recalculate total population
  N = sum(precincts$population)
  
  # Calculate and report final statistics
  overall_per_min = sum(precincts$cvap_min) / sum(precincts$population)
  cat('Overall population:', sum(precincts$population), '\n')
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  cat('Number of precincts:', nrow(precincts), '\n')
  
  
  ##########################################
  ### Step 5: Assign voters to precincts ###
  ##########################################
  
  assign_voters_to_precincts = function(precinct_data) {
    
    # Now we directly use cvap_maj and cvap_min - no rounding needed!
    precinct_data = precinct_data %>%
      mutate(
        num_min = cvap_min,
        num_maj = cvap_maj
      )
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
      
    } else if (voting_prob_assignment == 'ei') {
      
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } else if (voting_prob_assignment == 'contextual') {
      
      # Same as 'ei' but with added linear term for precinct-level minority share so that behavior is correlated with geography
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      per_minority_centered = precinct_data$per_minority - mean(precinct_data$per_minority)
      
      # Apply contextual effects 
      prob_minority_dem_vote_vec = beta[, 1] + b_min_context * per_minority_centered
      prob_majority_dem_vote_vec = beta[, 2] + b_maj_context * per_minority_centered
      
      # Ensure probabilities stay in [0,1]
      prob_minority_dem_vote_vec = pmax(0, pmin(1, prob_minority_dem_vote_vec))
      prob_majority_dem_vote_vec = pmax(0, pmin(1, prob_majority_dem_vote_vec))
      
    }
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  ###############################
  ### Step 6: Generate voters ###
  ###############################
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      n_minority = sum(race == 1),
      n_majority = sum(race == 0),
      dem_votes = sum(vote == 1),
      rep_votes = sum(vote == 0),
      dem_votes_minority = sum(vote[race == 1] == 1), 
      rep_votes_minority = sum(vote[race == 1] == 0), 
      dem_votes_majority = sum(vote[race == 0] == 1), 
      rep_votes_majority = sum(vote[race == 0] == 0),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Function returns:
  # Precinct-level data
  # Individual-level data
  # List of parameters used to seed the simulation for a state
  return(list(precinct_data = precincts, 
              voter_data = electorate, 
              seed_parameters = list(target_population = target_population, 
                                     state_type = state_type,
                                     prob_minority_dem_vote = prob_minority_dem_vote,
                                     prob_majority_dem_vote = prob_majority_dem_vote,
                                     voting_prob_assignment = voting_prob_assignment,
                                     sd_minority = sd_minority,
                                     sd_majority = sd_majority,
                                     rho = rho,
                                     b_min_context = b_min_context,
                                     b_maj_context = b_maj_context)))
  
}



sim_state = simulate_voters(
  target_population = TARGET_POPULATION,
  state_type = STATE_TYPE,
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = PROB_MINORITY_DEM_VOTE,
  prob_majority_dem_vote = PROB_MAJORITY_DEM_VOTE,
  sd_minority = SD_MINORITY,
  sd_majority = SD_MAJORITY,
  voting_prob_assignment = VOTING_PROB_ASSIGNMENT,
  rho = RHO,
  b_min_context = B_MIN_CONTEXT,
  b_maj_context = B_MAJ_CONTEXT
  # context_threshold = CONTEXT_THRESHOLD,
  # prob_shock_pos = PROB_SHOCK_POS,
  # shock_positive = PROB_SHOCK_NEG,
  # prob_shock_neg = SHOCK_NEGATIVE,
  # shock_negative = SHOCK_POSITIVE,
  # sd_ind_noise = SD_IND_NOISE,
  # sd_precinct_minority = SD_PRECINCT_MINORITY,
  # sd_precinct_majority = SD_PRECINCT_MAJORITY
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) +
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data", x = "Percentage Minority", y = "Democratic Voteshare")










simulate_voters = function(target_population = 1000000, 
                           state_type = 'medium',
                           seed_distributions = emp_pop_min_joint_dists,
                           prob_minority_dem_vote = 0.84,
                           prob_majority_dem_vote = 0.43,
                           sd_minority = 0.1,
                           sd_majority = 0.3,
                           voting_prob_assignment = 'hier',
                           rho = 0.2,
                           b_min_context = 0.1,
                           b_maj_context = 0.2
) {
  
  
  #########################################
  ### Step 1: Check arguments are valid ###
  #########################################
  
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei', 'contextual'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  
  ##################################################################################
  ### Step 2: Define target minority percentage and beta distribution parameters ###
  ##################################################################################
  
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('nc') # c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  # Select seed states for the given state_type
  selected_seed_states = seed_states[[state_type]]
  
  # Combine the precinct data from selected seed states
  census_blocks = data.frame(
    cvap_maj = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_maj)),
    cvap_min = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_min))
  )
  
  # Calculate derived values
  census_blocks = census_blocks %>%
    mutate(
      population = cvap_maj + cvap_min,  # Total population is sum of maj and min
      per_minority = cvap_min / population
    )
  
  # Remove precincts with missing or invalid data
  census_blocks = census_blocks %>%
    filter(!is.na(cvap_maj), !is.na(cvap_min), 
           cvap_maj >= 0, cvap_min >= 0, 
           population > 0)
  
  # Aggregate census blocks into precinct-sized units by grouping similar blocks
  # This preserves segregation patterns and extreme values
  
  # First, sort blocks by minority percentage to keep similar blocks together
  census_blocks = census_blocks %>%
    arrange(per_minority, population) %>%
    mutate(block_idx = row_number())
  
  # Create precincts with variable sizes
  # Rural areas (larger blocks) -> smaller precincts
  # Urban areas (smaller blocks) -> larger precincts
  avg_block_size = mean(census_blocks$population)
  
  precincts = data.frame()
  current_idx = 1
  
  while(current_idx <= nrow(census_blocks)) {
    # Determine target size based on block characteristics
    current_block_size = census_blocks$population[current_idx]
    
    # Variable target sizes: urban (small blocks) get larger precincts
    if(current_block_size < 50) {
      # Very small blocks (dense urban) - target 3000-6000 voters
      target_size = runif(1, 3000, 6000)
    } else if(current_block_size < 200) {
      # Small blocks (urban) - target 2000-4000 voters
      target_size = runif(1, 2000, 4000)
    } else if(current_block_size < 500) {
      # Medium blocks (suburban) - target 1500-3000 voters
      target_size = runif(1, 1500, 3000)
    } else {
      # Large blocks (rural) - target 800-2000 voters
      target_size = runif(1, 800, 2000)
    }
    
    # Aggregate blocks until we reach target size
    cumsum_pop = 0
    end_idx = current_idx
    
    while(cumsum_pop < target_size && end_idx <= nrow(census_blocks)) {
      cumsum_pop = cumsum_pop + census_blocks$population[end_idx]
      end_idx = end_idx + 1
    }
    
    # Don't create tiny precincts at the end
    if(end_idx > nrow(census_blocks) && cumsum_pop < 500) {
      # Add to previous precinct if too small
      if(nrow(precincts) > 0) {
        precincts$cvap_maj[nrow(precincts)] = precincts$cvap_maj[nrow(precincts)] + 
          sum(census_blocks$cvap_maj[current_idx:(end_idx-1)])
        precincts$cvap_min[nrow(precincts)] = precincts$cvap_min[nrow(precincts)] + 
          sum(census_blocks$cvap_min[current_idx:(end_idx-1)])
        precincts$population[nrow(precincts)] = precincts$cvap_maj[nrow(precincts)] + 
          precincts$cvap_min[nrow(precincts)]
        precincts$per_minority[nrow(precincts)] = precincts$cvap_min[nrow(precincts)] / 
          precincts$population[nrow(precincts)]
      }
    } else {
      # Create new precinct
      new_precinct = data.frame(
        cvap_maj = sum(census_blocks$cvap_maj[current_idx:(end_idx-1)]),
        cvap_min = sum(census_blocks$cvap_min[current_idx:(end_idx-1)]),
        n_blocks_aggregated = end_idx - current_idx
      )
      new_precinct$population = new_precinct$cvap_maj + new_precinct$cvap_min
      new_precinct$per_minority = new_precinct$cvap_min / new_precinct$population
      
      precincts = rbind(precincts, new_precinct)
    }
    
    current_idx = end_idx
  }
  
  # Add slight mixing to avoid perfect sorting while preserving extremes
  # Identify homogeneous precincts to preserve
  homogeneous_mask = precincts$per_minority <= 0.05 | precincts$per_minority >= 0.95
  
  # Shuffle only the mixed precincts slightly
  n_precincts = nrow(precincts)
  if(sum(!homogeneous_mask) > 0) {
    mixed_indices = which(!homogeneous_mask)
    # Small random swaps among nearby mixed precincts
    for(i in seq_along(mixed_indices)) {
      if(runif(1) < 0.3 && i < length(mixed_indices)) {
        # 30% chance to swap with next similar precinct
        swap_range = min(5, length(mixed_indices) - i)
        swap_with = i + sample(1:swap_range, 1)
        temp = mixed_indices[i]
        mixed_indices[i] = mixed_indices[swap_with]
        mixed_indices[swap_with] = temp
      }
    }
  }
  
  census_blocks = precincts
  
  
  ###################################################################
  ### Step 3: Preserve natural distribution with minimal adjustment ###
  ###################################################################
  
  # Set target mean minority percentage
  if (state_type == 'low') {
    target_per_min = 0.05   # 5%
  } else if (state_type == 'medium') {
    target_per_min = 0.175  # 17.5%
  } else if (state_type == 'high') {
    target_per_min = 0.325  # 32.5%
  }
  
  # Calculate natural distribution by deciles
  census_blocks = census_blocks %>%
    mutate(minority_decile = ntile(per_minority, 10))
  
  natural_dist = census_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(census_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  
  ##################################################################
  ### Step 4: Sample precincts maintaining natural distribution ###
  ##################################################################
  
  # Initialize precincts data frame
  precincts = data.frame()
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = census_blocks[census_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$cvap_min) / sum(precincts$population)
  
  # Remove the minority_decile column if it exists
  if ("minority_decile" %in% names(precincts)) {
    precincts = precincts %>% select(-minority_decile)
  }
  
  # Recalculate total population
  N = sum(precincts$population)
  
  # Calculate and report final statistics
  overall_per_min = sum(precincts$cvap_min) / sum(precincts$population)
  cat('Overall population:', sum(precincts$population), '\n')
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  cat('Number of precincts:', nrow(precincts), '\n')
  
  
  ##########################################
  ### Step 5: Assign voters to precincts ###
  ##########################################
  
  assign_voters_to_precincts = function(precinct_data) {
    
    # Now we directly use cvap_maj and cvap_min - no rounding needed!
    precinct_data = precinct_data %>%
      mutate(
        num_min = cvap_min,
        num_maj = cvap_maj
      )
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
      
    } else if (voting_prob_assignment == 'ei') {
      
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } else if (voting_prob_assignment == 'contextual') {
      
      # Same as 'ei' but with added linear term for precinct-level minority share so that behavior is correlated with geography
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      per_minority_centered = precinct_data$per_minority - mean(precinct_data$per_minority)
      
      # Apply contextual effects 
      prob_minority_dem_vote_vec = beta[, 1] + b_min_context * per_minority_centered
      prob_majority_dem_vote_vec = beta[, 2] + b_maj_context * per_minority_centered
      
      # Ensure probabilities stay in [0,1]
      prob_minority_dem_vote_vec = pmax(0, pmin(1, prob_minority_dem_vote_vec))
      prob_majority_dem_vote_vec = pmax(0, pmin(1, prob_majority_dem_vote_vec))
      
    }
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  ###############################
  ### Step 6: Generate voters ###
  ###############################
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      n_minority = sum(race == 1),
      n_majority = sum(race == 0),
      dem_votes = sum(vote == 1),
      rep_votes = sum(vote == 0),
      dem_votes_minority = sum(vote[race == 1] == 1), 
      rep_votes_minority = sum(vote[race == 1] == 0), 
      dem_votes_majority = sum(vote[race == 0] == 1), 
      rep_votes_majority = sum(vote[race == 0] == 0),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Function returns:
  # Precinct-level data
  # Individual-level data
  # List of parameters used to seed the simulation for a state
  return(list(precinct_data = precincts, 
              voter_data = electorate, 
              seed_parameters = list(target_population = target_population, 
                                     state_type = state_type,
                                     prob_minority_dem_vote = prob_minority_dem_vote,
                                     prob_majority_dem_vote = prob_majority_dem_vote,
                                     voting_prob_assignment = voting_prob_assignment,
                                     sd_minority = sd_minority,
                                     sd_majority = sd_majority,
                                     rho = rho,
                                     b_min_context = b_min_context,
                                     b_maj_context = b_maj_context)))
  
}



sim_state = simulate_voters(
  target_population = TARGET_POPULATION,
  state_type = STATE_TYPE,
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = PROB_MINORITY_DEM_VOTE,
  prob_majority_dem_vote = PROB_MAJORITY_DEM_VOTE,
  sd_minority = SD_MINORITY,
  sd_majority = SD_MAJORITY,
  voting_prob_assignment = VOTING_PROB_ASSIGNMENT,
  rho = RHO,
  b_min_context = B_MIN_CONTEXT,
  b_maj_context = B_MAJ_CONTEXT
  # context_threshold = CONTEXT_THRESHOLD,
  # prob_shock_pos = PROB_SHOCK_POS,
  # shock_positive = PROB_SHOCK_NEG,
  # prob_shock_neg = SHOCK_NEGATIVE,
  # shock_negative = SHOCK_POSITIVE,
  # sd_ind_noise = SD_IND_NOISE,
  # sd_precinct_minority = SD_PRECINCT_MINORITY,
  # sd_precinct_majority = SD_PRECINCT_MAJORITY
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) +
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data", x = "Percentage Minority", y = "Democratic Voteshare")



simulate_voters = function(target_population = 1000000, 
                           state_type = 'medium',
                           seed_distributions = emp_pop_min_joint_dists,
                           prob_minority_dem_vote = 0.84,
                           prob_majority_dem_vote = 0.43,
                           sd_minority = 0.1,
                           sd_majority = 0.3,
                           voting_prob_assignment = 'hier',
                           rho = 0.2,
                           b_min_context = 0.1,
                           b_maj_context = 0.2
) {
  
  
  #########################################
  ### Step 1: Check arguments are valid ###
  #########################################
  
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei', 'contextual'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  
  ##################################################################################
  ### Step 2: Define target minority percentage and beta distribution parameters ###
  ##################################################################################
  
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('nc') # c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
  )
  
  # Select seed states for the given state_type
  selected_seed_states = seed_states[[state_type]]
  
  # Combine the precinct data from selected seed states
  census_blocks = data.frame(
    cvap_maj = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_maj)),
    cvap_min = unlist(lapply(seed_distributions[selected_seed_states], function(df) df$cvap_min))
  )
  
  # Calculate derived values
  census_blocks = census_blocks %>%
    mutate(
      population = cvap_maj + cvap_min,  # Total population is sum of maj and min
      per_minority = cvap_min / population
    )
  
  # Remove precincts with missing or invalid data
  census_blocks = census_blocks %>%
    filter(!is.na(cvap_maj), !is.na(cvap_min), 
           cvap_maj >= 0, cvap_min >= 0, 
           population > 0)
  
  # Aggregate census blocks into precinct-sized units by grouping similar blocks
  # This preserves segregation patterns and extreme values
  
  # First, sort blocks by minority percentage to keep similar blocks together
  census_blocks = census_blocks %>%
    arrange(per_minority, population) %>%
    mutate(block_idx = row_number())
  
  # Create precincts with variable sizes
  # Urban areas (larger population blocks) -> larger precincts
  # Rural areas (smaller population blocks) -> smaller precincts
  avg_block_size = mean(census_blocks$population)
  
  precincts = data.frame()
  current_idx = 1
  
  while(current_idx <= nrow(census_blocks)) {
    # Determine target size based on block characteristics
    current_block_size = census_blocks$population[current_idx]
    
    # Variable target sizes: urban (high population blocks) get larger precincts
    if(current_block_size > 500) {
      # Very dense blocks (urban core) - target 4000-6000 voters
      target_size = runif(1, 4000, 6000)
    } else if(current_block_size > 200) {
      # Dense blocks (urban) - target 3000-5000 voters
      target_size = runif(1, 3000, 5000)
    } else if(current_block_size > 50) {
      # Medium density (suburban) - target 2000-3500 voters
      target_size = runif(1, 2000, 3500)
    } else {
      # Low density (rural) - target 800-2000 voters
      target_size = runif(1, 800, 2000)
    }
    
    # Aggregate blocks until we reach target size
    cumsum_pop = 0
    end_idx = current_idx
    
    while(cumsum_pop < target_size && end_idx <= nrow(census_blocks)) {
      cumsum_pop = cumsum_pop + census_blocks$population[end_idx]
      end_idx = end_idx + 1
    }
    
    # Don't create tiny precincts at the end
    if(end_idx > nrow(census_blocks) && cumsum_pop < 500) {
      # Add to previous precinct if too small
      if(nrow(precincts) > 0) {
        precincts$cvap_maj[nrow(precincts)] = precincts$cvap_maj[nrow(precincts)] + 
          sum(census_blocks$cvap_maj[current_idx:(end_idx-1)])
        precincts$cvap_min[nrow(precincts)] = precincts$cvap_min[nrow(precincts)] + 
          sum(census_blocks$cvap_min[current_idx:(end_idx-1)])
        precincts$population[nrow(precincts)] = precincts$cvap_maj[nrow(precincts)] + 
          precincts$cvap_min[nrow(precincts)]
        precincts$per_minority[nrow(precincts)] = precincts$cvap_min[nrow(precincts)] / 
          precincts$population[nrow(precincts)]
      }
    } else {
      # Create new precinct
      new_precinct = data.frame(
        cvap_maj = sum(census_blocks$cvap_maj[current_idx:(end_idx-1)]),
        cvap_min = sum(census_blocks$cvap_min[current_idx:(end_idx-1)]),
        n_blocks_aggregated = end_idx - current_idx
      )
      new_precinct$population = new_precinct$cvap_maj + new_precinct$cvap_min
      new_precinct$per_minority = new_precinct$cvap_min / new_precinct$population
      
      precincts = rbind(precincts, new_precinct)
    }
    
    current_idx = end_idx
  }
  
  # Add slight mixing to avoid perfect sorting while preserving extremes
  # Identify homogeneous precincts to preserve
  homogeneous_mask = precincts$per_minority <= 0.05 | precincts$per_minority >= 0.95
  
  # Shuffle only the mixed precincts slightly
  n_precincts = nrow(precincts)
  if(sum(!homogeneous_mask) > 0) {
    mixed_indices = which(!homogeneous_mask)
    # Small random swaps among nearby mixed precincts
    for(i in seq_along(mixed_indices)) {
      if(runif(1) < 0.3 && i < length(mixed_indices)) {
        # 30% chance to swap with next similar precinct
        swap_range = min(5, length(mixed_indices) - i)
        swap_with = i + sample(1:swap_range, 1)
        temp = mixed_indices[i]
        mixed_indices[i] = mixed_indices[swap_with]
        mixed_indices[swap_with] = temp
      }
    }
  }
  
  census_blocks = precincts
  
  
  ###################################################################
  ### Step 3: Preserve natural distribution with minimal adjustment ###
  ###################################################################
  
  # Set target mean minority percentage
  if (state_type == 'low') {
    target_per_min = 0.05   # 5%
  } else if (state_type == 'medium') {
    target_per_min = 0.175  # 17.5%
  } else if (state_type == 'high') {
    target_per_min = 0.325  # 32.5%
  }
  
  # Calculate natural distribution by deciles
  census_blocks = census_blocks %>%
    mutate(minority_decile = ntile(per_minority, 10))
  
  natural_dist = census_blocks %>%
    group_by(minority_decile) %>%
    summarise(
      prop = n() / nrow(census_blocks),
      avg_minority = mean(per_minority),
      .groups = 'drop'
    )
  
  
  ##################################################################
  ### Step 4: Sample precincts maintaining natural distribution ###
  ##################################################################
  
  # Initialize precincts data frame
  precincts = data.frame()
  
  # Sample maintaining natural proportions
  precincts = data.frame()
  n_target = ceiling(target_population / mean(census_blocks$population))
  
  for(decile in 1:10) {
    n_from_decile = round(n_target * natural_dist$prop[decile])
    decile_blocks = census_blocks[census_blocks$minority_decile == decile, ]
    
    if(nrow(decile_blocks) > 0 && n_from_decile > 0) {
      sampled_indices = sample(nrow(decile_blocks), n_from_decile, replace = TRUE)
      sampled = decile_blocks[sampled_indices, ]
      precincts = bind_rows(precincts, sampled)
    }
  }
  
  precincts$precinct_id = 1:nrow(precincts)
  
  # Minimal adjustment to hit target
  current_minority = sum(precincts$cvap_min) / sum(precincts$population)
  
  # Remove the minority_decile column if it exists
  if ("minority_decile" %in% names(precincts)) {
    precincts = precincts %>% select(-minority_decile)
  }
  
  # Recalculate total population
  N = sum(precincts$population)
  
  # Calculate and report final statistics
  overall_per_min = sum(precincts$cvap_min) / sum(precincts$population)
  cat('Overall population:', sum(precincts$population), '\n')
  cat('Overall percent minority:', round(overall_per_min * 100, 2), '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  cat('Number of precincts:', nrow(precincts), '\n')
  
  
  ##########################################
  ### Step 5: Assign voters to precincts ###
  ##########################################
  
  assign_voters_to_precincts = function(precinct_data) {
    
    # Now we directly use cvap_maj and cvap_min - no rounding needed!
    precinct_data = precinct_data %>%
      mutate(
        num_min = cvap_min,
        num_maj = cvap_maj
      )
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(mapply(function(min_count, maj_count) {
      c(rep(1, min_count), rep(0, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, SIMPLIFY = FALSE))
    
    if (voting_prob_assignment == 'hier') {
      
      prob_minority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_minority_dem_vote, sd = sd_minority, a = 0, b = 1)
      prob_majority_dem_vote_vec = rtruncnorm(nrow(precinct_data), mean = prob_majority_dem_vote, sd = sd_majority, a = 0, b = 1)
      
    } else if (voting_prob_assignment == 'ei') {
      
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } else if (voting_prob_assignment == 'contextual') {
      
      # Same as 'ei' but with added linear term for precinct-level minority share so that behavior is correlated with geography
      Sigma = matrix(c(
        sd_minority^2,                       # Var(beta_b)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
        sd_majority^2                        # Var(beta_w)
      ), nrow = 2)
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(n = nrow(precinct_data),
                      mean = c(prob_minority_dem_vote, prob_majority_dem_vote), 
                      sigma = Sigma, 
                      upper = c(1, 1), 
                      lower = c(0, 0))
      
      per_minority_centered = precinct_data$per_minority - mean(precinct_data$per_minority)
      
      # Apply contextual effects 
      prob_minority_dem_vote_vec = beta[, 1] + b_min_context * per_minority_centered
      prob_majority_dem_vote_vec = beta[, 2] + b_maj_context * per_minority_centered
      
      # Ensure probabilities stay in [0,1]
      prob_minority_dem_vote_vec = pmax(0, pmin(1, prob_minority_dem_vote_vec))
      prob_majority_dem_vote_vec = pmax(0, pmin(1, prob_majority_dem_vote_vec))
      
    }
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(mapply(function(min_count, maj_count, p_min, p_maj) {
      c(rep(p_min, min_count), rep(p_maj, maj_count))
    }, precinct_data$num_min, precinct_data$num_maj, prob_minority_dem_vote_vec, prob_majority_dem_vote_vec, SIMPLIFY = FALSE))
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  ###############################
  ### Step 6: Generate voters ###
  ###############################
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      n_minority = sum(race == 1),
      n_majority = sum(race == 0),
      dem_votes = sum(vote == 1),
      rep_votes = sum(vote == 0),
      dem_votes_minority = sum(vote[race == 1] == 1), 
      rep_votes_minority = sum(vote[race == 1] == 0), 
      dem_votes_majority = sum(vote[race == 0] == 1), 
      rep_votes_majority = sum(vote[race == 0] == 0),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Function returns:
  # Precinct-level data
  # Individual-level data
  # List of parameters used to seed the simulation for a state
  return(list(precinct_data = precincts, 
              voter_data = electorate, 
              seed_parameters = list(target_population = target_population, 
                                     state_type = state_type,
                                     prob_minority_dem_vote = prob_minority_dem_vote,
                                     prob_majority_dem_vote = prob_majority_dem_vote,
                                     voting_prob_assignment = voting_prob_assignment,
                                     sd_minority = sd_minority,
                                     sd_majority = sd_majority,
                                     rho = rho,
                                     b_min_context = b_min_context,
                                     b_maj_context = b_maj_context)))
  
}

sim_state = simulate_voters(
  target_population = TARGET_POPULATION,
  state_type = STATE_TYPE,
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = PROB_MINORITY_DEM_VOTE,
  prob_majority_dem_vote = PROB_MAJORITY_DEM_VOTE,
  sd_minority = SD_MINORITY,
  sd_majority = SD_MAJORITY,
  voting_prob_assignment = VOTING_PROB_ASSIGNMENT,
  rho = RHO,
  b_min_context = B_MIN_CONTEXT,
  b_maj_context = B_MAJ_CONTEXT
  # context_threshold = CONTEXT_THRESHOLD,
  # prob_shock_pos = PROB_SHOCK_POS,
  # shock_positive = PROB_SHOCK_NEG,
  # prob_shock_neg = SHOCK_NEGATIVE,
  # shock_negative = SHOCK_POSITIVE,
  # sd_ind_noise = SD_IND_NOISE,
  # sd_precinct_minority = SD_PRECINCT_MINORITY,
  # sd_precinct_majority = SD_PRECINCT_MAJORITY
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) +
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data", x = "Percentage Minority", y = "Democratic Voteshare")









simulate_voters_with_density = function(target_population = 1000000,
                                        state_type = 'medium',
                                        seed_distributions = emp_pop_min_joint_dists,
                                        prob_minority_dem_vote = 0.84,
                                        prob_majority_dem_vote = 0.43,
                                        sd_minority = 0.1,
                                        sd_majority = 0.3,
                                        voting_prob_assignment = 'ei',
                                        rho = 0.2,
                                        b_min_context = 0.1,
                                        b_maj_context = 0.1,
                                        target_precincts = 2600,
                                        # Density effects
                                        urban_dem_boost = 0.15,
                                        rural_rep_boost = 0.10,
                                        rural_precinct_pct = 0.55,      # 55% of precincts are rural
                                        suburban_precinct_pct = 0.30,   # 30% suburban
                                        urban_precinct_pct = 0.12,      # 12% urban
                                        urban_core_precinct_pct = 0.03) { # 3% urban_core
  
  # Steps 1-3: Same validation and setup as before
  if (!(state_type %in% c('low', 'medium', 'high'))) {
    stop('Invalid argument for state_type')
  }
  
  if (!(voting_prob_assignment %in% c('hier', 'ei', 'contextual'))) {
    stop('Invalid argument for voting_prob_assignment')
  }
  
  # Select seed states
  seed_states = list(
    low = c('mt', 'ia', 'me', 'vt', 'wy'),
    medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
    high = c('nc')
  )
  
  selected_seed_states = seed_states[[state_type]]
  
  # Combine census blocks from selected states
  census_blocks = data.frame(cvap_maj = unlist(lapply(seed_distributions[selected_seed_states], function(df)
    df$cvap_maj)),
    cvap_min = unlist(lapply(seed_distributions[selected_seed_states], function(df)
      df$cvap_min))) %>%
    mutate(population = cvap_maj + cvap_min,
           per_minority = cvap_min / population) %>%
    filter(!is.na(cvap_maj),
           !is.na(cvap_min),
           cvap_maj >= 0,
           cvap_min >= 0,
           population > 0)
  
  # NEW: Create density-based precinct aggregation
  # Urban areas have high-population blocks, rural areas have low-population blocks
  
  # Step 1: Classify blocks by density (using population as proxy)
  census_blocks = census_blocks %>%
    mutate(
      # Use population percentiles to classify density
      pop_percentile = percent_rank(population),
      density_class = case_when(
        pop_percentile > 0.95 ~ "urban_core",  # Top 5% - dense urban core
        pop_percentile > 0.80 ~ "urban",       # Next 15% - urban
        pop_percentile > 0.50 ~ "suburban",    # Next 30% - suburban
        TRUE ~ "rural"                         # Bottom 50% - rural
      )
    )
  
  # Step 2: Calculate target precincts for each density class
  target_by_density = list(
    rural = round(target_precincts * rural_precinct_pct),
    suburban = round(target_precincts * suburban_precinct_pct),
    urban = round(target_precincts * urban_precinct_pct),
    urban_core = round(target_precincts * urban_core_precinct_pct)
  )
  
  # Step 3: Create precincts by aggregating blocks to meet targets
  precincts = data.frame()
  precinct_id = 1
  
  for (density in c("rural", "suburban", "urban", "urban_core")) {
    blocks_in_class = census_blocks %>%
      filter(density_class == density)
    
    if (nrow(blocks_in_class) == 0) next
    
    # Calculate how many blocks per precinct based on targets
    n_target_precincts = target_by_density[[density]]
    blocks_per_precinct = max(1, floor(nrow(blocks_in_class) / n_target_precincts))
    
    # Sort blocks by minority percentage to maintain segregation patterns
    blocks_in_class = blocks_in_class %>%
      arrange(desc(per_minority * population))
    
    # Aggregate blocks into precincts
    i = 1
    precincts_created = 0
    
    while (i <= nrow(blocks_in_class) && precincts_created < n_target_precincts) {
      # For the last precinct, include all remaining blocks
      if (precincts_created == n_target_precincts - 1) {
        end_idx = nrow(blocks_in_class)
      } else {
        end_idx = min(i + blocks_per_precinct - 1, nrow(blocks_in_class))
      }
      
      # Aggregate this group of blocks
      precinct_blocks = blocks_in_class[i:end_idx, ]
      
      new_precinct = data.frame(
        precinct_id = precinct_id,
        cvap_maj = sum(precinct_blocks$cvap_maj),
        cvap_min = sum(precinct_blocks$cvap_min),
        population = sum(precinct_blocks$population),
        per_minority = sum(precinct_blocks$cvap_min) / sum(precinct_blocks$population),
        density_class = density,
        n_blocks_aggregated = nrow(precinct_blocks)
      )
      
      precincts = rbind(precincts, new_precinct)
      precinct_id = precinct_id + 1
      precincts_created = precincts_created + 1
      i = end_idx + 1
    }
  }
  
  # Reset precinct IDs
  precincts$precinct_id = 1:nrow(precincts)
  
  # Scale to target population
  current_pop = sum(precincts$population)
  scale_factor = target_population / current_pop
  
  precincts = precincts %>%
    mutate(
      # Scale and round the component populations
      cvap_maj = round(cvap_maj * scale_factor),
      cvap_min = round(cvap_min * scale_factor),
      # Population MUST be the sum of components to avoid mismatches
      population = cvap_maj + cvap_min,
      # Recalculate to avoid rounding issues
      per_minority = cvap_min / population
    )
  
  # Report statistics
  cat('\nPrecinct Statistics:\n')
  cat('Total precincts:', nrow(precincts), '\n')
  cat(
    'Population range:',
    min(precincts$population),
    '-',
    max(precincts$population),
    '\n'
  )
  cat('\nPrecincts by density:\n')
  print(table(precincts$density_class))
  cat('\nAverage population by density:\n')
  print(
    precincts %>%
      group_by(density_class) %>%
      summarise(avg_pop = mean(population), .groups = 'drop')
  )
  
  overall_per_min = sum(precincts$cvap_min) / sum(precincts$population)
  cat('\nOverall population:', sum(precincts$population), '\n')
  cat('Overall percent minority:',
      round(overall_per_min * 100, 2),
      '%\n')
  cat('Minority % SD across precincts:', round(sd(precincts$per_minority), 3), '\n')
  cat('Minority % range:', round(min(precincts$per_minority), 3), '-', round(max(precincts$per_minority), 3), '\n')
  
  assign_voters_to_precincts = function(precinct_data) {
    # Now we directly use cvap_maj and cvap_min - no rounding needed!
    precinct_data = precinct_data %>%
      mutate(num_min = cvap_min, num_maj = cvap_maj)
    
    # Total number of voters
    N = sum(precinct_data$population)
    
    # Generate voter IDs sequentially
    voter_id = 1:N
    
    # Generate precinct IDs for each voter
    precinct_ids = rep(precinct_data$precinct_id, times = precinct_data$population)
    
    # Assign race to each voter
    races = unlist(
      mapply(
        function(min_count, maj_count) {
          c(rep(1, min_count), rep(0, maj_count))
        },
        precinct_data$num_min,
        precinct_data$num_maj,
        SIMPLIFY = FALSE
      )
    )
    
    if (voting_prob_assignment == 'hier') {
      prob_minority_dem_vote_vec = rtruncnorm(
        nrow(precinct_data),
        mean = prob_minority_dem_vote,
        sd = sd_minority,
        a = 0,
        b = 1
      )
      prob_majority_dem_vote_vec = rtruncnorm(
        nrow(precinct_data),
        mean = prob_majority_dem_vote,
        sd = sd_majority,
        a = 0,
        b = 1
      )
      
    } else if (voting_prob_assignment == 'ei') {
      Sigma = matrix(
        c(
          sd_minority^2,                       # Var(beta_b)
          rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
          rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
          sd_majority^2                        # Var(beta_w)
        ),
        nrow = 2
      )
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(
        n = nrow(precinct_data),
        mean = c(prob_minority_dem_vote, prob_majority_dem_vote),
        sigma = Sigma,
        upper = c(1, 1),
        lower = c(0, 0)
      )
      
      # Extract precinct-specific probabilities
      prob_minority_dem_vote_vec = beta[, 1]
      prob_majority_dem_vote_vec = beta[, 2]
      
    } else if (voting_prob_assignment == 'contextual') {
      # Same as 'ei' but with added linear term for precinct-level minority share so that behavior is correlated with geography
      Sigma = matrix(
        c(
          sd_minority^2,                       # Var(beta_b)
          rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
          rho * sd_minority * sd_majority,     # Cov(beta_b, beta_w)
          sd_majority^2                        # Var(beta_w)
        ),
        nrow = 2
      )
      
      # Generate (beta_i^b, beta_i^w) from truncated bivariate normal
      beta = rtmvnorm(
        n = nrow(precinct_data),
        mean = c(prob_minority_dem_vote, prob_majority_dem_vote),
        sigma = Sigma,
        upper = c(1, 1),
        lower = c(0, 0)
      )
      
      per_minority_centered = precinct_data$per_minority - mean(precinct_data$per_minority)
      
      # Apply contextual effects
      prob_minority_dem_vote_vec = beta[, 1] + b_min_context * per_minority_centered
      prob_majority_dem_vote_vec = beta[, 2] + b_maj_context * per_minority_centered
      
      # Ensure probabilities stay in [0,1]
      prob_minority_dem_vote_vec = pmax(0, pmin(1, prob_minority_dem_vote_vec))
      prob_majority_dem_vote_vec = pmax(0, pmin(1, prob_majority_dem_vote_vec))
      
    }
    
    # # Modify voting probabilities based on density class
    # density_adjustment = case_when(
    #   precinct_data$density_class == "urban_core" ~ urban_dem_boost,
    #   precinct_data$density_class == "urban" ~ urban_dem_boost * 0.7,
    #   precinct_data$density_class == "suburban" ~ 0,
    #   precinct_data$density_class == "rural" ~ -rural_rep_boost
    # )
    #
    # # Apply density adjustments to base probabilities
    # prob_minority_dem_vote_vec = pmax(0, pmin(1, prob_minority_dem_vote_vec + density_adjustment))
    # prob_majority_dem_vote_vec = pmax(0, pmin(1, prob_majority_dem_vote_vec + density_adjustment))
    
    # Create vectors of base probabilities for each voter
    probabilities = unlist(
      mapply(
        function(min_count, maj_count, p_min, p_maj) {
          c(rep(p_min, min_count), rep(p_maj, maj_count))
        },
        precinct_data$num_min,
        precinct_data$num_maj,
        prob_minority_dem_vote_vec,
        prob_majority_dem_vote_vec,
        SIMPLIFY = FALSE
      )
    )
    
    # Generate votes based on individual probabilities
    votes = rbinom(length(probabilities), 1, prob = probabilities)
    
    # Assemble the voter data frame
    electorate = data.frame(
      voter_id = voter_id,
      precinct_id = precinct_ids,
      race = races,
      vote = votes
    )
    
    return(electorate)
  }
  
  electorate = assign_voters_to_precincts(precincts)
  
  # Calculate precinct-level statistics
  precincts = electorate %>%
    group_by(precinct_id) %>%
    summarise(
      population = n(),
      n_minority = sum(race == 1),
      n_majority = sum(race == 0),
      dem_votes = sum(vote == 1),
      rep_votes = sum(vote == 0),
      dem_votes_minority = sum(vote[race == 1] == 1),
      rep_votes_minority = sum(vote[race == 1] == 0),
      dem_votes_majority = sum(vote[race == 0] == 1),
      rep_votes_majority = sum(vote[race == 0] == 0),
      per_minority = sum(race) / population,
      dem_voteshare = sum(vote) / population,
      dem_voteshare_minority = sum(vote[race == 1]) / sum(race == 1),
      dem_voteshare_majority = sum(vote[race == 0]) / sum(race == 0),
      .groups = 'drop'
    ) %>%
    mutate(
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    )
  
  # Return includes density information
  return(list(
    precinct_data = precincts,
    voter_data = electorate,
    seed_parameters = list(
      target_population = target_population,
      state_type = state_type,
      prob_minority_dem_vote = prob_minority_dem_vote,
      prob_majority_dem_vote = prob_majority_dem_vote,
      voting_prob_assignment = voting_prob_assignment,
      sd_minority = sd_minority,
      sd_majority = sd_majority,
      rho = rho,
      b_min_context = b_min_context,
      b_maj_context = b_maj_context
    )
  ))
}

sim_state = simulate_voters_with_density(
  target_population = TARGET_POPULATION,
  state_type = STATE_TYPE,
  seed_distributions = emp_pop_min_joint_dists,
  prob_minority_dem_vote = PROB_MINORITY_DEM_VOTE,
  prob_majority_dem_vote = PROB_MAJORITY_DEM_VOTE,
  sd_minority = SD_MINORITY,
  sd_majority = SD_MAJORITY,
  voting_prob_assignment = VOTING_PROB_ASSIGNMENT,
  rho = RHO,
  b_min_context = B_MIN_CONTEXT,
  b_maj_context = B_MAJ_CONTEXT
)

ggplot(sim_state$precinct_data, aes(per_minority, dem_voteshare)) +
  geom_point() +
  theme_bw() +
  labs(title = "Simulated Voter Data", x = "Percentage Minority", y = "Democratic Voteshare")



random_region_assignment = function(precinct_data,
                                    num_geogs = 5,
                                    agg_bias = 'low',
                                    cell_size = 1000,
                                    output_dir = "Output/",
                                    random_seed = 42,
                                    save_shp = TRUE,
                                    max_attempts = 500) {
  
  # Set initial seed for reproducibility
  set.seed(random_seed)
  
  # Cluster precincts into num_geogs distinct regions based on demographic features
  # This is where aggregation bias is applied - it's already done after this line!
  precinct_data = induce_aggregation_bias_2d(precinct_data, num_regions = num_geogs, agg_bias = agg_bias)
  
  # Set up parameters of grid
  n_precinct = nrow(precinct_data)
  n_rows = floor(sqrt(n_precinct))
  n_cols = ceiling(n_precinct / n_rows)
  
  # Create grid
  grid = expand.grid(col = 1:n_cols, row = 1:n_rows) %>%
    mutate(
      x_grid = (col - 1) * cell_size,
      y_grid = (row - 1) * cell_size,
      grid_id = row_number()
    ) %>%
    filter(grid_id <= n_precinct) %>%
    mutate(
      geometry = st_sfc(
        lapply(1:nrow(.), function(i) {
          st_polygon(list(matrix(c(
            x_grid[i], y_grid[i],
            x_grid[i] + cell_size, y_grid[i],
            x_grid[i] + cell_size, y_grid[i] + cell_size,
            x_grid[i], y_grid[i] + cell_size,
            x_grid[i], y_grid[i]
          ), ncol = 2, byrow = TRUE)))
        }),
        crs = 3857
      )
    ) %>%
    st_sf()
  
  # STEP 1: Partition grid into num_geogs geographic regions using k-means
  grid_centroids = st_coordinates(st_centroid(grid$geometry))
  set.seed(random_seed)
  kmeans_result = kmeans(grid_centroids, centers = num_geogs, nstart = 25)
  grid$geog_id = kmeans_result$cluster
  
  # Optional: Fix isolated cells for better contiguity
  nb = poly2nb(grid, queen = TRUE)
  for(iter in 1:10) {
    changes_made = FALSE
    for(i in 1:nrow(grid)) {
      neighbors = nb[[i]]
      if(length(neighbors) == 0) next
      neighbor_regions = grid$geog_id[neighbors]
      same_region_neighbors = sum(neighbor_regions == grid$geog_id[i])
      if(same_region_neighbors == 0) {
        region_counts = table(neighbor_regions)
        grid$geog_id[i] = as.numeric(names(region_counts)[which.max(region_counts)])
        changes_made = TRUE
      }
    }
    if(!changes_made) break
  }
  
  # STEP 2: Assign precincts from each demographic cluster to corresponding geographic region
  # Count how many cells are in each geographic region
  cells_per_geo_region = table(grid$geog_id)
  
  # For each demographic cluster, assign its precincts to the corresponding geographic region
  grid$precinct_id = NA
  
  for(cluster_id in 1:num_geogs) {
    # Get precincts in this demographic cluster
    cluster_precincts = precinct_data %>%
      filter(geog_id == cluster_id) %>%
      pull(precinct_id)
    
    # Get grid cells in the corresponding geographic region
    region_cells = which(grid$geog_id == cluster_id)
    
    # Assign precincts to cells (as many as we can fit)
    n_to_assign = min(length(cluster_precincts), length(region_cells))
    if(n_to_assign > 0) {
      grid$precinct_id[region_cells[1:n_to_assign]] = cluster_precincts[1:n_to_assign]
    }
    
    # Handle any leftover precincts (if clusters are unbalanced)
    if(length(cluster_precincts) > length(region_cells)) {
      cat("Warning: Cluster", cluster_id, "has more precincts than available cells\n")
    }
  }
  
  # Handle any unassigned cells (shouldn't happen if clusters are balanced)
  unassigned_cells = which(is.na(grid$precinct_id))
  if(length(unassigned_cells) > 0) {
    unassigned_precincts = setdiff(precinct_data$precinct_id, grid$precinct_id)
    n_to_assign = min(length(unassigned_cells), length(unassigned_precincts))
    if(n_to_assign > 0) {
      grid$precinct_id[unassigned_cells[1:n_to_assign]] = unassigned_precincts[1:n_to_assign]
    }
  }
  
  # Join with precinct data
  grid_sf_assigned = grid %>%
    left_join(precinct_data %>% select(-geog_id), by = c("precinct_id" = "precinct_id"))
  
  # Create output
  region_centroids = grid_sf_assigned %>%
    group_by(geog_id) %>%
    summarise(geometry = st_centroid(st_union(geometry)), .groups = 'drop')
  
  map_data = grid_sf_assigned %>%
    select(geog_id, precinct_id, population, 
           n_minority, n_majority,                    
           dem_votes, rep_votes,                      
           dem_votes_minority, rep_votes_minority,    
           dem_votes_majority, rep_votes_majority,    
           per_minority, dem_voteshare,
           dem_voteshare_minority, dem_voteshare_majority) %>%
    mutate(geog_id = as.factor(geog_id)) %>%
    rename(pct_id = precinct_id,
           pop = population,
           n_min = n_minority,        
           n_maj = n_majority,        
           dem_v = dem_votes,         
           rep_v = rep_votes,         
           dem_v_min = dem_votes_minority,    
           rep_v_min = rep_votes_minority,    
           dem_v_maj = dem_votes_majority,    
           rep_v_maj = rep_votes_majority,    
           pct_min = per_minority,
           dem_vsh = dem_voteshare,
           dem_vsh_1 = dem_voteshare_minority,
           dem_vsh_0 = dem_voteshare_majority)
  
  # Save shapefile if requested
  if(save_shp) {
    dir.create(file.path(output_dir, "Shapefiles"), showWarnings = FALSE, recursive = TRUE)
    st_write(map_data,
             paste0(output_dir, 'Shapefiles/state_map.shp'),
             append = FALSE,
             quiet = TRUE)
    cat("Shapefile saved to", paste0(output_dir, 'Shapefiles/state_map.shp'), "\n")
  }
  
  return(list(
    map_data = map_data,
    grid = grid,
    region_centroids = region_centroids
  ))
}






# Modified region assignment that concentrates urban precincts geographically
density_aware_region_assignment = function(precinct_data,
                                           num_geogs = 5,
                                           agg_bias = 'low', 
                                           cell_size = 1000,
                                           output_dir = "Output/",
                                           random_seed = 42,
                                           save_shp = TRUE) {
  
  set.seed(random_seed)
  
  # Apply demographic clustering as before
  precinct_data = induce_aggregation_bias_2d(precinct_data, num_regions = num_geogs, agg_bias = agg_bias)
  
  # Add density classification
  precinct_data = precinct_data %>%
    mutate(
      pop_percentile = percent_rank(population),
      density_class = case_when(
        pop_percentile > 0.9 ~ "urban_core",
        pop_percentile > 0.7 ~ "urban", 
        pop_percentile > 0.4 ~ "suburban",
        TRUE ~ "rural"
      )
    )
  
  # Create grid
  n_precinct = nrow(precinct_data)
  n_rows = floor(sqrt(n_precinct))
  n_cols = ceiling(n_precinct / n_rows)
  
  grid = expand.grid(col = 1:n_cols, row = 1:n_rows) %>%
    mutate(
      x_grid = (col - 1) * cell_size,
      y_grid = (row - 1) * cell_size,
      grid_id = row_number()
    ) %>%
    filter(grid_id <= n_precinct) %>%
    mutate(
      geometry = st_sfc(
        lapply(1:nrow(.), function(i) {
          st_polygon(list(matrix(c(
            x_grid[i], y_grid[i],
            x_grid[i] + cell_size, y_grid[i],
            x_grid[i] + cell_size, y_grid[i] + cell_size,
            x_grid[i], y_grid[i] + cell_size,
            x_grid[i], y_grid[i]
          ), ncol = 2, byrow = TRUE)))
        }),
        crs = 3857
      )
    ) %>%
    st_sf()
  
  # MODIFIED: Create urban centers and rural areas
  # Define 1-2 urban centers
  urban_center_1 = c(col = n_cols * 0.25, row = n_rows * 0.25)  # NW urban center
  urban_center_2 = c(col = n_cols * 0.75, row = n_rows * 0.75)  # SE urban center
  
  # Calculate distance from each grid cell to urban centers
  grid = grid %>%
    mutate(
      dist_to_urban_1 = sqrt((col - urban_center_1["col"])^2 + (row - urban_center_1["row"])^2),
      dist_to_urban_2 = sqrt((col - urban_center_2["col"])^2 + (row - urban_center_2["row"])^2),
      min_dist_to_urban = pmin(dist_to_urban_1, dist_to_urban_2),
      # Cells closer to urban centers are more likely to be assigned urban precincts
      urban_probability = exp(-min_dist_to_urban^2 / (n_cols^2 / 10))
    )
  
  # Assign geographic regions based on modified k-means that considers urban probability
  # Weight coordinates by urban probability to pull urban regions toward urban centers
  weighted_coords = grid %>%
    st_drop_geometry() %>%
    mutate(
      weighted_col = col * (1 + urban_probability * 2),
      weighted_row = row * (1 + urban_probability * 2)
    ) %>%
    select(weighted_col, weighted_row)
  
  set.seed(random_seed)
  kmeans_result = kmeans(weighted_coords, centers = num_geogs, nstart = 25)
  grid$geog_id = kmeans_result$cluster
  
  # Sort regions by their average urban probability
  region_urban_score = grid %>%
    st_drop_geometry() %>%
    group_by(geog_id) %>%
    summarise(avg_urban_prob = mean(urban_probability), .groups = 'drop') %>%
    arrange(desc(avg_urban_prob))
  
  # Assign precincts: urban precincts go to high-urban-score regions
  grid$precinct_id = NA
  
  # Sort precincts by density
  urban_precincts = precinct_data %>%
    filter(density_class %in% c("urban", "urban_core")) %>%
    arrange(desc(population)) %>%
    pull(precinct_id)
  
  rural_precincts = precinct_data %>%
    filter(density_class %in% c("rural", "suburban")) %>%
    arrange(population) %>%
    pull(precinct_id)
  
  # Assign urban precincts to urban regions (top 2 regions by urban score)
  urban_regions = region_urban_score$geog_id[1:2]
  urban_cells = which(grid$geog_id %in% urban_regions)
  
  n_urban_to_assign = min(length(urban_precincts), length(urban_cells))
  if(n_urban_to_assign > 0) {
    # Sort urban cells by distance to urban center
    urban_cells_sorted = urban_cells[order(grid$min_dist_to_urban[urban_cells])]
    grid$precinct_id[urban_cells_sorted[1:n_urban_to_assign]] = urban_precincts[1:n_urban_to_assign]
  }
  
  # Assign rural precincts to remaining cells
  unassigned_cells = which(is.na(grid$precinct_id))
  n_rural_to_assign = min(length(rural_precincts), length(unassigned_cells))
  if(n_rural_to_assign > 0) {
    grid$precinct_id[unassigned_cells[1:n_rural_to_assign]] = rural_precincts[1:n_rural_to_assign]
  }
  
  # Handle any remaining assignments
  remaining_precincts = setdiff(precinct_data$precinct_id, grid$precinct_id[!is.na(grid$precinct_id)])
  remaining_cells = which(is.na(grid$precinct_id))
  
  if(length(remaining_cells) > 0 && length(remaining_precincts) > 0) {
    n_to_assign = min(length(remaining_cells), length(remaining_precincts))
    grid$precinct_id[remaining_cells[1:n_to_assign]] = remaining_precincts[1:n_to_assign]
  }
  
  # Now handle demographic clustering based on aggregation bias
  # (This respects the density-based assignment above)
  if(agg_bias == "high") {
    # Within each geographic region, sort by demographic cluster
    grid = grid %>%
      left_join(precinct_data %>% select(precinct_id, geog_id_demo = geog_id), 
                by = "precinct_id") %>%
      group_by(geog_id) %>%
      arrange(geog_id, geog_id_demo, precinct_id) %>%
      ungroup()
  }
  
  # Join with precinct data
  grid_sf_assigned = grid %>%
    left_join(precinct_data %>% select(-geog_id, -density_class, -pop_percentile), 
              by = "precinct_id")
  
  # Create output
  region_centroids = grid_sf_assigned %>%
    group_by(geog_id) %>%
    summarise(geometry = st_centroid(st_union(geometry)), .groups = 'drop')
  
  map_data = grid_sf_assigned %>%
    select(geog_id, precinct_id, population, 
           n_minority, n_majority,                    
           dem_votes, rep_votes,                      
           dem_votes_minority, rep_votes_minority,    
           dem_votes_majority, rep_votes_majority,    
           per_minority, dem_voteshare,
           dem_voteshare_minority, dem_voteshare_majority) %>%
    mutate(geog_id = as.factor(geog_id)) %>%
    rename(pct_id = precinct_id,
           pop = population,
           n_min = n_minority,        
           n_maj = n_majority,        
           dem_v = dem_votes,         
           rep_v = rep_votes,         
           dem_v_min = dem_votes_minority,    
           rep_v_min = rep_votes_minority,    
           dem_v_maj = dem_votes_majority,    
           rep_v_maj = rep_votes_majority,    
           pct_min = per_minority,
           dem_vsh = dem_voteshare,
           dem_vsh_1 = dem_voteshare_minority,
           dem_vsh_0 = dem_voteshare_majority)
  
  if(save_shp) {
    dir.create(file.path(output_dir, "Shapefiles"), showWarnings = FALSE, recursive = TRUE)
    st_write(map_data,
             paste0(output_dir, 'Shapefiles/state_map.shp'),
             append = FALSE,
             quiet = TRUE)
    cat("Shapefile saved to", paste0(output_dir, 'Shapefiles/state_map.shp'), "\n")
  }
  
  return(list(
    map_data = map_data,
    grid = grid,
    region_centroids = region_centroids
  ))
}
