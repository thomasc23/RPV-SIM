# Vote modeling functions for rpvsimulator package
# Uses canonical column names from constants.R

# =============================================================================
# WIN PROBABILITY MODEL
# =============================================================================
# These functions compute the probability that a party wins a district,
# given precinct demographics and DGP parameters. The mapmaker uses these
# when optimizing redistricting plans.

#' Compute district-level win probability using Normal CDF
#'
#' Given precinct-level demographics and DGP parameters, compute the expected
#' Democratic vote share and its variance for a district, then calculate
#' P(R wins) = P(Dem share < 0.5) using Normal approximation.
#'
#' @param n_b Numeric vector of minority population by precinct in the district
#' @param n_w Numeric vector of majority population by precinct in the district
#' @param mu_b Expected minority Democratic voting probability, E[P(Dem|minority)]
#' @param mu_w Expected majority Democratic voting probability, E[P(Dem|majority)]
#' @param sigma2_b Variance of minority voting probability across precincts
#' @param sigma2_w Variance of majority voting probability across precincts
#' @param sigma_bw Covariance between minority and majority voting probabilities
#' @return List with mean_dem_share, var_dem_share, p_rep_wins, p_dem_wins
#' @export
#' @examples
#' \dontrun{
#' # District with 3 precincts
#' result <- compute_district_win_prob(
#'   n_b = c(100, 200, 150),
#'   n_w = c(400, 300, 350),
#'   mu_b = 0.81, mu_w = 0.37,
#'   sigma2_b = 0.01, sigma2_w = 0.09, sigma_bw = 0.01
#' )
#' result$p_dem_wins  # Probability Democrats win this district
#' }
compute_district_win_prob <- function(n_b, n_w, mu_b, mu_w,
                                       sigma2_b, sigma2_w, sigma_bw) {
  # Total district population
  N_d <- sum(n_b) + sum(n_w)

  if (N_d == 0) {
    return(list(
      mean_dem_share = NA_real_,
      var_dem_share = NA_real_,
      p_rep_wins = NA_real_,
      p_dem_wins = NA_real_
    ))
  }

  # Expected Democratic votes: sum over precincts of E[votes]
  # E[votes_p] = n_b[p] * mu_b + n_w[p] * mu_w
  exp_dem <- sum(n_b * mu_b + n_w * mu_w)
  mean_dem_share <- exp_dem / N_d

  # Cross-precinct variance (uncertainty about realized betas)
  # Var(sum_p n_b[p]*beta_b[p] + n_w[p]*beta_w[p]) under independence across precincts
  # = sum_p [ n_b[p]^2 * sigma2_b + n_w[p]^2 * sigma2_w + 2*n_b[p]*n_w[p]*sigma_bw ]
  cross_var <- sum(n_b^2 * sigma2_b + n_w^2 * sigma2_w + 2 * n_b * n_w * sigma_bw)

  # Within-precinct variance (expected binomial variance given beta)
  # E[Var(X|beta)] = E[n*beta*(1-beta)] = n*(mu*(1-mu) - sigma2)
  # This is the expected residual variance after accounting for beta uncertainty
  within_var_b <- sum(n_b * (mu_b * (1 - mu_b) - sigma2_b))
  within_var_w <- sum(n_w * (mu_w * (1 - mu_w) - sigma2_w))
  within_var <- pmax(0, within_var_b + within_var_w)  # Ensure non-negative

  # Total variance of Democratic votes
  var_dem_votes <- cross_var + within_var

  # Variance of Democratic vote share
  var_dem_share <- var_dem_votes / N_d^2

  # P(R wins) = P(Dem share < 0.5) using Normal CDF
  sd_dem_share <- sqrt(pmax(1e-10, var_dem_share))
  p_rep_wins <- stats::pnorm(0.5, mean = mean_dem_share, sd = sd_dem_share)
  p_dem_wins <- 1 - p_rep_wins

  list(
    mean_dem_share = mean_dem_share,
    var_dem_share = var_dem_share,
    p_rep_wins = p_rep_wins,
    p_dem_wins = p_dem_wins
  )
}


#' Compute win probabilities for all districts in a plan
#'
#' Given precinct demographics and district assignments, compute the win
#' probability for each district under the specified DGP parameters.
#'
#' @param precincts sf object or data frame with precinct demographics
#' @param assignment Vector of district assignments (length = nrow(precincts))
#' @param dgp_params List with mu_b, mu_w, sigma2_b, sigma2_w, sigma_bw
#' @return Data frame with district-level statistics and win probabilities
#' @export
compute_plan_win_probs <- function(precincts, assignment, dgp_params) {
  # Validate DGP parameters
  validate_dgp_params(dgp_params)

  # Get column names (handle both canonical and standardized)
  precincts <- standardize_column_names(precincts, warn = FALSE)
  n_min_col <- COL_NAMES$n_minority
  n_maj_col <- COL_NAMES$n_majority

  districts <- unique(assignment)

  results <- lapply(districts, function(d) {
    idx <- which(assignment == d)
    n_b <- precincts[[n_min_col]][idx]
    n_w <- precincts[[n_maj_col]][idx]

    prob <- compute_district_win_prob(
      n_b = n_b, n_w = n_w,
      mu_b = dgp_params$mu_b,
      mu_w = dgp_params$mu_w,
      sigma2_b = dgp_params$sigma2_b,
      sigma2_w = dgp_params$sigma2_w,
      sigma_bw = dgp_params$sigma_bw
    )

    data.frame(
      district_id = d,
      n_precincts = length(idx),
      total_pop = sum(n_b) + sum(n_w),
      n_minority = sum(n_b),
      n_majority = sum(n_w),
      pct_minority = sum(n_b) / (sum(n_b) + sum(n_w)),
      mean_dem_share = prob$mean_dem_share,
      var_dem_share = prob$var_dem_share,
      p_dem_wins = prob$p_dem_wins,
      p_rep_wins = prob$p_rep_wins
    )
  })

  do.call(rbind, results)
}


#' Compute expected seats for a redistricting plan
#'
#' Sums the win probabilities across all districts to get expected seat count.
#'
#' @param precincts sf object or data frame with precinct demographics
#' @param assignment Vector of district assignments
#' @param dgp_params List with DGP parameters
#' @param party Either "democratic" or "republican"
#' @return Expected number of seats for the specified party
#' @export
compute_expected_seats <- function(precincts, assignment, dgp_params,
                                    party = c("democratic", "republican")) {
  party <- match.arg(party)

  plan_probs <- compute_plan_win_probs(precincts, assignment, dgp_params)

  if (party == "democratic") {
    sum(plan_probs$p_dem_wins, na.rm = TRUE)
  } else {
    sum(plan_probs$p_rep_wins, na.rm = TRUE)
  }
}


# =============================================================================
# SPATIAL FIELDS
# =============================================================================

#' Simulate spatial autoregressive (SAR) field
#' @param sf_obj sf object with spatial data
#' @param rho spatial autocorrelation parameter
#' @param sd standard deviation of the field
#' @return vector of simulated field values
simulate_sar_field = function(sf_obj, rho = 0.7, sd = 1.0) {
  n = nrow(sf_obj)
  
  # Handle single precinct case
  if (n == 1) {
    return(rnorm(1, 0, sd))
  }
  
  nb  = suppressWarnings(spdep::poly2nb(sf_obj, queen = TRUE))
  lw  = spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  W   = spdep::listw2mat(lw)
  I   = diag(n)
  e   = rnorm(n, 0, 1)                      # unit noise
  z   = as.numeric(solve(I - rho * W, e))
  z   = as.numeric(scale(z))                # mean 0, sd 1
  z[is.na(z)] = 0
  z * sd                                     # respect requested sd
}

#' Simulate precinct-level population and minority population data
#' @param n_precincts
#' @param input_data a dataframe with columns "total_pop" and "minority_pop" from which to sample precincts with replacement
#' @param pop_mean 
#' @param pop_sd
#' @param minority_mean state-wide minority population
#' @param minority_sd precinct standard deviation in minority population
#' @param correlation corr. between precinct size and minority population
#' @param alpha_beta shape parameter for beta distribution (minority population)
#' @return dataframe of precinct-level population and minority population
create_population_data = function(n_precincts        = 2600,
                                  input_data         = NULL,
                                  pop_mean           = 2300,
                                  pop_sd             = 1500,
                                  minority_mean      = 0.25,
                                  minority_sd        = 0.15,
                                  correlation        = 0.25,
                                  alpha_beta         = 0.5,
                                  seed               = 123) {
  set.seed(seed)

  if (!is.null(input_data)) {
    # Option 1: Sample from provided data 
    required_cols = c("total_pop", "minority_pop")
    if (!all(required_cols %in% names(input_data))) {
      stop("input_data must contain columns: total_pop and minority_pop")
    }
    
    if (nrow(input_data) >= n_precincts) {
      sampled_indices = sample(1:nrow(input_data), n_precincts, replace = FALSE)
    } else {
      warning(paste("Input data has", nrow(input_data), "rows but", n_precincts,
                    "precincts requested. Sampling with replacement."))
      sampled_indices = sample(1:nrow(input_data), n_precincts, replace = TRUE)
    }
    
    populations = input_data$total_pop[sampled_indices]
    n_minority_vec = input_data$minority_pop[sampled_indices]
    
  } else {
    # Option 2: Generate population data 
    Sigma = matrix(c(1, correlation, correlation, 1), 2, 2)
    Z = MASS::mvrnorm(n = n_precincts, mu = c(0, 0), Sigma = Sigma)
    
    # Transform to uniform [0,1] via CDF
    U = pnorm(Z)

    # Generate population from log-=normal using inverse CDF transform
    cv = pop_sd / pop_mean
    sigma_log = sqrt(log(1 + cv^2))
    mu_log = log(pop_mean) - sigma_log^2/2
    
    populations = round(qlnorm(U[,1], meanlog = mu_log, sdlog = sigma_log))
    populations = pmax(50, populations)  # Minimum population

    # Generate minority % from Beta with exponential shape
    # To achieve target mean with exponential-like shape:
    # Mean of Beta(alpha, beta) = alpha / (alpha + beta)
    # Given alpha (shape) and target mean, solve for beta
    beta_param = alpha_beta * (1 - minority_mean) / minority_mean
    
    # Use inverse CDF transform with correlated uniform
    minority_pct = qbeta(U[, 2], shape1 = alpha_beta, shape2 = beta_param)
    
    # Calculate counts
    n_minority_vec = round(populations * minority_pct)
    
    # Apply a floor for minority population that increases with population size
    min_minority_pct = pmin(0.01 + (populations / max(populations)) * 0.02, minority_pct)
    needs_adjustment = minority_pct < min_minority_pct
    minority_pct[needs_adjustment] = min_minority_pct[needs_adjustment]
    n_minority_vec = round(populations * minority_pct)
  }

  # Calculate majority counts
  n_majority_vec = populations - n_minority_vec
  per_minority = n_minority_vec / populations
  
  # Assemble data
  precincts = data.frame(
    precinct_id = 1:n_precincts,
    population = populations,
    n_minority = n_minority_vec,
    n_majority = n_majority_vec,
    per_minority = per_minority
  )
  
  # Shuffle rows
  precincts = precincts[sample(n_precincts), ]
  precincts$precinct_id = 1:n_precincts
  rownames(precincts) = NULL
  
  # Report summary statistics
  actual_minority_pct = sum(precincts$n_minority) / sum(precincts$population)
  cat("\nGenerated", n_precincts, "precincts \n")
  cat("Total population:", format(sum(precincts$population), big.mark=","), "\n")
  cat("Mean precinct population:", round(mean(precincts$population)), "\n")
  cat("Overall minority %:", round(actual_minority_pct * 100, 2), "%\n")
  cat("Correlation between pop and minority count:", 
      round(cor(precincts$population, precincts$n_minority), 3), "\n")
  cat("Correlation between pop and minority %:", 
      round(cor(precincts$population, precincts$per_minority), 3), "\n")
  
  # Distribution shape statistics
  cat("\nMinority % distribution:\n")
  cat("  Precincts with <5% minority:", sum(precincts$per_minority < 0.05), 
      "(", round(mean(precincts$per_minority < 0.05) * 100, 1), "%)\n")
  cat("  Precincts with <10% minority:", sum(precincts$per_minority < 0.10),
      "(", round(mean(precincts$per_minority < 0.10) * 100, 1), "%)\n")
  cat("  Precincts with >50% minority:", sum(precincts$per_minority > 0.50),
      "(", round(mean(precincts$per_minority > 0.50) * 100, 1), "%)\n")
  cat("  Precincts with >80% minority:", sum(precincts$per_minority > 0.80),
      "(", round(mean(precincts$per_minority > 0.80) * 100, 1), "%)\n")
  
  save_diagnostic_plot("population_diagnostics", function() {
    par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
    
    # 1. Population distribution
    hist(precincts$population, breaks = 50, main = "Population Distribution",
         xlab = "Population", col = "lightgreen", probability = TRUE)
    # lines(density(precincts$population), col = "darkgreen", lwd = 2)
    
    # 2. Minority % distribution 
    hist(precincts$per_minority, breaks = seq(0, 1, by = 0.02), 
         main = "Minority % Distribution",
         xlab = "Minority %", col = "lightblue", probability = TRUE)
    
    # # Overlay theoretical exponential-like beta
    # x_seq = seq(0.001, 0.999, by = 0.001)
    # y_beta = dbeta(x_seq, alpha_beta, beta_param)
    # lines(x_seq, y_beta, col = "darkblue", lwd = 2)
    
    # 3. Log-scale minority % to see tail better
    hist(log10(precincts$per_minority + 0.001), breaks = 30,
         main = "Log10(Minority %) Distribution", 
         xlab = "Log10(Minority % + 0.001)", col = "lightyellow")
    
    # 4. Population vs Minority Count
    plot(precincts$population, precincts$n_minority,
         main = paste("Pop vs Minority Count\n(r =", 
                     round(cor(precincts$population, precincts$n_minority), 2), ")"),
         xlab = "Population", ylab = "Minority Count",
         pch = 16, col = rgb(1, 0, 0, 0.3), cex = 0.6)
    # abline(lm(n_minority ~ population - 1, data = precincts), col = "red", lwd = 2)
    
    # 5. Population vs Minority %
    plot(precincts$population, precincts$per_minority,
         main = paste("Pop vs Minority %\n(r =", 
                     round(cor(precincts$population, precincts$per_minority), 2), ")"),
         xlab = "Population", ylab = "Minority %",
         pch = 16, col = rgb(0, 0, 1, 0.3), cex = 0.6)
    
    # # Add loess smooth
    # lo = loess(per_minority ~ population, data = precincts, span = 0.5)
    # pop_seq = seq(min(precincts$population), max(precincts$population), length = 100)
    # lines(pop_seq, predict(lo, pop_seq), col = "blue", lwd = 2)
    
    # 6. QQ plot vs exponential
    qqplot(qexp(ppoints(n_precincts)), precincts$per_minority,
           main = "QQ Plot vs Exponential",
           xlab = "Theoretical Exponential Quantiles",
           ylab = "Sample Minority % Quantiles",
           pch = 16, col = rgb(0, 0, 0, 0.5), cex = 0.6)
    abline(0, 1, col = "red", lty = 2)
    
    par(mfrow = c(1, 1))
  })
  
  return(precincts)
}

#' Add baseline votes to precinct data using RPV parameters
#' @param df data frame with precinct data
#' @param prob_minority_dem probability of minority voting Democratic
#' @param prob_majority_dem probability of majority voting Democratic
#' @param sd_minority standard deviation for minority voting
#' @param sd_majority standard deviation for majority voting
#' @param rho correlation between minority and majority voting
#' @param seed random seed for reproducibility
#' @return data frame with added vote columns
add_baseline_votes = function(df, 
                              prob_minority_dem = 0.81, 
                              prob_majority_dem = 0.37,
                              sd_minority       = 0.10, 
                              sd_majority       = 0.30,
                              rho               = 0.20,
                              seed              = 123) {

  set.seed(seed)
  n_precincts = nrow(df)
  
  Sigma = matrix(
    c(sd_minority^2, 
      rho * sd_minority * sd_majority, 
      rho * sd_minority * sd_majority,
      sd_majority^2), 
    2, 2
  )
  
  base = tmvtnorm::rtmvnorm(n     = n_precincts,
                            mean  = c(prob_minority_dem, prob_majority_dem),
                            sigma = Sigma, 
                            lower = c(0, 0), upper = c(1, 1))
  
  df$p_min_base = base[, 1]
  df$p_maj_base = base[, 2]
  
  # Expand votes
  df$dem_votes_minority     = rbinom(n_precincts, df$n_minority,  df$p_min_base)
  df$dem_votes_majority     = rbinom(n_precincts, df$n_majority,  df$p_maj_base)
  df$dem_votes              = df$dem_votes_minority + df$dem_votes_majority
  df$rep_votes              = df$population - df$dem_votes
  
  df$rep_votes_minority     = df$n_minority - df$dem_votes_minority
  df$rep_votes_majority     = df$n_majority - df$dem_votes_majority
  
  df$dem_voteshare          = df$dem_votes / df$population
  df$dem_voteshare_minority = ifelse(df$n_minority > 0, df$dem_votes_minority/df$n_minority, NA_real_)
  df$dem_voteshare_majority = ifelse(df$n_majority > 0, df$dem_votes_majority/df$n_majority, NA_real_)

  df
}

#' Apply vote model to spatial precincts with RPV parameters
#' @param spatial_precincts sf object with precinct data
#' @param voting_model type of voting model ("ei" or "contextual")
#' @param prob_minority_dem probability of minority voting Democratic
#' @param prob_majority_dem probability of majority voting Democratic
#' @param sd_minority standard deviation for minority voting
#' @param sd_majority standard deviation for majority voting
#' @param rho correlation between minority and majority voting
#' @param b_min_context contextual effect for minority voting
#' @param b_maj_context contextual effect for majority voting
#' @param sigma_precinct SD of SAR precinct field on logit scale
#' @param field_rho SAR rho for precinct field
#' @param shared_field_wt weight of shared "partisan climate" field
#' @param shared_field_rho SAR rho for shared field
#' @param use_base_probs whether to use base probabilities
#' @param base_shrink_lambda shrinkage parameter for base probabilities
#' @param seed random seed for reproducibility
#' @return sf object with added vote columns
apply_vote_model = function(spatial_precincts,
                            voting_model       = "ei",
                            prob_minority_dem  = 0.81,
                            prob_majority_dem  = 0.37,
                            sd_minority        = 0.10,
                            sd_majority        = 0.30,
                            rho                = 0.20,
                            b_min_context      = 0.1,
                            b_maj_context      = 0.0,
                            # spatial correlation pars
                            sigma_precinct     = 0.12,   # SD of SAR precinct field on logit scale
                            field_rho          = 0.75,   # SAR rho for precinct field
                            shared_field_wt    = 0.35,   # weight of shared "partisan climate" field (logit scale)
                            shared_field_rho   = 0.85,   # SAR rho for shared field
                            use_base_probs     = TRUE,
                            base_shrink_lambda = 1.0,
                            # reproducibility
                            seed               = 123) {
  
  set.seed(seed)
  stopifnot(voting_model %in% c("ei","contextual"))
  n_precincts = nrow(spatial_precincts)
  
  # helper logits
  logit     = function(p) { p = pmax(0.001, pmin(0.999, p)); log(p/(1-p)) }
  inv_logit = function(x) 1/(1+exp(-x))
  
  have_base = use_base_probs && all(c("p_min_base","p_maj_base") %in% names(spatial_precincts))
  
  if (have_base && base_shrink_lambda > 0) {
    p_min = spatial_precincts$p_min_base
    p_maj = spatial_precincts$p_maj_base
    if (base_shrink_lambda < 1) {
      Sigma = matrix(c(sd_minority^2, 
                       rho * sd_minority * sd_majority,
                       rho * sd_minority * sd_majority, 
                       sd_majority^2), 2, 2)
      draw = tmvtnorm::rtmvnorm(n_precincts, c(prob_minority_dem, prob_majority_dem),
                                 Sigma, lower = c(0, 0), upper = c(1, 1))
      
      p_min = inv_logit((1 - base_shrink_lambda) * logit(draw[, 1]) + base_shrink_lambda * logit(p_min))
      p_maj = inv_logit((1 - base_shrink_lambda) * logit(draw[, 2]) + base_shrink_lambda * logit(p_maj))
    }
  } else {
    Sigma = matrix(c(sd_minority^2, 
                     rho * sd_minority * sd_majority,
                     rho * sd_minority * sd_majority, 
                     sd_majority^2), 2, 2)
    base  = tmvtnorm::rtmvnorm(n_precincts, 
                               c(prob_minority_dem, prob_majority_dem),
                               Sigma, lower = c(0, 0), upper = c(1, 1))
    p_min = base[, 1] 
    p_maj = base[, 2]
  }
  
  if (voting_model == "contextual") {
    p_min = inv_logit(logit(p_min) + b_min_context*spatial_precincts$per_minority)
    p_maj = inv_logit(logit(p_maj) + b_maj_context*spatial_precincts$per_minority)
  }
  
  precinct_field = simulate_sar_field(spatial_precincts, rho = field_rho,     sd = sigma_precinct)
  shared_field   = simulate_sar_field(spatial_precincts, rho = shared_field_rho, sd = 1.0)
  
  p_min = inv_logit(logit(p_min) + precinct_field + shared_field_wt*shared_field)
  p_maj = inv_logit(logit(p_maj) + precinct_field + shared_field_wt*shared_field)
  
  # --- generate votes (fixed population) ---
  
  # simulate votes
  spatial_precincts$dem_votes_minority     = rbinom(n_precincts, spatial_precincts$n_minority,  p_min)
  spatial_precincts$dem_votes_majority     = rbinom(n_precincts, spatial_precincts$n_majority,  p_maj)
  spatial_precincts$dem_votes              = spatial_precincts$dem_votes_minority + spatial_precincts$dem_votes_majority
  spatial_precincts$rep_votes              = spatial_precincts$population - spatial_precincts$dem_votes
  
  spatial_precincts$rep_votes_minority     = spatial_precincts$n_minority - spatial_precincts$dem_votes_minority
  spatial_precincts$rep_votes_majority     = spatial_precincts$n_majority - spatial_precincts$dem_votes_majority
  
  spatial_precincts$dem_voteshare          = spatial_precincts$dem_votes / spatial_precincts$population
  spatial_precincts$dem_voteshare_minority = ifelse(spatial_precincts$n_minority > 0,
                                                    spatial_precincts$dem_votes_minority/spatial_precincts$n_minority, NA_real_)
  spatial_precincts$dem_voteshare_majority = ifelse(spatial_precincts$n_majority > 0,
                                                    spatial_precincts$dem_votes_majority/spatial_precincts$n_majority, NA_real_)
  
  spatial_precincts$prob_minority_dem = p_min
  spatial_precincts$prob_majority_dem = p_maj
  spatial_precincts$precinct_effect   = precinct_field
  spatial_precincts$shared_effect     = shared_field * shared_field_wt
  
  if (anyNA(spatial_precincts$population) ||
      sum(spatial_precincts$n_minority + spatial_precincts$n_majority) !=
      sum(spatial_precincts$population)) {
    warning("Population accounting mismatch after vote assignment.")
  }
  
  spatial_precincts
}

#' Place voters on map with specified segregation level
#' @param precinct_data data frame with precinct data
#' @param grid_result list with precincts sf object
#' @param segregation_level level of segregation ("low", "medium", "high")
#' @param voting_weight weight for voting in assignment score
#' @param seed random seed for reproducibility
#' @return sf object with voters placed on map
place_voters_on_map = function(precinct_data,
                               grid_result,
                               segregation_level = "medium",
                               voting_weight = 0.4,
                               seed = 123) {

  set.seed(seed)

  grid = grid_result$precincts
  n_precincts = nrow(precinct_data)

  cat("\nAssigning fixed population with", segregation_level, "segregation...\n")

  # --- Generate assignment scores ---

  if (segregation_level == "low") {
    # Pure random assignment for low segregation
    assignment_order = sample(1:n_precincts)

  } else {
    # Medium and High

    # Normalize minority share and dem voteshare
    precinct_data$per_minority_std = (precinct_data$per_minority - min(precinct_data$per_minority)) / (max(precinct_data$per_minority) - min(precinct_data$per_minority))
    precinct_data$dem_voteshare_std = (precinct_data$dem_voteshare - min(precinct_data$dem_voteshare)) / (max(precinct_data$dem_voteshare) - min(precinct_data$dem_voteshare))
    
    # combined score
    precinct_data$assignment_score = voting_weight * precinct_data$dem_voteshare_std + (1 - voting_weight) * precinct_data$per_minority_std
    
    rank_socio = rank(-precinct_data$assignment_score, ties.method = 'random')
    rank_random = rank(runif(n_precincts), ties.method = "random")

    alpha = switch(segregation_level, 'medium' = 0.5, 'high' = 1.0)
    final_rank = alpha * rank_socio + (1- alpha) * rank_random
    assignment_order = order(final_rank)

  }

  # --- Assignment ---

  # Sort precinct boundaries by decreasing urbanness
  grid_sorted = grid[order(grid$urbanness, decreasing = TRUE), ]

  # Arrange precinct data
  precinct_data_sorted = precinct_data[assignment_order, ]

  # Combine
  spatial_precincts = grid_sorted
  spatial_precincts[, names(precinct_data)] = precinct_data_sorted

  # --- Calculate spatial correlation ---

  if (inherits(spatial_precincts, "sf") && nrow(spatial_precincts) > 1) {
    # Suppress warnings about no neighbours, which can happen in small test grids
    nb = suppressWarnings(poly2nb(spatial_precincts, queen = TRUE))
    W = nb2listw(nb, style = "W", zero.policy = TRUE)

    # Calculate Moran's I for the two key variables
    morans_minority = moran.test(spatial_precincts$per_minority, W, zero.policy = TRUE, alternative = "greater")
    morans_dem = moran.test(spatial_precincts$dem_voteshare, W, zero.policy = TRUE, alternative = "greater")

    cat("Moran's I (Minority %):", round(morans_minority$estimate[[1]], 4), "\n")
    cat("Moran's I (Dem Voshare):", round(morans_dem$estimate[[1]], 4), "\n")
  }

  # Return the final sf object with data assigned
  return(spatial_precincts)

}

#' Calculate RPV metrics from precinct data
#' @param precinct_data sf object with precinct data
#' @return data frame with RPV metrics
calculate_rpv_metrics = function(precinct_data) {
  # Calculate basic RPV metrics
  metrics = data.frame(
    total_precincts = nrow(precinct_data),
    total_population = sum(precinct_data$population, na.rm = TRUE),
    total_minority = sum(precinct_data$n_minority, na.rm = TRUE),
    total_majority = sum(precinct_data$n_majority, na.rm = TRUE),
    total_dem_votes = sum(precinct_data$dem_votes, na.rm = TRUE),
    total_rep_votes = sum(precinct_data$rep_votes, na.rm = TRUE),
    mean_minority_share = mean(precinct_data$per_minority, na.rm = TRUE),
    mean_dem_voteshare = mean(precinct_data$dem_voteshare, na.rm = TRUE),
    mean_minority_dem_share = mean(precinct_data$dem_voteshare_minority, na.rm = TRUE),
    mean_majority_dem_share = mean(precinct_data$dem_voteshare_majority, na.rm = TRUE)
  )
  
  # Calculate RPV gap
  metrics$rpv_gap = metrics$mean_minority_dem_share - metrics$mean_majority_dem_share
  
  return(metrics)
}

#' Simulate RPV sensitivity analysis
#' @param precinct_data sf object with precinct data
#' @param rpv_scenarios list of RPV parameter scenarios
#' @param seed random seed for reproducibility
#' @return list of results for each scenario
simulate_rpv_sensitivity = function(precinct_data, 
                                   rpv_scenarios = list(
                                     low = list(prob_minority_dem = 0.7, prob_majority_dem = 0.4),
                                     medium = list(prob_minority_dem = 0.8, prob_majority_dem = 0.35),
                                     high = list(prob_minority_dem = 0.9, prob_majority_dem = 0.3)
                                   ),
                                   seed = 123) {
  set.seed(seed)
  
  results = list()
  
  for (scenario_name in names(rpv_scenarios)) {
    cat("\nSimulating RPV scenario:", scenario_name, "\n")
    
    scenario_params = rpv_scenarios[[scenario_name]]
    
    # Apply vote model with scenario parameters
    scenario_result = do.call(apply_vote_model, 
                             c(list(spatial_precincts = precinct_data), 
                               scenario_params, 
                               list(seed = seed)))
    
    # Calculate metrics
    metrics = calculate_rpv_metrics(scenario_result)
    metrics$scenario = scenario_name
    
    results[[scenario_name]] = list(
      data = scenario_result,
      metrics = metrics
    )
  }

  return(results)
}


# =============================================================================
# ELECTION SIMULATION (GROUND TRUTH GENERATION)
# =============================================================================

#' Simulate a single election with ground truth individual-level votes
#'
#' Given precinct demographics (n_minority, n_majority) and DGP parameters,
#' simulate an election by:
#' 1. Drawing precinct-specific voting probabilities (beta_b, beta_w) from the DGP
#' 2. Drawing individual votes from binomial distributions
#' 3. Storing both aggregate votes and ground-truth group-specific votes
#'
#' This function is called AFTER redistricting to generate realized elections
#' that can be analyzed with EI/ER/MrP methods.
#'
#' @param precincts sf object or data frame with columns n_minority, n_majority
#' @param dgp_params List with mu_b, mu_w, sigma2_b, sigma2_w, sigma_bw
#' @param seed Random seed for reproducibility (NULL for no seeding)
#' @return sf/data frame with added vote columns (votes_dem, votes_rep, etc.)
#'   and ground truth columns (mu_minority, mu_majority)
#' @export
#' @examples
#' \dontrun{
#' # Simulate election with default Texas-like RPV
#' election <- simulate_election(precincts, DEFAULT_DGP_PARAMS)
#'
#' # Compare ground truth to what EI would estimate
#' mean(election$share_dem_minority)  # True minority Dem share
#' }
simulate_election <- function(precincts, dgp_params, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)

  # Validate DGP parameters
  validate_dgp_params(dgp_params)

  # Standardize column names
  precincts <- standardize_column_names(precincts, warn = FALSE)

  n <- nrow(precincts)
  n_b <- precincts[[COL_NAMES$n_minority]]
  n_w <- precincts[[COL_NAMES$n_majority]]

  # Build covariance matrix for (beta_b, beta_w)
  Sigma <- matrix(c(
    dgp_params$sigma2_b, dgp_params$sigma_bw,
    dgp_params$sigma_bw, dgp_params$sigma2_w
  ), nrow = 2)

  # Draw precinct-specific voting probabilities from truncated MVN
  betas <- tmvtnorm::rtmvnorm(
    n = n,
    mean = c(dgp_params$mu_b, dgp_params$mu_w),
    sigma = Sigma,
    lower = c(0, 0),
    upper = c(1, 1)
  )

  beta_b <- betas[, 1]
  beta_w <- betas[, 2]

  # Simulate votes from binomial distributions
  votes_dem_minority <- rbinom(n, size = n_b, prob = beta_b)
  votes_dem_majority <- rbinom(n, size = n_w, prob = beta_w)
  votes_rep_minority <- n_b - votes_dem_minority
  votes_rep_majority <- n_w - votes_dem_majority

  # Add to output
  result <- precincts
  result[[COL_NAMES$votes_dem_minority]] <- votes_dem_minority
  result[[COL_NAMES$votes_dem_majority]] <- votes_dem_majority
  result[[COL_NAMES$votes_dem]] <- votes_dem_minority + votes_dem_majority
  result[[COL_NAMES$votes_rep_minority]] <- votes_rep_minority
  result[[COL_NAMES$votes_rep_majority]] <- votes_rep_majority
  result[[COL_NAMES$votes_rep]] <- votes_rep_minority + votes_rep_majority
  result[[COL_NAMES$votes_total]] <- n_b + n_w

  # Store realized probabilities (ground truth)
  result[[COL_NAMES$mu_minority]] <- beta_b
  result[[COL_NAMES$mu_majority]] <- beta_w

  # Compute vote shares
  result[[COL_NAMES$share_dem]] <- safe_div(
    result[[COL_NAMES$votes_dem]],
    result[[COL_NAMES$votes_total]]
  )
  result[[COL_NAMES$share_dem_minority]] <- safe_div(
    votes_dem_minority, n_b
  )
  result[[COL_NAMES$share_dem_majority]] <- safe_div(
    votes_dem_majority, n_w
  )

  result
}


#' Aggregate precinct-level election results to districts
#'
#' Given precinct-level election data and district assignments, aggregate
#' votes and demographics to the district level.
#'
#' @param election_data sf object or data frame with precinct-level election results
#' @param assignment Vector of district assignments (same length as election_data)
#' @return Data frame with district-level aggregates
#' @export
aggregate_to_districts <- function(election_data, assignment) {
  # Standardize column names
  election_data <- standardize_column_names(election_data, warn = FALSE)

  districts <- unique(assignment)

  results <- lapply(districts, function(d) {
    idx <- which(assignment == d)
    subset_data <- election_data[idx, , drop = FALSE]

    data.frame(
      district_id = d,
      n_precincts = length(idx),
      total_pop = sum(subset_data[[COL_NAMES$total_pop]], na.rm = TRUE),
      n_minority = sum(subset_data[[COL_NAMES$n_minority]], na.rm = TRUE),
      n_majority = sum(subset_data[[COL_NAMES$n_majority]], na.rm = TRUE),
      votes_dem = sum(subset_data[[COL_NAMES$votes_dem]], na.rm = TRUE),
      votes_rep = sum(subset_data[[COL_NAMES$votes_rep]], na.rm = TRUE),
      votes_dem_minority = sum(subset_data[[COL_NAMES$votes_dem_minority]], na.rm = TRUE),
      votes_dem_majority = sum(subset_data[[COL_NAMES$votes_dem_majority]], na.rm = TRUE)
    )
  })

  district_df <- do.call(rbind, results)

  # Compute derived columns
  district_df$pct_minority <- safe_div(district_df$n_minority, district_df$total_pop)
  district_df$share_dem <- safe_div(district_df$votes_dem, district_df$votes_dem + district_df$votes_rep)
  district_df$share_dem_minority <- safe_div(district_df$votes_dem_minority, district_df$n_minority)
  district_df$share_dem_majority <- safe_div(district_df$votes_dem_majority, district_df$n_majority)
  district_df$dem_wins <- as.integer(district_df$share_dem > 0.5)

  district_df
}


#' Extract ground truth voting rates from election data
#'
#' Computes the "true" group-specific voting rates from simulated election data.
#' These can be compared to EI/ER/MrP estimates.
#'
#' @param election_data sf object or data frame with simulated election results
#' @return List with beta_minority and beta_majority (population-weighted averages)
#' @export
extract_ground_truth <- function(election_data) {
  # Standardize column names
  election_data <- standardize_column_names(election_data, warn = FALSE)

  n_b <- election_data[[COL_NAMES$n_minority]]
  n_w <- election_data[[COL_NAMES$n_majority]]
  dem_min <- election_data[[COL_NAMES$votes_dem_minority]]
  dem_maj <- election_data[[COL_NAMES$votes_dem_majority]]

  # Population-weighted average voting rates
  beta_minority <- sum(dem_min, na.rm = TRUE) / sum(n_b, na.rm = TRUE)
  beta_majority <- sum(dem_maj, na.rm = TRUE) / sum(n_w, na.rm = TRUE)

  list(
    beta_minority = beta_minority,
    beta_majority = beta_majority,
    rpv_gap = beta_minority - beta_majority
  )
}
