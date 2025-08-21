source('R/setup_python_env.R')

# ===================================================================
# ==================== PART 1: GRID GENERATION ======================
# ===================================================================

# Function to create city centers
create_city_centers = function(n_centers = 3,
                               bounds    = c(0, 1000, 0, 1000),
                               seed      = 123) {
  
  set.seed(seed)
  buffer = (bounds[2] - bounds[1]) * 0.05
  centers = data.frame(
    id = 1:n_centers,
    x = runif(n_centers, bounds[1] + buffer, bounds[2] - buffer),
    y = runif(n_centers, bounds[3] + buffer, bounds[4] - buffer),
    intensity = runif(n_centers, 0.3, 1.0)
  )
  centers$intensity[1] = 1.0
  return(centers)
}

# Function to create density surface with a central plateau to avoid "starbursts"
create_density_surface = function(x,
                                  y,
                                  centers,
                                  decay_rate      = 15,
                                  base_density    = 0.02,
                                  peak_multiplier = 5.0,
                                  decay_power     = 4,
                                  center_radius   = 1.0) {
  
  density = base_density
  for (i in 1:nrow(centers)) {
    dist = sqrt((x - centers$x[i])^2 + (y - centers$y[i])^2)
    if (dist < center_radius) {
      density_contribution = centers$intensity[i] * peak_multiplier
    } else {
      eff_dist = dist - center_radius
      density_contribution = centers$intensity[i] * peak_multiplier * exp(-(eff_dist^decay_power) / (decay_rate^decay_power))
    }
    density = density + density_contribution
  }
  return(density)
}

# Function to generate seed points using robust rejection sampling
generate_seed_points = function(n_target,
                                centers,
                                bounds          = c(0, 1000, 0, 1000),
                                decay_rate      = 15,
                                base_density    = 0.01,
                                peak_multiplier = 5.0,
                                decay_power     = 4,
                                center_radius   = 1.0) {
  
  kept_points = data.frame()
  max_density = base_density + (sum(centers$intensity) * peak_multiplier)
  n_candidates_batch = n_target * 5
  while (nrow(kept_points) < n_target) {
    candidates = data.frame(
      x = runif(n_candidates_batch, bounds[1], bounds[2]),
      y = runif(n_candidates_batch, bounds[3], bounds[4])
    )
    candidates$density = mapply(
      create_density_surface,
      candidates$x,
      candidates$y,
      MoreArgs = list(
        centers = centers,
        decay_rate = decay_rate,
        base_density = base_density,
        peak_multiplier = peak_multiplier,
        decay_power = decay_power,
        center_radius = center_radius
      )
    )
    candidates$keep_prob = pmin(1, candidates$density / max_density)
    candidates$keep = runif(n_candidates_batch) < candidates$keep_prob
    kept_points = rbind(kept_points, candidates[candidates$keep, ])
  }
  final_points = kept_points[sample(nrow(kept_points), n_target), ]
  return(final_points[, c("x", "y", "density")])
}

# Function to create precinct boundaries from seed points
create_precinct_boundaries = function(seed_points, bounds = c(0, 1000, 0, 1000)) {
  
  window = owin(xrange = bounds[1:2], yrange = bounds[3:4])
  pp = ppp(seed_points$x, seed_points$y, window = window)
  vor = dirichlet(pp)
  vor_polys = list()
  
  for (i in 1:vor$n) {
    tile = vor$tiles[[i]]
    if (!is.null(tile)) {
      vertices = cbind(tile$bdry[[1]]$x, tile$bdry[[1]]$y)
      if (!all(vertices[1, ] == vertices[nrow(vertices), ])) {
        vertices = rbind(vertices, vertices[1, ])
      }
      vor_polys[[i]] = st_polygon(list(vertices))
    }
  }
  
  vor_polys = vor_polys[!sapply(vor_polys, is.null)]
  precincts = st_sf(
    precinct_id = 1:length(vor_polys),
    geometry = st_sfc(vor_polys),
    crs = 3857
  )
  precincts = st_make_valid(precincts)
  precincts = st_buffer(precincts, 0)
  precincts = st_snap_to_grid(precincts, size = 0.001)
  
  # Create spatial points and find which precinct each seed falls in
  seed_sf = st_as_sf(
    seed_points,
    coords = c("x", "y"),
    crs = 3857,
    remove = FALSE
  )
  seed_to_precinct_map = st_join(seed_sf, precincts, join = st_intersects)
  
  # Extract just the data (no geometry) from the join result
  seed_data = st_drop_geometry(seed_to_precinct_map)
  
  # Join the seed data to precincts
  precincts = precincts %>%
    left_join(seed_data, by = "precinct_id")
  
  # Calculate derived fields
  precincts$area = as.numeric(st_area(precincts))
  precincts$urbanness = precincts$density / max(precincts$density, na.rm = TRUE)
  precincts = precincts %>% rename(seed_x = x, seed_y = y)
  
  return(precincts)
}

# Main function to create the complete grid
create_realistic_grid = function(n_precincts     = 2600,
                                 n_centers       = 5,
                                 bounds          = NULL,
                                 decay_rate      = 15,
                                 base_density    = 0.01,
                                 peak_multiplier = 15.0,
                                 decay_power     = 4,
                                 center_radius   = 2.0,
                                 seed            = 123) {
  
  if (is.null(bounds)) {
    scale_factor = n_precincts / 400
    bound_size = 100 * scale_factor
    bounds = c(0, bound_size, 0, bound_size)
  }
  if (is.null(decay_rate)) {
    decay_rate = 8 * (bound_size / 100)^0.5
  }
  centers = create_city_centers(n_centers, bounds, seed)
  cat("Created", n_centers, "city centers\n")
  seeds = generate_seed_points(
    n_precincts,
    centers,
    bounds,
    decay_rate,
    base_density,
    peak_multiplier,
    decay_power,
    center_radius
  )
  cat("Generated", nrow(seeds), "seed points\n")
  precincts = create_precinct_boundaries(seeds, bounds) # ggplot(precincts) + geom_sf(fill = NA) + theme_void()
  cat("Created", nrow(precincts), "precincts\n")
  precincts$dist_to_center = NA
  precincts$nearest_center = NA
  valid_precincts = !is.na(precincts$seed_x)
  for (i in which(valid_precincts)) {
    distances = sqrt((precincts$seed_x[i] - centers$x)^2 + (precincts$seed_y[i] - centers$y)^2)
    precincts$dist_to_center[i] = min(distances)
    precincts$nearest_center[i] = which.min(distances)
  }
  
  # Plot
  outer_boundary = st_union(precincts)
  
  p1 = ggplot(precincts) +
    geom_sf(data = outer_boundary,
            fill = NA,
            color = "black") +  # Add outer boundary only
    geom_point(aes(seed_x, seed_y)) +
    coord_sf(xlim = st_bbox(precincts)[c(1, 3)], ylim = st_bbox(precincts)[c(2, 4)]) +
    theme_void()
  
  p2 = ggplot(precincts) +
    geom_sf(fill = NA, color = "black") +
    geom_point(aes(seed_x, seed_y)) +
    theme_void()
  
  p3 = ggplot(precincts) +
    geom_sf(fill = NA, color = "black") +
    theme_void()
  
  # ggsave(
  #   "~/Dropbox/RPV/Figures/Simulation Figures/precinct_geography_from_points.pdf",
  #   p1 + p2 + p3,
  #   width = 10,
  #   height = 8
  # )
  
  return(list(precincts = precincts, centers = centers))
}

simulate_sar_field = function(sf_obj, rho = 0.7, sd = 1.0) {
  nb  = suppressWarnings(spdep::poly2nb(sf_obj, queen = TRUE))
  lw  = spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
  W   = spdep::listw2mat(lw)
  n   = nrow(W)
  I   = diag(n)
  e   = rnorm(n, 0, 1)                      # unit noise
  z   = as.numeric(solve(I - rho * W, e))
  z   = as.numeric(scale(z))                # mean 0, sd 1
  z[is.na(z)] = 0
  z * sd                                     # respect requested sd
}

rank_match_assign = function(df_values, poly_scores) {
  # returns the row order that maps highest values onto highest scores
  ord_values = order(df_values, decreasing = TRUE)
  ord_poly   = order(poly_scores, decreasing = TRUE)
  assignment = integer(length(df_values))
  assignment[ord_poly] = ord_values
  assignment
}


# ===================================================================
# ======= PART 2: POPULATION GENERATION & PRECINCT ASSIGNMENT =======
# ===================================================================

# Generate precinct data with total and minority population counts
create_population_data = function(n_precincts    = 2600,
                                  input_data     = NULL,
                                  pop_mean       = 2300,
                                  pop_sd         = 1500,
                                  minority_mean  = 0.25,
                                  minority_sd    = 0.30,
                                  rho_size_share = 0.25,
                                  seed           = 123) {
  set.seed(seed)

  if (!is.null(input_data)) {
    # Option 1: Sample from provided data
    # Validate input
    required_cols = c("total_pop", "minority_pop")
    if (!all(required_cols %in% names(input_data))) {
      stop("input_data must contain columns: total_pop and minority_pop")
    }

    # Sample precincts (with replacement if needed)
    if (nrow(input_data) >= n_precincts) {
      sampled_indices = sample(1:nrow(input_data), n_precincts, replace = FALSE)
    } else {
      warning(paste("Input data has", nrow(input_data), "rows but", n_precincts,
                    "precincts requested. Sampling with replacement."))
      sampled_indices = sample(1:nrow(input_data), n_precincts, replace = TRUE)
    }

    # Extract populations
    populations = input_data$total_pop[sampled_indices]
    n_minority_vec = input_data$minority_pop[sampled_indices]

  } else {
    
    # Option 2: Simulate from distributions
    
    cv          = pop_sd / pop_mean
    sigma       = sqrt(log(1 + cv^2))
    mu          = log(pop_mean) - sigma^2/2
    populations = round(rlnorm(n_precincts, meanlog = mu, sdlog = sigma))
    populations = pmax(50, populations) # Minimum population of 50

    # ------------------------------------------
    # 2) MINORITY % with *monotone-decreasing* Beta
    # ------------------------------------------
    # Method-of-moments concentration (k), then CLAMP so that:
    #   alpha = m*k  < 1  and  beta = (1-m)*k > 1  ⇒ density decreases smoothly from 0→1
    m   = pmin(pmax(minority_mean, 0.01), 0.99)
    v   = pmin(pmax(minority_sd^2, 1e-6), m*(1-m) - 1e-6)  # keep feasible
    k0  = m*(1-m)/v - 1
    k_lo= 1/(1-m) + 1e-6   # ensures beta > 1
    k_hi= 1/m     - 1e-6   # ensures alpha < 1
    k   = min(max(k0, k_lo), k_hi)
    
    alpha = m * k
    beta  = (1 - m) * k
    per_minority = rbeta(n_precincts, alpha, beta)
    
    # ------------------------------------------
    # 3) Inject a mild size→share correlation
    # ------------------------------------------
    # Work on logit scale to keep (0,1) bounds.
    logit  = function(p) { p = pmin(pmax(p, 1e-6), 1-1e-6); log(p/(1-p)) }
    invlog = function(x) 1/(1+exp(-x))
    
    z_pop  = scale(log(populations))      # larger pop ⇒ larger z_pop
    per_adj= invlog(logit(per_minority) + rho_size_share * as.numeric(z_pop))
    
    # Gentle multiplicative calibration back toward requested mean
    eps    = 0.01
    s      = m / mean(per_adj)            # keep overall level roughly on target
    per_adj= pmin(pmax(per_adj * s, eps), 1 - eps)
    
    n_minority_vec = round(populations * per_adj)
  }
  
  # ---------------------------
  # 4) Assemble + report
  # ---------------------------
  n_majority_vec = pmax(0, populations - n_minority_vec)
  per_minority   = n_minority_vec / populations
  
  precincts = data.frame(
    precinct_id  = 1:n_precincts,
    population   = populations,
    n_minority   = n_minority_vec,
    n_majority   = n_majority_vec,
    per_minority = per_minority
  )
  
  precincts = precincts[sample(n_precincts), ]
  precincts$precinct_id = 1:n_precincts
  
  actual_minority_pct = sum(precincts$n_minority) / sum(precincts$population)
  cat("\nGenerated", n_precincts, "precincts (simulated)\n")
  cat("Total population:", format(sum(precincts$population), big.mark=","), "\n")
  cat("Mean precinct population:", round(mean(precincts$population)), "\n")
  cat("Overall minority %:", round(actual_minority_pct * 100, 2), "%\n")
  cat("Precincts with 0% minority:", sum(precincts$per_minority == 0), "\n")
  cat("Precincts with 100% minority:", sum(precincts$per_minority == 1), "\n")

  # Create visualizations
  par(mfrow = c(2, 2))

  # Population distribution
  hist(precincts$population, breaks = 50, main = "Distribution of Population",
       xlab = "Population", ylab = "Number of Precincts", col = "lightgreen")

  # Minority percentage distribution
  hist(precincts$per_minority, breaks = 50, main = "Distribution of Minority %",
       xlab = "Minority Percentage", ylab = "Number of Precincts", col = "lightblue")

  # Population vs Minority % scatter
  plot(precincts$population, precincts$per_minority,
       main = "Population vs Minority %",
       xlab = "Population", ylab = "Minority %",
       pch = 16, col = rgb(0, 0, 1, 0.3))

  # Cumulative distribution of minority %
  plot(ecdf(precincts$per_minority),
       main = "Cumulative Distribution of Minority %",
       xlab = "Minority %", ylab = "Cumulative Probability")

  par(mfrow = c(1, 1))

  return(precincts)
}


# Initial vote assignment BEFORE precincts assigned to geographic boundaries
# Serves as baseline vote
# Precinct-level group-speficic vote shares drawn from EI model
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
                            shared_field_wt    = 0.35,   # weight of shared “partisan climate” field (logit scale)
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



# ===================================================================
# =============== PART 3: REDISTRICTING FUNCTIONS ===================
# ===================================================================

# Function to format precincts for redistricting analysis
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

# Verify fixed population consistency
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
      nb = poly2nb(precincts, queen = TRUE)
      W = nb2listw(nb, style = "W", zero.policy = TRUE)
      morans_result = moran.test(precincts$per_minority, W, 
                                 alternative = "greater")
      cat(sprintf("  %s: I = %.4f, p-value = %.4f\n", 
                  level, morans_result$estimate[1], morans_result$p.value))
    }
  }
  
  return(TRUE)
}

create_district_summary = function(map_data, plan_id, cd_plan, plan_precinct_id = NULL) {
  
  df = map_data %>% st_drop_geometry() %>%
    select(precinct_id, population, n_minority, n_majority, dem_votes, rep_votes,
           dem_votes_minority, dem_votes_majority, per_minority, dem_voteshare,
           dem_voteshare_minority, dem_voteshare_majority)
  
  if (!is.null(plan_precinct_id)) {
    # align
    ord = match(df$precinct_id, plan_precinct_id)
    stopifnot(!any(is.na(ord)))
    df$district_id = as.integer(cd_plan[ord])
  } else {
    # fallback: assume order already matches
    df$district_id = as.integer(cd_plan)
  }
  
  district_summary = df %>% group_by(district_id) %>% summarise(
    n_precincts = n(),
    population = sum(population),
    n_minority_voters = sum(n_minority),
    n_majority_voters = sum(n_majority),
    dem_votes = sum(dem_votes),
    rep_votes = sum(rep_votes),
    dem_votes_minority = sum(dem_votes_minority),
    dem_votes_majority = sum(dem_votes_majority),
    per_minority = n_minority_voters / population,
    maj_min_district = per_minority > 0.5,
    dem_voteshare = dem_votes / population,
    dem_district = dem_voteshare >= 0.5,
    dem_voteshare_minority = dem_votes_minority / n_minority_voters,
    dem_voteshare_majority = dem_votes_majority / n_majority_voters,
    .groups = "drop"
  ) %>% mutate(map_id = plan_id, district_id = as.integer(district_id)) %>%
    mutate(across(c(dem_voteshare_minority, dem_voteshare_majority), ~ifelse(is.na(.), 0, .))) %>%
    select(map_id, district_id, everything())
  
  district_summary
}

create_map_summary = function(district_summary) {
  
  # Aggregate to map level
  map_summary = district_summary %>%
    group_by(map_id) %>%
    summarise(
      n_districts = n(),
      n_dem_districts = sum(dem_district),
      n_rep_districts = n_districts - n_dem_districts,
      n_maj_min_districts = sum(maj_min_district),
      avg_dem_voteshare = mean(dem_voteshare),
      min_dem_voteshare = min(dem_voteshare),
      max_dem_voteshare = max(dem_voteshare)
    )
  
  return(map_summary)
}

process_redistricting_results = function(map_data, redistricting_results, current_output_dir) {
  
  summaries = list()
  
  # Process neutral results
  if (!is.null(redistricting_results$neutral)) {
    neutral_plans_file = paste0(current_output_dir, "neutral/CD_plans.csv")
    if (file.exists(neutral_plans_file)) {
      CD_plans_neutral = fread(neutral_plans_file) %>%
        select(precinct_id, init_CD, everything())
      
      summaries$neutral = save_plan_summaries(
        map_data = map_data,
        plan_data = CD_plans_neutral,
        plan_type = "neutral",
        output_dir = paste0(current_output_dir, "neutral/")
      )
      
      cat("Processed", ncol(CD_plans_neutral) - 2, "neutral plans\n")
    }
  }
  
  # Process Republican ensemble
  if (!is.null(redistricting_results$republican)) {
    rep_ensemble_file = paste0(current_output_dir, "republican/CD_plans.csv")
    if (file.exists(rep_ensemble_file)) {
      rep_ensemble = fread(rep_ensemble_file)
      
      summaries$republican = save_plan_summaries(
        map_data = map_data,
        plan_data = rep_ensemble,
        plan_type = "republican",
        output_dir = paste0(current_output_dir, "republican/")
      )
    }
  }
  
  # Process Democratic ensemble
  if (!is.null(redistricting_results$democratic)) {
    dem_ensemble_file = paste0(current_output_dir, "democratic/CD_plans.csv")
    if (file.exists(dem_ensemble_file)) {
      dem_ensemble = fread(dem_ensemble_file)
      
      summaries$democratic = save_plan_summaries(
        map_data = map_data,
        plan_data = dem_ensemble,
        plan_type = "democratic",
        output_dir = paste0(current_output_dir, "democratic/")
      )
    }
  }
  
  return(summaries)
}

save_plan_summaries = function(map_data, plan_data, plan_type, output_dir) {
  all_district_summaries = NULL
  all_map_summaries = NULL
  
  # Identify plan columns + the precinct id column
  has_init = "init_CD" %in% names(plan_data)
  stopifnot("precinct_id" %in% names(plan_data))
  plan_precinct_id = plan_data$precinct_id
  plan_columns = setdiff(names(plan_data), c("precinct_id", "init_CD"))  # plan columns only
  n_plans = length(plan_columns)
  
  cat("Processing", n_plans, plan_type, if (plan_type=="neutral") "plans" else "ensemble plans", "\n")
  
  for (i in seq_len(n_plans)) {
    if (i %% ifelse(plan_type=="neutral", 100, 5) == 0)
      cat("  Processing", plan_type, "plan", i, "of", n_plans, "\n")
    
    plan_name = plan_columns[i]
    cd_plan   = plan_data[[plan_name]]
    
    # Align by precinct_id
    district_summary = create_district_summary(
      map_data, plan_id = i, cd_plan = cd_plan, plan_precinct_id = plan_precinct_id
    )
    if (plan_type != "neutral") district_summary$plan_name = plan_name
    district_summary$plan_type = plan_type
    
    map_summary = create_map_summary(district_summary)
    if (plan_type != "neutral") map_summary$plan_name = plan_name
    map_summary$plan_type = plan_type
    
    all_district_summaries = bind_rows(all_district_summaries, district_summary)
    all_map_summaries      = bind_rows(all_map_summaries, map_summary)
  }
  
  # Print + write
  cat("\n", stringr::str_to_title(plan_type), "results:\n")
  cat("  Democratic seats range:", min(all_map_summaries$n_dem_districts), "-", max(all_map_summaries$n_dem_districts), "\n")
  cat("  Republican seats range:", min(all_map_summaries$n_rep_districts), "-", max(all_map_summaries$n_rep_districts), "\n")
  cat("  Mean Democratic seats:", round(mean(all_map_summaries$n_dem_districts), 2), "\n")
  cat("  Mean Republican seats:", round(mean(all_map_summaries$n_rep_districts), 2), "\n")
  
  write_csv(all_district_summaries, file.path(output_dir, "district_summaries.csv"))
  write_csv(all_map_summaries,      file.path(output_dir, "map_summaries.csv"))
  
  if (plan_type != "neutral") {
    plan_scores = all_map_summaries %>%
      transmute(
        plan_id = map_id,
        plan_name = plan_name,
        dem_seats = n_dem_districts,
        rep_seats = n_rep_districts,
        plan_type = plan_type
      )
    write_csv(plan_scores, file.path(output_dir, "plan_scores.csv"))
    
    if (plan_type == "republican") {
      best_score = max(plan_scores$rep_seats)
      best_plan_row = plan_scores %>% filter(rep_seats == best_score) %>% slice(1)
    } else {
      best_score = max(plan_scores$dem_seats)
      best_plan_row = plan_scores %>% filter(dem_seats == best_score) %>% slice(1)
    }
    best_plan = plan_data[[best_plan_row$plan_name]]
    
    return(list(
      district_all = all_district_summaries,
      map_all      = all_map_summaries,
      plan_scores  = plan_scores,
      ensemble_size = n_plans,
      best_score   = best_score,
      best_plan    = best_plan
    ))
  } else {
    return(list(
      district_all = all_district_summaries,
      map_all      = all_map_summaries
    ))
  }
}

# Helper function to print redistricting summary
print_ensemble_summary = function(summaries, level) {
  cat("\n=== Redistricting Summary for", stringr::str_to_title(level), "Segregation ===\n")
  
  if (!is.null(summaries$neutral)) {
    cat("\nNeutral Redistricting:\n")
    cat("  Mean Democratic seats:", round(mean(summaries$neutral$map_all$n_dem_districts), 2), "\n")
    cat("  SD:", round(sd(summaries$neutral$map_all$n_dem_districts), 2), "\n")
    cat("  Range:", min(summaries$neutral$map_all$n_dem_districts), "-", 
        max(summaries$neutral$map_all$n_dem_districts), "\n")
  }
  
  if (!is.null(summaries$republican)) {
    rep_scores = summaries$republican$plan_scores
    cat("\nRepublican Gerrymander Ensemble (", summaries$republican$ensemble_size, "plans):\n")
    cat("  Republican seats range:", min(rep_scores$rep_seats), "-", max(rep_scores$rep_seats), "\n")
    cat("  Best Republican plan:", summaries$republican$best_score, "Republican seats\n")
    cat("  Mean Republican seats across ensemble:", round(mean(rep_scores$rep_seats), 2), "\n")
  }
  
  if (!is.null(summaries$democratic)) {
    dem_scores = summaries$democratic$plan_scores
    cat("\nDemocratic Gerrymander Ensemble (", summaries$democratic$ensemble_size, "plans):\n")
    cat("  Democratic seats range:", min(dem_scores$dem_seats), "-", max(dem_scores$dem_seats), "\n")
    cat("  Best Democratic plan:", summaries$democratic$best_score, "Democratic seats\n")
    cat("  Mean Democratic seats across ensemble:", round(mean(dem_scores$dem_seats), 2), "\n")
  }
}

# Summarize segregation results
summarize_segregation_results = function(all_results) {
  cat("\n=== REDISTRICTING RESULTS SUMMARY ===\n")
  
  summary_table = data.frame(
    segregation_level = character(),
    mean_dem_seats = numeric(),
    sd_dem_seats = numeric(),
    min_dem_seats = numeric(),
    max_dem_seats = numeric(),
    mean_minority_districts = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (level in c("low", "medium", "high")) {
    cat(paste("Processing level:", level, "\n"))
    
    # Check if summaries exist for this level
    if (is.null(all_results[[level]]) || is.null(all_results[[level]]$summaries)) {
      cat(paste("  No summaries found for", level, "level\n"))
      next
    }
    
    summaries = all_results[[level]]$summaries
    
    # Check if the required summary data exists
    if (is.null(summaries$neutral) && is.null(summaries$republican) && is.null(summaries$democratic)) {
      cat(paste("  No redistricting results found for", level, "level\n"))
      next
    }
    
    # Try to get map_summary and district_summary from available sources
    map_summary = NULL
    district_summary = NULL
    
    # Prioritize neutral results, then fall back to others
    if (!is.null(summaries$neutral)) {
      map_summary = summaries$neutral$map_all
      district_summary = summaries$neutral$district_all
      cat(paste("  Using neutral results for", level, "\n"))
    } else if (!is.null(summaries$republican)) {
      map_summary = summaries$republican$map_all
      district_summary = summaries$republican$district_all
      cat(paste("  Using republican results for", level, "\n"))
    } else if (!is.null(summaries$democratic)) {
      map_summary = summaries$democratic$map_all
      district_summary = summaries$democratic$district_all
      cat(paste("  Using democratic results for", level, "\n"))
    }
    
    # Check if we have the required data
    if (is.null(map_summary) || is.null(district_summary)) {
      cat(paste("  Missing required summary data for", level, "level\n"))
      next
    }
    
    # Check if data frames have required columns
    if (!"n_dem_districts" %in% names(map_summary)) {
      cat(paste("  Missing n_dem_districts column in map_summary for", level, "\n"))
      next
    }
    
    if (!"per_minority" %in% names(district_summary) || !"map_id" %in% names(district_summary)) {
      cat(paste("  Missing required columns in district_summary for", level, "\n"))
      next
    }
    
    # Calculate minority-majority districts with error handling
    minority_districts = tryCatch({
      district_summary %>%
        group_by(map_id) %>%
        summarise(n_minority_districts = sum(per_minority > 0.5, na.rm = TRUE), .groups = 'drop') %>%
        pull(n_minority_districts) %>%
        mean(na.rm = TRUE)
    }, error = function(e) {
      cat(paste("  Error calculating minority districts for", level, ":", e$message, "\n"))
      return(0)
    })
    
    # Add to summary table
    summary_table = rbind(summary_table, data.frame(
      segregation_level = level,
      mean_dem_seats = mean(map_summary$n_dem_districts, na.rm = TRUE),
      sd_dem_seats = sd(map_summary$n_dem_districts, na.rm = TRUE),
      min_dem_seats = min(map_summary$n_dem_districts, na.rm = TRUE),
      max_dem_seats = max(map_summary$n_dem_districts, na.rm = TRUE),
      mean_minority_districts = minority_districts,
      stringsAsFactors = FALSE
    ))
    
    cat(paste("  Successfully processed", level, "level\n"))
  }
  
  if (nrow(summary_table) == 0) {
    cat("No valid redistricting results found for any segregation level\n")
    return(NULL)
  }
  
  print(summary_table)
  
  # Test hypothesis only if we have results for low and high
  if (nrow(summary_table) >= 2) {
    low_row = summary_table[summary_table$segregation_level == "low", ]
    high_row = summary_table[summary_table$segregation_level == "high", ]
    
    if (nrow(low_row) > 0 && nrow(high_row) > 0) {
      cat("\n=== HYPOTHESIS TEST ===\n")
      cat("Expected: Lower segregation → More Democratic seats\n")
      
      low_seats = low_row$mean_dem_seats
      high_seats = high_row$mean_dem_seats
      
      diff = low_seats - high_seats
      cat(sprintf("Difference (Low - High): %.2f Democratic seats\n", diff))
      
      if (diff > 0) {
        cat("✓ Result supports hypothesis: Lower segregation yields more Democratic seats\n")
      } else {
        cat("✗ Result contradicts hypothesis: Lower segregation yields fewer Democratic seats\n")
      }
    } else {
      cat("Cannot perform hypothesis test: missing low or high segregation results\n")
    }
  } else {
    cat("Cannot perform hypothesis test: insufficient data\n")
  }
  
  return(summary_table)
}

# Helper function to save simulation parameters
save_simulation_parameters = function(simulation_results,
                                      output_dir,
                                      random_seed,
                                      timestamp,
                                      n_precincts,
                                      input_data,
                                      pop_mean,
                                      pop_sd,
                                      minority_mean,
                                      minority_sd,
                                      voting_model,
                                      prob_minority_dem,
                                      prob_majority_dem,
                                      sd_minority,
                                      sd_majority,
                                      rho,
                                      b_min_context,
                                      b_maj_context,
                                      sigma_precinct,
                                      field_rho,   
                                      shared_field_wt, 
                                      shared_field_rho,
                                      n_centers,
                                      decay_rate,
                                      base_density,
                                      peak_multiplier,
                                      decay_power,
                                      center_radius,
                                      n_districts,
                                      n_plans,
                                      pop_deviation,
                                      ensemble_size,
                                      dev_mode,
                                      burst_length,
                                      num_bursts,    
                                      patience_bursts,
                                      soft_k,
                                      simplify_tolerance,
                                      score_models) {
  
  params_text = paste0(
    "SIMULATION PARAMETERS\n",
    "====================\n\n",
    
    "Seed Parameters:\n",
    sprintf("%-25s = %d\n", "random_seed", random_seed),
    sprintf("%-25s = %s\n", "timestamp", timestamp),
    "\n",
    
    "Population Parameters:\n",
    sprintf("%-25s = %d\n", "n_precincts", n_precincts),
    sprintf(
      "%-25s = %s\n",
      "input_data",
      ifelse(is.null(input_data), "None (simulated)", "Provided")
    ),
    sprintf("%-25s = %.0f\n", "pop_mean", pop_mean),
    sprintf("%-25s = %.0f\n", "pop_sd", pop_sd),
    sprintf("%-25s = %.2f\n", "minority_mean", minority_mean),
    sprintf("%-25s = %.2f\n", "minority_sd", minority_sd),
    "\n",
    
    "Voting Model Parameters:\n",
    sprintf("%-25s = %s\n", "voting_model", voting_model),
    sprintf("%-25s = %.2f\n", "prob_minority_dem", prob_minority_dem),
    sprintf("%-25s = %.2f\n", "prob_majority_dem", prob_majority_dem),
    sprintf("%-25s = %.2f\n", "sd_minority", sd_minority),
    sprintf("%-25s = %.2f\n", "sd_majority", sd_majority),
    sprintf("%-25s = %.2f\n", "rho", rho),
    sprintf("%-25s = %.2f\n", "b_min_context", b_min_context),
    sprintf("%-25s = %.2f\n", "b_maj_context", b_maj_context),
    sprintf("%-25s = %.2f\n", "sigma_precinct", sigma_precinct),
    sprintf("%-25s = %.2f\n", "field_rho", field_rho),
    sprintf("%-25s = %.2f\n", "shared_field_wt", shared_field_wt),
    sprintf("%-25s = %.2f\n", "shared_field_rho", shared_field_rho),
    "\n",
    
    "Spatial Grid Parameters:\n",
    sprintf("%-25s = %d\n", "n_centers", n_centers),
    sprintf("%-25s = %.2f\n", "decay_rate", decay_rate),
    sprintf("%-25s = %.2f\n", "base_density", base_density),
    sprintf("%-25s = %.2f\n", "peak_multiplier", peak_multiplier),
    sprintf("%-25s = %.2f\n", "decay_power", decay_power),
    sprintf("%-25s = %.2f\n", "center_radius", center_radius),
    # sprintf("%-25s = %.2f\n", "voting_weight", voting_weight),
    "\n",
    
    "Redistricting Parameters:\n",
    sprintf("%-25s = %d\n", "n_districts", n_districts),
    sprintf("%-25s = %d\n", "n_plans", n_plans),
    sprintf("%-25s = %d\n", "ensemble_size", ensemble_size),
    sprintf("%-25s = %.2f\n", "pop_deviation", pop_deviation),
    
    "Dev Mode Parameters:\n",
    sprintf("%-25s = %s\n", "dev_mode", ifelse(dev_mode, "Enabled", "Disabled")),
    sprintf("%-25s = %d\n", "burst_length", burst_length),
    sprintf("%-25s = %d\n", "num_bursts", num_bursts),
    sprintf("%-25s = %d\n", "patience_bursts", patience_bursts),
    sprintf("%-25s = %d\n", "soft_k", soft_k),
    sprintf("%-25s = %s\n", "simplify_tolerance", 
            ifelse(is.na(simplify_tolerance), "None", as.character(simplify_tolerance))),
    sprintf("%-25s = %s\n", "score_models", ifelse(score_models, "Enabled", "Disabled"))
  )
  
  # Add summary statistics (unchanged)
  fixed_pop_data = simulation_results[["low"]]$precincts
  total_pop = sum(fixed_pop_data$population)
  total_minority = sum(fixed_pop_data$n_minority)
  total_majority = sum(fixed_pop_data$n_majority)
  total_dem = sum(fixed_pop_data$dem_votes)
  total_rep = sum(fixed_pop_data$rep_votes)
  minority_pct = total_minority / total_pop
  
  summary_stats = paste0(
    "\nSummary Statistics:\n",
    sprintf(
      "%-25s = %s\n",
      "Overall population",
      format(total_pop, big.mark = ",")
    ),
    sprintf(
      "%-25s = %.1f%%\n",
      "Overall percent minority",
      minority_pct * 100
    ),
    sprintf(
      "%-25s = %s\n",
      "Total minority voters",
      format(total_minority, big.mark = ",")
    ),
    sprintf(
      "%-25s = %s\n",
      "Total majority voters",
      format(total_majority, big.mark = ",")
    ),
    sprintf(
      "%-25s = %s\n",
      "Total Democratic votes",
      format(total_dem, big.mark = ",")
    ),
    sprintf(
      "%-25s = %s\n",
      "Total Republican votes",
      format(total_rep, big.mark = ",")
    ),
    sprintf(
      "%-25s = %.3f\n",
      "Minority % mean",
      mean(fixed_pop_data$per_minority)
    ),
    sprintf(
      "%-25s = %.3f\n",
      "Minority % SD",
      sd(fixed_pop_data$per_minority)
    ),
    sprintf(
      "%-25s = %.2f - %.2f\n",
      "Minority % range",
      min(fixed_pop_data$per_minority),
      max(fixed_pop_data$per_minority)
    ),
    sprintf("%-25s = %d\n", "Number of precincts", nrow(fixed_pop_data)),
    sprintf(
      "%-25s = %d\n",
      "Precincts with 0% minority",
      sum(fixed_pop_data$per_minority == 0)
    ),
    sprintf(
      "%-25s = %d\n",
      "Precincts with 100% minority",
      sum(fixed_pop_data$per_minority == 1)
    )
  )
  
  writeLines(
    paste0(params_text, summary_stats),
    paste0(output_dir, "simulation_parameters.txt")
  )
  
  cat("Saved simulation parameters to simulation_parameters.txt\n")
}

# Run redistricting analysis
run_redistricting = function(shapefile_path,
                             output_dir,
                             num_steps            = 1000,
                             ensemble_size        = 50,
                             pop_deviation        = 0.01,
                             num_districts        = NULL,
                             target_pop           = NULL,
                             dev_mode             = TRUE,
                             burst_length         = 250,
                             num_bursts           = 20,
                             patience_bursts      = 8,
                             soft_k               = 60,
                             simplify_tolerance   = NA) {
  
  python_script = "Python/run_recom_short_bursts.py"
  
  message("Loading Python script:", python_script)
  py_run_file(python_script)
  
  # Create clean directory structure
  subdirs = c("neutral", "republican", "democratic")
  
  for (subdir in subdirs) {
    dir.create(paste0(output_dir, subdir), recursive = TRUE, showWarnings = FALSE)
  }
  
  results = list()
  
  # 1. Neutral redistricting
  message("\nRunning neutral redistricting analysis...")
  results$neutral = py$run_redistricting_analysis(
    shapefile_path = shapefile_path,
    output_dir = paste0(output_dir, "neutral/"),
    num_steps = as.integer(num_steps),
    pop_deviation = as.numeric(pop_deviation),
    num_districts = as.integer(num_districts)
  )
  
  # 2. Republican gerrymander ensemble
  message("\nRunning Republican gerrymandering ensemble...")
  results$republican = py$create_biased_ensemble(
    shapefile_path = shapefile_path,
    output_dir = paste0(output_dir, "republican/"),
    ensemble_size = as.integer(ensemble_size),
    bias_type = "republican",
    pop_deviation = as.numeric(pop_deviation),
    num_districts = as.integer(num_districts),
    # Dev mode parameters
    dev_mode           = dev_mode,
    burst_length       = as.integer(burst_length),
    num_bursts         = as.integer(num_bursts),
    patience_bursts    = as.integer(patience_bursts),
    soft_k             = as.numeric(soft_k),
    simplify_tolerance = if (is.na(simplify_tolerance)) NULL else as.numeric(simplify_tolerance)
  )
  
  # 3. Democratic gerrymander ensemble
  message("\nRunning Democratic gerrymandering ensemble...")
  results$democratic = py$create_biased_ensemble(
    shapefile_path = shapefile_path,
    output_dir = paste0(output_dir, "democratic/"),
    ensemble_size = as.integer(ensemble_size),
    bias_type = "democratic",
    pop_deviation = as.numeric(pop_deviation),
    num_districts = as.integer(num_districts),
    # Dev mode parameters
    dev_mode           = dev_mode,
    burst_length       = as.integer(burst_length),
    num_bursts         = as.integer(num_bursts),
    patience_bursts    = as.integer(patience_bursts),
    soft_k             = as.numeric(soft_k),
    simplify_tolerance = if (is.na(simplify_tolerance)) NULL else as.numeric(simplify_tolerance)
  )
  
  return(results)
}


# ===================================================================
# ==================== PART 3b: PLAN SCORING ========================
# ===================================================================

score_plans = function(map_data,
                       plan_data,
                       plan_type,
                       output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  save_plan_summaries(
    map_data = map_data,
    plan_data = plan_data,
    plan_type = plan_type,
    output_dir = output_dir
  )
}

evaluate_vote_model_on_fixed_plans = function(level_dir,
                                              base_precincts_sf,
                                              vote_model_name,
                                              vm_params) {
  scored = do.call(apply_vote_model, c(list(spatial_precincts = base_precincts_sf), vm_params))
  
  # Load plan files written by Python
  neutral_file = file.path(level_dir, "neutral/CD_plans.csv")
  rep_file     = file.path(level_dir, "republican/CD_plans.csv")
  dem_file     = file.path(level_dir, "democratic/CD_plans.csv")
  
  out_dir = file.path(level_dir, "models", vote_model_name)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  
  summaries = list()
  if (file.exists(neutral_file))
    summaries$neutral    = score_plans(
      scored,
      fread(neutral_file) %>% select(precinct_id, init_CD, everything()),
      "neutral",
      file.path(out_dir, "neutral")
    )
  if (file.exists(rep_file))
    summaries$republican = score_plans(scored,
                                       fread(rep_file),
                                       "republican",
                                       file.path(out_dir, "republican"))
  if (file.exists(dem_file))
    summaries$democratic = score_plans(scored,
                                       fread(dem_file),
                                       "democratic",
                                       file.path(out_dir, "democratic"))
  summaries
}



# ===================================================================
# =============== PART 4: VISUALIZATION FUNCTIONS ===================
# ===================================================================

# 1. PRECINCT-LEVEL VISUALIZATIONS (no districts)

# Show urbanicity, minority %, and dem vote share for a single map
plot_precinct_characteristics = function(map_data) {
  # Formerly plot_agg_bias
  
  p1 = ggplot(map_data) +
    geom_sf(aes(fill = urb), color = NA) +
    scale_fill_gradient(low = "#E0F2F1", high = "#00695C") +
    theme_void() +
    theme(legend.position = 'right') +
    ggtitle('Urbanicity')
  
  p2 = ggplot(map_data) +
    geom_sf(aes(fill = pct_min), color = NA) +
    scale_fill_gradient2(low = "#F3E5F5", mid = "#B38AE7", high = "#4A148C",
                         midpoint = median(map_data$pct_min)) +
    theme_void() +
    theme(legend.position = 'right') +
    ggtitle('Percent Minority')
  
  p3 = ggplot(map_data) +
    geom_sf(aes(fill = dem_vsh), color = NA) +
    scale_fill_gradient2(low = "#D32F2F", mid = "#F5F5F5", high = "#1976D2",
                         midpoint = 0.5) +
    theme_void() +
    theme(legend.position = 'right') +
    ggtitle('Democratic Vote Share')
  
  comb_plot = p1 + p2 + p3 + plot_layout(nrow = 1, ncol = 3)
  return(comb_plot)
}

# Compare minority % across all three segregation levels
plot_segregation_levels_comparison = function(results, output_dir = NULL) {
  # Formerly visualize_segregation_comparison
  
  plots = list()
  
  for(level in c("low", "medium", "high")) {
    p = ggplot(results[[level]]$precincts) +
      geom_sf(aes(fill = per_minority), color = NA) +
      scale_fill_gradient2(low = "white", mid = "purple", high = "black", 
                           midpoint = 0.5, limits = c(0, 1),
                           labels = scales::percent) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = paste(stringr::str_to_title(level), "Segregation"),
           fill = "Minority %")
    
    plots[[level]] = p
  }
  
  combined_plot = plots[["low"]] + plots[["medium"]] + plots[["high"]] + 
    plot_layout(nrow = 1) 
  
  if (!is.null(output_dir)) {
    ggsave(paste0(output_dir, "segregation_comparison.pdf"), 
           combined_plot, width = 15, height = 5, dpi = 300)
  }
  
  return(combined_plot)
}

# Compare dem vote share across all three segregation levels
plot_voteshare_levels_comparison = function(results, output_dir = NULL) {
  # Formerly visualize_dem_voteshare_comparison
  
  plots = list()
  
  for(level in c("low", "medium", "high")) {
    p = ggplot(results[[level]]$precincts) +
      geom_sf(aes(fill = dem_voteshare), color = NA) +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                           midpoint = 0.5, limits = c(0, 1),
                           labels = scales::percent) +
      theme_void() +
      theme(legend.position = "bottom") +
      labs(title = paste(stringr::str_to_title(level), "Segregation"),
           fill = "Dem Vote %")
    
    plots[[level]] = p
  }
  
  combined_plot = plots[["low"]] + plots[["medium"]] + plots[["high"]] + 
    plot_layout(nrow = 1) 
  
  if (!is.null(output_dir)) {
    ggsave(paste0(output_dir, "dem_voteshare_comparison.pdf"), 
           combined_plot, width = 15, height = 5, dpi = 300)
  }
  
  return(combined_plot)
}

# Combined comparison showing both minority % and voteshare
plot_segregation_and_voteshare_grid = function(results, output_dir = NULL) {
  # Formerly visualize_segregation_and_voteshare_comparison
  
  minority_plots = list()
  voteshare_plots = list()
  
  for(level in c("low", "medium", "high")) {
    # Minority percentage plots
    minority_plots[[level]] = ggplot(results[[level]]$precincts) +
      geom_sf(aes(fill = per_minority), color = NA) +
      scale_fill_gradient2(low = "white", mid = "purple", high = "black", 
                           midpoint = 0.5, limits = c(0, 1),
                           labels = scales::percent) +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            plot.title = element_text(size = 10, hjust = 0.5)) +
      labs(title = paste(stringr::str_to_title(level))) +
      guides(fill = "none")
    
    # Democratic voteshare plots
    voteshare_plots[[level]] = ggplot(results[[level]]$precincts) +
      geom_sf(aes(fill = dem_voteshare), color = NA) +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", 
                           midpoint = 0.5, limits = c(0, 1),
                           labels = scales::percent) +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 7),
            plot.title = element_text(size = 10, hjust = 0.5)) +
      labs(title = paste(stringr::str_to_title(level))) +
      guides(fill = "none")
  }
  
  # Combine all plots in a 2x3 grid
  combined_plot = (minority_plots[["low"]] | minority_plots[["medium"]] | minority_plots[["high"]]) /
    (voteshare_plots[["low"]] | voteshare_plots[["medium"]] | voteshare_plots[["high"]]) 
  
  if (!is.null(output_dir)) {
    ggsave(paste0(output_dir, "segregation_and_voteshare_comparison.pdf"), 
           combined_plot, width = 15, height = 10, dpi = 300)
  }
  
  # Also save individual comparisons
  minority_only = minority_plots[["low"]] + minority_plots[["medium"]] + minority_plots[["high"]] + 
    plot_layout(nrow = 1) +
    plot_annotation(title = "Spatial Distribution of Minority Population Under Different Segregation Levels")
  
  voteshare_only = voteshare_plots[["low"]] + voteshare_plots[["medium"]] + voteshare_plots[["high"]] + 
    plot_layout(nrow = 1) +
    plot_annotation(title = "Spatial Distribution of Democratic Vote Share Under Different Segregation Levels")
  
  if (!is.null(output_dir)) {
    ggsave(paste0(output_dir, "minority_comparison.pdf"), 
           minority_only, width = 15, height = 5, dpi = 300)
    ggsave(paste0(output_dir, "dem_voteshare_comparison.pdf"), 
           voteshare_only, width = 15, height = 5, dpi = 300)
  }
  
  return(list(
    combined = combined_plot,
    minority = minority_only,
    voteshare = voteshare_only
  ))
}

# 2. DISTRICT-LEVEL VISUALIZATIONS

# Show single district map with party colors
plot_district_winners = function(map_data, title) {
  # Formerly create_district_visualization
  
  # Calculate district-level statistics
  cd_summary = map_data %>% 
    st_drop_geometry() %>%
    group_by(district_id) %>%
    summarise(
      dem_voteshare = sum(dem_votes) / sum(population),
      .groups = 'drop'
    ) %>%
    mutate(party = ifelse(dem_voteshare > 0.5, "Democrat", "Republican"))
  
  # Create district boundaries
  cd_borders = map_data %>%
    group_by(district_id) %>%
    summarise(geometry = st_union(geometry)) %>%
    left_join(cd_summary, by = "district_id")
  
  # Count seats
  dem_seats = sum(cd_summary$party == "Democrat")
  rep_seats = sum(cd_summary$party == "Republican")
  
  # Create plot
  ggplot(cd_borders) +
    geom_sf(aes(fill = party), color = "black", linewidth = 0.5) +
    scale_fill_manual(values = c("Democrat" = "blue", "Republican" = "red")) +
    theme_void() +
    theme(legend.position = "bottom") +
    labs(title = title,
         subtitle = paste0("D: ", dem_seats, " seats, R: ", rep_seats, " seats"))
}

# Show precinct-level data with district boundaries overlaid
plot_precincts_with_districts = function(map_data, CD_plans, map_ids, category_name, output_dir) {
  # This is the 3-panel view: minority %, dem voteshare, district winners
  
  # Sample one plan from the category
  sample_id = sample(map_ids, 1)
  
  # base column offset depending on presence of init_CD
  base_col = if ("init_CD" %in% names(CD_plans)) 2 else 1
  col_index = base_col + sample_id
  
  # align by precinct_id (robust to ordering)
  ord = match(map_data$precinct_id, CD_plans$precinct_id)
  stopifnot(!any(is.na(ord)))
  map_data$district_id = as.integer(CD_plans[[col_index]][ord])
  
  # Calculate district-level statistics for the seat winner plot
  cd_summary = map_data %>% 
    st_drop_geometry() %>%
    # mutate(
    #   dem_vote_count = dem_voteshare * population,
    #   minority_voter_count = per_minority * population,
    #   majority_voter_count = population - minority_voter_count
    # ) %>% 
    group_by(district_id) %>%
    summarise(
      n_precincts = n(),
      population = sum(population),
      n_minority = sum(n_minority),
      n_majority = sum(n_majority),
      per_minority = n_minority / population,
      dem_votes = sum(dem_votes),
      dem_voteshare = dem_votes / population, 
      dem_cd = ifelse(dem_voteshare > 0.5, 1, 0)
    )
  
  # Merge district-level data back to map data
  plot_df = map_data %>% 
    left_join(
      cd_summary %>% select(district_id, dem_cd, dem_voteshare), 
      by = "district_id",
      suffix = c("", "_district")
    )
  
  # Create district boundaries by aggregating precinct polygons
  cd_borders = map_data %>%
    group_by(district_id) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  # Plot 1: District-level seat winners (red/blue)
  p1 = ggplot(plot_df) +
    geom_sf(aes(fill = as.factor(dem_cd), color = as.factor(dem_cd)), alpha = 0.75) +
    geom_sf(data = cd_borders, fill = NA, color = "darkred", linewidth = 0.8) +
    scale_fill_manual(values = c('1' = 'blue', '0' = 'red'),
                      labels = c('1' = 'D', '0' = 'R'),
                      name = '') +
    scale_color_manual(values = c('1' = 'blue', '0' = 'red'),
                       labels = c('1' = 'D', '0' = 'R'),
                       name = '') +
    theme_void() +
    guides(fill = 'none', color = 'none')
  
  # Plot 2: Democratic vote share at precinct level with district boundaries
  p2 = ggplot() +
    geom_sf(data = map_data, aes(fill = dem_voteshare), color = NA) +
    geom_sf(data = cd_borders, fill = NA, color = "darkred", linewidth = 0.8) +
    scale_fill_gradient2(low = "#D32F2F", mid = "#F5F5F5", high = "#1976D2",
                         midpoint = 0.5,
                         labels = scales::percent,
                         name = "Dem %") +
    theme_void() +
    guides(fill = 'none', color = 'none')
  
  # Plot 3: Minority population at precinct level with district boundaries
  p3 = ggplot() +
    geom_sf(data = map_data, aes(fill = per_minority), color = NA) +
    geom_sf(data = cd_borders, fill = NA, color = "darkred", linewidth = 0.8) +
    scale_fill_gradient(low = "lightgrey", high = "black",
                        labels = scales::percent,
                        name = "% Min") +
    theme_void() +
    guides(fill = 'none', color = 'none')
  
  # Combine all three plots in a row
  comb_plot = p3 + p2 + p1 + plot_layout(nrow = 1, ncol = 3)
  
  # Save the plot with a sanitized filename
  sanitized_category = gsub("[%]", "pct", gsub("[ ]", "_", tolower(category_name)))
  plot_filename = paste0(output_dir, sanitized_category, '_precinct_district_map.pdf')
  ggsave(plot_filename, comb_plot, width = 18, height = 6, dpi = 300)
  
  return(comb_plot)
}

# 3. ENSEMBLE/DISTRIBUTION VISUALIZATIONS

# Show distribution of seats for a single ensemble
plot_ensemble_seat_distribution = function(scores, party_type, segregation_level, output_dir) {
  # Extract from create_ensemble_visualizations
  
  if (party_type == "republican") {
    seat_col = "rep_seats"
    color = "red"
    dark_color = "darkred"
    label = "Republican Seats"
  } else {
    seat_col = "dem_seats"
    color = "blue"
    dark_color = "darkblue"
    label = "Democratic Seats"
  }
  
  plot = ggplot(scores, aes_string(x = seat_col)) +
    geom_histogram(binwidth = 1, fill = color, alpha = 0.7, color = dark_color) +
    geom_vline(xintercept = max(scores[[seat_col]]), linetype = "dashed", color = dark_color, size = 1) +
    labs(
      title = paste(stringr::str_to_title(party_type), "Gerrymander Ensemble -", stringr::str_to_title(segregation_level)),
      subtitle = paste("Best:", max(scores[[seat_col]]), "seats out of", nrow(scores), "attempts"),
      x = label,
      y = "Number of Plans"
    ) +
    theme_minimal()
  
  ggsave(paste0(output_dir, party_type, "_ensemble_distribution.pdf"), 
         plot, width = 10, height = 6, dpi = 300)
  
  return(plot)
}

# Compare seat distributions across redistricting types for one segregation level
plot_redistricting_comparison = function(summaries, segregation_level, output_dir) {
  # Formerly create_seat_distribution_comparison_ensemble
  
  comparison_data = data.frame()
  
  # Add neutral distribution if available
  if (!is.null(summaries$neutral)) {
    neutral_data = data.frame(
      type = "Neutral",
      dem_seats = summaries$neutral$map_all$n_dem_districts
    )
    comparison_data = rbind(comparison_data, neutral_data)
  }
  
  # Add ensemble results
  if (!is.null(summaries$republican)) {
    rep_data = data.frame(
      type = "Republican\nGerrymander",
      dem_seats = summaries$republican$plan_scores$dem_seats
    )
    comparison_data = rbind(comparison_data, rep_data)
  }
  
  if (!is.null(summaries$democratic)) {
    dem_data = data.frame(
      type = "Democratic\nGerrymander", 
      dem_seats = summaries$democratic$plan_scores$dem_seats
    )
    comparison_data = rbind(comparison_data, dem_data)
  }
  
  if (nrow(comparison_data) > 0) {
    comparison_plot = ggplot(comparison_data, aes(x = dem_seats, fill = type)) +
      geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
      scale_fill_manual(values = c(
        "Neutral" = "gray50",
        "Republican\nGerrymander" = "red",
        "Democratic\nGerrymander" = "blue"
      )) +
      scale_x_continuous(breaks = 0:14) +
      labs(
        title = paste("Democratic Seat Distribution -", stringr::str_to_title(segregation_level), "Segregation"),
        x = "Number of Democratic Seats",
        y = "Count",
        fill = "Redistricting\nType"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave(
      paste0(output_dir, "seat_distribution_comparison.pdf"),
      comparison_plot,
      width = 10,
      height = 6,
      dpi = 300
    )
    
    return(comparison_plot)
  }
}

# Compare seat distributions across all segregation levels
plot_cross_level_comparison = function(all_results, output_dir, n_districts) {
  # Formerly create_cross_level_comparisons_ensemble
  
  comparison_data = data.frame()
  
  for (level in names(all_results)) {
    if (!is.null(all_results[[level]]$summaries)) {
      
      # Add neutral results
      if (!is.null(all_results[[level]]$summaries$neutral)) {
        neutral_data = data.frame(
          segregation_level = level,
          redistricting_type = "Neutral",
          dem_seats = all_results[[level]]$summaries$neutral$map_all$n_dem_districts
        )
        comparison_data = rbind(comparison_data, neutral_data)
      }
      
      # Add ALL ensemble members
      if (!is.null(all_results[[level]]$summaries$republican)) {
        rep_data = data.frame(
          segregation_level = level,
          redistricting_type = "Republican",
          dem_seats = all_results[[level]]$summaries$republican$plan_scores$dem_seats
        )
        comparison_data = rbind(comparison_data, rep_data)
      }
      
      if (!is.null(all_results[[level]]$summaries$democratic)) {
        dem_data = data.frame(
          segregation_level = level,
          redistricting_type = "Democratic",
          dem_seats = all_results[[level]]$summaries$democratic$plan_scores$dem_seats
        )
        comparison_data = rbind(comparison_data, dem_data)
      }
    }
  }
  
  if (nrow(comparison_data) > 0) {
    comparison_data$segregation_level = factor(
      comparison_data$segregation_level,
      levels = c("low", "medium", "high")
    )
    
    comparison_plot = ggplot(comparison_data, aes(x = dem_seats, fill = redistricting_type)) +
      geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
      facet_wrap(~ segregation_level, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c(
        "Neutral" = "gray50",
        "Republican" = "red",
        "Democratic" = "blue"
      )) +
      scale_x_continuous(breaks = 0:n_districts) +
      labs(
        title = "Democratic Seat Distribution Across Segregation Levels",
        x = "Number of Democratic Seats",
        y = "Count",
        fill = "Redistricting Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave(
      paste0(output_dir, "cross_level_comparison.pdf"),
      comparison_plot,
      width = 10,
      height = 12,
      dpi = 300
    )
    
    # Create summary table
    summary_table = comparison_data %>%
      group_by(segregation_level, redistricting_type) %>%
      summarise(
        mean_dem_seats = mean(dem_seats),
        sd_dem_seats = sd(dem_seats),
        min_dem_seats = min(dem_seats),
        max_dem_seats = max(dem_seats),
        .groups = 'drop'
      )
    
    write.csv(summary_table, paste0(output_dir, "summary_table.csv"), row.names = FALSE)
    print(summary_table)
    
    return(list(plot = comparison_plot, table = summary_table))
  }
}

# 4. MAIN VISUALIZATION CREATION FUNCTIONS (updated to use new names)

# Create initial visualizations for a segregation level
create_initial_visualizations = function(precincts, map_data_formatted, output_dir, level) {
  
  figures_dir = paste0(output_dir, "figures/")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create segregation visualization
  seg_plot = ggplot(precincts) +
    geom_sf(aes(fill = per_minority), color = "white", linewidth = 0.1) +
    scale_fill_viridis(option = "plasma", labels = scales::percent) +
    theme_void() +
    ggtitle(paste(stringr::str_to_title(level), "Segregation - Minority Distribution"))
  
  ggsave(
    paste0(figures_dir, "segregation_pattern.pdf"),
    plot = seg_plot,
    width = 10,
    height = 8,
    dpi = 300
  )
  
  # Create precinct characteristics visualization (urbanicity, minority %, dem vote)
  precinct_chars_plot = plot_precinct_characteristics(map_data_formatted)
  
  ggsave(
    paste0(figures_dir, "aggregation_bias.pdf"),
    plot = precinct_chars_plot,
    width = 10,
    height = 8,
    dpi = 300
  )
}

# Create all visualizations for redistricting results
create_redistricting_visualizations = function(map_data, summaries, CD_plans_neutral, 
                                               output_dir, segregation_level) {
  
  figures_dir = paste0(output_dir, "figures/")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  
  plots = list()
  
  # Create district winner maps for each redistricting type
  
  # -- Democratic
  if (!is.null(summaries$democratic)) {
    dem_plans_file = paste0(output_dir, "democratic/CD_plans.csv")
    if (file.exists(dem_plans_file)) {
      dem_plans = fread(dem_plans_file)
      best_dem_idx = which.max(summaries$democratic$plan_scores$dem_seats)
      base_col = if ("init_CD" %in% names(dem_plans)) 2 else 1  # here it's 1
      col_index = base_col + best_dem_idx
      ord = match(map_data$precinct_id, dem_plans$precinct_id)
      map_data$district_id = as.integer(dem_plans[[col_index]][ord])
      plots$democratic = plot_district_winners(
        map_data,
        paste("Democratic Gerrymander (Best of", summaries$democratic$ensemble_size, ")")
      )
    }
  }
  
  # -- Republican
  if (!is.null(summaries$republican)) {
    rep_plans_file = paste0(output_dir, "republican/CD_plans.csv")
    if (file.exists(rep_plans_file)) {
      rep_plans = fread(rep_plans_file)
      best_rep_idx = which.max(summaries$republican$plan_scores$rep_seats)
      base_col = if ("init_CD" %in% names(rep_plans)) 2 else 1  # here it's 1
      col_index = base_col + best_rep_idx
      ord = match(map_data$precinct_id, rep_plans$precinct_id)
      map_data$district_id = as.integer(rep_plans[[col_index]][ord])
      plots$republican = plot_district_winners(
        map_data,
        paste("Republican Gerrymander (Best of", summaries$republican$ensemble_size, ")")
      )
    }
  }
  
  # Neutral
  if (!is.null(CD_plans_neutral) && !is.null(summaries$neutral)) {
    median_dem_seats = median(summaries$neutral$map_all$n_dem_districts)
    representative_map = summaries$neutral$map_all %>%
      filter(n_dem_districts == median_dem_seats) %>%
      slice(1) %>%
      pull(map_id)
    
    if (length(representative_map) == 0) {
      representative_map = 1
    }
    
    col_index = 2 + representative_map
    map_data$district_id = CD_plans_neutral[[col_index]]
    
    plots$neutral = plot_district_winners(map_data, "Neutral Redistricting")
  }
  
  # Combine district winner plots
  if (length(plots) > 0) {
    combined_plot = wrap_plots(plots, ncol = 3)
    ggsave(
      paste0(figures_dir, "redistricting_comparison.pdf"),
      combined_plot,
      width = 18,
      height = 6,
      dpi = 300
    )
  }
  
  # Create seat distribution comparison
  plot_redistricting_comparison(summaries, segregation_level, figures_dir)
  
  # Create precinct-level visualizations with district boundaries
  cat("\nCreating precinct-level visualizations with district boundaries...\n")
  
  # Neutral plans
  if (!is.null(CD_plans_neutral) && !is.null(summaries$neutral)) {
    neutral_map_ids = 1:(ncol(CD_plans_neutral) - 2)
    plot_precincts_with_districts(
      map_data = map_data,
      CD_plans = CD_plans_neutral,
      map_ids = neutral_map_ids,
      category_name = paste("Neutral -", stringr::str_to_title(segregation_level), "Segregation"),
      output_dir = figures_dir
    )
  }
  
  # Republican ensemble
  if (!is.null(summaries$republican)) {
    rep_plans_file = paste0(output_dir, "republican/CD_plans.csv")
    if (file.exists(rep_plans_file)) {
      rep_plans = fread(rep_plans_file)
      best_rep_plan_idx = which.max(summaries$republican$plan_scores$rep_seats)
      plot_precincts_with_districts(
        map_data = map_data,
        CD_plans = rep_plans,
        map_ids = best_rep_plan_idx,
        category_name = paste("Republican Gerrymander -", stringr::str_to_title(segregation_level), "Segregation"),
        output_dir = figures_dir
      )
    }
  }
  
  # Democratic ensemble
  if (!is.null(summaries$democratic)) {
    dem_plans_file = paste0(output_dir, "democratic/CD_plans.csv")
    if (file.exists(dem_plans_file)) {
      dem_plans = fread(dem_plans_file)
      best_dem_plan_idx = which.max(summaries$democratic$plan_scores$dem_seats)
      plot_precincts_with_districts(
        map_data = map_data,
        CD_plans = dem_plans,
        map_ids = best_dem_plan_idx,
        category_name = paste("Democratic Gerrymander -", stringr::str_to_title(segregation_level), "Segregation"),
        output_dir = figures_dir
      )
    }
  }
  
  # Create individual ensemble distributions
  create_ensemble_visualizations(summaries, output_dir, segregation_level)
}

# Create ensemble-specific visualizations
create_ensemble_visualizations = function(summaries, output_dir, segregation_level) {
  
  figures_dir = paste0(output_dir, "figures/")
  dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Republican ensemble visualization
  if (!is.null(summaries$republican) && !is.null(summaries$republican$plan_scores)) {
    plot_ensemble_seat_distribution(
      summaries$republican$plan_scores,
      "republican",
      segregation_level,
      figures_dir
    )
  }
  
  # Democratic ensemble visualization  
  if (!is.null(summaries$democratic) && !is.null(summaries$democratic$plan_scores)) {
    plot_ensemble_seat_distribution(
      summaries$democratic$plan_scores,
      "democratic", 
      segregation_level,
      figures_dir
    )
  }
  
  # Combined comparison plot
  if (!is.null(summaries$republican) && !is.null(summaries$democratic)) {
    combined_data = bind_rows(
      summaries$republican$plan_scores %>% mutate(plan_type = "Republican"),
      summaries$democratic$plan_scores %>% mutate(plan_type = "Democratic")
    )
    
    combined_plot = ggplot(combined_data, aes(x = dem_seats, 
                                              fill = plan_type)) +
      geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
      scale_fill_manual(values = c("Republican" = "red", "Democratic" = "blue")) +
      labs(
        title = paste("Gerrymander Ensemble Comparison -", stringr::str_to_title(segregation_level)),
        x = "Seats Won by Democratic Party",
        y = "Number of Plans",
        fill = "Gerrymander Type"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave(paste0(figures_dir, "ensemble_comparison.pdf"),
           combined_plot, width = 12, height = 6, dpi = 300)
  }
}

# 5. GALLERY OF DISTRICTING PLANS

# Winners-by-district small map (precincts outlined) ----
plot_winner_small = function(map_sf, assignment_vec) {
  # map_sf must have: precinct_id, dem_votes, rep_votes, population, geometry
  # assignment_vec is integer vector of district ids aligned to map_sf$precinct_id
  
  # Align and attach district ids
  stopifnot("precinct_id" %in% names(map_sf))
  stopifnot(length(assignment_vec) == nrow(map_sf))
  map_sf$district_id = as.integer(assignment_vec)
  
  # Make sure we have vote COUNT columns
  vn = names(map_sf)
  if (!("dem_votes" %in% vn) && ("dem_v" %in% vn)) {
    map_sf = dplyr::rename(map_sf, dem_votes = dem_v)
  }
  if (!("rep_votes" %in% vn) && ("rep_v" %in% vn)) {
    map_sf = dplyr::rename(map_sf, rep_votes = rep_v)
  }
  if (!("population" %in% vn) && ("pop" %in% vn)) {
    map_sf = dplyr::rename(map_sf, population = pop)
  }
  
  # District winners
  cd_summary = map_sf |>
    sf::st_drop_geometry() |>
    dplyr::group_by(district_id) |>
    dplyr::summarise(
      dem_votes = sum(dem_votes, na.rm = TRUE),
      rep_votes = sum(rep_votes, na.rm = TRUE),
      population = sum(as.numeric(population), na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(party = ifelse(dem_votes >= rep_votes, "Democrat", "Republican"))
  
  # District polygons
  cd_polys = map_sf |>
    dplyr::group_by(district_id) |>
    dplyr::summarise(geometry = sf::st_union(geometry), .groups = "drop") |>
    dplyr::left_join(cd_summary, by = "district_id")
  
  # Seat counts (for a tiny caption)
  dem_seats = sum(cd_summary$party == "Democrat")
  rep_seats = sum(cd_summary$party == "Republican")
  
  ggplot() +
    geom_sf(data = cd_polys, aes(fill = party), color = NA) +                  # districts shaded by winner
    geom_sf(data = map_sf, fill = NA, color = "#2b2b2b", linewidth = 0.05) +  # precinct outlines
    scale_fill_manual(values = c("Democrat" = "#1f77b4", "Republican" = "#d62728")) +
    guides(fill = "none") +
    theme_void() +
    theme(plot.margin = margin(1, 1, 1, 1, "pt"))
}

#  Build a 5×3 gallery page for one segregation level ----
build_party_grid_page = function(level_dir, title = "Segregation Level") {
  # Load the shapefile written earlier
  shp = file.path(level_dir, "state_map.shp")
  stopifnot(file.exists(shp))
  map_sf = sf::st_read(shp, quiet = TRUE)
  # Standardize column names used downstream
  map_sf = dplyr::rename(
    map_sf,
    precinct_id = dplyr::coalesce("precinct_id", "pct_id"),
    population  = dplyr::coalesce("population",  "pop")
  )
  
  # Helper to read a plan csv safely
  read_plans = function(path) if (file.exists(path)) data.table::fread(path) else NULL
  
  # Plan matrices
  dem_df  = read_plans(file.path(level_dir, "democratic", "CD_plans.csv"))
  rep_df  = read_plans(file.path(level_dir, "republican", "CD_plans.csv"))
  neu_df  = read_plans(file.path(level_dir, "neutral",   "CD_plans.csv"))
  
  # Column pickers (5 random plans; sample with replacement if fewer exist)
  pick_cols = function(df) {
    if (is.null(df)) return(character(0))
    base_drop = intersect(c("precinct_id","init_CD"), names(df))
    plan_cols = setdiff(names(df), base_drop)
    if (!length(plan_cols)) return(character(0))
    sample(plan_cols, size = min(5, length(plan_cols)), replace = length(plan_cols) < 5)
  }
  
  dem_cols = pick_cols(dem_df)
  neu_cols = pick_cols(neu_df)
  rep_cols = pick_cols(rep_df)
  
  # Return placeholders if missing
  make_empty = function() {
    ggplot() + theme_void() + geom_text(aes(0,0,label="No plans"), inherit.aes = FALSE)
  }
  
  # One small plot builder for a specific column in a df
  make_small = function(df, colname) {
    if (is.null(df) || !length(colname)) return(make_empty())
    ord = match(map_sf$precinct_id, df$precinct_id)
    stopifnot(!any(is.na(ord)))
    assignment = as.integer(df[[colname]][ord])
    plot_winner_small(map_sf, assignment)
  }
  
  # Top row: DEM (5)
  row_dem = lapply(dem_cols, function(cn) make_small(dem_df, cn))
  while (length(row_dem) < 5) row_dem[[length(row_dem)+1]] = make_empty()
  
  # Middle row: NEUTRAL (5)
  row_neu = lapply(neu_cols, function(cn) make_small(neu_df, cn))
  while (length(row_neu) < 5) row_neu[[length(row_neu)+1]] = make_empty()
  
  # Bottom row: REP (5)
  row_rep = lapply(rep_cols, function(cn) make_small(rep_df, cn))
  while (length(row_rep) < 5) row_rep[[length(row_rep)+1]] = make_empty()
  
  # Assemble 3×5
  page = (wrap_plots(row_dem, ncol = 5) /
            wrap_plots(row_neu, ncol = 5) /
            wrap_plots(row_rep, ncol = 5)) +
    plot_annotation(
      title = title,
      subtitle = "Top row: Democratic gerrymanders   ·   Middle: Neutral   ·   Bottom: Republican gerrymanders"
    )
  page
}

# Export a single 3-page PDF (low/medium/high) ----
export_plan_gallery_3pages = function(main_output_dir, file_name = "plan_gallery_by_seg_level_plan_type.pdf") {
  levels = c("low","medium","high")
  pages  = list(
    build_party_grid_page(file.path(main_output_dir, "low"),    "LOW segregation"),
    build_party_grid_page(file.path(main_output_dir, "medium"), "MEDIUM segregation"),
    build_party_grid_page(file.path(main_output_dir, "high"),   "HIGH segregation")
  )
  out = file.path(main_output_dir, file_name)
  grDevices::pdf(out, width = 18, height = 10, onefile = TRUE)
  for (p in pages) print(p)
  grDevices::dev.off()
  message("Wrote 3-page gallery: ", out)
}



# ===================================================================
# =============== PART 5: MAIN ANALYSIS FUNCTIONS ===================
# ===================================================================


# Run complete simulation with fixed population
simulate_segregation_scenarios = function(n_precincts       = 2600,
                                          n_centers         = 5,
                                          input_data        = NULL,
                                          pop_mean          = 2300,
                                          pop_sd            = 1500,
                                          minority_mean     = 0.25,
                                          minority_sd       = 0.30,
                                          decay_rate        = 40,
                                          base_density      = 0.05,
                                          peak_multiplier   = 10.0,
                                          decay_power       = 2,
                                          center_radius     = 10.0,
                                          voting_model      = "ei",
                                          prob_minority_dem = 0.81,
                                          prob_majority_dem = 0.37,
                                          sd_minority       = 0.1,
                                          sd_majority       = 0.3,
                                          rho               = 0.2,
                                          b_min_context     = 0.1,
                                          b_maj_context     = 0.0,
                                          sigma_precinct    = 0.05,
                                          field_rho         = 0.75,   
                                          shared_field_wt   = 0.35,   
                                          shared_field_rho  = 0.85,
                                          seed              = 123) {
  
  # Step 1: Generate the fixed population
  cat("=== GENERATING FIXED POPULATION ===\n")
  fixed_population = create_population_data(
    n_precincts = n_precincts,
    input_data = input_data,
    pop_mean = pop_mean,
    pop_sd = pop_sd,
    minority_mean = minority_mean,
    minority_sd = minority_sd,
    seed = seed
  )
  
  # # (OLD) 
  # # Step 2: Assign voting preferences
  # cat("\n=== ASSIGNING VOTING PREFERENCES ===\n")
  # fixed_population_with_votes = add_voting_behavior(
  #   fixed_population,
  #   prob_minority_dem = prob_minority_dem,
  #   prob_majority_dem = prob_majority_dem,
  #   sd_minority = sd_minority,
  #   sd_majority = sd_majority,
  #   voting_model = voting_model,
  #   rho = rho,
  #   b_min_context = b_min_context,
  #   b_maj_context = b_maj_context,
  #   sigma_precinct = sigma_precinct,
  #   seed = seed + 1
  # )
  
  # Step 2: Assign voting preferences
  fixed_population = add_baseline_votes(
    fixed_population,
    prob_minority_dem, prob_majority_dem, sd_minority, sd_majority, rho,
    seed = seed + 1
  )
  
  # Step 3: Create spatial grid with better urban structure
  cat("\n=== CREATING SPATIAL GRID ===\n")
  grid_result = create_realistic_grid(
    n_precincts = n_precincts,
    n_centers = n_centers,
    decay_rate = decay_rate,
    base_density = base_density,
    peak_multiplier = peak_multiplier,
    decay_power = decay_power,
    center_radius = center_radius,
    seed = seed + 2
  )
  
  # Step 4: Assign population with improved spatial assignment
  results = list()
  
  for (level in c("low", "medium", "high")) {
    cat("\n--- Assigning population with",
        level,
        "segregation ---\n")
    
    spatial_precincts = place_voters_on_map(
      fixed_population,
      grid_result,
      segregation_level = level,
      seed = seed + which(c("low", "medium", "high") == level) + 2
    )
    
    # (NEW) assign votes *after* placement using spatial fields
    spatial_precincts = apply_vote_model(
      spatial_precincts  = spatial_precincts,
      voting_model       = voting_model,
      prob_minority_dem  = prob_minority_dem,
      prob_majority_dem  = prob_majority_dem,
      sd_minority        = sd_minority,
      sd_majority        = sd_majority,
      rho                = rho,
      b_min_context      = b_min_context,
      b_maj_context      = b_maj_context,
      sigma_precinct     = sigma_precinct,
      field_rho          = field_rho,   
      shared_field_wt    = shared_field_wt,   
      shared_field_rho   = shared_field_rho,
      use_base_probs     = TRUE, 
      base_shrink_lambda = 1.0,            # 0.6–0.8 for extra randomness
      seed               = seed + 10       # keep same across levels to isolate placement
    )
    
    results[[level]] = list(precincts = spatial_precincts, centers = grid_result$centers)
  }
  
  # Verify all three have identical totals
  cat("\n=== VERIFICATION ===\n")
  for (level in c("low", "medium", "high")) {
    total_pop = sum(results[[level]]$precincts$population)
    total_minority = sum(results[[level]]$precincts$n_minority)
    total_dem_votes = sum(results[[level]]$precincts$dem_votes)
    
    cat(
      level,
      "segregation: Pop =",
      format(total_pop, big.mark = ","),
      ", Minority =",
      format(total_minority, big.mark = ","),
      ", Dem votes =",
      format(total_dem_votes, big.mark = ","),
      "\n"
    )
  }
  
  return(results)
}

# Main redistricting loop with cleaner directory structure
analyze_redistricting_impact = function(output_base_dir      = "Output/Tests/",
                                        n_precincts          = 2600,
                                        n_centers            = 5,
                                        input_data           = NULL,
                                        pop_mean             = 2300,
                                        pop_sd               = 1500,
                                        minority_mean        = 0.25,
                                        minority_sd          = 0.30,
                                        n_plans              = 1000,
                                        ensemble_size        = 50,
                                        pop_deviation        = 0.01,
                                        n_districts          = 14,
                                        voting_model         = "ei",
                                        prob_minority_dem    = 0.81,
                                        prob_majority_dem    = 0.37,
                                        sd_minority          = 0.1,
                                        sd_majority          = 0.3,
                                        rho                  = 0.2,
                                        b_min_context        = 0.1,
                                        b_maj_context        = 0.0,
                                        sigma_precinct       = 0.05,
                                        field_rho            = 0.75,   
                                        shared_field_wt      = 0.35,   
                                        shared_field_rho     = 0.85,
                                        decay_rate           = 40,
                                        base_density         = 0.05,
                                        peak_multiplier      = 10.0,
                                        decay_power          = 2,
                                        center_radius        = 10.0,
                                        dev_mode             = TRUE,
                                        burst_length         = 250,
                                        num_bursts           = 20,
                                        patience_bursts      = 8,
                                        soft_k               = 60,
                                        simplify_tolerance   = NA,
                                        score_models         = FALSE,
                                        random_seed          = 123) {
  
  set.seed(random_seed)
  
  # Run the improved fixed population simulation
  cat("=== RUNNING SIMULATION ===\n")
  simulation_results = simulate_segregation_scenarios(
    n_precincts       = n_precincts,
    n_centers         = n_centers,
    input_data        = input_data,
    pop_mean          = pop_mean,
    pop_sd            = pop_sd,
    minority_mean     = minority_mean,
    minority_sd       = minority_sd,
    decay_rate        = decay_rate,
    base_density      = base_density,
    peak_multiplier   = peak_multiplier,
    decay_power       = decay_power,
    center_radius     = center_radius,
    seed              = random_seed,
    voting_model      = voting_model,
    prob_minority_dem = prob_minority_dem,
    prob_majority_dem = prob_majority_dem,
    sd_minority       = sd_minority,
    sd_majority       = sd_majority,
    rho               = rho,
    b_min_context     = b_min_context,   
    b_maj_context     = b_maj_context,
    sigma_precinct    = sigma_precinct,
    field_rho         = field_rho,   
    shared_field_wt   = shared_field_wt,   
    shared_field_rho  = shared_field_rho
  )
  
  # Create main output directory
  timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
  main_output_dir = paste0(output_base_dir, "test_", timestamp, "/")
  dir.create(main_output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Save the fixed population data in root directory
  fixed_population_data = simulation_results[["low"]]$precincts %>%
    st_drop_geometry() %>%
    dplyr::select(
      precinct_id,
      population,
      n_minority,
      n_majority,
      dem_votes,
      rep_votes,
      dem_votes_minority,
      rep_votes_minority,
      dem_votes_majority,
      rep_votes_majority,
      per_minority,
      dem_voteshare,
      dem_voteshare_minority,
      dem_voteshare_majority
    )
  
  write.csv(
    fixed_population_data,
    file = paste0(main_output_dir, "precincts.csv"),
    row.names = FALSE
  )
  cat("Saved fixed population data to precincts.csv\n")
  
  # Save simulation parameters in root directory
  save_simulation_parameters(
    simulation_results,
    main_output_dir,
    random_seed,
    timestamp,
    n_precincts,
    input_data,
    pop_mean,
    pop_sd,
    minority_mean,
    minority_sd,
    voting_model,
    prob_minority_dem,
    prob_majority_dem,
    sd_minority,
    sd_majority,
    rho,
    b_min_context,
    b_maj_context,
    sigma_precinct,
    field_rho,   
    shared_field_wt,   
    shared_field_rho,
    n_centers,
    decay_rate,
    base_density,
    peak_multiplier,
    decay_power,
    center_radius,
    n_districts,
    n_plans,
    pop_deviation,
    ensemble_size,
    dev_mode,
    burst_length,
    num_bursts,    
    patience_bursts,
    soft_k,
    simplify_tolerance,
    score_models
  )
  
  # Define segregation levels
  segregation_levels = c("low", "medium", "high")
  all_results = list()
  results_for_comp_plots = list()
  
  # Loop through each segregation level
  for (current_level in segregation_levels) {
    cat(paste0(
      "\n\n===== PROCESSING SEGREGATION LEVEL: ",
      toupper(current_level),
      " =====\n\n"
    ))
    
    # Create segregation level directory
    current_output_dir = paste0(main_output_dir, current_level, "/")
    dir.create(current_output_dir, recursive = TRUE, showWarnings = FALSE)
    
    # Create subdirectories for each redistricting type
    for (map_type in c("neutral", "republican", "democratic")) {
      dir.create(
        paste0(current_output_dir, map_type),
        recursive = TRUE,
        showWarnings = FALSE
      )
    }
    dir.create(paste0(current_output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)
    
    # Get precincts for this level and generate baseline votes used by Python (E0)
    precincts = simulation_results[[current_level]]$precincts
    precincts_E0 = apply_vote_model(
      spatial_precincts = precincts, 
      voting_model      = "ei",
      prob_minority_dem = prob_minority_dem,
      prob_majority_dem = prob_majority_dem,
      sd_minority       = sd_minority, 
      sd_majority       = sd_majority, 
      rho               = rho,
      b_min_context     = b_min_context,
      b_maj_context     = b_maj_context, 
      sigma_precinct    = sigma_precinct,
      field_rho         = field_rho,   
      shared_field_wt   = shared_field_wt,   
      shared_field_rho  = shared_field_rho,
      seed              = random_seed + 123
    )
    
    # Save for plotting
    results_for_comp_plots[[current_level]] = list(precincts = precincts_E0)
    
    # Format for redistricting 
    map_data_formatted = prepare_for_redistricting(precincts_E0)
    
    # Save shapefile in segregation level directory
    shapefile_path = paste0(current_output_dir, "state_map.shp")
    st_write(map_data_formatted, shapefile_path, append = FALSE, quiet = TRUE)
    cat("Saved shapefile with baseline election:", shapefile_path, "\n")
    
    # Initial figs
    create_initial_visualizations(precincts, map_data_formatted, current_output_dir, current_level)
    
    map_data_formatted = sf::st_make_valid(map_data_formatted)
    map_data_formatted = sf::st_simplify(map_data_formatted, dTolerance = 0.5)
    
    # Run all three types of redistricting
    cat("\nRunning redistricting analyses...\n")
    
    redistricting_results = list()
    model_summaries = NULL   # predefine to avoid scope issues
    summaries = NULL         # what downstream plots will use
    
    tryCatch({
      redistricting_results = run_redistricting(
        shapefile_path        = shapefile_path,
        output_dir            = current_output_dir,
        num_steps             = n_plans,
        ensemble_size         = ensemble_size,
        pop_deviation         = pop_deviation,
        num_districts         = n_districts,
        target_pop            = NULL,
        dev_mode              = dev_mode,
        burst_length          = burst_length,
        num_bursts            = num_bursts,
        patience_bursts       = patience_bursts,
        soft_k                = soft_k,
        simplify_tolerance    = simplify_tolerance
      )
      
      # Read shapefile back for processing
      map_data = st_read(shapefile_path, quiet = TRUE) %>%
        dplyr::rename(
          precinct_id = pct_id,
          population  = pop,
          n_minority  = n_min,
          n_majority  = n_maj,
          dem_votes   = dem_v,
          rep_votes   = rep_v,
          dem_votes_minority = dem_v_min,
          rep_votes_minority = rep_v_min,
          dem_votes_majority = dem_v_maj,
          rep_votes_majority = rep_v_maj,
          per_minority       = pct_min,
          dem_voteshare      = dem_vsh,
          dem_voteshare_minority = dem_vsh_1,
          dem_voteshare_majority = dem_vsh_0
        )
      
      cat("\nProcessing redistricting results...\n")
      
      if (isTRUE(score_models)) {
        # Optional re-scoring under alternative vote models (prints once per model)
        base_sf = precincts %>% dplyr::select(precinct_id, population, n_minority, n_majority, per_minority, geometry)
        vote_models = list(
          baseline_ei = list(
            voting_model="ei",
            prob_minority_dem=0.81, prob_majority_dem=0.37,
            sd_minority=0.10, sd_majority=0.30,
            rho=0.20, sigma_precinct=0.05
          )
          # Add others here if desired
        )
        model_summaries = lapply(names(vote_models), function(nm)
          evaluate_vote_model_on_fixed_plans(current_output_dir, base_sf, nm, vote_models[[nm]]))
        names(model_summaries) = names(vote_models)
        
        summaries = model_summaries$baseline_ei
        
        if (!is.null(summaries) && length(summaries) > 0) {
          create_ensemble_visualizations(summaries, current_output_dir, current_level)
          
          CD_plans_neutral = NULL
          neutral_plans_file = file.path(current_output_dir, "neutral/CD_plans.csv")
          if (file.exists(neutral_plans_file)) CD_plans_neutral = data.table::fread(neutral_plans_file)
          
          create_redistricting_visualizations(
            map_data              = precincts_E0,
            summaries             = summaries,
            CD_plans_neutral      = CD_plans_neutral,
            output_dir            = current_output_dir,
            segregation_level     = current_level
          )
        }
        
      } else {
        # Use the ensembles exactly as written by Python (single set of prints)
        summaries = process_redistricting_results(
          map_data = map_data,
          redistricting_results = redistricting_results,
          current_output_dir = current_output_dir
        )
        
        if (!is.null(summaries)) {
          # Neutral plans for overlays if present
          CD_plans_neutral = NULL
          neutral_plans_file = file.path(current_output_dir, "neutral/CD_plans.csv")
          if (file.exists(neutral_plans_file)) CD_plans_neutral = data.table::fread(neutral_plans_file)
          
          create_redistricting_visualizations(
            map_data              = precincts_E0,
            summaries             = summaries,
            CD_plans_neutral      = CD_plans_neutral,
            output_dir            = current_output_dir,
            segregation_level     = current_level
          )
        }
      }
      
    }, error = function(e) {
      cat("ERROR in redistricting for", current_level, ":", e$message, "\n")
    })
    
    # Store results safely
    all_results[[current_level]]$summaries = summaries
    if (!is.null(model_summaries)) {
      all_results[[current_level]]$model_summaries = model_summaries
    }
  }
  
  comparisons_dir = paste0(main_output_dir, "comparisons/")
  dir.create(comparisons_dir, recursive = TRUE, showWarnings = FALSE)
  
  seg_comparison        = plot_segregation_levels_comparison(results_for_comp_plots, comparisons_dir)
  voteshare_comparison  = plot_voteshare_levels_comparison(results_for_comp_plots, comparisons_dir)
  combined_visualations = plot_segregation_and_voteshare_grid(results_for_comp_plots, comparisons_dir)
  
  # Create cross-level comparisons
  if (length(all_results) > 0) {
    cat("\n=== CREATING CROSS-LEVEL COMPARISONS ===\n")
    plot_cross_level_comparison(all_results, main_output_dir, n_districts)
  }
  
  # Save all results
  saveRDS(all_results, paste0(main_output_dir, "all_results.rds"))
  
  # Gallery of plans
  export_plan_gallery_3pages(main_output_dir)
  
  cat("\n=== ANALYSIS COMPLETE ===\n")
  cat("Results saved to:", main_output_dir, "\n")
  
  list(
    results = all_results,
    output_dir = main_output_dir,
    simulation_results = simulation_results
  )
}






