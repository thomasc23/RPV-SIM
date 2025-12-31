# Synthetic data generation functions for rpvsimulator package
# Uses canonical column names from constants.R

#' Create city centers for grid generation
#' @param n_centers number of city centers to create
#' @param bounds bounding box as c(xmin, xmax, ymin, ymax)
#' @param seed random seed for reproducibility
#' @return data.frame with center coordinates and intensity
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

#' Create density surface with a central plateau to avoid "starbursts"
#' @param x x coordinate
#' @param y y coordinate
#' @param centers data.frame with center coordinates and intensity
#' @param decay_rate rate of density decay from centers
#' @param base_density baseline density level
#' @param peak_multiplier multiplier for peak density at centers
#' @param decay_power power for decay function
#' @param center_radius radius of central plateau
#' @return density value at the given coordinates
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

#' Generate seed points using robust rejection sampling
#' @param n_target target number of seed points
#' @param centers data.frame with center coordinates and intensity
#' @param bounds bounding box as c(xmin, xmax, ymin, ymax)
#' @param decay_rate rate of density decay from centers
#' @param base_density baseline density level
#' @param peak_multiplier multiplier for peak density at centers
#' @param decay_power power for decay function
#' @param center_radius radius of central plateau
#' @param seed random seed for reproducibility
#' @return data.frame with seed point coordinates and density
generate_seed_points = function(n_target,
                                centers,
                                bounds          = c(0, 1000, 0, 1000),
                                decay_rate      = 15,
                                base_density    = 0.01,
                                peak_multiplier = 5.0,
                                decay_power     = 4,
                                center_radius   = 1.0,
                                seed            = 123) {
  
  set.seed(seed)
  
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

#' Create precinct boundaries from seed points using Voronoi tessellation
#' @param seed_points data.frame with seed point coordinates
#' @param bounds bounding box as c(xmin, xmax, ymin, ymax)
#' @return sf object with precinct boundaries
create_precinct_boundaries = function(seed_points, bounds = c(0, 1000, 0, 1000)) {
  
  window = spatstat.geom::owin(xrange = bounds[1:2], yrange = bounds[3:4])
  pp = spatstat.geom::ppp(seed_points$x, seed_points$y, window = window)
  vor = spatstat.geom::dirichlet(pp)
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

  # Create sf object with canonical column names
  precincts_df <- data.frame(id = 1:length(vor_polys))
  names(precincts_df) <- COL_NAMES$precinct_id
  precincts = sf::st_sf(
    precincts_df,
    geometry = sf::st_sfc(vor_polys),
    crs = 3857
  )
  precincts = sf::st_make_valid(precincts)
  precincts = sf::st_buffer(precincts, 0)

  # Create spatial points and find which precinct each seed falls in
  seed_sf = sf::st_as_sf(
    seed_points,
    coords = c("x", "y"),
    crs = 3857,
    remove = FALSE
  )
  seed_to_precinct_map = sf::st_join(seed_sf, precincts, join = sf::st_intersects)

  # Extract just the data (no geometry) from the join result
  seed_data = sf::st_drop_geometry(seed_to_precinct_map)

  # Join the seed data to precincts
  precincts = precincts %>%
    dplyr::left_join(seed_data, by = COL_NAMES$precinct_id)

  # Calculate derived fields with canonical names
  precincts$area = as.numeric(sf::st_area(precincts))
  precincts[[COL_NAMES$urbanness]] = precincts$density / max(precincts$density, na.rm = TRUE)
  precincts[[COL_NAMES$seed_x]] = precincts$x
  precincts[[COL_NAMES$seed_y]] = precincts$y
  precincts$x <- NULL
  precincts$y <- NULL

  return(precincts)
}

#' Create a realistic spatial grid for synthetic data generation
#' @param n_precincts number of precincts to create
#' @param n_centers number of city centers
#' @param bounds bounding box as c(xmin, xmax, ymin, ymax)
#' @param decay_rate rate of density decay from centers
#' @param base_density baseline density level
#' @param peak_multiplier multiplier for peak density at centers
#' @param decay_power power for decay function
#' @param center_radius radius of central plateau
#' @param seed random seed for reproducibility
#' @return list with precincts sf object and centers data.frame
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
  
  precincts = create_precinct_boundaries(seeds, bounds)
  cat("Created", nrow(precincts), "precincts\n")
  
  # Calculate distance to nearest center (using canonical column names)
  precincts$dist_to_center = NA
  precincts$nearest_center = NA
  seed_x_col <- COL_NAMES$seed_x
  seed_y_col <- COL_NAMES$seed_y
  valid_precincts = !is.na(precincts[[seed_x_col]])
  for (i in which(valid_precincts)) {
    distances = sqrt((precincts[[seed_x_col]][i] - centers$x)^2 +
                     (precincts[[seed_y_col]][i] - centers$y)^2)
    precincts$dist_to_center[i] = min(distances)
    precincts$nearest_center[i] = which.min(distances)
  }

  save_diagnostic_plot("sim_shapefile_figures", function() {
    par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))
    
    # 1. City centers
    plot(centers$x, centers$y, main = "City Centers", xlab = "", ylab = "", col = "black")
    
    # 2. Seed points via rejection sampling
    plot(seeds$x, seeds$y, main = "Precinct Centroids", xlab = "", ylab = "", col = "black")

    # 3. Precinct boundaries
    plot(precincts$geometry, main = "Precinct Geographic Boundaries", xlab = "", ylab = "")
    
    par(mfrow = c(1, 1))
  })
  
  return(list(precincts = precincts, centers = centers))
}



simulate_segregation_scenarios = function(
  
) {

}
