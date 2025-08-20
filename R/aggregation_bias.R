cluster_and_reassign_precincts = function(precinct_df, voters_df = NULL, num_clusters) {
  # scale data
  feat_mat = precinct_df %>%
    select(per_minority, dem_voteshare) %>%
    mutate(across(everything(), ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE))) %>%
    as.matrix()
  
  # Perform clustering
  cat('Clustering based on precinct minority population and democratic voteshares.')
  
  clusters = kmeans(feat_mat, num_clusters)
  
  # Add cluster assignments to the data frame
  precincts = precinct_df %>%
    mutate(geog_id = as.factor(clusters$cluster))
  
  # # check 2d cluster
  # ggplot(precincts, aes(per_minority, dem_voteshare, color = geog_id)) +
  #   geom_point(alpha = 0.5) +
  #   theme_bw()
  
  # Reorder data based on clusters and then by original id within clusters
  precincts = precincts %>%
    arrange(geog_id, precinct_id)
  
  if (!is.null(voters_df)) {
    voters_df %>%
      left_join(precincts %>% select(precinct_id, new_precinct_id, geog_id),
                by = 'precinct_id') %>%
      select(voter_id, new_precinct_id, geog_id, race, vote) %>%
      rename(precinct_id = new_precinct_id) %>%
      arrange(geog_id, precinct_id) %>%
      as_tibble()
    
  } else {
    precincts
    
  }
  
}

induce_aggregation_bias = function(precinct_df, num_regions = 5, agg_bias = "medium") {
  # Make a copy of the input data
  precincts = precinct_df %>% 
    mutate(precinct_id = row_number())  # Ensure unique IDs
  
  # Set segregation parameters based on level
  agg_par = case_when(
    agg_bias == "low" ~ 0.1,
    agg_bias == "medium" ~ 0.5,
    agg_bias == "high" ~ 0.9
  )
  
  # Step 1: Assign each precinct a location score primarily based on minority percentage
  # Higher agg_par means location is more strongly determined by minority percentage
  precincts = precincts %>%
    mutate(
      # Normalize minority percentage to [0,1] range if not already
      norm_minority = (per_minority - min(per_minority)) / 
        (max(per_minority) - min(per_minority)),
      
      # Create location score as weighted average of minority percentage and random noise
      # Higher agg_par means minority percentage has more influence
      location_score = agg_par * norm_minority + 
        (1 - agg_par) * runif(n())
    )
  
  # Step 2: Sort precincts by location score (this creates our 1D spatial ordering)
  precincts = precincts %>% arrange(location_score)
  
  # Step 3: Divide into regions (this simulates spatial clustering)
  precincts = precincts %>%
    mutate(geog_id = as.factor(cut(
      row_number(), 
      breaks = seq(0, n(), length.out = num_regions + 1),
      labels = FALSE,
      include.lowest = TRUE
    )))
  
  # Step 4: Calculate segregation index for validation
  total_var = var(precincts$per_minority)
  within_var = precincts %>%
    group_by(geog_id) %>%
    summarize(within_var = var(per_minority)) %>%
    summarize(mean_within_var = mean(within_var, na.rm = TRUE))
  
  segregation_index = 1 - (within_var$mean_within_var / total_var)
  
  # Return the results
  return(list(
    map_data = precincts,
    segregation_index = segregation_index
  ))
}

# Function to scale points from [0,1] x [0,1] to spatial area given by xmin, ymin, xmax, ymax
approximate_to_grid = function(precinct_df, xmin, ymin, xmax, ymax) {
  
  # Ensure precinct_df has a per_minority and dem_voteshare column
  if (!is.data.frame(precinct_df) || !all(c('per_minority', 'dem_voteshare') %in% names(precinct_df))) {
    stop('Input must be a data frame with \'per_minority\' and \'dem_voteshare\' columns')
  }
  
  # Sort precincts by per_minority and dem_voteshare
  precinct_df_sorted = precinct_df[order(precinct_df$per_minority, precinct_df$dem_voteshare), ]
  
  # Assign new coordinates based on rank
  n = nrow(precinct_df_sorted)
  precinct_df_sorted$x = (rank(precinct_df_sorted$per_minority, ties.method = 'first') - 1) / (n - 1)
  precinct_df_sorted$y = (rank(precinct_df_sorted$dem_voteshare, ties.method = 'first') - 1) / (n - 1)
  
  # # Calculate the aspect ratio to make the grid as square as possible
  # max_x = max(precinct_df_sorted$x)
  # max_y = max(precinct_df_sorted$y)
  # aspect_ratio = max_x / max_y
  # 
  # # Scale by aspect ratio
  # precinct_df_sorted$x = precinct_df_sorted$x / aspect_ratio
  # precinct_df_sorted$y = precinct_df_sorted$y / aspect_ratio
  
  # Normalize to [0, 1] range
  precinct_df_sorted$x = xmin + (precinct_df_sorted$x * (xmax - xmin))
  precinct_df_sorted$y = ymin + (precinct_df_sorted$y * (ymax - ymin))
  
  # Add small amount of jitter so no points share the same location
  while(any(duplicated(precinct_df_sorted[c('x', 'y')]))) {
    # Add jitter to both x and y
    precinct_df_sorted$x = jitter(precinct_df_sorted$x, amount = 1)
    precinct_df_sorted$y = jitter(precinct_df_sorted$y, amount = 1)
  }
  
  return(precinct_df_sorted)
}

# Order perimeter cells starting from (1,1) in counter-clockwise direction.
# This function takes in a dataframe of cells that are located on the perimeter
# of a given grid. 
order_perimeter_cells = function(perimeter_df) {
  
  # Function to determine if a cell (given by (row, col) is on an edge in df
  # First, get all cells that are at most 1 row/col away from current cell
  # Second, remove the cell itself
  # Third, return TRUE if there are fewer than 8 neighboring cells 
  is_edge_cell = function(df, row, col) {
    neighbors = df %>%
      filter(abs(row - .data$row) <= 1 & abs(col - .data$col) <= 1) %>%
      filter(!(row == .data$row & col == .data$col))
    return(nrow(neighbors) < 8)
  }
  
  # Identify edge cells
  edge_cells = perimeter_df %>%
    rowwise() %>%
    filter(is_edge_cell(perimeter_df, row, col)) %>%
    ungroup()
  
  # Start from (1,1)
  start = edge_cells %>%
    filter(row == 1, col == 1)
  
  # Function to find next cell in counter-clockwise order
  find_next_cell = function(current, remaining) {
    # All possible directions, ordered counter-clockwise
    directions = list(
      c(1, 0), c(1, 1), c(0, 1), c(-1, 1),
      c(-1, 0), c(-1, -1), c(0, -1), c(1, -1)
    )
    
    for (dir in directions) {
      next_cell = remaining %>%
        filter(row == current$row + dir[1], col == current$col + dir[2]) %>%
        slice(1)
      
      if (nrow(next_cell) > 0) return(next_cell)
    }
    return(NULL)  # Should not reach here if perimeter is continuous
  }
  
  # Traverse the perimeter
  ordered_cells = start
  remaining_cells = edge_cells %>% anti_join(start, by = c('row', 'col'))
  
  while (nrow(remaining_cells) > 0) {
    current = ordered_cells %>% slice_tail(n = 1)
    next_cell = find_next_cell(current, remaining_cells)
    
    if (is.null(next_cell)) break  # Perimeter is complete
    
    ordered_cells = bind_rows(ordered_cells, next_cell)
    remaining_cells = remaining_cells %>% anti_join(next_cell, by = c('row', 'col'))
  }
  
  # Add order column
  ordered_cells = ordered_cells %>%
    mutate(order = row_number())
  
  return(ordered_cells)
}

# Function to calculate Euclidean distance
euclidean_distance = function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Assign clustered Precincts to Random Geographic Regions
random_region_assignment = function(precinct_data, 
                                    num_geogs = 5, 
                                    agg_bias = 'low',
                                    cell_size = 1000,
                                    output_dir = "Output/",
                                    random_seed = 42,
                                    save_shp = TRUE,
                                    max_attempts = 10) {
  
  # Set initial seed for reproducibility
  set.seed(random_seed)
  
  # Cluster precincts into num_geogs distinct regions based on demographic features
  # precinct_data = cluster_and_reassign_precincts(precinct_data, num_clusters = num_geogs)
  precinct_data = induce_aggregation_bias(precinct_data, num_regions = num_geogs, agg_bias = agg_bias)
  precinct_data = precinct_data$map_data
  
  # Set up parameters of grid
  n_precinct = length(unique(precinct_data$precinct_id))
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
    arrange(x_grid, desc(y_grid)) %>%
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
  
  bbox = st_bbox(grid)
  
  # Scale precincts to grid 
  precinct_data = approximate_to_grid(precinct_data, 
                                       xmin = bbox$xmin, ymin = bbox$ymin,
                                       xmax = bbox$xmax, ymax = bbox$ymax)
  
  # Check we haven't lost precincts
  if(length(unique(precinct_data$precinct_id)) != nrow(precinct_data)) {
    warning("Some precincts may have been lost during grid scaling")
  }
  
  # Make spatial 
  precinct_sf = precinct_data %>%
    mutate(latitude = y, 
           longitude = x) %>%
    st_as_sf(coords = c('longitude', 'latitude'),
             crs = 3857)
  
  # Compute adjacency using Queen's contiguity once (reused across attempts)
  nb = poly2nb(grid)
  adjacency_list = nb
  
  # Compute centroids of grid cells once
  grid$centroid = st_centroid(grid$geometry)
  grid_coords = st_coordinates(grid$centroid)
  
  # Desired cell counts per region
  desired_cell_counts = precinct_data %>%
    count(geog_id, name = "desired_num_cells")
  
  # Define attempt function
  attempt_region_assignment = function(attempt_seed) {
    # Set seed for this attempt
    set.seed(attempt_seed)
    cat("Trying region assignment with seed:", attempt_seed, "\n")
    
    # Get random seed points for each geographic region
    random_centroid_indices = sample(1:nrow(grid), num_geogs)
    
    # Create a dataframe of random centroids
    random_region_centroids = grid[random_centroid_indices, ] %>%
      mutate(geog_id = 1:num_geogs) %>%
      select(geog_id, geometry)
    
    # Use our random centroids
    region_centroids = random_region_centroids
    
    # Get centroid indices directly from our random selection
    geog_id_centroids = data.frame(
      geog_id = 1:num_geogs,
      centroid_index = random_centroid_indices
    )
    
    n_cells = nrow(grid)
    region_assignments = rep(NA_integer_, n_cells)
    
    # Assign starting cells to their respective regions
    for(i in seq_len(nrow(geog_id_centroids))) {
      index = geog_id_centroids$centroid_index[i]
      geog_id = geog_id_centroids$geog_id[i]
      region_assignments[index] = geog_id
    }
    
    assigned_counts = data.frame(
      geog_id = as.factor(geog_id_centroids$geog_id),
      assigned_cells = 1  # Each region starts with one cell
    )
    
    # Merge with desired counts
    assigned_counts = assigned_counts %>%
      left_join(desired_cell_counts, by = "geog_id")
    
    # List of unassigned cells
    unassigned_cells = which(is.na(region_assignments))
    
    # Continue until all cells are assigned or no regions can grow
    iteration = 0
    max_iterations = 500  # Prevent infinite loops
    
    # Track progress variables for early detection of stuck state
    prev_unassigned = length(unassigned_cells)
    stalled_count = 0
    
    while(length(unassigned_cells) > 0 && iteration < max_iterations) {
      no_growth = TRUE  # Flag to check if any region grew in this iteration
      
      iteration = iteration + 1
      
      if(iteration %% 10 == 0) {
        cat('Iteration:', iteration, '\n')
        cat('Unassigned cells remaining:', length(unassigned_cells), '\n')
        cat('Area capacity remaining:', sum(pmax(0, assigned_counts$desired_num_cells - assigned_counts$assigned_cells)), '\n')
        
        # Check if we're making progress
        if(prev_unassigned == length(unassigned_cells)) {
          stalled_count = stalled_count + 1
          cat("Stalled for", stalled_count, "checks\n")
          
          # If stalled for too long (5 progress checks), consider it stuck
          if(stalled_count >= 5) {
            cat("Assignment appears stuck. Trying with new seed.\n")
            return(NULL)  # Signal to try again with new seed
          }
        } else {
          prev_unassigned = length(unassigned_cells)
          stalled_count = 0
        }
      }
      
      # Check if we've exceeded our available capacity
      total_remaining_capacity = sum(pmax(0, assigned_counts$desired_num_cells - assigned_counts$assigned_cells))
      if(length(unassigned_cells) > total_remaining_capacity) {
        cat("Not enough capacity to assign all cells. Trying with new seed.\n")
        return(NULL)  # Signal to try again with new seed
      }
      
      # Loop over each region in random order to avoid favoring earlier regions
      region_order = sample(seq_len(nrow(assigned_counts)))
      
      for(i in region_order) {
        geog_id = assigned_counts$geog_id[i]
        assigned_cells = assigned_counts$assigned_cells[i]
        desired_num_cells = assigned_counts$desired_num_cells[i]
        
        # Skip if region has reached desired cell count
        if(assigned_cells >= desired_num_cells) {
          next
        }
        
        # Find boundary cells of the current region
        current_region_cells = which(region_assignments == geog_id)
        
        if(length(current_region_cells) == 0) {
          cat("Region", geog_id, "has no cells assigned. This shouldn't happen.\n")
          next
        }
        
        boundary_cells = unique(unlist(adjacency_list[current_region_cells]))
        
        # Exclude cells already assigned
        boundary_cells = boundary_cells[is.na(region_assignments[boundary_cells])]
        
        # If no boundary cells are available, skip
        if(length(boundary_cells) == 0) {
          next
        }
        
        # Compute distances from boundary cells to region centroid
        centroid_index = geog_id_centroids$centroid_index[geog_id_centroids$geog_id == geog_id]
        centroid_coord = grid_coords[centroid_index, ]
        boundary_coords = grid_coords[boundary_cells, , drop = FALSE]
        distances = sqrt((boundary_coords[,1] - centroid_coord[1])^2 + (boundary_coords[,2] - centroid_coord[2])^2)
        
        # Select the closest cell
        min_dist_index = which.min(distances)
        selected_cell = boundary_cells[min_dist_index]
        
        # Assign the cell to the region
        region_assignments[selected_cell] = geog_id
        assigned_counts$assigned_cells[i] = assigned_counts$assigned_cells[i] + 1
        no_growth = FALSE  # Region grew in this iteration
        
        # Update unassigned cells
        unassigned_cells = unassigned_cells[unassigned_cells != selected_cell]
        
        # Check if we've just completed a region
        if(assigned_counts$assigned_cells[i] >= desired_num_cells) {
          cat("Region", geog_id, "filled to capacity:", assigned_counts$assigned_cells[i], "\n")
        }
      }
      
      # Break loop if no regions grew in this iteration
      if(no_growth) {
        cat("No growth in this iteration. Assignment may be stuck.\n")
        break
      }
    }
    
    # Check if we've succeeded in assigning all cells
    if(length(unassigned_cells) > 0) {
      cat("Failed to assign all cells. ", length(unassigned_cells), "cells remain unassigned.\n")
      
      # If we're very close to completion, force assignment of remaining cells
      if(length(unassigned_cells) < 20) {
        cat("Few cells remain unassigned. Forcing assignment of remaining cells.\n")
        
        for(cell in unassigned_cells) {
          # Find neighboring regions
          neighbors = adjacency_list[[cell]]
          neighbor_regions = region_assignments[neighbors]
          neighbor_regions = neighbor_regions[!is.na(neighbor_regions)]
          
          if(length(neighbor_regions) > 0) {
            # Assign to the most frequent neighboring region
            assigned_region = as.integer(names(sort(table(neighbor_regions), decreasing = TRUE))[1])
          } else {
            # Assign to the closest centroid
            cell_coord = grid_coords[cell, ]
            distances = sqrt((cell_coord[1] - grid_coords[geog_id_centroids$centroid_index,1])^2 +
                                (cell_coord[2] - grid_coords[geog_id_centroids$centroid_index,2])^2)
            assigned_region = geog_id_centroids$geog_id[which.min(distances)]
          }
          
          region_assignments[cell] = assigned_region
          # Update assigned_counts if necessary
          idx = which(assigned_counts$geog_id == assigned_region)
          assigned_counts$assigned_cells[idx] = assigned_counts$assigned_cells[idx] + 1
        }
        
        # Check again
        if(any(is.na(region_assignments))) {
          cat("Still failed to assign all cells after forcing. Try with new seed.\n")
          return(NULL)
        } else {
          cat("Successfully assigned all cells after forcing assignment.\n")
        }
      } else {
        return(NULL)  # Signal to try again with new seed
      }
    }
    
    grid$geog_id = region_assignments
    
    # Return results
    return(list(
      grid = grid,
      region_centroids = region_centroids,
      geog_id_centroids = geog_id_centroids
    ))
  }
  
  # Try multiple attempts if needed
  for(attempt in 1:max_attempts) {
    attempt_seed = random_seed + attempt - 1  # Use a different seed for each attempt
    result = attempt_region_assignment(attempt_seed)
    
    if(!is.null(result)) {
      cat("Successfully assigned all cells on attempt", attempt, "\n")
      
      # Use the successful grid assignment
      grid = result$grid
      region_centroids = result$region_centroids
      
      # Shuffle and order precinct data
      precinct_data_ordered = precinct_data %>%
        group_by(geog_id) %>%
        sample_frac(1) %>%
        arrange(geog_id) %>%
        ungroup()
      
      # Order grid cells
      grid_ordered = grid %>%
        arrange(geog_id)
      
      # Ensure counts match
      if(nrow(precinct_data_ordered) != nrow(grid_ordered)) {
        stop("Counts do not match after adjustments.")
      }
      
      # Combine dataframes
      grid_sf_assigned = grid_ordered %>%
        bind_cols(precinct_data_ordered %>% select(-geog_id))
      
      # Check we haven't lost precincts
      if(length(unique(grid_sf_assigned$precinct_id)) != nrow(precinct_data)) {
        warning("Some precincts may have been lost during assignment")
      }
      
      # map_data = grid_sf_assigned %>%
      #   select(geog_id, precinct_id, population, per_minority, dem_voteshare) %>%
      #   mutate(geog_id = as.factor(geog_id)) %>%
      #   # compact column names for st_write
      #   rename(pct_id = precinct_id, 
      #          pop = population, 
      #          pct_min = per_minority,
      #          dem_vsh = dem_voteshare)
      
      map_data = grid_sf_assigned %>%
        select(geog_id, precinct_id, population, per_minority, dem_voteshare, 
               dem_voteshare_minority, dem_voteshare_majority) %>%
        mutate(geog_id = as.factor(geog_id)) %>%
        # compact column names for st_write
        rename(pct_id = precinct_id, 
               pop = population, 
               pct_min = per_minority,
               dem_vsh = dem_voteshare,
               dem_vsh_1 = dem_voteshare_minority,
               dem_vsh_0 = dem_voteshare_majority)
      
      # Save shapefile if requested
      if(save_shp) {
        # Create directory if it doesn't exist
        dir.create(file.path(output_dir, "Shapefiles"), showWarnings = FALSE, recursive = TRUE)
        
        # Write shapefile
        st_write(map_data, 
                 paste0(output_dir, 'Shapefiles/state_map.shp'), 
                 append = FALSE, 
                 quiet = TRUE)
        
        cat("Shapefile saved to", paste0(output_dir, 'Shapefiles/state_map.shp'), "\n")
      }
      
      # Return the map data and grid
      return(list(
        map_data = map_data,
        grid = grid,
        region_centroids = region_centroids
      ))
    }
    
    cat("Attempt", attempt, "failed. Starting new attempt.\n")
  }
  
  stop("Failed to assign cells after", max_attempts, "attempts. Consider adjusting parameters.")
}

plot_agg_bias = function(map_data) {
  
  p1 = ggplot(map_data) +
    geom_sf(aes(fill = geog_id), color = 'lightgrey') +
    scale_fill_viridis_d(option = 'plasma') +
    scale_color_viridis_d(option = 'plasma') +
    guides(color = 'none') +
    theme_void() +
    theme(
      legend.position = 'right'
    ) +
    ggtitle('Geographic Cluster')
  
  p2 = ggplot(map_data) +
    geom_sf(aes(fill = pct_min), color = 'lightgrey') +
    scale_fill_viridis(option = 'plasma', labels = scales::percent) +
    scale_color_viridis(option = 'plasma', labels = scales::percent) +
    guides(color = 'none') +
    theme_void() +
    theme(
      legend.position = 'right'
    ) +
    ggtitle('Percent Minority')
  
  p3 = ggplot(map_data) +
    geom_sf(aes(fill = dem_vsh), color = 'lightgrey') +
    scale_fill_viridis(option = 'plasma', labels = scales::percent) +
    scale_color_viridis(option = 'plasma', labels = scales::percent) +
    guides(color = 'none') +
    theme_void() +
    theme(
      legend.position = 'right'
    ) +
    ggtitle('Democratic Vote Share')
  
  
  comb_plot = p1 + p2 + p3 + plot_layout(nrow = 1, ncol = 3)
  comb_plot
}


create_district_summary = function(map_data, plan_id, cd_plan) {
  # Extract the district assignments for this plan
  plan = map_data %>%
    st_drop_geometry() %>%
    select(precinct_id, population, per_minority, dem_voteshare, 
           dem_voteshare_minority, dem_voteshare_majority)
  
  # Assign district IDs from the plan
  plan$district_id = cd_plan
  
  # Calculate district-level metrics
  district_summary = plan %>% 
    mutate(
      dem_vote_count = dem_voteshare * population,
      minority_voter_count = per_minority * population,
      majority_voter_count = population - minority_voter_count,
      dem_vote_count_minority = dem_voteshare_minority * minority_voter_count,
      dem_vote_count_majority = dem_voteshare_majority * majority_voter_count
    ) %>% 
    group_by(district_id) %>% 
    summarise(
      n_precincts = n(),
      population = sum(population),
      n_minority_voters = sum(minority_voter_count),
      n_majority_voters = sum(majority_voter_count),
      per_minority = n_minority_voters / population,
      maj_min_district = per_minority > 0.5,
      dem_votes = sum(dem_vote_count),
      dem_voteshare = dem_votes / population,
      dem_district = dem_voteshare >= 0.5,
      # Get race-specific vote counts and voteshares
      dem_votes_minority = sum(dem_vote_count_minority),
      dem_votes_majority = sum(dem_vote_count_majority),
      dem_voteshare_minority = dem_votes_minority / n_minority_voters,
      dem_voteshare_majority = dem_votes_majority / n_majority_voters
    ) %>%
    mutate(
      map_id = plan_id,
      district_id = as.integer(district_id),
      # Handle potential NA values
      dem_voteshare_minority = ifelse(is.na(dem_voteshare_minority), 0, dem_voteshare_minority),
      dem_voteshare_majority = ifelse(is.na(dem_voteshare_majority), 0, dem_voteshare_majority)
    ) %>%
    select(map_id, district_id, everything())
  
  return(district_summary)
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

# Main function to generate and save all summaries
save_district_and_map_summaries = function(map_data, CD_plans, output_dir) {
  # Initialize storage for district and map summaries
  all_district_summaries = NULL
  
  # Number of plans to process
  n_plans = ncol(CD_plans) - 2  # Subtract 2 for precinct_id and init_CD columns
  
  # Process each plan
  for (p in 1:n_plans) {
    if (p %% 100 == 0) cat('Processing plan', p, 'of', n_plans, '\n')
    
    # Get district assignments for this plan
    plan_id = p
    cd_plan = CD_plans[[p + 2]]  # +2 to skip precinct_id and init_CD columns
    
    # Create district summary for this plan
    district_summary = create_district_summary(map_data, plan_id, cd_plan)
    
    # Add to the collection of all district summaries
    all_district_summaries = bind_rows(all_district_summaries, district_summary)
  }
  
  # Create map summaries from district summaries
  all_map_summaries = create_map_summary(all_district_summaries)
  
  # Now filter for the four categories
  # 1. Top 2.5% Republican maps
  rep_quantile = quantile(all_map_summaries$n_rep_districts, 0.975)
  top_rep_map_ids = all_map_summaries %>% 
    filter(n_rep_districts >= rep_quantile) %>% 
    pull(map_id)
  
  # 2. Top 2.5% Democratic maps
  dem_quantile = quantile(all_map_summaries$n_dem_districts, 0.975)
  top_dem_map_ids = all_map_summaries %>% 
    filter(n_dem_districts >= dem_quantile) %>% 
    pull(map_id)
  
  # 3. Middle 50% maps
  rep_mid_low = quantile(all_map_summaries$n_rep_districts, 0.25)
  rep_mid_high = quantile(all_map_summaries$n_rep_districts, 0.75)
  middle_map_ids = all_map_summaries %>% 
    filter(n_rep_districts >= rep_mid_low, n_rep_districts <= rep_mid_high) %>% 
    pull(map_id)
  
  # Create filtered summaries
  district_summaries_top_rep = all_district_summaries %>% filter(map_id %in% top_rep_map_ids)
  district_summaries_top_dem = all_district_summaries %>% filter(map_id %in% top_dem_map_ids)
  district_summaries_middle = all_district_summaries %>% filter(map_id %in% middle_map_ids)
  
  map_summaries_top_rep = all_map_summaries %>% filter(map_id %in% top_rep_map_ids)
  map_summaries_top_dem = all_map_summaries %>% filter(map_id %in% top_dem_map_ids)
  map_summaries_middle = all_map_summaries %>% filter(map_id %in% middle_map_ids)
  
  # Create output directory if it doesn't exist
  dir.create(file.path(output_dir, "Summaries"), showWarnings = FALSE, recursive = TRUE)
  
  # Save all files
  # All maps
  write_csv(all_district_summaries, paste0(output_dir, "Summaries/district_summaries_all.csv"))
  write_csv(all_map_summaries, paste0(output_dir, "Summaries/map_summaries_all.csv"))
  
  # Top Republican maps
  write_csv(district_summaries_top_rep, paste0(output_dir, "Summaries/district_summaries_top_rep.csv"))
  write_csv(map_summaries_top_rep, paste0(output_dir, "Summaries/map_summaries_top_rep.csv"))
  
  # Top Democratic maps
  write_csv(district_summaries_top_dem, paste0(output_dir, "Summaries/district_summaries_top_dem.csv"))
  write_csv(map_summaries_top_dem, paste0(output_dir, "Summaries/map_summaries_top_dem.csv"))
  
  # Middle maps
  write_csv(district_summaries_middle, paste0(output_dir, "Summaries/district_summaries_middle.csv"))
  write_csv(map_summaries_middle, paste0(output_dir, "Summaries/map_summaries_middle.csv"))
  
  # Return a list of all summaries
  return(list(
    district_all = all_district_summaries,
    map_all = all_map_summaries,
    district_top_rep = district_summaries_top_rep,
    map_top_rep = map_summaries_top_rep,
    district_top_dem = district_summaries_top_dem,
    map_top_dem = map_summaries_top_dem,
    district_middle = district_summaries_middle,
    map_middle = map_summaries_middle
  ))
}


# PLOTS
visualize_map_category = function(map_data, CD_plans, map_ids, category_name, output_dir) {
  # Sample one plan from the category
  sample_id = sample(map_ids, 1)
  
  # Get the column index in CD_plans
  col_index = 2 + sample_id  # +2 to skip precinct_id and init_CD
  
  # Assign the district IDs to map_data
  map_data$district_id = CD_plans[[col_index]]
  
  # Calculate district-level statistics
  cd_summary = map_data %>% 
    st_drop_geometry() %>%
    mutate(
      dem_vote_count = dem_voteshare * population,
      minority_voter_count = per_minority * population,
      majority_voter_count = population - minority_voter_count
    ) %>% 
    group_by(district_id) %>%
    summarise(
      n_precincts = n(),
      n_voters = sum(population),
      n_minority_voters = sum(minority_voter_count),
      n_majority_voters = sum(majority_voter_count),
      per_minority = n_minority_voters / n_voters,
      maj_min_gap = (1 - per_minority) - per_minority,
      dem_votes = sum(dem_vote_count),
      dem_voteshare = dem_votes / n_voters, 
      dem_cd = ifelse(dem_voteshare > 0.5, 1, 0)
    )
  
  # Merge back to map data
  plot_df = map_data %>% 
    left_join(
      cd_summary %>% select(district_id, dem_cd, per_minority, dem_voteshare), 
      by = "district_id",
      suffix = c("_precinct", "_district")  # Add suffixes to disambiguate columns
    )
  
  # Create district boundaries by aggregating polygons
  cd_borders = plot_df %>%
    group_by(district_id) %>%
    summarise(geometry = st_union(geometry)) %>%
    ungroup()
  
  # Create the four plots
  p1 = ggplot() +
    geom_sf(data = plot_df, fill = NA, color = NA) +
    geom_sf(data = cd_borders, fill = 'grey90', color = "darkred", linewidth = 0.5) +
    theme_void() +
    ggtitle(paste('Congressional Districts -', category_name))
  
  p2 = ggplot(plot_df) +
    geom_sf(aes(fill = as.factor(dem_cd), color = as.factor(dem_cd)), alpha = 0.75) +
    geom_sf(data = cd_borders, fill = NA, color = "darkred", linewidth = 0.5) +
    scale_fill_manual(values = c('1' = 'blue', '0' = 'red'),
                      labels = c('1' = 'D', '0' = 'R'),
                      name = '') +
    scale_color_manual(values = c('1' = 'blue', '0' = 'red'),
                       labels = c('1' = 'D', '0' = 'R'),
                       name = '') +
    theme_void() +
    ggtitle(paste('Election Results -', category_name))
  
  p3 = ggplot(plot_df) +
    geom_sf(aes(fill = per_minority_district, color = per_minority_district)) +
    geom_sf(data = cd_borders, fill = NA, color = "darkred", linewidth = 0.5) +
    scale_fill_gradient(low = "lightgrey", high = "black", 
                        labels = scales::percent) +
    scale_color_gradient(low = "lightgrey", high = "black", 
                         labels = scales::percent) +
    theme_void() +
    labs(fill = '',
         color = '') +
    theme(
      legend.position = 'right'
    ) +
    ggtitle(paste('Percent Minority -', category_name))
  
  p4 = ggplot(plot_df) +
    geom_sf(aes(fill = dem_voteshare_district, color = dem_voteshare_district)) +
    geom_sf(data = cd_borders, fill = NA, color = "darkred", linewidth = 0.5) +
    scale_fill_gradient(low = "lightgrey", high = "black", 
                        labels = scales::percent) +
    scale_color_gradient(low = "lightgrey", high = "black", 
                         labels = scales::percent) +
    theme_void() +
    labs(fill = '',
         color = '') +
    theme(
      legend.position = 'right'
    ) +
    ggtitle(paste('Democratic Vote Share -', category_name))
  
  # Combine all plots into one figure
  comb_plot = p1 + p2 + p3 + p4 + plot_layout(nrow = 2, ncol = 2)
  
  # Create directory if it doesn't exist
  dir.create(file.path(output_dir, "Figures"), showWarnings = FALSE, recursive = TRUE)
  
  # Save the plot with a sanitized filename
  sanitized_category = gsub("[%]", "pct", gsub("[ ]", "_", tolower(category_name)))
  plot_filename = paste0(output_dir, 'Figures/', sanitized_category, '_district_map.pdf')
  ggsave(plot_filename, comb_plot, width = 12, height = 10, dpi = 300)
  
  # Return the plot for display
  return(comb_plot)
}

visualize_voteshare_distribution = function(district_summaries, map_ids, map_summaries, category_name, output_dir) {
  # Filter for the specific map category and calculate statistics
  category_district_data = district_summaries %>% 
    filter(map_id %in% map_ids) %>%
    group_by(district_id) %>%
    summarise(
      avg_dem_voteshare = mean(dem_voteshare),
      se = sd(dem_voteshare) / sqrt(n()),
      ci_lo = avg_dem_voteshare - 1.96 * se,
      ci_hi = avg_dem_voteshare + 1.96 * se
    )
  
  # Get the ordering of districts based on avg_dem_voteshare
  district_order = category_district_data %>%
    arrange(avg_dem_voteshare) %>%
    pull(district_id)
  
  # Convert district_id to factor with custom ordering
  category_district_data = category_district_data %>%
    mutate(district_id = factor(district_id, levels = district_order))
  
  # Extract raw data and join with the ordered district ids
  raw_district_data = district_summaries %>%
    filter(map_id %in% map_ids) %>%
    mutate(district_id = factor(district_id, levels = district_order))
  
  # Create the voteshare distribution plot
  vote_dist_plot = ggplot() +
    # Add jittered raw data points
    geom_jitter(
      data = raw_district_data,
      aes(x = district_id, y = dem_voteshare),
      color = 'black', alpha = 0.1, pch = 1, width = 0.15
    ) +
    # Add horizontal line at 50%
    geom_hline(yintercept = 0.5, linetype = 2, color = 'gray30') +
    # Add main data points with confidence intervals
    geom_point(
      data = category_district_data,
      aes(x = district_id, y = avg_dem_voteshare),
      size = 4, color = "#4575b4"
    ) +
    geom_errorbar(
      data = category_district_data,
      aes(x = district_id, y = avg_dem_voteshare, ymin = ci_lo, ymax = ci_hi),
      width = 0.2, color = "#4575b4"
    ) +
    # Labels and theme
    labs(
      title = paste("Democratic Vote Share by District -", category_name),
      subtitle = paste0(
        "Average of ", length(map_ids), " plans, ",
        "Avg Democratic Seats: ", round(mean(map_summaries$n_dem_districts[map_summaries$map_id %in% map_ids]), 1)
      ),
      x = "Congressional District (ordered by Democratic vote share)",
      y = "Mean Democratic Vote Share"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank())
  
  # Save the plot
  sanitized_category = gsub("[%]", "pct", gsub("[ ]", "_", tolower(category_name)))
  plot_filename = paste0(output_dir, 'Figures/', gsub(" ", "_", tolower(sanitized_category)), '_voteshare_dist.pdf')
  ggsave(plot_filename, vote_dist_plot, width = 10, height = 6, dpi = 300)
  
  # Return the plot for display
  return(vote_dist_plot)
}

compare_voteshare_categories = function(district_summaries, top_rep_ids, top_dem_ids, middle_ids, output_dir) {
  # Process data for each category, maintaining original district IDs
  process_category = function(map_ids, category_name) {
    district_summaries %>% 
      filter(map_id %in% map_ids) %>%
      group_by(district_id) %>%
      summarise(
        avg_dem_voteshare = mean(dem_voteshare),
        se = sd(dem_voteshare) / sqrt(n()),
        ci_lo = avg_dem_voteshare - 1.96 * se,
        ci_hi = avg_dem_voteshare + 1.96 * se
      ) %>%
      mutate(category = category_name)
  }
  
  # Combine data from all categories
  combined_data = bind_rows(
    process_category(top_rep_ids, "Top Republican"),
    process_category(top_dem_ids, "Top Democratic"),
    process_category(middle_ids, "Middle 50%")
  )
  
  # Get the ordering from Middle 50% category
  middle_order = combined_data %>%
    filter(category == "Middle 50%") %>%
    arrange(avg_dem_voteshare) %>%
    pull(district_id)
  
  # Convert district_id to factor with custom ordering
  combined_data = combined_data %>%
    mutate(district_id = factor(district_id, levels = middle_order))
  
  # Create comparison plot using ordered district IDs
  comparison_plot = ggplot(combined_data, 
                           aes(x = district_id, y = avg_dem_voteshare, 
                               color = category, group = category)) +
    geom_line(linewidth = 1) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2) +
    geom_hline(yintercept = 0.5, linetype = 2, color = 'gray30') +
    scale_color_manual(
      values = c("Top Republican" = "#d73027", 
                 "Top Democratic" = "#1a9850", 
                 "Middle 50%" = "#4575b4"),
      name = "Map Category"
    ) +
    labs(
      title = "Comparison of Democratic Vote Share Across Map Categories",
      x = "District ID",
      y = "Mean Democratic Vote Share"
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
  
  # Save the plot
  plot_filename = paste0(output_dir, 'Figures/category_comparison.pdf')
  ggsave(plot_filename, comparison_plot, width = 10, height = 6, dpi = 300)
  
  # Return the plot for display
  return(comparison_plot)
}

create_all_visualizations = function(map_data, CD_plans, district_summaries, map_summaries, output_dir) {
  
  # map_data = map_data
  # CD_plans = CD_plans
  district_summaries = summaries$district_all
  map_summaries = summaries$map_all
  output_dir = 'Output/'
  
  # Create directory for plots if it doesn't exist
  dir.create(file.path(output_dir, "Figures"), showWarnings = FALSE, recursive = TRUE)
  
  # Define map categories
  top_rep_quantile = quantile(map_summaries$n_rep_districts, 0.975)
  top_rep_ids = map_summaries %>% 
    filter(n_rep_districts >= top_rep_quantile) %>% 
    pull(map_id)
  
  top_dem_quantile = quantile(map_summaries$n_dem_districts, 0.975)
  top_dem_ids = map_summaries %>% 
    filter(n_dem_districts >= top_dem_quantile) %>% 
    pull(map_id)
  
  rep_mid_low = quantile(map_summaries$n_rep_districts, 0.25)
  rep_mid_high = quantile(map_summaries$n_rep_districts, 0.75)
  middle_ids = map_summaries %>% 
    filter(n_rep_districts >= rep_mid_low, n_rep_districts <= rep_mid_high) %>% 
    pull(map_id)
  
  # Create district maps for each category
  cat("Creating district maps for each category...\n")
  top_rep_map = visualize_map_category(map_data, CD_plans, top_rep_ids, "Top Republican", output_dir)
  top_dem_map = visualize_map_category(map_data, CD_plans, top_dem_ids, "Top Democratic", output_dir)
  middle_map = visualize_map_category(map_data, CD_plans, middle_ids, "Middle 50%", output_dir)
  
  # Create vote share distribution plots for each category
  cat("Creating vote share distribution plots for each category...\n")
  top_rep_dist = visualize_voteshare_distribution(district_summaries, top_rep_ids, map_summaries, "Top Republican", output_dir)
  top_dem_dist = visualize_voteshare_distribution(district_summaries, top_dem_ids, map_summaries, "Top Democratic", output_dir)
  middle_dist = visualize_voteshare_distribution(district_summaries, middle_ids, map_summaries, "Middle 50%", output_dir)
  
  # Create comparison plot
  cat("Creating comparison plot...\n")
  comparison = compare_voteshare_categories(district_summaries, top_rep_ids, top_dem_ids, middle_ids, output_dir)
  
  # Return all plots
  return(list(
    top_rep_map = top_rep_map,
    top_dem_map = top_dem_map,
    middle_map = middle_map,
    top_rep_dist = top_rep_dist,
    top_dem_dist = top_dem_dist,
    middle_dist = middle_dist,
    comparison = comparison
  ))
}