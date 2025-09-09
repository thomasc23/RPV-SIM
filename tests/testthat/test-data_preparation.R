# Test data preparation functions for rpvsimulator package

library(sf)
library(dplyr)
library(spdep)

# Create test data with polygon geometries
test_precincts = data.frame(
  precinct_id = 1:5,
  population = c(1000, 2000, 1500, 3000, 2500),
  n_minority = c(100, 600, 300, 1200, 375),
  n_majority = c(900, 1400, 1200, 1800, 2125),
  dem_votes = c(300, 600, 450, 900, 750),
  rep_votes = c(200, 400, 300, 600, 500),
  dem_votes_minority = c(150, 300, 225, 450, 375),
  rep_votes_minority = c(50, 100, 75, 150, 125),
  dem_votes_majority = c(150, 300, 225, 450, 375),
  rep_votes_majority = c(150, 300, 225, 450, 375),
  per_minority = c(0.1, 0.3, 0.2, 0.4, 0.15),
  dem_voteshare = c(0.6, 0.6, 0.6, 0.6, 0.6),
  dem_voteshare_minority = c(0.75, 0.75, 0.75, 0.75, 0.75),
  dem_voteshare_majority = c(0.5, 0.5, 0.5, 0.5, 0.5),
  urbanness = c(0.1, 0.2, 0.15, 0.3, 0.25)
) %>%
  mutate(
    geometry = st_sfc(
      st_polygon(list(matrix(c(0,0,1,0,1,1,0,1,0,0), ncol=2, byrow=TRUE))),
      st_polygon(list(matrix(c(1,0,2,0,2,1,1,1,1,0), ncol=2, byrow=TRUE))),
      st_polygon(list(matrix(c(2,0,3,0,3,1,2,1,2,0), ncol=2, byrow=TRUE))),
      st_polygon(list(matrix(c(0,1,1,1,1,2,0,2,0,1), ncol=2, byrow=TRUE))),
      st_polygon(list(matrix(c(1,1,2,1,2,2,1,2,1,1), ncol=2, byrow=TRUE)))
    )
  ) %>%
  st_as_sf(crs = 4326)

test_that("prepare_for_redistricting works correctly", {
  result = prepare_for_redistricting(test_precincts)
  
  # Check that it returns an sf object
  expect_s3_class(result, "sf")
  
  # Check required columns are present
  expected_cols = c("pct_id", "pop", "n_min", "n_maj", "dem_v", "rep_v", 
                   "dem_v_min", "rep_v_min", "dem_v_maj", "rep_v_maj",
                   "pct_min", "dem_vsh", "dem_vsh_1", "dem_vsh_0", "urb")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that values are correctly mapped
  expect_equal(result$pct_id, test_precincts$precinct_id)
  expect_equal(result$pop, test_precincts$population)
  expect_equal(result$dem_v, test_precincts$dem_votes)
})

test_that("load_and_validate_shapefile works correctly", {
  # Test with non-existent file
  expect_error(load_and_validate_shapefile("nonexistent.shp"))
  
  # Test with valid file (we'll create a temporary one with shorter column names)
  temp_data = test_precincts %>%
    select(precinct_id, population, dem_votes, rep_votes) %>%
    rename(pct_id = precinct_id, pop = population, dem = dem_votes, rep = rep_votes)
  
  temp_file = tempfile(fileext = ".shp")
  st_write(temp_data, temp_file, quiet = TRUE)
  
  result = load_and_validate_shapefile(temp_file)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(temp_data))
  
  # Clean up
  unlink(temp_file)
  unlink(gsub("\\.shp$", ".*", temp_file))
})

test_that("standardize_precinct_data works correctly", {
  # Create test data with different column names
  test_data = test_precincts %>%
    select(precinct_id, population, dem_votes, rep_votes) %>%
    rename(
      pct_id = precinct_id,
      pop = population,
      dem = dem_votes,
      rep = rep_votes
    )
  
  result = standardize_precinct_data(test_data, 
                                   id_col = "pct_id",
                                   pop_col = "pop",
                                   dem_col = "dem", 
                                   rep_col = "rep")
  
  # Check that columns are renamed correctly
  expect_true("precinct_id" %in% names(result))
  expect_true("population" %in% names(result))
  expect_true("dem_votes" %in% names(result))
  expect_true("rep_votes" %in% names(result))
  
  # Check that values are preserved
  expect_equal(result$precinct_id, test_data$pct_id)
  expect_equal(result$population, test_data$pop)
})

test_that("validate_precinct_data works correctly", {
  # Test with valid data
  expect_true(validate_precinct_data(test_precincts))
  
  # Test with missing columns
  invalid_data = test_precincts %>% select(-precinct_id)
  expect_error(validate_precinct_data(invalid_data), "Missing required columns")
  
  # Test with negative values
  invalid_data = test_precincts %>% mutate(population = -1000)
  expect_error(validate_precinct_data(invalid_data), "Population cannot be negative")
  
  # Test with missing precinct IDs
  invalid_data = test_precincts %>% mutate(precinct_id = NA)
  expect_error(validate_precinct_data(invalid_data), "Precinct ID cannot be missing")
  
  # Test with duplicate precinct IDs
  invalid_data = test_precincts %>% mutate(precinct_id = 1)
  expect_error(validate_precinct_data(invalid_data), "Duplicate precinct IDs found")
})

test_that("verify_fixed_population_consistency works correctly", {
  # Create mock simulation results with proper structure
  simulation_results = list(
    low = list(precincts = test_precincts %>% mutate(per_minority = n_minority / population)),
    medium = list(precincts = test_precincts %>% mutate(per_minority = n_minority / population)),
    high = list(precincts = test_precincts %>% mutate(per_minority = n_minority / population))
  )
  
  # This should run without error (but may have warnings about spatial analysis)
  expect_true(verify_fixed_population_consistency(simulation_results))
})
