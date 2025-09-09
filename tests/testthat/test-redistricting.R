# Test redistricting functions for rpvsimulator package

library(sf)
library(dplyr)
library(purrr)

# Create test precinct data with polygon geometries
test_precinct_data = data.frame(
  pct_id = 1:10,
  pop = rep(1000, 10),
  n_min = c(200, 300, 400, 500, 600, 700, 800, 900, 100, 150),
  n_maj = c(800, 700, 600, 500, 400, 300, 200, 100, 900, 850),
  dem_v = c(400, 450, 500, 550, 600, 650, 700, 750, 350, 375),
  rep_v = c(600, 550, 500, 450, 400, 350, 300, 250, 650, 625)
) %>%
  mutate(
    geometry = map(1:10, ~ st_polygon(list(matrix(c(
      .x, .x+1, .x+1, .x, .x,
      .x, .x, .x+1, .x+1, .x
    ), ncol = 2, byrow = FALSE))))
  ) %>%
  st_as_sf(crs = 4326)

# Create test district plan data
test_cd_plan = data.frame(
  pct_id = 1:10,
  district_1 = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
)

test_that("validate_redistricting_params works correctly", {
  # Test with valid parameters
  temp_dir = tempfile()
  dir.create(temp_dir)
  
  temp_shapefile = file.path(temp_dir, "test.shp")
  st_write(test_precinct_data, temp_shapefile, quiet = TRUE)
  
  expect_true(validate_redistricting_params(temp_shapefile, temp_dir, num_districts = 3))
  
  # Test with invalid shapefile
  expect_error(validate_redistricting_params("nonexistent.shp", temp_dir))
  
  # Test with invalid number of districts
  expect_error(validate_redistricting_params(temp_shapefile, temp_dir, num_districts = 1))
  
  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("create_district_summary works correctly", {
  result = create_district_summary(
    test_precinct_data,
    "test_plan",
    test_cd_plan,
    "pct_id"
  )
  
  expect_type(result, "list")
  expect_true("district_summary" %in% names(result))
  expect_true("map_summary" %in% names(result))
  
  # Check district summary
  expect_s3_class(result$district_summary, "data.frame")
  expect_true("n_precincts" %in% names(result$district_summary))
  expect_true("population" %in% names(result$district_summary))
  expect_true("dem_cd" %in% names(result$district_summary))
  
  # Check map summary
  expect_s3_class(result$map_summary, "data.frame")
  expect_true("n_districts" %in% names(result$map_summary))
  expect_true("n_dem_districts" %in% names(result$map_summary))
  
  # Check that we have the expected number of districts
  expect_equal(nrow(result$district_summary), 3)  # Three districts in test plan
})

test_that("create_map_summary works correctly", {
  # Create test district summary
  district_summary = data.frame(
    district_id = 1:3,
    n_precincts = c(3, 3, 4),
    population = c(3000, 3000, 4000),
    dem_cd = c(1, 0, 1),
    dem_vshare = c(0.6, 0.4, 0.7),
    per_minority = c(0.3, 0.2, 0.5)
  )
  
  result = create_map_summary(district_summary)
  
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 1)
  expect_true("n_districts" %in% names(result))
  expect_true("n_dem_districts" %in% names(result))
  expect_true("n_rep_districts" %in% names(result))
  
  # Check calculations
  expect_equal(result$n_districts, 3)
  expect_equal(result$n_dem_districts, 2)
  expect_equal(result$n_rep_districts, 1)
})

test_that("score_plans works correctly", {
  temp_dir = tempfile()
  dir.create(temp_dir)
  
  result = score_plans(
    test_precinct_data,
    test_cd_plan,
    "test_plan",
    temp_dir
  )
  
  expect_type(result, "list")
  expect_true("district_summary" %in% names(result))
  expect_true("map_summary" %in% names(result))
  
  # Check that files were created
  expect_true(file.exists(file.path(temp_dir, "district_summary.csv")))
  expect_true(file.exists(file.path(temp_dir, "map_summary.csv")))
  
  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("print_redistricting_summary works without errors", {
  # Create test summaries
  test_summaries = list(
    neutral = list(
      map_summary = data.frame(
        n_dem_districts = c(2, 3, 2),
        mean_dem_vshare = c(0.55, 0.60, 0.52)
      )
    ),
    republican = list(
      map_summary = data.frame(
        n_dem_districts = c(1, 2, 1),
        mean_dem_vshare = c(0.45, 0.50, 0.48)
      )
    ),
    democratic = list(
      map_summary = data.frame(
        n_dem_districts = c(3, 4, 3),
        mean_dem_vshare = c(0.65, 0.70, 0.68)
      )
    )
  )
  
  # Should not error
  expect_output(print_redistricting_summary(test_summaries, "medium"), "Redistricting Summary")
})

test_that("redistricting functions handle edge cases", {
  # Test with single district
  single_district_plan = data.frame(
    pct_id = 1:10,
    district_1 = rep(1, 10)
  )
  
  result = create_district_summary(
    test_precinct_data,
    "single_district",
    single_district_plan,
    "pct_id"
  )
  
  expect_s3_class(result$district_summary, "data.frame")
  expect_equal(nrow(result$district_summary), 1)  # One district
  
  # Test with empty plan
  empty_plan = data.frame(
    pct_id = integer(0),
    district_1 = integer(0)
  )
  
  # Empty plan should work but return empty results
  result_empty = create_district_summary(
    test_precinct_data,
    "empty",
    empty_plan,
    "pct_id"
  )
  expect_equal(nrow(result_empty$district_summary), 0)
})

test_that("redistricting functions work with different data types", {
  # Test with different column names
  custom_plan = data.frame(
    pct_id = 1:10,  # Match the test data column name
    district = c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3)
  )
  
  result = create_district_summary(
    test_precinct_data,
    "custom",
    custom_plan,
    "pct_id"
  )
  
  expect_type(result, "list")
  expect_true("district_summary" %in% names(result))
})

# Note: We don't test run_redistricting and setup_python_environment 
# as they require Python environment setup and external dependencies
