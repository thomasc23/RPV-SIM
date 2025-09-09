# Test vote modeling functions for rpvsimulator package

library(sf)
library(dplyr)
library(purrr)

# Create test precinct data with polygon geometries
test_precinct_data = data.frame(
  precinct_id = 1:10,
  population = rep(1000, 10),
  n_minority = c(200, 300, 400, 500, 600, 700, 800, 900, 100, 150),
  n_majority = c(800, 700, 600, 500, 400, 300, 200, 100, 900, 850),
  per_minority = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.1, 0.15)
) %>%
  mutate(
    geometry = map(1:10, ~ st_polygon(list(matrix(c(
      .x, .x+1, .x+1, .x, .x,
      .x, .x, .x+1, .x+1, .x
    ), ncol = 2, byrow = FALSE))))
  ) %>%
  st_as_sf(crs = 4326)

test_that("add_baseline_votes works correctly", {
  result = add_baseline_votes(test_precinct_data, seed = 123)
  
  # Check that vote columns are added
  expect_true("dem_votes" %in% names(result))
  expect_true("rep_votes" %in% names(result))
  expect_true("dem_votes_minority" %in% names(result))
  expect_true("dem_votes_majority" %in% names(result))
  expect_true("dem_voteshare" %in% names(result))
  
  # Check that votes are non-negative
  expect_true(all(result$dem_votes >= 0))
  expect_true(all(result$rep_votes >= 0))
  expect_true(all(result$dem_votes_minority >= 0))
  expect_true(all(result$dem_votes_majority >= 0))
  
  # Check that votes sum to population
  expect_equal(result$dem_votes + result$rep_votes, result$population)
  expect_equal(result$dem_votes_minority + result$rep_votes_minority, result$n_minority)
  expect_equal(result$dem_votes_majority + result$rep_votes_majority, result$n_majority)
  
  # Check that voteshares are between 0 and 1
  expect_true(all(result$dem_voteshare >= 0 & result$dem_voteshare <= 1))
})

test_that("add_baseline_votes is reproducible with same seed", {
  result1 = add_baseline_votes(test_precinct_data, seed = 123)
  result2 = add_baseline_votes(test_precinct_data, seed = 123)
  
  expect_equal(result1$dem_votes, result2$dem_votes)
  expect_equal(result1$rep_votes, result2$rep_votes)
  expect_equal(result1$dem_voteshare, result2$dem_voteshare)
})

test_that("add_baseline_votes handles different RPV parameters", {
  result = add_baseline_votes(
    test_precinct_data, 
    prob_minority_dem = 0.9,
    prob_majority_dem = 0.2,
    sd_minority = 0.05,
    sd_majority = 0.15,
    rho = 0.1,
    seed = 123
  )
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(test_precinct_data))
  expect_true(all(result$dem_votes + result$rep_votes == result$population))
})

test_that("apply_vote_model works with ei model", {
  result = apply_vote_model(
    test_precinct_data,
    voting_model = "ei",
    seed = 123
  )
  
  # Check that vote columns are added
  expect_true("dem_votes" %in% names(result))
  expect_true("rep_votes" %in% names(result))
  expect_true("dem_voteshare" %in% names(result))
  expect_true("prob_minority_dem" %in% names(result))
  expect_true("prob_majority_dem" %in% names(result))
  
  # Check that votes are non-negative
  expect_true(all(result$dem_votes >= 0))
  expect_true(all(result$rep_votes >= 0))
  
  # Check that votes sum to population
  expect_equal(result$dem_votes + result$rep_votes, result$population)
})

test_that("apply_vote_model works with contextual model", {
  result = apply_vote_model(
    test_precinct_data,
    voting_model = "contextual",
    b_min_context = 0.2,
    b_maj_context = -0.1,
    seed = 123
  )
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(test_precinct_data))
  expect_true(all(result$dem_votes + result$rep_votes == result$population))
})

test_that("apply_vote_model handles different parameters", {
  result = apply_vote_model(
    test_precinct_data,
    prob_minority_dem = 0.85,
    prob_majority_dem = 0.25,
    sd_minority = 0.08,
    sd_majority = 0.25,
    rho = 0.15,
    sigma_precinct = 0.1,
    field_rho = 0.8,
    seed = 123
  )
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(test_precinct_data))
  expect_true(all(result$dem_votes + result$rep_votes == result$population))
})

test_that("place_voters_on_map works with different segregation levels", {
  # Create test grid result with vote data
  test_grid_result = list(
    precincts = test_precinct_data %>%
      mutate(
        urbanness = runif(n()),
        dem_voteshare = runif(n())
      )
  )
  
  # Create test precinct data with vote information
  test_precinct_data_with_votes = test_precinct_data %>%
    mutate(dem_voteshare = runif(n()))
  
  # Test low segregation
  result_low = place_voters_on_map(
    test_precinct_data_with_votes,
    test_grid_result,
    segregation_level = "low",
    seed = 123
  )
  
  expect_s3_class(result_low, "sf")
  expect_equal(nrow(result_low), nrow(test_precinct_data))
  
  # Test medium segregation
  result_medium = place_voters_on_map(
    test_precinct_data_with_votes,
    test_grid_result,
    segregation_level = "medium",
    seed = 123
  )
  
  expect_s3_class(result_medium, "sf")
  expect_equal(nrow(result_medium), nrow(test_precinct_data))
  
  # Test high segregation
  result_high = place_voters_on_map(
    test_precinct_data_with_votes,
    test_grid_result,
    segregation_level = "high",
    seed = 123
  )
  
  expect_s3_class(result_high, "sf")
  expect_equal(nrow(result_high), nrow(test_precinct_data))
})

test_that("calculate_rpv_metrics works correctly", {
  # Add some vote data to test
  test_data_with_votes = test_precinct_data %>%
    mutate(
      dem_votes = c(400, 450, 500, 550, 600, 650, 700, 750, 350, 375),
      rep_votes = c(600, 550, 500, 450, 400, 350, 300, 250, 650, 625),
      dem_voteshare = dem_votes / population,
      dem_voteshare_minority = c(0.8, 0.75, 0.7, 0.65, 0.6, 0.55, 0.5, 0.45, 0.9, 0.85),
      dem_voteshare_majority = c(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.25, 0.3)
    )
  
  metrics = calculate_rpv_metrics(test_data_with_votes)
  
  expect_s3_class(metrics, "data.frame")
  expect_equal(nrow(metrics), 1)
  expect_true("total_precincts" %in% names(metrics))
  expect_true("total_population" %in% names(metrics))
  expect_true("rpv_gap" %in% names(metrics))
  
  # Check that RPV gap is calculated correctly
  expected_gap = mean(test_data_with_votes$dem_voteshare_minority) - 
                 mean(test_data_with_votes$dem_voteshare_majority)
  expect_equal(metrics$rpv_gap, expected_gap)
})

test_that("simulate_rpv_sensitivity works correctly", {
  # Create test data with vote information
  test_data_with_votes = test_precinct_data %>%
    mutate(
      dem_votes = c(400, 450, 500, 550, 600, 650, 700, 750, 350, 375),
      rep_votes = c(600, 550, 500, 450, 400, 350, 300, 250, 650, 625),
      dem_voteshare = dem_votes / population,
      dem_voteshare_minority = c(0.8, 0.75, 0.7, 0.65, 0.6, 0.55, 0.5, 0.45, 0.9, 0.85),
      dem_voteshare_majority = c(0.3, 0.35, 0.4, 0.45, 0.5, 0.55, 0.6, 0.65, 0.25, 0.3)
    )
  
  # Test with default scenarios
  results = simulate_rpv_sensitivity(test_data_with_votes, seed = 123)
  
  expect_type(results, "list")
  expect_true("low" %in% names(results))
  expect_true("medium" %in% names(results))
  expect_true("high" %in% names(results))
  
  # Check that each scenario has data and metrics
  for (scenario in names(results)) {
    expect_true("data" %in% names(results[[scenario]]))
    expect_true("metrics" %in% names(results[[scenario]]))
    expect_s3_class(results[[scenario]]$data, "sf")
    expect_s3_class(results[[scenario]]$metrics, "data.frame")
  }
})

test_that("simulate_rpv_sensitivity works with custom scenarios", {
  custom_scenarios = list(
    scenario1 = list(prob_minority_dem = 0.6, prob_majority_dem = 0.5),
    scenario2 = list(prob_minority_dem = 0.8, prob_majority_dem = 0.3)
  )
  
  results = simulate_rpv_sensitivity(
    test_precinct_data, 
    rpv_scenarios = custom_scenarios,
    seed = 123
  )
  
  expect_type(results, "list")
  expect_true("scenario1" %in% names(results))
  expect_true("scenario2" %in% names(results))
  
  # Check that each scenario has data and metrics
  for (scenario in names(results)) {
    expect_true("data" %in% names(results[[scenario]]))
    expect_true("metrics" %in% names(results[[scenario]]))
    expect_s3_class(results[[scenario]]$data, "sf")
    expect_s3_class(results[[scenario]]$metrics, "data.frame")
  }
})

test_that("vote modeling functions handle edge cases", {
  # Test with zero population
  zero_pop_data = test_precinct_data %>%
    mutate(population = 0, n_minority = 0, n_majority = 0)
  
  result = add_baseline_votes(zero_pop_data, seed = 123)
  expect_true(all(result$dem_votes == 0))
  expect_true(all(result$rep_votes == 0))
  
  # Test with single precinct
  single_precinct = test_precinct_data[1, ]
  result = apply_vote_model(single_precinct, seed = 123)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), 1)
})
