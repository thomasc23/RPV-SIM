# Test synthetic data generation functions for rpvsimulator package

library(sf)
library(dplyr)
library(spatstat)

test_that("create_city_centers works correctly", {
  # Test with default parameters
  centers = create_city_centers()
  expect_equal(nrow(centers), 3)
  expect_true(all(c("id", "x", "y", "intensity") %in% names(centers)))
  expect_equal(centers$intensity[1], 1.0)
  
  # Test with custom parameters
  centers = create_city_centers(n_centers = 5, bounds = c(0, 100, 0, 100), seed = 42)
  expect_equal(nrow(centers), 5)
  expect_true(all(centers$x >= 5 & centers$x <= 95))  # Within bounds with buffer
  expect_true(all(centers$y >= 5 & centers$y <= 95))
})

test_that("create_density_surface works correctly", {
  centers = data.frame(x = c(0, 10), y = c(0, 10), intensity = c(1.0, 0.5))
  
  # Test at center
  density_at_center = create_density_surface(0, 0, centers)
  expect_true(density_at_center > 0)
  
  # Test away from centers
  density_away = create_density_surface(100, 100, centers)
  expect_true(density_away > 0)
  expect_true(density_away < density_at_center)
  
  # Test with different parameters
  density_custom = create_density_surface(0, 0, centers, 
                                        decay_rate = 5, 
                                        base_density = 0.1,
                                        peak_multiplier = 10.0)
  expect_true(density_custom > 0)
})

test_that("generate_seed_points works correctly", {
  centers = data.frame(x = c(0, 10), y = c(0, 10), intensity = c(1.0, 0.5))
  
  # Test with small number of points
  seeds = generate_seed_points(10, centers, bounds = c(0, 20, 0, 20), seed = 123)
  expect_equal(nrow(seeds), 10)
  expect_true(all(c("x", "y", "density") %in% names(seeds)))
  expect_true(all(seeds$x >= 0 & seeds$x <= 20))
  expect_true(all(seeds$y >= 0 & seeds$y <= 20))
  expect_true(all(seeds$density > 0))
})

test_that("create_precinct_boundaries works correctly", {
  # Create simple seed points
  seed_points = data.frame(
    x = c(1, 3, 1, 3),
    y = c(1, 1, 3, 3),
    density = c(1, 1, 1, 1)
  )
  
  precincts = create_precinct_boundaries(seed_points, bounds = c(0, 4, 0, 4))
  
  # Check that it returns an sf object
  expect_s3_class(precincts, "sf")
  
  # Check required columns
  expect_true("precinct_id" %in% names(precincts))
  expect_true("geometry" %in% names(precincts))
  expect_true("area" %in% names(precincts))
  expect_true("urbanness" %in% names(precincts))
  
  # Check that we have the right number of precincts
  expect_equal(nrow(precincts), 4)
  
  # Check that areas are positive
  expect_true(all(precincts$area > 0))
})

test_that("create_realistic_grid works correctly", {
  # Test with small grid
  result = create_realistic_grid(n_precincts = 20, n_centers = 2, seed = 123)
  
  # Check that it returns a list with the right components
  expect_true(is.list(result))
  expect_true("precincts" %in% names(result))
  expect_true("centers" %in% names(result))
  
  # Check precincts
  precincts = result$precincts
  expect_s3_class(precincts, "sf")
  expect_true(nrow(precincts) <= 20)  # May be slightly less due to edge effects
  
  # Check centers
  centers = result$centers
  expect_equal(nrow(centers), 2)
  expect_true(all(c("id", "x", "y", "intensity") %in% names(centers)))
  
  # Check that required columns are present
  required_cols = c("precinct_id", "geometry", "area", "urbanness", 
                   "dist_to_center", "nearest_center")
  expect_true(all(required_cols %in% names(precincts)))
})

test_that("create_realistic_grid handles different parameters", {
  # Test with custom bounds
  result = create_realistic_grid(n_precincts = 10, n_centers = 1, 
                               bounds = c(0, 50, 0, 50), seed = 456)
  
  expect_true(is.list(result))
  expect_s3_class(result$precincts, "sf")
  expect_equal(nrow(result$centers), 1)
  
  # Test with different decay parameters
  result2 = create_realistic_grid(n_precincts = 10, n_centers = 2,
                                decay_rate = 5, peak_multiplier = 20.0, seed = 789)
  
  expect_true(is.list(result2))
  expect_s3_class(result2$precincts, "sf")
})

test_that("synthetic data functions are reproducible with same seed", {
  # Test reproducibility
  result1 = create_realistic_grid(n_precincts = 10, n_centers = 2, seed = 999)
  result2 = create_realistic_grid(n_precincts = 10, n_centers = 2, seed = 999)
  
  # Should produce identical results
  expect_equal(result1$centers$x, result2$centers$x)
  expect_equal(result1$centers$y, result2$centers$y)
  expect_equal(nrow(result1$precincts), nrow(result2$precincts))
})
