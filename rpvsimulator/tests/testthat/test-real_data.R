# Test real data processing functions for rpvsimulator package

library(sf)
library(dplyr)

# Create test Texas data with proper column names
test_tx_data = data.frame(
  prec_id = 1:5,
  dem_vts = c(300, 600, 450, 900, 750),
  rep_vts = c(200, 400, 300, 600, 500),
  cvap_tt = c(1000, 2000, 1500, 3000, 2500),  # Total CVAP
  cvp_wht = c(800, 1600, 1200, 2400, 2000),
  cvp_blk = c(100, 200, 150, 300, 250),
  cvp_hsp = c(100, 200, 150, 300, 250),
  VTDKEY = paste0("VTD", 1:5),
  x = c(1, 2, 3, 4, 5),
  y = c(1, 2, 3, 4, 5)
) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326)

test_that("load_tx_shapefile works correctly", {
  # Test with non-existent file
  expect_error(load_tx_shapefile("nonexistent.shp"))
  
  # Test with valid file (we'll create a temporary one)
  temp_file = tempfile(fileext = ".shp")
  st_write(test_tx_data, temp_file, quiet = TRUE)
  
  result = load_tx_shapefile(temp_file)
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(test_tx_data))
  
  # Clean up
  unlink(temp_file)
  unlink(gsub("\\.shp$", ".*", temp_file))
})

test_that("load_tx_shapefile validates required columns", {
  # Create data with missing columns
  incomplete_data = test_tx_data %>% select(-dem_vts)
  temp_file = tempfile(fileext = ".shp")
  st_write(incomplete_data, temp_file, quiet = TRUE)
  
  expect_error(load_tx_shapefile(temp_file), "Missing required Texas columns")
  
  # Clean up
  unlink(temp_file)
  unlink(gsub("\\.shp$", ".*", temp_file))
})

test_that("validate_tx_data works correctly", {
  # Test with valid data
  expect_true(validate_tx_data(test_tx_data))
  
  # Test with missing columns
  invalid_data = test_tx_data %>% select(-dem_vts)
  expect_error(validate_tx_data(invalid_data), "Missing required Texas columns")
  
  # Test with negative values
  invalid_data = test_tx_data %>% mutate(dem_vts = -100)
  expect_error(validate_tx_data(invalid_data), "Democratic votes cannot be negative")
  
  # Test with missing precinct IDs
  invalid_data = test_tx_data %>% mutate(prec_id = NA)
  expect_error(validate_tx_data(invalid_data), "Precinct ID cannot be missing")
  
  # Test with duplicate precinct IDs
  invalid_data = test_tx_data %>% mutate(prec_id = 1)
  expect_error(validate_tx_data(invalid_data), "Duplicate precinct IDs found")
})

test_that("prepare_tx_for_redistricting works with valid data", {
  # Create temporary input file
  temp_input = tempfile(fileext = ".shp")
  temp_output = tempfile(fileext = ".shp")
  
  st_write(test_tx_data, temp_input, quiet = TRUE)
  
  # Test the function
  result = prepare_tx_for_redistricting(
    in_shapefile = temp_input,
    out_shapefile = temp_output,
    seed = 123
  )
  
  # Check that it returns an sf object
  expect_s3_class(result, "sf")
  
  # Check required output columns
  expected_cols = c("pct_id", "orig_id", "pop", "n_min", "n_maj", 
                   "dem_v", "rep_v", "dem_v_min", "rep_v_min", 
                   "dem_v_maj", "rep_v_maj", "pct_min", "dem_vsh", 
                   "dem_vsh_1", "dem_vsh_0", "geometry")
  expect_true(all(expected_cols %in% names(result)))
  
  # Check that output file was created
  expect_true(file.exists(temp_output))
  
  # Check data consistency
  expect_equal(nrow(result), nrow(test_tx_data))
  expect_true(all(result$dem_v + result$rep_v == result$pop))
  expect_true(all(result$dem_v == result$dem_v_min + result$dem_v_maj))
  expect_true(all(result$rep_v == result$rep_v_min + result$rep_v_maj))
  
  # Clean up
  unlink(temp_input)
  unlink(gsub("\\.shp$", ".*", temp_input))
  unlink(temp_output)
  unlink(gsub("\\.shp$", ".*", temp_output))
})

test_that("prepare_tx_for_redistricting handles different EI parameters", {
  # Create temporary input file
  temp_input = tempfile(fileext = ".shp")
  temp_output = tempfile(fileext = ".shp")
  
  st_write(test_tx_data, temp_input, quiet = TRUE)
  
  # Test with custom EI parameters
  result = prepare_tx_for_redistricting(
    in_shapefile = temp_input,
    out_shapefile = temp_output,
    ei_means = c(white = 0.4, black = 0.8, hisp = 0.6),
    ei_sds = c(white = 0.2, black = 0.15, hisp = 0.1),
    ei_corr = list(rho_wb = 0.3, rho_wh = 0.4, rho_bh = 0.2),
    seed = 456
  )
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(test_tx_data))
  
  # Clean up
  unlink(temp_input)
  unlink(gsub("\\.shp$", ".*", temp_input))
  unlink(temp_output)
  unlink(gsub("\\.shp$", ".*", temp_output))
})

test_that("prepare_tx_for_redistricting is reproducible with same seed", {
  # Create temporary input file
  temp_input = tempfile(fileext = ".shp")
  temp_output1 = tempfile(fileext = ".shp")
  temp_output2 = tempfile(fileext = ".shp")
  
  st_write(test_tx_data, temp_input, quiet = TRUE)
  
  # Run twice with same seed
  result1 = prepare_tx_for_redistricting(
    in_shapefile = temp_input,
    out_shapefile = temp_output1,
    seed = 999
  )
  
  result2 = prepare_tx_for_redistricting(
    in_shapefile = temp_input,
    out_shapefile = temp_output2,
    seed = 999
  )
  
  # Should produce identical results
  expect_equal(result1$dem_v, result2$dem_v)
  expect_equal(result1$rep_v, result2$rep_v)
  expect_equal(result1$dem_v_min, result2$dem_v_min)
  expect_equal(result1$dem_v_maj, result2$dem_v_maj)
  
  # Clean up
  unlink(temp_input)
  unlink(gsub("\\.shp$", ".*", temp_input))
  unlink(temp_output1)
  unlink(gsub("\\.shp$", ".*", temp_output1))
  unlink(temp_output2)
  unlink(gsub("\\.shp$", ".*", temp_output2))
})

test_that("prepare_tx_for_redistricting handles edge cases", {
  # Create data with zero votes
  zero_vote_data = test_tx_data %>%
    mutate(dem_vts = 0, rep_vts = 0)
  
  temp_input = tempfile(fileext = ".shp")
  temp_output = tempfile(fileext = ".shp")
  
  st_write(zero_vote_data, temp_input, quiet = TRUE)
  
  # Should not error
  result = prepare_tx_for_redistricting(
    in_shapefile = temp_input,
    out_shapefile = temp_output,
    seed = 123
  )
  
  expect_s3_class(result, "sf")
  expect_equal(nrow(result), nrow(zero_vote_data))
  
  # Clean up
  unlink(temp_input)
  unlink(gsub("\\.shp$", ".*", temp_input))
  unlink(temp_output)
  unlink(gsub("\\.shp$", ".*", temp_output))
})
