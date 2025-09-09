# Test utility functions for rpvsimulator package

test_that("safe_div works correctly", {
  # Test normal division
  expect_equal(safe_div(10, 2), 5)
  expect_equal(safe_div(15, 3), 5)
  
  # Test division by zero
  expect_equal(safe_div(10, 0), 0)
  expect_equal(safe_div(0, 0), 0)
  
  # Test negative numbers
  expect_equal(safe_div(-10, 2), -5)
  expect_equal(safe_div(10, -2), -5)
  
  # Test with vectors
  expect_equal(safe_div(c(10, 20, 30), c(2, 4, 0)), c(5, 5, 0))
})

test_that("int_alloc_by_weights works correctly", {
  # Test normal allocation
  expect_equal(int_alloc_by_weights(10, c(0.3, 0.7)), c(3, 7))
  expect_equal(int_alloc_by_weights(100, c(0.25, 0.25, 0.5)), c(25, 25, 50))
  
  # Test with zero votes
  expect_equal(int_alloc_by_weights(0, c(0.3, 0.7)), c(0, 0))
  
  # Test with zero weights
  expect_equal(int_alloc_by_weights(10, c(0, 0)), c(0, 0))
  
  # Test with NA weights
  expect_equal(int_alloc_by_weights(10, c(0.5, NA)), c(10, 0))
  
  # Test that sum equals total
  result = int_alloc_by_weights(100, c(0.33, 0.33, 0.34))
  expect_equal(sum(result), 100)
})

test_that("adjust_group_D_to_match works correctly", {
  # Test perfect match (no adjustment needed)
  expect_equal(adjust_group_D_to_match(c(5, 10), c(10, 20), 15), c(5, 10))
  
  # Test need to add Democratic votes
  result = adjust_group_D_to_match(c(3, 5), c(10, 20), 15)
  expect_equal(sum(result), 15)
  expect_true(all(result <= c(10, 20)))
  
  # Test need to subtract Democratic votes
  result = adjust_group_D_to_match(c(8, 12), c(10, 20), 15)
  expect_equal(sum(result), 15)
  expect_true(all(result >= 0))
  
  # Test edge case: zero total votes
  expect_equal(adjust_group_D_to_match(c(0, 0), c(0, 0), 0), c(0, 0))
})

test_that("build_corr works correctly", {
  # Test with default parameters
  R = build_corr()
  expect_equal(dim(R), c(3, 3))
  expect_equal(diag(R), c(1, 1, 1))
  expect_equal(R[1, 2], R[2, 1])  # Symmetric
  
  # Test with custom parameters
  R = build_corr(rho_wb = 0.8, rho_wh = 0.3, rho_bh = 0.6)
  expect_equal(R[1, 2], 0.8)
  expect_equal(R[1, 3], 0.3)
  expect_equal(R[2, 3], 0.6)
  
  # Test with existing matrix
  existing_R = matrix(c(1, 0.5, 0.3, 0.5, 1, 0.4, 0.3, 0.4, 1), nrow = 3)
  result = build_corr(R = existing_R)
  expect_equal(result, existing_R)
  
  # Test that result is positive definite
  R = build_corr()
  eigenvals = eigen(R, symmetric = TRUE)$values
  expect_true(all(eigenvals > 0))
})
