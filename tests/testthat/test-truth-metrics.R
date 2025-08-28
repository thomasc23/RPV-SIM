test_that("truth metrics compute RPV and waste even with missing group votes", {
  # Tiny synthetic precincts + plan
  precincts <- tibble::tibble(
    precinct_id = 1:4,
    population = c(100,100,100,100),
    n_minority = c(60, 20, 50, 10),
    n_majority = population - n_minority,
    dem_votes  = c(60,40,50,30),
    rep_votes  = population - dem_votes,
    dem_votes_minority = c(40,10,30,5),
    dem_votes_majority = dem_votes - dem_votes_minority
  )
  plans <- tibble::tibble(precinct_id = 1:4, step_1 = c(1,1,2,2))
  
  ps <- build_precinct_stats(plans, precincts, map_id = 1, agg_level = "low", partisan = "rep")
  truth <- calculate_ground_truth(ps)
  expect_equal(unique(truth$map_id), 1)
  expect_true(all(truth$total_population == 200))
  expect_true(all(!is.na(truth$true_rpv)))
  
  waste <- calculate_wasted_votes(truth)
  expect_true(all(waste$total_dem_votes + waste$total_rep_votes == waste$total_population))
  expect_true(all(!is.na(waste$minority_waste_rate)))
})
