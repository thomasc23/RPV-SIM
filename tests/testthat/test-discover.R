test_that("discover_scenarios finds maps/ even when Summaries/Plans missing", {
  withr::local_tempdir()
  dir <- getwd()
  fs::dir_create(fs::path(dir, "data", "low", "rep", "maps"))
  fs::file_create(fs::path(dir, "data", "precincts.csv"))              # present
  # create a couple of fake per-map CSVs
  readr::write_csv(tibble::tibble(seatshare_dem = c(0.5, 0.6)), fs::path(dir, "data", "low", "rep", "maps", "map_1.csv"))
  readr::write_csv(tibble::tibble(seatshare_dem = c(0.4, 0.7)), fs::path(dir, "data", "low", "rep", "maps", "2.csv"))
  
  sc <- discover_scenarios(fs::path(dir, "data"))
  lr <- dplyr::filter(sc, agg_level=="low", partisan=="rep")
  
  expect_true(lr$exists_maps_dir[[1]])
  expect_equal(lr$n_map_files[[1]], 2L)
  
  maps <- load_map_csvs_for_scenario(lr$maps_dir[[1]], "low", "rep")
  expect_true(nrow(maps) > 0)
  expect_true("map_id" %in% names(maps))
})
