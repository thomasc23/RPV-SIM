# Redistricting functions for rpvsimulator package

#' Setup Python environment for redistricting analysis using uv
#'
#' This function uses uv (https://docs.astral.sh/uv/) to manage the Python
#' environment. uv sync will create the virtual environment and install
#' all dependencies specified in pyproject.toml.
#'
#' @param project_dir path to the Python project directory (containing pyproject.toml)
#' @return path to the Python executable (invisibly)
#' @export
setup_python_environment = function(project_dir = NULL) {
  # Default to the inst/python directory in the package
  if (is.null(project_dir)) {
    project_dir = system.file("python", package = "rpvsimulator")
    if (project_dir == "") {
      # Fallback for development: use local inst/python
      project_dir = file.path(getwd(), "inst", "python")
    }
  }

  venv_path = file.path(project_dir, ".venv")

  # Check if uv is available
  uv_check = suppressWarnings(system2("uv", "--version", stdout = TRUE, stderr = TRUE))
  if (!is.null(attr(uv_check, "status")) && attr(uv_check, "status") != 0) {
    stop(
      "uv not found. Please install uv first:\n",
      "  macOS/Linux: curl -LsSf https://astral.sh/uv/install.sh | sh\n",
      "  or via Homebrew: brew install uv\n",
      "  Windows: powershell -ExecutionPolicy ByPass -c \"irm https://astral.sh/uv/install.ps1 | iex\""
    )
  }

  # Run uv sync to create venv and install dependencies
  # uv sync handles everything: creates .venv if needed, installs/updates deps
  message("Syncing Python environment with uv...")
  result = system2(
    "uv", "sync",
    stdout = TRUE,
    stderr = TRUE,
    env = c(paste0("UV_PROJECT_ENVIRONMENT=", venv_path)),
    wait = TRUE
  )

  status = attr(result, "status")
  if (!is.null(status) && status != 0) {
    stop("uv sync failed:\n", paste(result, collapse = "\n"))
  }

  # Configure reticulate to use the venv
  if (.Platform$OS.type == "windows") {
    python_path = file.path(venv_path, "Scripts", "python.exe")
  } else {
    python_path = file.path(venv_path, "bin", "python")
  }

  if (!file.exists(python_path)) {
    stop("Python executable not found at: ", python_path)
  }

  reticulate::use_python(python_path, required = TRUE)

  message("Python environment configured. Using: ", python_path)
  invisible(python_path)
}

#' Run redistricting analysis using Python gerrychain
#' @param shapefile_path path to input shapefile
#' @param output_dir output directory for results
#' @param num_steps number of MCMC steps for neutral redistricting
#' @param ensemble_size number of plans in biased ensembles
#' @param pop_deviation population deviation tolerance
#' @param num_districts number of districts to create
#' @param target_pop target population per district
#' @param dev_mode whether to use development mode
#' @param burst_length length of each burst in dev mode
#' @param num_bursts number of bursts in dev mode
#' @param patience_bursts patience for burst convergence
#' @param soft_k soft constraint parameter
#' @param simplify_tolerance geometry simplification tolerance
#' @return list of redistricting results
run_redistricting = function(shapefile_path,
                             output_dir,
                             num_steps            = 1000,
                             ensemble_size        = 50,
                             pop_deviation        = 0.01,
                             num_districts        = NULL,
                             target_pop           = NULL,
                             dev_mode             = TRUE,
                             burst_length         = 250,
                             num_bursts           = 20,
                             patience_bursts      = 8,
                             soft_k               = 60,
                             simplify_tolerance   = NA) {
  
  # Setup Python environment
  setup_python_environment()
  
  # Load Python script
  python_script = system.file("python", "run_recom_short_bursts.py", package = "rpvsimulator")
  if (!file.exists(python_script)) {
    stop("Python script not found: ", python_script)
  }
  
  message("Loading Python script:", python_script)
  py_run_file(python_script)
  
  # Create clean directory structure
  subdirs = c("neutral", "republican", "democratic")
  
  for (subdir in subdirs) {
    dir.create(paste0(output_dir, subdir), recursive = TRUE, showWarnings = FALSE)
  }
  
  results = list()
  
  # 1. Neutral redistricting
  message("\nRunning neutral redistricting analysis...")
  results$neutral = py$run_redistricting_analysis(
    shapefile_path = shapefile_path,
    output_dir     = paste0(output_dir, "neutral/"),
    num_steps      = as.integer(num_steps),
    pop_deviation  = as.numeric(pop_deviation),
    num_districts  = as.integer(num_districts)
  )
  
  # 2. Republican gerrymander ensemble
  message("\nRunning Republican gerrymandering ensemble...")
  results$republican = py$create_biased_ensemble(
    shapefile_path     = shapefile_path,
    output_dir         = paste0(output_dir, "republican/"),
    ensemble_size      = as.integer(ensemble_size),
    bias_type          = "republican",
    pop_deviation      = as.numeric(pop_deviation),
    num_districts      = as.integer(num_districts),
    # Dev mode parameters
    dev_mode           = dev_mode,
    burst_length       = as.integer(burst_length),
    num_bursts         = as.integer(num_bursts),
    patience_bursts    = as.integer(patience_bursts),
    soft_k             = as.numeric(soft_k),
    simplify_tolerance = if (is.na(simplify_tolerance)) NULL else as.numeric(simplify_tolerance)
  )
  
  # 3. Democratic gerrymander ensemble
  message("\nRunning Democratic gerrymandering ensemble...")
  results$democratic = py$create_biased_ensemble(
    shapefile_path     = shapefile_path,
    output_dir         = paste0(output_dir, "democratic/"),
    ensemble_size      = as.integer(ensemble_size),
    bias_type          = "democratic",
    pop_deviation      = as.numeric(pop_deviation),
    num_districts      = as.integer(num_districts),
    # Dev mode parameters
    dev_mode           = dev_mode,
    burst_length       = as.integer(burst_length),
    num_bursts         = as.integer(num_bursts),
    patience_bursts    = as.integer(patience_bursts),
    soft_k             = as.numeric(soft_k),
    simplify_tolerance = if (is.na(simplify_tolerance)) NULL else as.numeric(simplify_tolerance)
  )
  
  return(results)
}

#' Process redistricting results and create summaries
#' @param map_data sf object with precinct data
#' @param redistricting_results list of redistricting results
#' @param current_output_dir output directory
#' @return list of processed summaries
process_redistricting_results = function(map_data, redistricting_results, current_output_dir) {
  
  summaries = list()
  
  # Process neutral results
  if (!is.null(redistricting_results$neutral)) {
    neutral_plans_file = paste0(current_output_dir, "neutral/CD_plans.csv")
    if (file.exists(neutral_plans_file)) {
      CD_plans_neutral = data.table::fread(neutral_plans_file) %>%
        mutate(plan_id = row_number())
      
      summaries$neutral = create_district_summary(
        map_data, 
        "neutral", 
        CD_plans_neutral, 
        "pct_id"
      )
    }
  }
  
  # Process Republican ensemble
  if (!is.null(redistricting_results$republican)) {
    rep_ensemble_file = paste0(current_output_dir, "republican/CD_plans.csv")
    if (file.exists(rep_ensemble_file)) {
      rep_ensemble = data.table::fread(rep_ensemble_file)
      summaries$republican = create_district_summary(
        map_data, 
        "republican", 
        rep_ensemble, 
        "pct_id"
      )
    }
  }
  
  # Process Democratic ensemble
  if (!is.null(redistricting_results$democratic)) {
    dem_ensemble_file = paste0(current_output_dir, "democratic/CD_plans.csv")
    if (file.exists(dem_ensemble_file)) {
      dem_ensemble = data.table::fread(dem_ensemble_file)
      summaries$democratic = create_district_summary(
        map_data, 
        "democratic", 
        dem_ensemble, 
        "pct_id"
      )
    }
  }
  
  return(summaries)
}

#' Create district summary from redistricting plans
#' @param map_data sf object with precinct data
#' @param plan_id identifier for the plan type
#' @param cd_plan data frame with district assignments
#' @param plan_precinct_id column name for precinct ID
#' @return list with district and map summaries
create_district_summary = function(map_data,
                                   plan_id,
                                   cd_plan,
                                   plan_precinct_id = NULL) {
  # Get the district column name (second column)
  district_col = names(cd_plan)[2]
  
  df = map_data %>%
    sf::st_drop_geometry() %>%
    left_join(cd_plan, by = setNames(plan_precinct_id, names(cd_plan)[1])) %>%
    filter(!is.na(!!rlang::sym(district_col))) %>%  # Remove rows with no district assignment
    group_by(!!rlang::sym(district_col)) %>%
    summarise(
      n_precincts    = n(),
      population     = sum(pop, na.rm = TRUE),
      n_minority     = sum(n_min, na.rm = TRUE),
      n_majority     = sum(n_maj, na.rm = TRUE),
      per_minority   = n_minority / population,
      dem_votes      = sum(dem_v, na.rm = TRUE),
      dem_vshare     = dem_votes / population,
      dem_cd         = as.integer(dem_vshare > 0.5),
      .groups = "drop"
    )
  
  # Map-level summary
  map_summary = df %>%
    group_by() %>%
    summarise(
      n_districts = n(),
      n_dem_districts = sum(dem_cd),
      n_rep_districts = n_districts - n_dem_districts,
      mean_dem_vshare = mean(dem_vshare, na.rm = TRUE),
      mean_minority_share = mean(per_minority, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(list(
    district_summary = df,
    map_summary = map_summary
  ))
}

#' Create map summary from district summaries
#' @param district_summary data frame with district-level summaries
#' @return data frame with map-level summary
create_map_summary = function(district_summary) {
  
  # Aggregate to map level
  map_summary = district_summary %>%
    group_by() %>%
    summarise(
      n_districts = n(),
      n_dem_districts = sum(dem_cd, na.rm = TRUE),
      n_rep_districts = n_districts - n_dem_districts,
      mean_dem_vshare = mean(dem_vshare, na.rm = TRUE),
      mean_minority_share = mean(per_minority, na.rm = TRUE),
      .groups = "drop"
    )
  
  return(map_summary)
}

#' Score redistricting plans
#' @param map_data sf object with precinct data
#' @param plan_data data frame with plan assignments
#' @param plan_type type of plan ("neutral", "republican", "democratic")
#' @param output_dir output directory for results
#' @return scored plan data
score_plans = function(map_data,
                       plan_data,
                       plan_type,
                       output_dir) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Create district summaries
  district_summary = create_district_summary(
    map_data, 
    plan_type, 
    plan_data, 
    "pct_id"
  )
  
  # Save results
  write.csv(district_summary$district_summary, 
            file.path(output_dir, "district_summary.csv"), 
            row.names = FALSE)
  write.csv(district_summary$map_summary, 
            file.path(output_dir, "map_summary.csv"), 
            row.names = FALSE)
  
  return(district_summary)
}

#' Print redistricting summary
#' @param summaries list of redistricting summaries
#' @param level segregation level
print_redistricting_summary = function(summaries, level) {
  cat("\n=== Redistricting Summary for", stringr::str_to_title(level), "Segregation ===\n")
  
  if (!is.null(summaries$neutral)) {
    cat("\nNeutral Redistricting:\n")
    cat("  Mean Democratic seats:", round(mean(summaries$neutral$map_summary$n_dem_districts), 2), "\n")
    cat("  Mean Democratic vote share:", round(mean(summaries$neutral$map_summary$mean_dem_vshare), 3), "\n")
  }
  
  if (!is.null(summaries$republican)) {
    cat("\nRepublican Gerrymandering:\n")
    cat("  Mean Democratic seats:", round(mean(summaries$republican$map_summary$n_dem_districts), 2), "\n")
    cat("  Mean Democratic vote share:", round(mean(summaries$republican$map_summary$mean_dem_vshare), 3), "\n")
  }
  
  if (!is.null(summaries$democratic)) {
    cat("\nDemocratic Gerrymandering:\n")
    cat("  Mean Democratic seats:", round(mean(summaries$democratic$map_summary$n_dem_districts), 2), "\n")
    cat("  Mean Democratic vote share:", round(mean(summaries$democratic$map_summary$mean_dem_vshare), 3), "\n")
  }
}

#' Validate redistricting parameters
#' @param shapefile_path path to shapefile
#' @param output_dir output directory
#' @param num_districts number of districts
#' @return TRUE if validation passes
validate_redistricting_params = function(shapefile_path, output_dir, num_districts = NULL) {
  # Check shapefile exists
  if (!file.exists(shapefile_path)) {
    stop("Shapefile does not exist: ", shapefile_path)
  }
  
  # Check output directory is writable
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Check if we can write to output directory
  test_file = file.path(output_dir, "test_write.tmp")
  tryCatch({
    writeLines("test", test_file)
    unlink(test_file)
  }, error = function(e) {
    stop("Cannot write to output directory: ", output_dir)
  })
  
  # Validate number of districts if provided
  if (!is.null(num_districts)) {
    if (num_districts < 2) {
      stop("Number of districts must be at least 2")
    }
  }
  
  return(TRUE)
}
