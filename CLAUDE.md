# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

rpvsimulator is an R package for simulating redistricting plans and evaluating Ecological Regression (ER) and Ecological Inference (EI) methods against ground-truth individual voting behavior. The package generates synthetic or processes real precinct data, applies racially polarized voting (RPV) models, runs redistricting via Python's GerryChain, and analyzes results.

## Development Commands

```bash
# Load package for development
devtools::load_all(".")

# Run all tests
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-utils.R")

# Check package (R CMD check equivalent)
devtools::check()

# Build documentation
devtools::document()

# Install package locally
devtools::install()
```

## Architecture

### Data Flow Pipeline

1. **Synthetic Grid Generation** (`R/synthetic_data.R`)
   - Creates city centers with variable intensity
   - Uses rejection sampling to generate spatially-varying precinct densities
   - Builds Voronoi tessellation for precinct boundaries

2. **Population Assignment** (`R/vote_modeling.R`)
   - `create_population_data()`: Generates correlated population/minority counts from log-normal and beta distributions
   - `add_baseline_votes()`: Assigns baseline voting probabilities using truncated multivariate normal
   - `place_voters_on_map()`: Assigns population to precincts with configurable segregation levels (low/medium/high)

3. **Spatial Vote Modeling** (`R/vote_modeling.R`)
   - `apply_vote_model()`: Core function that combines:
     - RPV parameters (minority/majority Democratic voting probabilities)
     - Spatial autoregressive (SAR) fields via `simulate_sar_field()`
     - Optional contextual effects based on neighborhood composition

4. **Redistricting** (`R/redistricting.R`)
   - `setup_python_environment()`: Creates venv and installs GerryChain dependencies
   - `run_redistricting()`: Interfaces with Python scripts for neutral and biased (partisan) ensembles
   - Outputs CSV files with district assignments

5. **Analysis** (`R/ei_er.R` - planned)
   - EI/ER methods to estimate group voting behavior from aggregate data
   - Comparison with ground-truth individual votes

### Key Data Structures

- **sf objects**: All spatial data uses the `sf` package with CRS 3857
- Standard columns: `population`, `n_minority`, `n_majority`, `per_minority`, `dem_votes`, `rep_votes`, `dem_voteshare`
- Vote columns include both aggregate (`dem_votes`) and group-specific (`dem_votes_minority`, `dem_votes_majority`)

### Python Integration

Uses `reticulate` to run GerryChain redistricting. Python scripts in `inst/python/` handle MCMC-based redistricting with:
- Neutral redistricting (ReCom algorithm)
- Biased ensembles (Republican/Democratic gerrymanders)

## Testing

Tests use `testthat`. Each R file has a corresponding test file in `tests/testthat/`. Run tests frequently during development as spatial operations can have edge cases.

## Dependencies

R: sf, spdep, spatstat, reticulate, tmvtnorm, MASS, dplyr, data.table
Python: gerrychain, geopandas, pandas, numpy
