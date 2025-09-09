# rpvsimulator: Racially Polarized Voting Redistricting Simulator

A package for simulating redistricting plans and analyzing racially polarized voting patterns through Ecological Inference (EI) and Ecological Regression (ER) methods.

## Package Structure

```
rpvsimulator/
├── DESCRIPTION                   # Package metadata and dependencies
├── NAMESPACE                     # Function exports
├── README.md                     # This file
├── R/
│   ├── utils.R                   # ✅ Basic utility functions
│   ├── synthetic_data.R          # Grid generation, synthetic precinct creation
│   ├── real_data.R               # Texas/real shapefile processing
│   ├── vote_modeling.R           # RPV modeling, vote assignment functions
│   ├── redistricting.R           # Python interface, redistricting pipeline
│   ├── ei_er_analysis.R          # Ecological inference functions
│   ├── visualization.R           # All plotting and visualization functions
│   ├── data_preparation.R        # Shapefile loading, validation, standardization
│   └── zzz.R                     # Package setup, Python environment
├── inst/
│   ├── python/                   # Python scripts for redistricting
│   │   ├── run_recom.py
│   │   └── run_recom_short_bursts.py
│   └── extdata/                  # Example data files
│       └── example_precincts.shp
├── tests/
│   └── testthat/
│       ├── test-utils.R          # ✅ Tests for utility functions
│       ├── test-synthetic_data.R
│       ├── test-real_data.R
│       ├── test-vote_modeling.R
│       ├── test-redistricting.R
│       └── test-ei_er_analysis.R
├── vignettes/
│   ├── getting-started.Rmd
│   ├── synthetic-analysis.Rmd
│   └── real-data-analysis.Rmd
└── man/                           # Generated documentation (roxygen2)
```

## File Responsibilities

### **R/utils.R** 
- `safe_div()`                       - Safe division function
- `int_alloc_by_weights()`           - Integer allocation by weights
- `adjust_group_D_to_match()`        - Adjust group Democratic votes
- `build_corr()`                     - Build correlation matrix

### **R/synthetic_data.R** 
- `create_city_centers()`            - Create city centers for grid generation
- `create_density_surface()`         - Create density surface with plateau
- `generate_seed_points()`           - Generate seed points using rejection sampling
- `create_precinct_boundaries()`     - Create precinct boundaries from seed points
- `create_realistic_grid()`          - Create realistic spatial grid
- `simulate_segregation_scenarios()` - Simulate different segregation levels

### **R/real_data.R** 
- `prepare_tx_for_redistricting()`   - Prepare Texas shapefile for redistricting
- `load_shapefile()`                 - Load and validate shapefiles
- `validate_shapefile()`             - Validate shapefile structure
- `standardize_columns()`            - Standardize column names and types

### **R/vote_modeling.R** 
- `create_population_data()`         - Create population data with demographics
- `add_baseline_votes()`             - Add baseline voting patterns
- `apply_vote_model()`               - Apply vote model to precincts
- `place_voters_on_map()`            - Place voters on spatial map
- `simulate_sar_field()`             - Simulate spatial autoregressive field

### **R/redistricting.R** (Planned)
- `run_redistricting()` - Run redistricting analysis via Python
- `score_plans()` - Score redistricting plans
- `prepare_for_redistricting()` - Prepare data for redistricting
- `process_redistricting_results()` - Process redistricting results

### **R/ei_er_analysis.R** (Planned)
- `run_ei_analysis()` - Run Ecological Inference analysis
- `run_er_analysis()` - Run Ecological Regression analysis
- `evaluate_vote_model_on_fixed_plans()` - Evaluate vote models
- `create_district_summary()` - Create district-level summaries

### **R/visualization.R** (Planned)
- `plot_precinct_characteristics()` - Plot precinct characteristics
- `plot_segregation_levels_comparison()` - Compare segregation levels
- `plot_redistricting_comparison()` - Compare redistricting plans
- `create_redistricting_visualizations()` - Create comprehensive visualizations

### **R/data_preparation.R** (Planned)
- `load_and_validate_shapefile()` - Load and validate shapefiles
- `standardize_precinct_data()` - Standardize precinct data format
- `verify_fixed_population_consistency()` - Verify population consistency

### **R/zzz.R** (Planned)
- `setup_python_environment()` - Set up Python environment
- `.onLoad()` - Package load hook
- Python environment configuration

## Main User Interface (Planned)

The package will provide a unified interface:

```r
analyze_redistricting(
  input_data = NULL,  # NULL for synthetic, path/object for real data
  input_type = c("synthetic", "real"),
  rpv_parameters = list(
    minority_dem_prob = 0.81,
    majority_dem_prob = 0.37,
    # ... other parameters
  ),
  redistricting_params = list(
    n_plans = 100,
    n_districts = 14,
    # ... other parameters
  ),
  sensitivity_analysis = list(
    vary_rpv = TRUE,
    rpv_grid = expand.grid(
      minority_dem_prob = seq(0.7, 0.9, 0.05),
      majority_dem_prob = seq(0.3, 0.5, 0.05)
    )
  )
)
```

## Development Status

- ✅ **Package Structure**: Basic package files created
- ✅ **Utils Functions**: Basic utility functions implemented and tested
- 🔄 **Data Preparation**: In progress
- ⏳ **Synthetic Data**: Planned
- ⏳ **Real Data**: Planned
- ⏳ **Vote Modeling**: Planned
- ⏳ **Redistricting Interface**: Planned
- ⏳ **ER/EI Analysis**: Planned
- ⏳ **Visualization**: Planned

## Installation

```r
# Install from local directory
devtools::install("path/to/rpvsimulator")
```

## Usage

```r
library(rpvsimulator)


```
