# Test runner for rpvsimulator package
# Run this from the rpvsimulator directory

library(devtools)
library(testthat)

# Load the package
load_all(".")

# Save diagnostic plots in development mode
options(rpvsimulator.save_diagnostics = TRUE)

# Run all tests in the testthat directory
test_dir("tests/testthat/")

