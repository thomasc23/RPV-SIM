# Script to test package development

setwd("~/Dropbox/RPV/Code/Simulation")

source('R/normalize.R')
# source("R/joiners.R")
# source("R/truth_metrics.R")
# source("R/aggregators.R")
# source("R/figures.R")
# source("R/tables.R")

sc = find_scenarios('data')
sc

# pick one scenario that actually exists
low_rep = read_maps_for_scenario(sc[8, ])


