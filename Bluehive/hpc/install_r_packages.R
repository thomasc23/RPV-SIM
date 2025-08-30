#!/usr/bin/env Rscript
# Install only what the simulation needs, into R_LIBS_USER.
# Links 'sf' against conda-forge GDAL/GEOS/PROJ already exported by the job.

setRepositories(ind=1) # CRAN
repos = c(CRAN="https://cloud.r-project.org")
options(repos=repos, Ncpus = max(1L, parallel::detectCores()-1L))
.libPaths(Sys.getenv("R_LIBS_USER"))

need = c(
  "reticulate","sf","spdep","truncnorm","tmvtnorm","patchwork", "lwgeom",
  "data.table","MASS","tidyverse","viridis","spatstat" # meta pulls split pkgs
)

have = rownames(installed.packages())
todo = setdiff(need, have)

if (length(todo)) {
  message("[R install] Installing: ", paste(todo, collapse=", "))
  install.packages(todo, dependencies = TRUE)
} else {
  message("[R install] All required packages already present")
}

# Sanity: load key ones
for (pkg in c("sf","spdep")) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}
print(sf::sf_extSoftVersion())
message("[R install] Ready")

