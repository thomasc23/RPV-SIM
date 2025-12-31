# Constants and canonical column names for rpvsimulator package
# This file provides a single source of truth for all column naming and default parameters

#' Canonical column names used throughout rpvsimulator
#'
#' All functions in the package use these standard column names to ensure
#' consistent data structures and easy merging/joining across modules.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{precinct_id}{Unique precinct identifier}
#'   \item{district_id}{District assignment identifier}
#'   \item{total_pop}{Total population count}
#'   \item{n_minority}{Minority population count}
#'   \item{n_majority}{Majority population count}
#'   \item{pct_minority}{Minority proportion (0-1)}
#'   \item{votes_dem}{Total Democratic votes}
#'   \item{votes_rep}{Total Republican votes}
#'   \item{votes_total}{Total votes cast}
#'   \item{votes_dem_minority}{Democratic votes from minority group (ground truth)}
#'   \item{votes_dem_majority}{Democratic votes from majority group (ground truth)}
#'   \item{votes_rep_minority}{Republican votes from minority group (ground truth)}
#'   \item{votes_rep_majority}{Republican votes from majority group (ground truth)}
#'   \item{share_dem}{Democratic vote share (0-1)}
#'   \item{share_dem_minority}{Minority group Democratic vote share (ground truth)}
#'   \item{share_dem_majority}{Majority group Democratic vote share (ground truth)}
#'   \item{mu_minority}{Realized minority Dem probability (ground truth per precinct)}
#'   \item{mu_majority}{Realized majority Dem probability (ground truth per precinct)}
#'   \item{geometry}{Spatial geometry column for sf objects}
#' }
#' @export
COL_NAMES <- list(
  # Identifiers
  precinct_id = "precinct_id",
  district_id = "district_id",

  # Population counts
  total_pop = "total_pop",
  n_minority = "n_minority",
  n_majority = "n_majority",
  pct_minority = "pct_minority",

  # Aggregate vote counts (observed in real data)
  votes_dem = "votes_dem",
  votes_rep = "votes_rep",
  votes_total = "votes_total",


  # Ground truth: group-specific votes (only known in simulation)
  votes_dem_minority = "votes_dem_minority",
  votes_dem_majority = "votes_dem_majority",
  votes_rep_minority = "votes_rep_minority",
  votes_rep_majority = "votes_rep_majority",

  # Vote shares
  share_dem = "share_dem",
  share_dem_minority = "share_dem_minority",
  share_dem_majority = "share_dem_majority",

  # DGP: realized precinct-level probabilities (ground truth)
  mu_minority = "mu_minority",
  mu_majority = "mu_majority",

  # Spatial
  geometry = "geometry",

  # Synthetic data specific
  urbanness = "urbanness",
  seed_x = "seed_x",
  seed_y = "seed_y"
)


#' Default DGP parameters for two-group model
#'
#' These parameters define the data-generating process for voting behavior.
#' The mapmaker observes these parameters when optimizing redistricting plans.
#'
#' @format A named list with the following elements:
#' \describe{
#'   \item{mu_b}{Expected minority Democratic voting probability, E[P(Dem | minority)]}
#'   \item{mu_w}{Expected majority Democratic voting probability, E[P(Dem | majority)]}
#'   \item{sigma2_b}{Variance of minority voting probability across precincts, Var(beta_b)}
#'   \item{sigma2_w}{Variance of majority voting probability across precincts, Var(beta_w)}
#'   \item{sigma_bw}{Covariance between minority and majority voting probabilities}
#' }
#' @details
#' Default values:
#' \itemize{
#'   \item mu_b = 0.81: Minority voters favor Democrats ~81%
#'   \item mu_w = 0.37: Majority voters favor Democrats ~37%
#'   \item sigma2_b = 0.01: Low variance for minority voting (SD ~ 0.10)
#'   \item sigma2_w = 0.09: Higher variance for majority voting (SD ~ 0.30)
#'   \item sigma_bw = 0.01: Small positive covariance
#' }
#' @export
DEFAULT_DGP_PARAMS <- list(
  mu_b = 0.81,
  mu_w = 0.37,
  sigma2_b = 0.01,
  sigma2_w = 0.09,
  sigma_bw = 0.01
)


#' Default DGP parameters for three-group model (White/Black/Hispanic)
#'
#' Extended parameters for three-group racial analysis.
#'
#' @format A named list with group-specific means, variances, and correlations
#' @export
DEFAULT_DGP_PARAMS_3GROUP <- list(
  # Group-specific means
  mu_w = 0.35,    # White
  mu_b = 0.90,    # Black
  mu_h = 0.65,    # Hispanic

  # Group-specific variances
  sigma2_w = 0.09,
  sigma2_b = 0.01,
  sigma2_h = 0.0144,

  # Pairwise correlations
  rho_wb = 0.5,
  rho_wh = 0.5,
  rho_bh = 0.5
)


#' MrP demographic variable names
#'
#' Standard variable names for multilevel regression and poststratification.
#'
#' @format A named list with demographic variable names
#' @export
MRP_VARS <- list(
  race = "race",
  age = "age",
  education = "education",
  income = "income",
  gender = "gender",
  party_id = "party_id"
)


#' Legacy column name mapping
#'
#' Maps old/abbreviated column names to canonical names for backward compatibility.
#'
#' @format A named character vector where names are legacy names and values are canonical names
#' @keywords internal
LEGACY_COL_MAP <- c(

  # Population columns
  "population" = "total_pop",
  "pop" = "total_pop",
  "n_min" = "n_minority",
  "n_maj" = "n_majority",
  "per_minority" = "pct_minority",
  "pct_min" = "pct_minority",

  # Vote columns
  "dem_votes" = "votes_dem",
  "dem_v" = "votes_dem",
  "rep_votes" = "votes_rep",
  "rep_v" = "votes_rep",

  # Group-specific vote columns
  "dem_votes_minority" = "votes_dem_minority",
  "dem_v_min" = "votes_dem_minority",
  "dem_votes_majority" = "votes_dem_majority",
  "dem_v_maj" = "votes_dem_majority",
  "rep_votes_minority" = "votes_rep_minority",
  "rep_v_min" = "votes_rep_minority",
  "rep_votes_majority" = "votes_rep_majority",
  "rep_v_maj" = "votes_rep_majority",

  # Vote share columns
  "dem_voteshare" = "share_dem",
  "dem_vsh" = "share_dem",
  "dem_voteshare_minority" = "share_dem_minority",
  "dem_vsh_1" = "share_dem_minority",
  "dem_voteshare_majority" = "share_dem_majority",
  "dem_vsh_0" = "share_dem_majority",

  # DGP columns
  "prob_minority_dem" = "mu_minority",
  "prob_majority_dem" = "mu_majority",
  "p_min_base" = "mu_minority",
  "p_maj_base" = "mu_majority"
)


#' Validate DGP parameters
#'
#' Checks that DGP parameters are valid (probabilities in [0,1], variances positive, etc.)
#'
#' @param params Named list of DGP parameters
#' @param three_group Logical, TRUE for three-group model validation
#' @return TRUE if valid, otherwise stops with error
#' @export
validate_dgp_params <- function(params, three_group = FALSE) {
  if (three_group) {
    required <- c("mu_w", "mu_b", "mu_h", "sigma2_w", "sigma2_b", "sigma2_h")
    means <- c("mu_w", "mu_b", "mu_h")
    vars <- c("sigma2_w", "sigma2_b", "sigma2_h")
  } else {
    required <- c("mu_b", "mu_w", "sigma2_b", "sigma2_w", "sigma_bw")
    means <- c("mu_b", "mu_w")
    vars <- c("sigma2_b", "sigma2_w")
  }

  # Check all required parameters present
  missing <- setdiff(required, names(params))
  if (length(missing) > 0) {
    stop("Missing DGP parameters: ", paste(missing, collapse = ", "))
  }

  # Check means are probabilities
  for (m in means) {
    if (params[[m]] < 0 || params[[m]] > 1) {
      stop(sprintf("DGP parameter %s must be between 0 and 1, got %f", m, params[[m]]))
    }
  }

  # Check variances are positive
  for (v in vars) {
    if (params[[v]] < 0) {
      stop(sprintf("DGP parameter %s must be non-negative, got %f", v, params[[v]]))
    }
  }

  # For two-group, check covariance matrix is valid
  if (!three_group) {
    # Correlation implied by covariance
    rho_implied <- params$sigma_bw / sqrt(params$sigma2_b * params$sigma2_w)
    if (abs(rho_implied) > 1) {
      stop("DGP covariance sigma_bw implies correlation outside [-1, 1]")
    }
  }

  invisible(TRUE)
}
