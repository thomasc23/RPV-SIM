# Real data processing functions for rpvsimulator package
# Uses canonical column names from constants.R

#' Prepare Texas shapefile for redistricting with EI-imposed group-specific votes
#'
#' This function processes Texas precinct data, allocating votes to racial groups
#' and simulating group-specific voting behavior using EI priors. Output uses
#' shapefile-compatible abbreviated column names unless `use_canonical_names = TRUE`.
#'
#' @param in_shapefile path to input shapefile
#' @param out_shapefile path to output shapefile (NULL to skip writing)
#' @param ei_means EI hyperparameters (probability scale, truncated to [0,1])
#' @param ei_sds EI standard deviations
#' @param ei_corr EI correlation parameters
#' @param seed random seed for reproducibility
#' @param simplify_tolerance geometry simplification tolerance
#' @param use_canonical_names if TRUE, return canonical column names; if FALSE (default),
#'   use abbreviated names for shapefile compatibility
#' @return sf object with EI-imposed group votes
#' @export
prepare_tx_for_redistricting = function(
    in_shapefile,
    out_shapefile = NULL,
    # EI hyperparameters (probability scale, truncated to [0,1])
    ei_means = c(white = 0.35, black = 0.90, hisp = 0.65),
    ei_sds   = c(white = 0.3,  black = 0.10, hisp = 0.12),
    ei_corr  = list(rho_wb = 0.5, rho_wh = 0.5, rho_bh = 0.5, R = NULL),
    # Randomness
    seed = 123,
    # Geometry simplification
    simplify_tolerance = NA_real_,
    # Column naming
    use_canonical_names = FALSE
) {
  stopifnot(file.exists(in_shapefile))
  set.seed(seed)
  
  # small helpers
  safe_div  = function(a, b) ifelse(b > 0, a / b, 0)
  logit     = function(p) { p = pmin(pmax(p, 1e-6), 1 - 1e-6); log(p/(1 - p)) }
  inv_logit = function(x) 1 / (1 + exp(-x))
  
  sf0 = sf::read_sf(in_shapefile) %>% sf::st_make_valid()
  
  # Rename columns
  sf0 = sf0 %>%
    rename(
      dem_votes = dem_vts, rep_votes = rep_vts,
      cvap_tot = cvap_tt, cvap_wht = cvp_wht, 
      cvap_blk = cvp_blk, cvap_hsp = cvp_hsp
    )
  
  # Required columns present?
  need = c("prec_id","dem_votes","rep_votes","cvap_wht","cvap_blk","cvap_hsp")
  missing = setdiff(need, names(sf0))
  if (length(missing)) stop("Missing required columns: ", paste(missing, collapse = ", "))
  
  # ------------------------------------------------------------------
  # NEW: KEEP *ALL* PRECINCTS (no filtering). Zeros stay in the table.
  # ------------------------------------------------------------------
  sf1 = sf0 %>%
    mutate(
      V_total = as.integer(dem_votes + rep_votes),
      N_white = pmax(0L, as.integer(round(cvap_wht))),
      N_black = pmax(0L, as.integer(round(cvap_blk))),
      N_hisp  = pmax(0L, as.integer(round(cvap_hsp))),
      N_tot   = N_white + N_black + N_hisp
    )
  
  n = nrow(sf1)
  
  # ------------------------------
  # Votes → groups (robust weights)
  # ------------------------------
  W = sf1$N_white; B = sf1$N_black; H = sf1$N_hisp
  sum_cvap = W + B + H
  
  # base weights (avoid divide-by-zero)
  wts = cbind(W, B, H) / pmax(1L, sum_cvap)
  
  # if CVAP is all zero but turnout > 0, fall back to equal split
  ix_zero_cvap_and_votes = which(sum_cvap == 0 & sf1$V_total > 0)
  if (length(ix_zero_cvap_and_votes)) wts[ix_zero_cvap_and_votes, ] = 1/3
  
  # integer allocation of total votes to groups (works with V_total = 0 too)
  vg_mat = matrix(0L, nrow = n, ncol = 3)
  for (i in seq_len(n)) vg_mat[i, ] = int_alloc_by_weights(sf1$V_total[i], wts[i, ])
  colnames(vg_mat) = c("v_white","v_black","v_hisp")
  
  # ---------------------------------------
  # EI draws ONLY where V_total > 0 (ix_V)
  # ---------------------------------------
  means = as.numeric(ei_means[c("white","black","hisp")])
  sds   = as.numeric(ei_sds[c("white","black","hisp")])
  R     = build_corr(ei_corr$rho_wb, ei_corr$rho_wh, ei_corr$rho_bh, ei_corr$R)
  Sigma = diag(sds) %*% R %*% diag(sds)
  
  ix_V = which(sf1$V_total > 0)
  
  P = matrix(0, nrow = n, ncol = 3)
  if (length(ix_V)) {
    P[ix_V, ] = tmvtnorm::rtmvnorm(
      n = length(ix_V),
      mean = means, 
      sigma = Sigma,
      lower = c(0,0,0), upper = c(1,1,1)
    )
  }
  colnames(P) = c("p_white","p_black","p_hisp")
  
  # ---- logit pre-shift per-precinct ----
  # For each precinct i with turnout, find δ_i s.t.
  #   sum_g v_{ig} * logistic( logit(p_{ig}) + δ_i )  ≈ dem_votes_i
  # Then draw binomial with shifted probabilities.
  v_white = vg_mat[, 1]; v_black = vg_mat[, 2]; v_hisp = vg_mat[, 3]
  p_white0 = P[, 1];      p_black0 = P[, 2];      p_hisp0 = P[, 3]
  
  p_white = p_white0; p_black = p_black0; p_hisp = p_hisp0
  logit_shift = rep(0, n)
  
  target_D = as.numeric(sf1$dem_votes)
  
  find_delta = function(vg, pg, D) {
    V = sum(vg)
    if (V <= 0 || is.na(D)) return(0)
    if (D <= 0)  return(-15) # practically 0
    if (D >= V)  return(+15) # practically 1
    
    f = function(delta) {
      q = inv_logit(logit(pg) + delta)
      sum(vg * q) - D
    }
    lo = -15; hi = 15
    flo = f(lo); fhi = f(hi)
    
    if (is.finite(flo) && is.finite(fhi) && flo * fhi <= 0) {
      uniroot(f, interval = c(lo, hi))$root
    } else {
      # fallback: match *overall* share on logit scale as approximation
      E0 = sum(vg * pg); t = D / V
      if (E0 <= 0) return(+15)          # push up
      if (E0 >= V) return(-15)          # push down
      logit(t) - logit(E0 / V)
    }
  }
  
  for (i in ix_V) {
    vg = c(v_white[i], v_black[i], v_hisp[i])
    pg = c(p_white0[i], p_black0[i], p_hisp0[i])
    delta = find_delta(vg, pg, target_D[i])
    logit_shift[i] = delta
    
    p_white[i] = inv_logit(logit(p_white0[i]) + delta)
    p_black[i] = inv_logit(logit(p_black0[i]) + delta)
    p_hisp[i]  = inv_logit(logit(p_hisp0[i])  + delta)
  }
  
  # Draw binomials with *shifted* probabilities (no hard calibration)
  D_white = integer(n); D_black = integer(n); D_hisp = integer(n)
  if (length(ix_V)) {
    D_white[ix_V] = rbinom(length(ix_V), size = v_white[ix_V], prob = p_white[ix_V])
    D_black[ix_V] = rbinom(length(ix_V), size = v_black[ix_V], prob = p_black[ix_V])
    D_hisp[ix_V]  = rbinom(length(ix_V), size = v_hisp[ix_V],  prob = p_hisp[ix_V])
  }
  
  # --- totals / simulated votes ---
  
  pop_turnout = as.integer(sf1$V_total)                  # total ballots (same as v_white+v_black+v_hisp)
  n_min       = as.integer(v_black + v_hisp)             # minority electorate (ballots by minority groups)
  n_maj       = as.integer(v_white)                      # majority electorate
  
  dem_min     = as.integer(D_black + D_hisp)             # simulated Dem votes by minority groups
  dem_maj     = as.integer(D_white)                      # simulated Dem votes by majority group
  dem_sim     = as.integer(dem_min + dem_maj)            # simulated Dem total
  
  rep_min     = as.integer(n_min - dem_min)              # simulated Rep by minority
  rep_maj     = as.integer(n_maj - dem_maj)              # simulated Rep by majority
  rep_sim     = as.integer(pop_turnout - dem_sim)        # simulated Rep total
  
  # optional diagnostics (obs vs expected after shift)
  dem_share_obs = safe_div(as.numeric(sf1$dem_votes), pmax(1L, pop_turnout))
  dem_share_exp = safe_div(v_white * p_white + v_black * p_black + v_hisp * p_hisp,
                          pmax(1L, pop_turnout))
  
  # --- assemble output ---
  # Use canonical names from constants.R, then optionally abbreviate for shapefile

  out = sf1 %>%
    dplyr::mutate(
      !!COL_NAMES$precinct_id := prec_id,
      orig_id     = VTDKEY,
      !!COL_NAMES$total_pop := pop_turnout,
      # electorates + simulated votes
      !!COL_NAMES$n_minority := n_min,
      !!COL_NAMES$n_majority := n_maj,
      !!COL_NAMES$votes_dem := dem_sim,
      !!COL_NAMES$votes_rep := rep_sim,
      !!COL_NAMES$votes_dem_minority := dem_min,
      !!COL_NAMES$votes_rep_minority := rep_min,
      !!COL_NAMES$votes_dem_majority := dem_maj,
      !!COL_NAMES$votes_rep_majority := rep_maj,
      !!COL_NAMES$pct_minority := safe_div(n_min, pmax(1L, pop_turnout)),
      !!COL_NAMES$share_dem := safe_div(dem_sim, pmax(1L, pop_turnout)),
      !!COL_NAMES$share_dem_minority := safe_div(dem_min, pmax(1L, n_min)),
      !!COL_NAMES$share_dem_majority := safe_div(dem_maj, pmax(1L, n_maj)),
      lshift      = logit_shift,
      dsh_obs     = dem_share_obs,
      dsh_exp     = dem_share_exp
    ) %>%
    dplyr::select(
      dplyr::all_of(c(
        COL_NAMES$precinct_id, "orig_id", COL_NAMES$total_pop,
        COL_NAMES$n_minority, COL_NAMES$n_majority,
        COL_NAMES$votes_dem, COL_NAMES$votes_rep,
        COL_NAMES$votes_dem_minority, COL_NAMES$votes_rep_minority,
        COL_NAMES$votes_dem_majority, COL_NAMES$votes_rep_majority,
        COL_NAMES$pct_minority, COL_NAMES$share_dem,
        COL_NAMES$share_dem_minority, COL_NAMES$share_dem_majority,
        "lshift", "dsh_obs", "dsh_exp"
      )),
      geometry
    )

  # Optional simplify
  if (!is.na(simplify_tolerance) && is.finite(simplify_tolerance) && simplify_tolerance > 0) {
    out = sf::st_simplify(out, dTolerance = simplify_tolerance, preserveTopology = TRUE)
    out = sf::st_make_valid(out)
  }

  # Integrity checks (using canonical names)
  dem_col <- COL_NAMES$votes_dem
  rep_col <- COL_NAMES$votes_rep
  dem_min_col <- COL_NAMES$votes_dem_minority
  dem_maj_col <- COL_NAMES$votes_dem_majority
  rep_min_col <- COL_NAMES$votes_rep_minority
  rep_maj_col <- COL_NAMES$votes_rep_majority

  stopifnot(sum(out[[dem_col]] + out[[rep_col]]) == sum(pop_turnout))
  stopifnot(all(out[[dem_col]] == out[[dem_min_col]] + out[[dem_maj_col]]))
  stopifnot(all(out[[rep_col]] == out[[rep_min_col]] + out[[rep_maj_col]]))

  # Abbreviate column names for shapefile compatibility if needed
  if (!use_canonical_names) {
    out <- abbreviate_column_names(out)
  }

  # Write shapefile if path provided
 if (!is.null(out_shapefile)) {
    dir.create(dirname(out_shapefile), recursive = TRUE, showWarnings = FALSE)
    sf::st_write(out, out_shapefile, append = FALSE, quiet = TRUE)
    message("Wrote shapefile with EI-imposed group votes: ", out_shapefile)
  }

  invisible(out)
}

#' Load and validate Texas shapefile with required columns
#' @param file_path path to Texas shapefile
#' @return validated sf object
load_tx_shapefile = function(file_path) {
  if (!file.exists(file_path)) {
    stop("Texas shapefile does not exist: ", file_path)
  }
  
  # Load shapefile
  sf_obj = sf::read_sf(file_path) %>% sf::st_make_valid()
  
  # Check for required Texas columns
  required_cols = c("prec_id", "dem_vts", "rep_vts", "cvp_wht", "cvp_blk", "cvp_hsp")
  missing_cols = setdiff(required_cols, names(sf_obj))
  if (length(missing_cols) > 0) {
    stop("Missing required Texas columns: ", paste(missing_cols, collapse = ", "))
  }
  
  return(sf_obj)
}

#' Validate Texas shapefile data consistency
#' @param sf_obj sf object with Texas data
#' @return TRUE if validation passes, stops with error if not
validate_tx_data = function(sf_obj) {
  # Check for required columns
  required_cols = c("prec_id", "dem_vts", "rep_vts", "cvp_wht", "cvp_blk", "cvp_hsp")
  missing_cols = setdiff(required_cols, names(sf_obj))
  if (length(missing_cols) > 0) {
    stop("Missing required Texas columns: ", paste(missing_cols, collapse = ", "))
  }
  
  # Check for negative values
  if (any(sf_obj$dem_vts < 0, na.rm = TRUE)) {
    stop("Democratic votes cannot be negative")
  }
  
  if (any(sf_obj$rep_vts < 0, na.rm = TRUE)) {
    stop("Republican votes cannot be negative")
  }
  
  if (any(sf_obj$cvp_wht < 0, na.rm = TRUE)) {
    stop("White CVAP cannot be negative")
  }
  
  if (any(sf_obj$cvp_blk < 0, na.rm = TRUE)) {
    stop("Black CVAP cannot be negative")
  }
  
  if (any(sf_obj$cvp_hsp < 0, na.rm = TRUE)) {
    stop("Hispanic CVAP cannot be negative")
  }
  
  # Check for missing precinct IDs
  if (any(is.na(sf_obj$prec_id))) {
    stop("Precinct ID cannot be missing")
  }
  
  # Check for duplicate precinct IDs
  if (any(duplicated(sf_obj$prec_id))) {
    stop("Duplicate precinct IDs found")
  }
  
  return(TRUE)
}
