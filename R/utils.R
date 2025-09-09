# Utility functions for rpvsimulator package

#' Logit function
#' @param p probability
#' @return log(p/(1-p))
logit = function(p) log(p / (1 - p))

#' Inverse logit function
#' @param x logit
#' @return 1/(1+exp(-x))
inv_logit = function(x) 1 / (1 + exp(-x))

#' Safe division function
#' @param a numerator
#' @param b denominator
#' @return a/b if b != 0, otherwise 0
safe_div = function(a, b) ifelse(b != 0, a/b, 0)

#' Integer allocation by weights
#' @param V total votes
#' @param w weight vector
#' @return integer allocation
int_alloc_by_weights = function(V, w) {
  w[is.na(w)] = 0
  
  if (sum(w) <= 0 || V <= 0) return(integer(length(w)))
  
  w = w / sum(w)
  raw = V * w
  base = floor(raw)
  diff = V - sum(base)
  
  if (diff > 0) {
    frac = raw - base
    add_order = order(frac, decreasing = TRUE)
    for (i in seq_len(diff)) base[add_order[i]] = base[add_order[i]] + 1L
  } else if (diff < 0) {
    sub_order = order((raw - base), decreasing = FALSE)
    for (i in seq_len(abs(diff))) {
      j = sub_order[i]
      if (base[j] > 0) base[j] = base[j] - 1L
    }
  }
  as.integer(base)
}

#' Adjust group Democratic votes to match total
#' @param Dg group Democratic votes
#' @param vg group total votes
#' @param dem_total total Democratic votes
#' @return adjusted group Democratic votes
adjust_group_D_to_match = function(Dg, vg, dem_total) {
  Dg = as.integer(Dg); vg = as.integer(vg)
  resid = as.integer(dem_total) - sum(Dg)
  if (resid == 0L) return(Dg)
  if (resid > 0L) {
    room = vg - Dg
    while (resid > 0L && any(room > 0L)) {
      j = which.max(room)
      Dg[j] = Dg[j] + 1L
      room[j] = room[j] - 1L
      resid = resid - 1L
    }
  } else {
    while (resid < 0L && any(Dg > 0L)) {
      j = which.max(Dg)
      Dg[j] = Dg[j] - 1L
      resid = resid + 1L
    }
  }
  Dg
}

#' Build correlation matrix
#' @param rho_wb white-black correlation
#' @param rho_wh white-hispanic correlation  
#' @param rho_bh black-hispanic correlation
#' @param R existing correlation matrix
#' @return 3x3 correlation matrix
build_corr = function(rho_wb = 0.5, rho_wh = 0.5, rho_bh = 0.5, R = NULL) {
  if (!is.null(R)) return(R)
  R = matrix(c(
    1,       rho_wb, rho_wh,
    rho_wb,  1,      rho_bh,
    rho_wh,  rho_bh, 1
  ), nrow = 3, byrow = TRUE)
  # quick PD fix if needed
  eig = eigen(R, symmetric = TRUE)
  if (min(eig$values) <= 1e-6) {
    eps = 1e-3
    R = (1 - eps) * R + eps * diag(3)
  }
  R
}


#' Save diagnostic plot if in development mode
#' @param plot_name name for the plot file
#' @param plot_fn function that creates the plot
#' @param width plot width in inches
#' @param height plot height in inches
save_diagnostic_plot = function(plot_name, plot_fn, width = 10, height = 10) {
  # Check if we're in dev mode (set via option)
  if (!isTRUE(getOption("rpvsimulator.save_diagnostics", FALSE))) {
    return(invisible(NULL))
  }
  
  # Create diagnostic directory if needed
  diag_dir = file.path(getwd(), "diagnostics")
  dir.create(diag_dir, recursive = TRUE, showWarnings = FALSE)
  
  # Generate filename with timestamp
  # timestamp = format(Sys.time(), "%Y%m%d_%H%M%S")
  filename = file.path(diag_dir, paste0(plot_name, ".pdf"))
  
  # Save plot - PDF uses inches directly, no res parameter
  pdf(filename, width = width, height = height)
  plot_fn()
  dev.off()
  
  message("Saved diagnostic plot: ", basename(filename))
}
