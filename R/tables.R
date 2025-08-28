#' LaTeX table builders
#' @keywords internal
#' @importFrom dplyr mutate arrange
NULL

fmt_pct   = function(x, digits = 1) sprintf(paste0("%.", digits, "f"), 100 * x)
fmt_pp    = function(x, digits = 1) sprintf(paste0("%.", digits, "f"), 100 * x)
fmt_1     = function(x) sprintf("%.1f", x)
fmt_round = function(x, k=3) sprintf(paste0("%.", k, "f"), x)

#' ER/EI comparison table (scenario-level)
#' @param ci_summary from summarise_with_ci()
#' @return character (LaTeX)
latex_er_ei_comparison = function(ci_summary) {
  if (nrow(ci_summary) == 0) return("% No data")
  # Prepare readable labels
  df = ci_summary %>%
    dplyr::mutate(
      Seg = factor(agg_level, levels = c("low","medium","high"), labels = c("Low","Medium","High")),
      Red = factor(partisan, levels = c("dem","middle","rep"), labels = c("Dem","Neutral","Rep"))
    ) %>%
    dplyr::arrange(Seg, Red, method)
  
  header = "\\begin{table}[!htbp]
\\centering
\\small
\\setlength{\\tabcolsep}{3pt}
\\caption{Performance Comparison of ER and EI}
\\begin{tabular}{@{}lllrrrrr@{}}
\\toprule
Seg. & Redist. & Method & MAE Min & MAE Maj & Bias Min & Bias Maj & RPV Acc. \\\\
\\midrule\n"
  rows = apply(df, 1, function(r) {
    paste(
      r[["Seg"]], "&", r[["Red"]], "&", r[["method"]], "&",
      fmt_1(as.numeric(r[["mae_minority_mean"]])), "&",
      fmt_1(as.numeric(r[["mae_majority_mean"]])), "&",
      fmt_1(as.numeric(r[["bias_minority_mean"]])), "&",
      fmt_1(as.numeric(r[["bias_majority_mean"]])), "&",
      fmt_1(as.numeric(r[["rpv_accuracy_mean"]])), "\\\\"
    )
  })
  footer = "\n\\bottomrule\n\\end{tabular}\n\\end{table}"
  paste0(header, paste(rows, collapse = "\n"), footer)
}

#' Descriptive outcomes by race table
#' @param desc_df output of descriptive_outcomes_by_scenario()
latex_outcomes_by_race = function(desc_df) {
  if (nrow(desc_df) == 0) return("% No data")
  df = desc_df %>%
    dplyr::mutate(
      Seg = factor(agg_level, levels = c("low","medium","high"), labels = c("Low","Medium","High")),
      Red = factor(partisan, levels = c("dem","middle","rep"), labels = c("Dem","Neutral","Rep"))
    ) %>%
    dplyr::arrange(Seg, Red)
  
  header = "\\begin{table}[!htbp]
\\centering
\\small
\\setlength{\\tabcolsep}{3pt}
\\caption{Electoral Outcomes by Race}
\\begin{tabular}{@{}llrrrrrr@{}}
\\toprule
Seg. & Redist. & Pr(Copartisan, Min) & Pr(Copartisan, Maj) & Pr(Comp., Min) & Pr(Comp., Maj) & Pr(Wasted, Min) & Pr(Wasted, Maj) \\\\
\\midrule\n"
  
  rows = apply(df, 1, function(r) {
    paste(
      r[["Seg"]], "&", r[["Red"]], "&",
      fmt_pct(as.numeric(r[["pr_minority_copartisan"]])), "&",
      fmt_pct(as.numeric(r[["pr_majority_copartisan"]])), "&",
      fmt_pct(as.numeric(r[["pr_minority_competitive"]])), "&",
      fmt_pct(as.numeric(r[["pr_majority_competitive"]])), "&",
      fmt_pct(as.numeric(r[["pr_minority_wasted"]])), "&",
      fmt_pct(as.numeric(r[["pr_majority_wasted"]])), "\\\\"
    )
  })
  
  footer = "\n\\bottomrule\n\\end{tabular}\n\\end{table}"
  paste0(header, paste(rows, collapse = "\n"), footer)
}
