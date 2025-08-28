#' Plotting helpers (ggplot2)
#' @keywords internal
#' @import ggplot2
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
NULL

#' Error bars for MAE by scenario
#' @param ci_summary from summarise_with_ci()
plot_mae_by_scenario = function(ci_summary) {
  if (nrow(ci_summary) == 0) return(invisible(NULL))
  df = ci_summary %>%
    dplyr::mutate(
      agg_level = factor(agg_level, levels = c("low","medium","high")),
      mm_type  = factor(mm_type, levels = c("dem","neutral","rep"), labels = c("Dem","Neutral","Rep"))
    ) %>%
    tidyr::pivot_longer(
      cols = c(mae_minority_mean, mae_majority_mean),
      names_to = "group",
      values_to = "mean"
    ) %>%
    dplyr::mutate(
      lower = ifelse(group == "mae_minority_mean", mae_minority_lower, mae_majority_lower),
      upper = ifelse(group == "mae_minority_mean", mae_minority_upper, mae_majority_upper),
      group = ifelse(group == "mae_minority_mean", "Minority", "Majority")
    )
  ggplot(df, aes(x = agg_level, y = mean, color = method)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(position = position_dodge(width = 0.35), size = 2.7) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.35), width = 0.2) +
    facet_grid(group ~ mm_type) +
    labs(x = "Aggregation bias", y = "MAE", color = "Method") +
    theme_minimal()
}

#' RPV accuracy by scenario
plot_rpv_accuracy = function(ci_summary) {
  if (nrow(ci_summary) == 0) return(invisible(NULL))
  df = ci_summary %>%
    dplyr::mutate(
      agg_level = factor(agg_level, levels = c("low","medium","high")),
      mm_type  = factor(mm_type, levels = c("dem","neutral","rep"), labels = c("Dem","Neutral","Rep"))
    )
  ggplot(df, aes(x = agg_level, y = rpv_accuracy_mean, color = method)) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    geom_point(position = position_dodge(width = 0.35), size = 2.7) +
    geom_errorbar(aes(ymin = rpv_accuracy_lower, ymax = rpv_accuracy_upper),
                  position = position_dodge(width = 0.35), width = 0.2) +
    facet_wrap(~ mm_type) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Aggregation bias", y = "RPV accuracy", color = "Method") +
    theme_minimal()
}

#' Scatter: estimate vs truth (group share)
#' @param er_df, ei_df district-level frames
plot_estimate_vs_truth = function(er_df, ei_df = tibble::tibble(), group = c("minority","majority")) {
  group = match.arg(group)
  if (nrow(er_df) == 0 && nrow(ei_df) == 0) return(invisible(NULL))
  
  col_true = if (group == "minority") "true_minority_dem_share" else "true_majority_dem_share"
  col_er   = if (group == "minority") "er_minority_dem_share"   else "er_majority_dem_share"
  col_ei   = if (group == "minority") "ei_minority_dem_share"   else "ei_majority_dem_share"
  
  er = if (nrow(er_df) > 0) {
    er_df %>%
      dplyr::select(agg_level, mm_type, !!col_true, !!col_er) %>%
      dplyr::rename(true = !!col_true, estimate = !!col_er) %>%
      dplyr::mutate(method = "ER")
  } else tibble::tibble()
  ei = if (nrow(ei_df) > 0) {
    ei_df %>%
      dplyr::select(agg_level, mm_type, !!col_true, !!col_ei) %>%
      dplyr::rename(true = !!col_true, estimate = !!col_ei) %>%
      dplyr::mutate(method = "EI")
  } else tibble::tibble()
  
  df = dplyr::bind_rows(er, ei) %>%
    dplyr::mutate(
      agg_level = factor(agg_level, levels = c("low","medium","high")),
      mm_type  = factor(mm_type, levels = c("dem","neutral","rep"), labels = c("Dem","Neutral","Rep"))
    )
  
  ggplot(df, aes(x = true, y = estimate, color = method)) +
    geom_point(alpha = 0.25, size = 1.5) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    facet_grid(mm_type ~ agg_level) +
    coord_fixed(xlim = c(0,1), ylim = c(0,1)) +
    labs(x = "True share", y = "Estimated share", color = "Method") +
    theme_minimal()
}

#' Bar plot of population-weighted waste disparity by scenario
plot_waste_disparity = function(desc_df) {
  if (nrow(desc_df) == 0) return(invisible(NULL))
  df = desc_df %>%
    dplyr::mutate(
      agg_level = factor(agg_level, levels = c("low","medium","high")),
      mm_type  = factor(mm_type, levels = c("dem","neutral","rep"), labels = c("Dem","Neutral","Rep"))
    )
  ggplot(df, aes(x = agg_level, y = waste_disparity_weighted, fill = mm_type)) +
    geom_col(position = position_dodge(width = 0.7)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(x = "Aggregation bias", y = "Waste disparity (minority - majority)") +
    theme_minimal()
}
