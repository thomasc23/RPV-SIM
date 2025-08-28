#' Aggregators & partial-progress combiners
#' @keywords internal
#' @importFrom dplyr group_by summarise n mutate across left_join arrange distinct
#' @importFrom purrr map_dfr
#' @importFrom yardstick conf_mat
NULL



#' Hepler for classifying RPV
classify_rpv = function(true_min, true_maj, pred_min, pred_maj) {
  true_rpv = (true_min > 0.5 & true_maj < 0.5) | (true_min < 0.5 & true_maj > 0.5)
  pred_rpv = (pred_min > 0.5 & pred_maj < 0.5) | (pred_min < 0.5 & pred_maj > 0.5)
  
  if (true_rpv & pred_rpv) return("TP")
  if (!true_rpv & !pred_rpv) return("TN")
  if (!true_rpv & pred_rpv) return("FP")
  if (true_rpv & !pred_rpv) return("FN")
  return(NA_character_)  
}


#' Reduce district-level ER/EI results into per-map metrics
#' @param er_df tibble with ER columns/residuals
#' @param ei_df tibble with EI columns/residuals (optional)
#' @return long tibble (method ∈ {ER, EI})
aggregate_map_performance = function(er_df, ei_df = tibble::tibble()) {
  parts = list()
  if (nrow(er_df) > 0) {
    er = er_df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rpv_class = classify_rpv(
        true_minority_dem_share, true_majority_dem_share, 
        er_minority_dem_share, er_majority_dem_share
      )) %>%
      ungroup() %>%
      dplyr::mutate(
        truth = factor((true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) | (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5),
                       levels = c(FALSE, TRUE)),
        estimate = factor((er_minority_dem_share > 0.5 & er_majority_dem_share < 0.5) | (er_minority_dem_share < 0.5 & er_majority_dem_share > 0.5),
                          levels = c(FALSE, TRUE))
      ) %>%
      yardstick::conf_mat(truth, estimate) %>%
      dplyr::group_by(map_id, agg_level, mm_type) %>%
      dplyr::summarise(
        mae_minority = mean(abs(er_minority_resid), na.rm = TRUE),
        mae_majority = mean(abs(er_majority_resid), na.rm = TRUE),
        rpv_accuracy = mean(rpv_correct, na.rm = TRUE),
        bias_minority = mean(er_minority_resid, na.rm = TRUE),
        bias_majority = mean(er_majority_resid, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(method = "ER")
    parts$er = er
  }
  if (nrow(ei_df) > 0) {
    ei = ei_df %>%
      dplyr::rowwise() %>%
      dplyr::mutate(rpv_class = classify_rpv(
        true_minority_dem_share, true_majority_dem_share, 
        ei_minority_dem_share, ei_majority_dem_share
      )) %>%
      ungroup() %>%
      dplyr::mutate(
        truth = factor((true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) | (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5),
                       levels = c(FALSE, TRUE)),
        estimate = factor((ei_minority_dem_share > 0.5 & ei_majority_dem_share < 0.5) | (ei_minority_dem_share < 0.5 & ei_majority_dem_share > 0.5),
                          levels = c(FALSE, TRUE))
      ) %>%
      yardstick::conf_mat(truth, estimate) %>%
      dplyr::group_by(map_id, agg_level, mm_type) %>%
      dplyr::summarise(
        mae_minority = mean(abs(ei_minority_resid), na.rm = TRUE),
        mae_majority = mean(abs(ei_majority_resid), na.rm = TRUE),
        rpv_accuracy = mean(rpv_correct, na.rm = TRUE),
        bias_minority = mean(ei_minority_resid, na.rm = TRUE),
        bias_majority = mean(ei_majority_resid, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(method = "EI")
    parts$ei = ei
  }
  dplyr::bind_rows(parts)
}

#' Scenario-level CI summary from per-map metrics
#' @param perf tibble from aggregate_map_performance
#' @return tibble with quantile CIs and n_maps
summarise_with_ci = function(perf) {
  if (nrow(perf) == 0) return(tibble::tibble())
  perf %>%
    dplyr::group_by(agg_level, mm_type, method) %>%
    dplyr::summarise(
      n_maps              = dplyr::n(),
      mae_minority_mean   = mean(mae_minority, na.rm = TRUE),
      mae_minority_lower  = stats::quantile(mae_minority, 0.025, na.rm = TRUE),
      mae_minority_upper  = stats::quantile(mae_minority, 0.975, na.rm = TRUE),
      mae_majority_mean   = mean(mae_majority, na.rm = TRUE),
      mae_majority_lower  = stats::quantile(mae_majority, 0.025, na.rm = TRUE),
      mae_majority_upper  = stats::quantile(mae_majority, 0.975, na.rm = TRUE),
      rpv_accuracy_mean   = mean(rpv_accuracy, na.rm = TRUE),
      rpv_accuracy_lower  = stats::quantile(rpv_accuracy, 0.025, na.rm = TRUE),
      rpv_accuracy_upper  = stats::quantile(rpv_accuracy, 0.975, na.rm = TRUE),
      bias_minority_mean  = mean(bias_minority, na.rm = TRUE),
      bias_minority_lower = stats::quantile(bias_minority, 0.025, na.rm = TRUE),
      bias_minority_upper = stats::quantile(bias_minority, 0.975, na.rm = TRUE),
      bias_majority_mean  = mean(bias_majority, na.rm = TRUE),
      bias_majority_lower = stats::quantile(bias_majority, 0.025, na.rm = TRUE),
      bias_majority_upper = stats::quantile(bias_majority, 0.975, na.rm = TRUE),
      .groups             = "drop"
    )
}

#' Population-weighted descriptive outcomes by scenario
#' (copartisan representation, competitive residency, wasted votes)
#' @param truth_waste from calculate_wasted_votes()
#' @param competitive_threshold numeric margin (default .05)
descriptive_outcomes_by_scenario = function(truth_waste, competitive_threshold = 0.05) {
  if (nrow(truth_waste) == 0) return(tibble::tibble())
  
  truth_waste %>%
    dplyr::mutate(
      dem_margin              = (total_dem_votes - total_rep_votes) / total_population,
      is_competitive          = abs(dem_margin) < competitive_threshold,
      # copartisan counts
      minority_dem_copartisan = true_minority_dem_votes * dem_district,
      minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (1 - dem_district),
      minority_copartisan     = minority_dem_copartisan + minority_rep_copartisan,
      majority_dem_copartisan = true_majority_dem_votes * dem_district,
      majority_rep_copartisan = (total_majority - true_majority_dem_votes) * (1 - dem_district),
      majority_copartisan     = majority_dem_copartisan + majority_rep_copartisan,
      minority_in_competitive = total_minority * is_competitive,
      majority_in_competitive = total_majority * is_competitive
    ) %>%
    dplyr::group_by(agg_level, mm_type) %>%
    dplyr::summarise(
      pr_minority_copartisan   = sum(minority_copartisan, na.rm = TRUE) / sum(total_minority, na.rm = TRUE),
      pr_majority_copartisan   = sum(majority_copartisan, na.rm = TRUE) / sum(total_majority, na.rm = TRUE),
      pr_minority_competitive  = sum(minority_in_competitive, na.rm = TRUE) / sum(total_minority, na.rm = TRUE),
      pr_majority_competitive  = sum(majority_in_competitive, na.rm = TRUE) / sum(total_majority, na.rm = TRUE),
      pr_minority_wasted       = sum(minority_wasted_total, na.rm = TRUE) / sum(total_minority, na.rm = TRUE),
      pr_majority_wasted       = sum(majority_wasted_total, na.rm = TRUE) / sum(total_majority, na.rm = TRUE),
      waste_disparity_weighted = pr_minority_wasted - pr_majority_wasted,
      .groups                  = "drop"
    )
}
