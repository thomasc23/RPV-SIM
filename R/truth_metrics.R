#' Truth metrics: ground truth shares, RPV, wasted votes
#' @keywords internal
#' @importFrom dplyr group_by summarise n mutate case_when across
#' @importFrom tidyr replace_na
NULL

#' Aggregate to district-level ground truth
#' @param precinct_stats tibble from build_precinct_stats()
#' @return tibble district-level metrics incl. true shares & RPV flag
calculate_ground_truth = function(precinct_stats) {
  if (nrow(precinct_stats) == 0) return(tibble::tibble())
  
  precinct_stats %>%
    dplyr::group_by(map_id, district_id, agg_level, mm_type) %>%
    dplyr::summarise(
      distr_population        = sum(.data$population, na.rm = TRUE),
      distr_n_minority          = sum(.data$n_minority, na.rm = TRUE),
      distr_n_majority          = sum(.data$n_majority, na.rm = TRUE),
      distr_dem_votes         = sum(.data$dem_votes, na.rm = TRUE),
      distr_rep_votes         = sum(.data$rep_votes, na.rm = TRUE),
      true_minority_dem_votes = sum(.data$dem_votes_minority, na.rm = TRUE),
      true_majority_dem_votes = sum(.data$dem_votes_majority, na.rm = TRUE),
      n_precincts             = dplyr::n(),
      .groups                 = "drop"
    ) %>%
    dplyr::mutate(
      true_minority_dem_share = dplyr::case_when(
        distr_n_minority > 0 ~ true_minority_dem_votes / distr_n_minority, 
        TRUE ~ NA_real_
      ), 
      true_majority_dem_share = dplyr::case_when(
        distr_n_majority > 0 ~ true_majority_dem_votes / distr_n_majority, 
        TRUE ~ NA_real_
      ),
      true_rpv = dplyr::case_when(
        is.na(true_minority_dem_share) | is.na(true_majority_dem_share) ~ NA_real_,
        (true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) ~ 1,
        (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5) ~ 1,
        TRUE ~ 0
      )
    )
}


#' Wasted votes via packed/cracked decomposition
#' Uses actual group votes where available; fall back gracefully if NA
#' @param truth district truth tibble (from calculate_ground_truth())
#' @return truth augmented with wasted vote metrics
calculate_wasted_votes = function(truth) {
  if (nrow(truth) == 0) return(tibble::tibble())
  
  truth %>%
    dplyr::mutate(
      dem_voteshare           = dplyr::if_else(distr_dem_votes + distr_rep_votes > 0, distr_dem_votes / (distr_dem_votes + distr_rep_votes), NA_real_),
      dem_district            = as.integer(dem_voteshare > 0.5),
      votes_to_win            = floor(distr_population / 2) + 1L,
      
      # reconstruct group GOP votes if possible
      true_minority_rep_votes = ifelse(!is.na(true_minority_dem_votes), distr_n_minority - true_minority_dem_votes, NA_real_),
      true_majority_rep_votes = ifelse(!is.na(true_majority_dem_votes), distr_n_majority - true_majority_dem_votes, NA_real_),
      
      # Packed (surplus of winner) apportioned within party by group share
      packed_votes =
        dem_district * pmax(0, distr_dem_votes - votes_to_win) +
        (1 - dem_district) * pmax(0, distr_rep_votes - votes_to_win),
      
      packed_min_votes = dplyr::case_when(
        dem_district == 1 & distr_dem_votes > 0 ~ packed_votes * (true_minority_dem_votes / distr_dem_votes),
        dem_district == 0 & distr_rep_votes > 0 ~ packed_votes * (true_minority_rep_votes / distr_rep_votes),
        TRUE ~ NA_real_
      ),
      
      packed_maj_votes = dplyr::case_when(
        dem_district == 1 & distr_dem_votes > 0 ~ packed_votes * (true_majority_dem_votes / distr_dem_votes),
        dem_district == 0 & distr_rep_votes > 0 ~ packed_votes * (true_majority_rep_votes / distr_rep_votes),
        TRUE ~ NA_real_
      ),
      
      # Cracked (all losing-party votes), apportioned to group by losing party composition
      cracked_votes =
        dem_district * distr_rep_votes +
        (1 - dem_district) * distr_dem_votes,
      
      cracked_min_votes = dplyr::case_when(
        dem_district == 1 & distr_rep_votes > 0 ~ cracked_votes * (true_minority_rep_votes / distr_rep_votes),
        dem_district == 0 & distr_dem_votes > 0 ~ cracked_votes * (true_minority_dem_votes / distr_dem_votes),
        TRUE ~ NA_real_
      ),
      
      cracked_maj_votes = dplyr::case_when(
        dem_district == 1 & distr_rep_votes > 0 ~ cracked_votes * (true_majority_rep_votes / distr_rep_votes),
        dem_district == 0 & distr_dem_votes > 0 ~ cracked_votes * (true_majority_dem_votes / distr_dem_votes),
        TRUE ~ NA_real_
      ),
      
      # Totals
      minority_wasted_total = packed_min_votes + cracked_min_votes,
      majority_wasted_total = packed_maj_votes + cracked_maj_votes, # (packed_votes + cracked_votes) - minority_wasted_total,
      
      minority_waste_rate = ifelse(distr_n_minority > 0, minority_wasted_total / distr_n_minority, NA_real_),
      majority_waste_rate = ifelse(distr_n_majority > 0, majority_wasted_total / distr_n_majority, NA_real_),
      waste_disparity     = minority_waste_rate - majority_waste_rate
    )
}





