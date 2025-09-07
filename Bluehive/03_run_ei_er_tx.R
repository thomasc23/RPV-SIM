# 03_run_ei_er_tx.R
# Run ER/EI over republican/neutral/democratic ensembles using prepared inputs

suppressPackageStartupMessages({
  library(readr);  library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(ei)
})

# -------------------- CONFIG --------------------
prepared_dir = "TX2/Output/EI_ER_prepared"   # = match 02_ script output
progress_dir = file.path(prepared_dir, "progress")
dir.create(progress_dir, recursive = TRUE, showWarnings = FALSE)

ei_burnin  = 5000
ei_sample  = 10000
min_precincts_per_district = 3
use_parallel = TRUE

# -------------------- LOAD --------------------
assignments_long = readr::read_csv(file.path(prepared_dir, "assignments_long.csv"), show_col_types = FALSE)
map_index        = readr::read_csv(file.path(prepared_dir, "map_index.csv"),        show_col_types = FALSE)
precincts        = readr::read_csv(file.path(prepared_dir, "precincts.csv"),        show_col_types = FALSE)

# -------------------- HELPERS --------------------
get_district_assignments = function(assignments_long, ptype, map_id) {
  df = assignments_long %>%
    filter(plan_type == tolower(ptype), map_id == !!map_id) %>%
    select(precinct_id, district_id)
  if (nrow(df) == 0) stop("No assignments for plan_type=", ptype, " map_id=", map_id)
  df
}

calculate_precinct_stats = function(ptype, map_id, precincts, assignments_long) {
  assign_df = get_district_assignments(assignments_long, ptype, map_id)
  precinct_stats = precincts %>%
    inner_join(assign_df, by = "precinct_id") %>%
    mutate(plan_type = tolower(ptype), map_id = map_id)
  
  if (!"minority_vap" %in% names(precinct_stats)) {
    precinct_stats$minority_vap = precinct_stats$n_minority
    precinct_stats$majority_vap = precinct_stats$n_majority
  }
  precinct_stats
}

calculate_ground_truth = function(precinct_stats) {
  precinct_stats %>%
    group_by(map_id, plan_type, district_id) %>%
    summarise(
      total_population = sum(population),
      total_minority   = sum(n_minority),
      total_majority   = sum(n_majority),
      total_dem_votes  = sum(dem_votes),
      total_rep_votes  = sum(rep_votes),
      true_minority_dem_votes = sum(dem_votes_minority),
      true_majority_dem_votes = sum(dem_votes_majority),
      true_minority_dem_share = ifelse(sum(n_minority) > 0,
                                       sum(dem_votes_minority) / sum(n_minority), NA_real_),
      true_majority_dem_share = ifelse(sum(n_majority) > 0,
                                       sum(dem_votes_majority) / sum(n_majority), NA_real_),
      true_overall_dem_share  = sum(dem_votes) / sum(population),
      n_precincts = n(),
      .groups = "drop"
    ) %>%
    mutate(
      true_rpv = dplyr::case_when(
        is.na(true_minority_dem_share) | is.na(true_majority_dem_share) ~ NA_real_,
        (true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5) ~ 1,
        (true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5) ~ 1,
        TRUE ~ 0
      )
    )
}

run_ecological_regression = function(precinct_stats, ground_truth) {
  plan_type = unique(precinct_stats$plan_type)
  map_id    = unique(precinct_stats$map_id)
  er_results = list()
  
  for (dist_id in sort(unique(precinct_stats$district_id))) {
    district_precincts = precinct_stats %>% filter(district_id == dist_id)
    if (nrow(district_precincts) < 3) next
    
    dat = district_precincts %>%
      mutate(
        minority_share = n_minority / population,
        majority_share = n_majority / population,
        dem_voteshare  = dem_votes  / population
      ) %>% as.data.frame()
    if (nrow(dat) < 3) next
    
    out = try({
      fit = lm(dem_voteshare ~ minority_share + majority_share - 1,
                data = dat, weights = population)
      coef_min = coef(fit)[["minority_share"]]
      coef_maj = coef(fit)[["majority_share"]]
      se_min   = summary(fit)$coefficients["minority_share", "Std. Error"]
      se_maj   = summary(fit)$coefficients["majority_share", "Std. Error"]
      
      truth = ground_truth %>% filter(district_id == dist_id)
      
      pred_rpv = as.numeric(
        (coef_min > 0.5 & coef_maj < 0.5) |
          (coef_min < 0.5 & coef_maj > 0.5)
      )
      
      tibble(
        plan_type = plan_type, map_id = map_id, district_id = dist_id,
        er_minority_dem_share = coef_min,
        er_majority_dem_share = coef_maj,
        er_minority_se = se_min, er_majority_se = se_maj,
        er_rpv = pred_rpv,
        true_minority_dem_share = truth$true_minority_dem_share,
        true_majority_dem_share = truth$true_majority_dem_share,
        true_rpv = truth$true_rpv,
        er_minority_resid = ifelse(is.na(truth$true_minority_dem_share), NA, coef_min - truth$true_minority_dem_share),
        er_majority_resid = ifelse(is.na(truth$true_majority_dem_share), NA, coef_maj - truth$true_majority_dem_share),
        er_rpv_error = ifelse(is.na(truth$true_rpv), NA, pred_rpv - truth$true_rpv),
        n_precincts = nrow(dat),
        total_population = truth$total_population
      )
    }, silent = TRUE)
    
    if (!inherits(out, "try-error")) er_results[[length(er_results) + 1]] = out
  }
  
  if (!length(er_results)) return(tibble())
  bind_rows(er_results)
}

run_ei_district = function(precinct_stats, ground_truth,
                            burnin = ei_burnin, sample = ei_sample,
                            min_precincts = min_precincts_per_district,
                            parallel = use_parallel) {
  
  plan_type = unique(precinct_stats$plan_type)
  map_id    = unique(precinct_stats$map_id)
  districts = sort(unique(precinct_stats$district_id))
  
  run_one = function(dist_id) {
    dat = precinct_stats %>%
      filter(district_id == dist_id) %>%
      transmute(
        dem_votes, rep_votes,
        minority_vap = n_minority,
        majority_vap = n_majority
      )
    if (nrow(dat) < min_precincts) return(NULL)
    
    res = try({
      capture.output({
        fit = ei(cbind(dem_votes, rep_votes) ~ cbind(minority_vap, majority_vap),
                  data = dat, burnin = burnin, sample = sample, verbose = FALSE)
      }, file = nullfile())
      
      B = fit$draws$Beta
      n_draws = nrow(B)
      min_cols = paste0("beta.minority_vap.dem_votes.", seq_len(nrow(dat)))
      maj_cols = paste0("beta.majority_vap.dem_votes.", seq_len(nrow(dat)))
      w_min = dat$minority_vap; w_maj = dat$majority_vap
      
      # vectorized aggregation
      min_draws = as.numeric((as.matrix(B[, min_cols, drop = FALSE]) %*% w_min) / sum(w_min))
      maj_draws = as.numeric((as.matrix(B[, maj_cols, drop = FALSE]) %*% w_maj) / sum(w_maj))
      
      ei_min = mean(min_draws); ei_maj = mean(maj_draws)
      ei_sd_min = sd(min_draws); ei_sd_maj = sd(maj_draws)
      
      truth = ground_truth %>% filter(district_id == dist_id)
      pred_rpv = as.numeric((ei_min > 0.5 & ei_maj < 0.5) | (ei_min < 0.5 & ei_maj > 0.5))
      
      tibble(
        plan_type = plan_type, map_id = map_id, district_id = dist_id,
        ei_minority_dem_share = ei_min, ei_majority_dem_share = ei_maj,
        ei_minority_sd = ei_sd_min, ei_majority_sd = ei_sd_maj,
        ei_rpv = pred_rpv,
        true_minority_dem_share = truth$true_minority_dem_share,
        true_majority_dem_share = truth$true_majority_dem_share,
        true_rpv = truth$true_rpv,
        ei_minority_resid = ifelse(is.na(truth$true_minority_dem_share), NA, ei_min - truth$true_minority_dem_share),
        ei_majority_resid = ifelse(is.na(truth$true_majority_dem_share), NA, ei_maj - truth$true_majority_dem_share),
        total_population = truth$total_population,
        n_precincts = truth$n_precincts,
        prop_minority = truth$total_minority / pmax(1, truth$total_population),
        prop_dem = truth$total_dem_votes / pmax(1, truth$total_dem_votes + truth$total_rep_votes)
      )
    }, silent = TRUE)
    
    if (inherits(res, "try-error")) return(NULL)
    res
  }
  
  if (parallel && requireNamespace("parallel", quietly = TRUE)) {
    ncores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = parallel::detectCores() - 1))
    ncores = max(1L, ncores)
    cl = parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterEvalQ(cl, {
      suppressPackageStartupMessages({ library(ei); library(dplyr); library(magrittr) })
      NULL
    })
    parallel::clusterExport(cl,
                            varlist = c("precinct_stats", "ground_truth", "burnin", "sample", "min_precincts"),
                            envir = environment()
    )
    out = parallel::parLapply(cl, districts, run_one)
  } else {
    cat("Running EI over", length(districts), "districts ...\n")
    out = lapply(districts, run_one)
  }
  
  out = out[!vapply(out, is.null, logical(1))]
  if (!length(out)) return(tibble())
  bind_rows(out)
}

progress_path = function(plan_type, map_id) file.path(progress_dir, paste0("map_", map_id, "_", plan_type, ".rds"))
is_done       = function(plan_type, map_id) file.exists(progress_path(plan_type, map_id))
save_map_results = function(plan_type, map_id, er_df, ei_df) {
  saveRDS(list(plan_type = plan_type, map_id = map_id, er = er_df, ei = ei_df),
          file = progress_path(plan_type, map_id))
}

load_all_results = function() {
  files = list.files(progress_dir, pattern = "^map_.*\\.rds$", full.names = TRUE)
  if (!length(files)) return(list(er = tibble(), ei = tibble()))
  lst = lapply(files, readRDS)
  er = bind_rows(lapply(lst, `[[`, "er"))
  ei = bind_rows(lapply(lst, `[[`, "ei"))
  list(er = er, ei = ei)
}

# -------------------- MAIN LOOP --------------------
plan_types = c("neutral", "republican", "democratic")

counts = assignments_long %>%
  distinct(plan_type, map_id) %>%
  count(plan_type, name = "n_maps")
cat("Maps available:\n"); print(counts)

for (ptype in plan_types) {
  maps = assignments_long %>%
    filter(plan_type == ptype) %>%
    distinct(map_id) %>%
    arrange(map_id) %>%
    pull(map_id)
  
  if (!length(maps)) { cat("\n[", ptype, "] no maps found, skipping.\n", sep = ""); next }
  
  cat("\n[", ptype, "] processing ", length(maps), " maps ...\n", sep = "")
  done = sum(is_done(ptype, maps))
  cat("  already completed: ", done, " / ", length(maps), "\n", sep = "")
  
  pb = txtProgressBar(min = 0, max = length(maps), style = 3); k = 0
  for (mid in maps) {
    k = k + 1
    if (is_done(ptype, mid)) { setTxtProgressBar(pb, k); next }
    
    pst   = calculate_precinct_stats(ptype, mid, precincts, assignments_long)
    truth = calculate_ground_truth(pst)
    
    er_df = run_ecological_regression(pst, truth)
    ei_df = run_ei_district(pst, truth)
    
    save_map_results(ptype, mid, er_df, ei_df)
    setTxtProgressBar(pb, k)
  }
  close(pb)
}

all_res = load_all_results()
readr::write_csv(all_res$er, file.path(prepared_dir, "ER_results_all_maps.csv"))
readr::write_csv(all_res$ei, file.path(prepared_dir, "EI_results_all_maps.csv"))

cat("\nDone.\n  ER rows: ", nrow(all_res$er),
    "\n  EI rows: ", nrow(all_res$ei),
    "\nResults written in: ", prepared_dir, "\n", sep = "")
