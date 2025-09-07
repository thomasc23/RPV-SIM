# 05_make_tx_tables_figs.R
# Build ER/EI summary tables + one example figure from panel produced by 04_*

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tidyr); library(stringr); library(purrr)
  library(ggplot2); library(sf)
})

rm(list = ls())

# ---------- CONFIG ----------
prep_dir  = "Output/BLUEHIVE MAPS/TEXAS/EI_ER_prepared"
panel_rds = file.path(prep_dir, "district_panel.rds")
stopifnot(file.exists(panel_rds))

out_tab_dir = file.path(prep_dir, "tables"); dir.create(out_tab_dir, showWarnings = FALSE, recursive = TRUE)
out_fig_dir = file.path(prep_dir, "figs");   dir.create(out_fig_dir,  showWarnings = FALSE, recursive = TRUE)
latex_out   = file.path(out_tab_dir, "er_ei_comparison_table.tex")

# ---------- LOAD ----------
df = readRDS(panel_rds)

# Keep only districts where truth is defined
df = df %>% filter(!is.na(true_minority_dem_share), !is.na(true_majority_dem_share))

# Add display factors to mimic "Segregation & Redistricting" layout
df = df %>%
  mutate(
    seg_level   = factor("Texas", levels = "Texas"),
    redist_level = factor(case_when(
      plan_type == "democratic" ~ "Dem",
      plan_type == "neutral"    ~ "Neutral",
      plan_type == "republican" ~ "Rep",
      TRUE ~ plan_type
    ), levels = c("Dem","Neutral","Rep"))
  )

# ---------- HELPERS ----------
fmt = function(tab) tab %>% mutate(across(where(is.numeric), ~sprintf("%.1f", .)))

strict_confusion = function(data, prefix){
  correct = sym(paste0(prefix, "_rpv_correct_strict"))
  tp = sym(paste0(prefix, "_tp_strict"))
  tn = sym(paste0(prefix, "_tn_strict"))
  fp = sym(paste0(prefix, "_fp_strict"))
  fn = sym(paste0(prefix, "_fn_strict"))
  data %>%
    summarise(
      pct_correct_rpv = mean(!!correct, na.rm=TRUE) * 100,
      tp_rate = mean(!!tp, na.rm=TRUE) * 100,
      tn_rate = mean(!!tn, na.rm=TRUE) * 100,
      fp_rate = mean(!!fp, na.rm=TRUE) * 100,
      fn_rate = mean(!!fn, na.rm=TRUE) * 100,
      .groups = "drop"
    )
}

# ---------- ER/EI PERFORMANCE TABLES (Texas) ----------
er_tab = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean((er_minority_resid * 100)^2, na.rm = TRUE)),
    rmse_maj = sqrt(mean((er_majority_resid * 100)^2, na.rm = TRUE)),
    bias_min = mean(er_minority_resid * 100, na.rm = TRUE),
    bias_maj = mean(er_majority_resid * 100, na.rm = TRUE),
    mae_min  = mean(abs(er_minority_resid) * 100, na.rm = TRUE),
    mae_maj  = mean(abs(er_majority_resid) * 100, na.rm = TRUE),
    true_spread = mean((true_minority_dem_share - true_majority_dem_share) * 100, na.rm = TRUE),
    est_spread  = mean((er_minority_dem_share  - er_majority_dem_share)  * 100, na.rm = TRUE),
    bias_spread = est_spread - true_spread,
    true_rpv = mean(true_rpv, na.rm = TRUE) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share - true_minority_dem_share) !=
                            sign(er_majority_dem_share   - er_minority_dem_share), na.rm = TRUE) * 100,
    pct_under = mean(er_minority_dem_share < true_minority_dem_share, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  left_join(df %>% 
              group_by(seg_level, redist_level) %>% 
              strict_confusion("er"),
            by = c("seg_level","redist_level")) %>%
  arrange(seg_level, redist_level)

ei_tab = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean((ei_minority_resid * 100)^2, na.rm = TRUE)),
    rmse_maj = sqrt(mean((ei_majority_resid * 100)^2, na.rm = TRUE)),
    bias_min = mean(ei_minority_resid * 100, na.rm = TRUE),
    bias_maj = mean(ei_majority_resid * 100, na.rm = TRUE),
    mae_min  = mean(abs(ei_minority_resid) * 100, na.rm = TRUE),
    mae_maj  = mean(abs(ei_majority_resid) * 100, na.rm = TRUE),
    true_spread = mean((true_minority_dem_share - true_majority_dem_share) * 100, na.rm = TRUE),
    est_spread  = mean((ei_minority_dem_share  - ei_majority_dem_share)  * 100, na.rm = TRUE),
    bias_spread = est_spread - true_spread,
    true_rpv = mean(true_rpv, na.rm = TRUE) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share - true_minority_dem_share) !=
                            sign(ei_majority_dem_share   - ei_minority_dem_share), na.rm = TRUE) * 100,
    pct_under = mean(ei_minority_dem_share < true_minority_dem_share, na.rm = TRUE) * 100,
    .groups = "drop"
  ) %>%
  left_join(df %>% 
              group_by(seg_level, redist_level) %>% 
              strict_confusion("ei"),
            by = c("seg_level","redist_level")) %>%
  arrange(seg_level, redist_level)

er_fmt = fmt(er_tab)
ei_fmt = fmt(ei_tab)

readr::write_csv(er_tab, file.path(out_tab_dir, "tx_er_summary_by_redist.csv"))
readr::write_csv(ei_tab, file.path(out_tab_dir, "tx_ei_summary_by_redist.csv"))

# ---------- LaTeX (same formatting pattern) ----------
make_perf_table_tex = function(er_fmt, ei_fmt, out_path){
  latex = "\\begin{table}[!htbp]
\\centering
\\small
\\setlength{\\tabcolsep}{3pt}
\\caption{Performance Comparison of Ecological Regression (ER) and Ecological Inference (EI) -- Texas}
\\label{tab:er_ei_tx}
\\begin{tabular}{@{}llcccccccccc@{}}
\\toprule
& & \\multicolumn{2}{c}{RMSE} & \\multicolumn{3}{c}{Bias} & \\multicolumn{2}{c}{Ground Truth} & \\multicolumn{3}{c}{Classification (Strict)} \\\\
\\cmidrule(lr){3-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-9} \\cmidrule(lr){10-12}
Segregation & Redistricting & Min. & Maj. & Min. & Maj. & Spread & Spread & True RPV & RPV Correct & Sign Flips & \\\\% Under \\\\
\\midrule
\\multicolumn{11}{l}{\\textit{Panel A: Ecological Regression (ER)}} \\\\ \\\\
"
  for (i in seq_len(nrow(er_fmt))) {
    row = er_fmt[i,]
    seg_label = if (i == 1) as.character(row$seg_level) else ""
    latex = paste0(
      latex,
      seg_label, " & ", row$redist_level, " & ",
      row$rmse_min, " & ", row$rmse_maj, " & ",
      row$bias_min, " & ", row$bias_maj, " & ", row$bias_spread, " & ",
      row$true_spread, " & ", row$pct_correct_rpv, " & ", row$pct_sign_flips, " & ", row$pct_under, " \\\\",
      "\n"
    )
  }
  latex = paste0(latex, "\\midrule
\\multicolumn{11}{l}{\\textit{Panel B: Ecological Inference (EI)}} \\\\ \\\\
")
  for (i in seq_len(nrow(ei_fmt))) {
    row = ei_fmt[i,]
    seg_label = if (i == 1) as.character(row$seg_level) else ""
    latex = paste0(
      latex,
      seg_label, " & ", row$redist_level, " & ",
      row$rmse_min, " & ", row$rmse_maj, " & ",
      row$bias_min, " & ", row$bias_maj, " & ", row$bias_spread, " & ",
      row$true_spread, " & ", row$pct_correct_rpv, " & ", row$pct_sign_flips, " & ", row$pct_under, " \\\\",
      "\n"
    )
  }
  latex = paste0(latex, "\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.5cm}
\\footnotesize
\\textit{Note:} TX-only results. Classification uses strict, orientation-aware RPV. RMSE/MAE/Bias are percentage points.
\\end{minipage}
\\end{table}")
  writeLines(latex, out_path)
  invisible(out_path)
}

perf_tex_path = file.path(out_tab_dir, "tx_er_ei_comparison_table.tex")
make_perf_table_tex(er_fmt, ei_fmt, perf_tex_path)

# ---------- DISTRICT-LEVEL / MAP-LEVEL / WASTE SUMMARIES (Texas) ----------
district_summary = df %>%
  mutate(
    dem_win_margin = (total_dem_votes - total_rep_votes) / total_population,
    is_competitive = abs(dem_win_margin) < 0.05,
    is_safe_dem = dem_win_margin > 0.05,
    is_safe_rep = dem_win_margin < -0.05
  ) %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    n_districts = n(),
    pct_competitive = mean(is_competitive) * 100,
    margin_sd = sd(dem_win_margin) * 100,
    pct_safe_dem = mean(is_safe_dem) * 100,
    pct_safe_rep = mean(is_safe_rep) * 100,
    .groups = "drop"
  )

map_summary = df %>%
  mutate(
    dem_win_margin = (total_dem_votes - total_rep_votes) / total_population,
    dem_won = dem_win_margin > 0,
    maj_min = (total_minority / total_population) > 0.5
  ) %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    n_dem_seats = sum(dem_won, na.rm=TRUE),
    n_total_seats = n(),
    n_maj_min = sum(maj_min, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    avg_dem_seats = mean(n_dem_seats),
    avg_maj_min   = mean(n_maj_min),
    min_dem_seats = min(n_dem_seats),
    max_dem_seats = max(n_dem_seats),
    .groups = "drop"
  )

scenario_summary = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    minority_waste_rate = 100 * sum(minority_wasted_total, na.rm=TRUE) / sum(total_minority, na.rm=TRUE),
    majority_waste_rate = 100 * sum(majority_wasted_total, na.rm=TRUE) / sum(total_majority, na.rm=TRUE),
    pop_weighted_waste_disparity = minority_waste_rate - majority_waste_rate,
    .groups = "drop"
  )

full_summary = district_summary %>%
  left_join(map_summary,      by = c("seg_level","redist_level")) %>%
  left_join(scenario_summary, by = c("seg_level","redist_level")) %>%
  arrange(seg_level, redist_level)

readr::write_csv(full_summary, file.path(out_tab_dir, "tx_full_summary.csv"))

# LaTeX for the competitiveness table 
comp_tex <- "\\begin{table}[!htbp]
\\centering
\\footnotesize
\\setlength{\\tabcolsep}{3pt}
\\caption{Electoral Competitiveness Across Redistricting Scenarios (Texas)}
\\label{tab:electoral_competitiveness_tx}
\\begin{tabular}{@{}llrrrrrrrrr@{}}
\\toprule
% group header row (must precede cmidrule)
Seg. & Redist. & \\multicolumn{5}{c}{District metrics} & \\multicolumn{3}{c}{Map metrics} & \\multicolumn{1}{c}{Waste} \\\\
\\cmidrule(lr){3-7} \\cmidrule(lr){8-10} \\cmidrule(lr){11-11}
% subheader row
& & N & Comp. (\\%) & SD (\\%) & Safe D (\\%) & Safe R (\\%) & D Seats (Avg) & Min--Max D Seats & Maj--Min Dists & Disp. (pp) \\\\
\\midrule
"
for (i in seq_len(nrow(full_summary))) {
  row <- full_summary[i,]
  seg_label <- if (i == 1) as.character(row$seg_level) else ""
  row_text <- sprintf("%s & %s & %d & %.1f & %.1f & %.1f & %.1f & %.1f & %d-%d & %.1f & %.1f \\\\",
                      seg_label,
                      row$redist_level,
                      row$n_districts,
                      row$pct_competitive,
                      row$margin_sd,
                      row$pct_safe_dem,
                      row$pct_safe_rep,
                      row$avg_dem_seats,
                      row$min_dem_seats,
                      row$max_dem_seats,
                      row$avg_maj_min,
                      row$pop_weighted_waste_disparity)
  comp_tex <- paste0(comp_tex, row_text, "\n")
}
comp_tex <- paste0(comp_tex, "\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.2cm}
\\scriptsize
\\textit{Note:} TX-only. Comp. = $|$margin$|$ < 5 pp. Waste disparity is population-weighted (minority minus majority waste rates).
\\end{minipage}
\\end{table}")
writeLines(comp_tex, file.path(out_tab_dir, "tx_competitiveness_table.tex"))

# ---------- Outcomes by race table ----------
summary_table = df %>%
  mutate(
    minority_dem_copartisan = true_minority_dem_votes * (prop_dem > 0.5),
    minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (prop_dem <= 0.5),
    minority_copartisan     = minority_dem_copartisan + minority_rep_copartisan,
    
    majority_dem_copartisan = true_majority_dem_votes * (prop_dem > 0.5),
    majority_rep_copartisan = (total_majority - true_majority_dem_votes) * (prop_dem <= 0.5),
    majority_copartisan     = majority_dem_copartisan + majority_rep_copartisan,
    
    dem_margin = (total_dem_votes - total_rep_votes) / total_population,
    is_competitive = abs(dem_margin) < 0.05,
    
    minority_in_competitive = total_minority * is_competitive,
    majority_in_competitive = total_majority * is_competitive
  ) %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    pr_minority_copartisan   = 100 * sum(minority_copartisan, na.rm=TRUE) / sum(total_minority, na.rm=TRUE),
    pr_majority_copartisan   = 100 * sum(majority_copartisan, na.rm=TRUE) / sum(total_majority, na.rm=TRUE),
    pr_minority_competitive  = 100 * sum(minority_in_competitive, na.rm=TRUE) / sum(total_minority, na.rm=TRUE),
    pr_majority_competitive  = 100 * sum(majority_in_competitive, na.rm=TRUE) / sum(total_majority, na.rm=TRUE),
    pr_minority_wasted       = 100 * sum(minority_wasted_total, na.rm=TRUE) / sum(total_minority, na.rm=TRUE),
    pr_majority_wasted       = 100 * sum(majority_wasted_total, na.rm=TRUE) / sum(total_majority, na.rm=TRUE),
    .groups = "drop"
  ) %>%
  arrange(seg_level, redist_level)

readr::write_csv(summary_table, file.path(out_tab_dir, "tx_outcomes_by_race.csv"))

race_tex = "\\begin{table}[!htbp]
\\centering
\\small
\\setlength{\\tabcolsep}{3pt}
\\caption{Electoral Outcomes by Race Across Redistricting Scenarios (Texas)}
\\label{tab:electoral_outcomes_by_race_tx}
\\begin{tabular}{@{}llcccccc@{}}
\\toprule
& & \\multicolumn{2}{c}{Pr(Represented by} & \\multicolumn{2}{c}{Pr(Reside in} & \\multicolumn{2}{c}{Pr(Wasted)} \\\\
& & \\multicolumn{2}{c}{Copartisan)} & \\multicolumn{2}{c}{Competitive District)} & \\multicolumn{2}{c}{} \\\\
\\cmidrule(lr){3-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-8}
Segregation & Redistricting & Min. & Maj. & Min. & Maj. & Min. & Maj. \\\\
\\midrule
"
for (i in seq_len(nrow(summary_table))) {
  row = summary_table[i,]
  seg_label = if (i == 1) as.character(row$seg_level) else ""
  row_text = sprintf("%s & %s & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f \\\\",
                      seg_label,
                      row$redist_level,
                      row$pr_minority_copartisan,
                      row$pr_majority_copartisan,
                      row$pr_minority_competitive,
                      row$pr_majority_competitive,
                      row$pr_minority_wasted,
                      row$pr_majority_wasted)
  race_tex = paste0(race_tex, row_text, "\n")
}
race_tex = paste0(race_tex, "\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.3cm}
\\footnotesize
\\textit{Note:} TX-only. Copartisan = represented by preferred party; Competitive = |margin| < 5pp; Wasted = packed+cracked share.
\\end{minipage}
\\end{table}")
writeLines(race_tex, file.path(out_tab_dir, "tx_outcomes_by_race_table.tex"))





















# ---------- EXAMPLE FIGURE: pooled ER/EI vs Truth by group & plan ----------
fig_df = df %>%
  transmute(
    plan_type, map_id, district_id,
    true_b_min = true_minority_dem_share,
    true_b_maj = true_majority_dem_share,
    er_b_min   = er_minority_dem_share,
    er_b_maj   = er_majority_dem_share,
    ei_b_min   = ei_minority_dem_share,
    ei_b_maj   = ei_majority_dem_share
  ) %>%
  pivot_longer(
    cols = -c(plan_type, map_id, district_id),
    names_to = c("model","group"),
    names_pattern = "(er|ei|true)_b_(min|maj)",
    values_to = "value"
  ) %>%
  pivot_wider(names_from = model, values_from = value) %>%
  filter(!is.na(true)) %>%
  mutate(group = ifelse(group == "min", "Minority", "Majority"))

p = ggplot(fig_df, aes(x = true)) +
  geom_point(aes(y = er, color = "ER"), alpha = 0.25, size = 1) +
  geom_point(aes(y = ei, color = "EI"), alpha = 0.25, size = 1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  facet_grid(group ~ plan_type) +
  labs(x = "True Group Democratic Share", y = "Estimated Share",
       color = "Model",
       title = "ER / EI vs Truth by Group and Plan Type") +
  theme_bw()

ggsave(file.path(out_fig_dir, "er_ei_vs_truth_by_plan.pdf"), p, width = 10, height = 6)
cat("Figure saved: ", file.path(out_fig_dir, "er_ei_vs_truth_by_plan.pdf"), "\n", sep = "")





