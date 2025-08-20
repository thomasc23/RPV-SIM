# Packages
library(tidyverse)
library(fixest)
library(xtable)

rm(list = ls())

setwd('~/Dropbox/RPV/Code/Simulation/Workflow/OLD/POPULATION_DENSITY/')

# Suppress dplyr messages
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)


df = readRDS('ER_EI_results_final.rds')
head(df)



# Order scenarios as factors
df$seg_level = factor(df$seg_level, levels = c('low', 'medium', 'high'), labels = c('Low', 'Medium', 'High'))
df$redist_level = factor(df$redist_level, levels = c('dem', 'middle', 'rep'), labels = c('Dem', 'Neutral', 'Rep'))

# Add map-level democratic seatshare
df = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  mutate(n_dem_seats = sum(prop_dem > 0.5),
         n_rep_seats = sum(prop_dem < 0.5)) %>%
  ungroup()

# Create the summary tables
er_tab = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean((er_minority_resid * 100)^2)),
    rmse_maj = sqrt(mean((er_majority_resid * 100)^2)), 
    bias_min = mean((er_minority_resid) * 100),
    bias_maj = mean((er_majority_resid) * 100),
    mae_min = mean(abs(er_minority_resid) * 100),
    mae_maj = mean(abs(er_majority_resid) * 100),
    true_spread = mean(true_minority_dem_share - true_majority_dem_share) * 100,
    est_spread = mean(er_minority_dem_share - er_majority_dem_share) * 100,
    bias_spread = est_spread - true_spread,
    true_rpv = mean(true_rpv) * 100,
    pct_correct_rpv = mean(er_rpv_correct) * 100,
    pct_under = mean(er_minority_dem_share < true_minority_dem_share) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share - true_minority_dem_share) != sign(er_majority_dem_share - er_minority_dem_share)) * 100
  ) %>%
  ungroup()

ei_tab = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean((ei_minority_resid * 100)^2)),
    rmse_maj = sqrt(mean((ei_majority_resid * 100)^2)), 
    bias_min = mean(ei_minority_resid * 100),
    bias_maj = mean(ei_majority_resid * 100),
    mae_min = mean(abs(ei_minority_resid) * 100),
    mae_maj = mean(abs(ei_majority_resid) * 100),
    true_spread = mean(true_minority_dem_share - true_majority_dem_share) * 100,
    est_spread = mean(ei_minority_dem_share - ei_majority_dem_share) * 100,
    bias_spread = est_spread - true_spread,
    true_rpv = mean(true_rpv) * 100,
    pct_correct_rpv = mean(ei_rpv_correct) * 100,
    pct_under = mean(ei_minority_dem_share < true_minority_dem_share) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share - true_minority_dem_share) != sign(ei_majority_dem_share - ei_minority_dem_share)) * 100
  ) %>%
  ungroup()

# Format the tables
format_table = function(tab) {
  tab %>%
    mutate(
      across(where(is.numeric), ~sprintf("%.1f", .))
    ) %>%
    arrange(seg_level, redist_level)
}

er_fmt = format_table(er_tab)
ei_fmt = format_table(ei_tab)

# Add map-level democratic seatshare
df = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  mutate(n_dem_seats = sum(prop_dem > 0.5),
         n_rep_seats = sum(prop_dem < 0.5)) %>%
  ungroup()

# Create the summary tables
er_tab = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean((er_minority_resid * 100)^2)),
    rmse_maj = sqrt(mean((er_majority_resid * 100)^2)), 
    bias_min = mean((er_minority_resid) * 100),
    bias_maj = mean((er_majority_resid) * 100),
    mae_min = mean(abs(er_minority_resid) * 100),
    mae_maj = mean(abs(er_majority_resid) * 100),
    true_spread = mean(true_minority_dem_share - true_majority_dem_share) * 100,
    est_spread = mean(er_minority_dem_share - er_majority_dem_share) * 100,
    bias_spread = est_spread - true_spread,
    true_rpv = mean(true_rpv) * 100,
    pct_correct_rpv = mean(er_rpv_correct) * 100,
    pct_under = mean(er_minority_dem_share < true_minority_dem_share) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share - true_minority_dem_share) != sign(er_majority_dem_share - er_minority_dem_share)) * 100
  ) %>%
  ungroup()

ei_tab = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean((ei_minority_resid * 100)^2)),
    rmse_maj = sqrt(mean((ei_majority_resid * 100)^2)), 
    bias_min = mean(ei_minority_resid * 100),
    bias_maj = mean(ei_majority_resid * 100),
    mae_min = mean(abs(ei_minority_resid) * 100),
    mae_maj = mean(abs(ei_majority_resid) * 100),
    true_spread = mean(true_minority_dem_share - true_majority_dem_share) * 100,
    est_spread = mean(ei_minority_dem_share - ei_majority_dem_share) * 100,
    bias_spread = est_spread - true_spread,
    true_rpv = mean(true_rpv) * 100,
    pct_correct_rpv = mean(ei_rpv_correct) * 100,
    pct_under = mean(ei_minority_dem_share < true_minority_dem_share) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share - true_minority_dem_share) != sign(ei_majority_dem_share - ei_majority_dem_share)) * 100
  ) %>%
  ungroup()

# Format the tables
format_table = function(tab) {
  tab %>%
    mutate(
      across(where(is.numeric), ~sprintf("%.1f", .))
    ) %>%
    arrange(seg_level, redist_level)
}

er_fmt = format_table(er_tab)
ei_fmt = format_table(ei_tab)

latex_code = "\\begin{table}[!htbp]
\\centering
\\small 
\\setlength{\\tabcolsep}{3pt} 
\\caption{Performance Comparison of Ecological Regression (ER) and Ecological Inference (EI)}
\\label{tab:er_ei_comparison}
\\begin{tabular}{@{}llcccccccccc@{}} 
\\toprule
& & \\multicolumn{2}{c}{RMSE} & \\multicolumn{3}{c}{Bias} & \\multicolumn{2}{c}{Ground Truth} & \\multicolumn{3}{c}{Classification} \\\\
\\cmidrule(lr){3-4} \\cmidrule(lr){5-7} \\cmidrule(lr){8-9} \\cmidrule(lr){10-12}
Segregation & Redistricting & Min. & Maj. & Min. & Maj. & Spread & Spread & True RPV & RPV Correct & Sign Flips & \\% Under \\\\
\\midrule
\\multicolumn{11}{l}{\\textit{Panel A: Ecological Regression (ER)}} \\\\ \\\\
"

# Add ER data rows
for(i in 1:nrow(er_fmt)) {
  row = er_fmt[i,]
  
  # Determine segregation label (only for first row of each group)
  if(i %in% c(1, 4, 7)) {
    seg_label = row$seg_level
  } else {
    seg_label = ""
  }
  
  # Clean redistricting label
  redist_label = row$redist_level
  
  # Add row with better spacing
  latex_code = paste0(latex_code,
                      seg_label, " & ", redist_label, " & ",
                      row$rmse_min, " & ", row$rmse_maj, " & ",
                      row$bias_min, " & ", row$bias_maj, " & ", row$bias_spread, " & ",
                      row$true_spread, " & ", row$true_rpv, " & ", 
                      row$pct_correct_rpv, " & ", row$pct_sign_flips, " & ", row$pct_under, " \\\\")
  
  latex_code = paste0(latex_code, "\n")
}

# Add midrule and EI section
latex_code = paste0(latex_code, "\\midrule
\\multicolumn{11}{l}{\\textit{Panel B: Ecological Inference (EI)}} \\\\ \\\\
")

# Add EI data rows
for(i in 1:nrow(ei_fmt)) {
  row = ei_fmt[i,]
  
  # Determine segregation label (only for first row of each group)
  if(i %in% c(1, 4, 7)) {
    seg_label = row$seg_level
  } else {
    seg_label = ""
  }
  
  # Clean redistricting label
  redist_label = row$redist_level
  
  # Add row
  latex_code = paste0(latex_code,
                      seg_label, " & ", redist_label, " & ",
                      row$rmse_min, " & ", row$rmse_maj, " & ",
                      row$bias_min, " & ", row$bias_maj, " & ", row$bias_spread, " & ",
                      row$true_spread, " & ", row$true_rpv, " & ", 
                      row$pct_correct_rpv, " & ", row$pct_sign_flips, " & ", row$pct_under, " \\\\")
  
  latex_code = paste0(latex_code, "\n")
}

# Complete the table with bottomrule and notes
latex_code = paste0(latex_code, "\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.5cm}
\\footnotesize
\\textit{Note:} Performance metrics for ecological regression (ER) and ecological inference (EI) across different levels of segregation (Low, Medium, High) and redistricting scenarios (Democratic-favoring, Neutral, Republican-favoring). RMSE = Root Mean Square Error; Min. and Maj. refer to minority and majority groups, respectively. Under the Bias columns, Min. and Maj. show bias in estimating group-specific Democratic vote shares, while Spread shows bias in estimating the difference between minority and majority Democratic support. Ground Truth Spread is the average true difference Minority Dem \\% - Majority Dem \\%; True RPV shows the actual proportion of racially polarized voting in the data. RPV Correct indicates the proportion of correctly identified RPV cases. Sign Flips shows the proportion of cases where the estimated difference between majority and minority Democratic vote share has the opposite sign from the true difference. \\% Under shows the proportion of cases where ER or EI under-predict district-level minority voteshares for the Democratic candidate. 
\\end{minipage}
\\end{table}")

# Print the LaTeX code
cat(latex_code)

# Save to file
writeLines(latex_code, "~/Dropbox/RPV/Figures/Preliminary Analysis/TC/er_ei_comparison_table.tex")















# ## INCORRECT
# # district-level metrics
# district_summary = df %>%
#   mutate(
#     dem_win_margin = (total_dem_votes - total_rep_votes) / total_population,
#     is_competitive = abs(dem_win_margin) < 0.05,
#     is_safe_dem = dem_win_margin > 0.05,
#     is_safe_rep = dem_win_margin < -0.05
#   ) %>%
#   group_by(seg_level, redist_level) %>%
#   summarise(
#     n_districts = n(),
#     pct_competitive = mean(is_competitive) * 100,
#     margin_sd = sd(dem_win_margin) * 100,
#     pct_safe_dem = mean(is_safe_dem) * 100,
#     pct_safe_rep = mean(is_safe_rep) * 100,
#     .groups = "drop"
#   )
# 
# # map-level metrics
# map_summary = df %>%
#   mutate(
#     dem_win_margin = (total_dem_votes - total_rep_votes) / total_population,
#     dem_won = dem_win_margin > 0,
#     maj_min = (total_minority / total_population) > 0.5  # Using 0.5 for majority-minority
#   ) %>%
#   group_by(map_id, seg_level, redist_level) %>%
#   summarise(
#     n_dem_seats = sum(dem_won),
#     n_total_seats = n(),
#     n_maj_min = sum(maj_min),
#     .groups = "drop"
#   ) %>%
#   group_by(seg_level, redist_level) %>%
#   summarise(
#     avg_dem_seats = mean(n_dem_seats),
#     avg_maj_min = mean(n_maj_min),
#     min_dem_seats = min(n_dem_seats),
#     max_dem_seats = max(n_dem_seats),
#     .groups = "drop"
#   )
# 
# # Calculate scenario-level metrics using waste_disparity
# scenario_summary = df %>%
#   group_by(seg_level, redist_level) %>%
#   summarise(
#     avg_waste_disparity = mean(waste_disparity) * 100,  # Convert to percentage points
#     .groups = "drop"
#   )
# 
# # Merge summaries
# full_summary = district_summary %>%
#   left_join(map_summary, by = c("seg_level", "redist_level")) %>%
#   left_join(scenario_summary, by = c("seg_level", "redist_level")) %>%
#   # Reorder seg_level factor to go Low -> Medium -> High
#   mutate(seg_level = factor(seg_level, levels = c("Low", "Medium", "High"))) %>%
#   arrange(seg_level, redist_level)
# 
# # Create more compact LaTeX table with proper multicolumn
# latex_code = "\\begin{table}[!htbp]
# \\centering
# \\footnotesize
# \\setlength{\\tabcolsep}{3pt}
# \\caption{Electoral Competitiveness Across Segregation and Redistricting Scenarios}
# \\label{tab:electoral_competitiveness}
# \\begin{tabular}{@{}llrrrrrrrrr@{}}
# \\toprule
# & &
# \\cmidrule(lr){3-7} \\cmidrule(lr){8-10} \\cmidrule(lr){11-11}
# Seg. & Redist. & N & Comp. & SD & Safe D & Safe R & D Seats & Min-Max & Maj-Min & Waste \\\\
# & & & (\\%) & (\\%) & (\\%) & (\\%) & (Avg) & D Seats & Dists & Disp. (pp) \\\\
# \\midrule
# "
# 
# # Add data rows
# for(i in 1:nrow(full_summary)) {
#   row = full_summary[i,]
# 
#   # Determine segregation label (only for first row of each segregation group)
#   if(i %in% c(1, 4, 7)) {
#     seg_label = as.character(row$seg_level)
#   } else {
#     seg_label = ""
#   }
# 
#   # Format the row
#   row_text = sprintf("%s & %s & %d & %.1f & %.1f & %.1f & %.1f & %.1f & %d-%d & %.1f & %.1f \\\\",
#                      seg_label,
#                      row$redist_level,
#                      row$n_districts,
#                      row$pct_competitive,
#                      row$margin_sd,
#                      row$pct_safe_dem,
#                      row$pct_safe_rep,
#                      row$avg_dem_seats,
#                      row$min_dem_seats,
#                      row$max_dem_seats,
#                      row$avg_maj_min,
#                      row$avg_waste_disparity)
# 
#   latex_code = paste0(latex_code, row_text, "\n")
# 
#   # Add spacing between segregation groups
#   if(i %in% c(3, 6)) {
#     latex_code = paste0(latex_code, "\\addlinespace\n")
#   }
# }
# 
# # Complete the table
# latex_code = paste0(latex_code, "\\bottomrule
# \\end{tabular}
# \\begin{minipage}{\\textwidth}
# \\vspace{0.2cm}
# \\scriptsize
# \\textit{Note:} N = number of districts; Comp. = competitive districts (margin $<$ 5\\%);
# SD = standard deviation of win margins; Safe D/R = safe Democratic/Republican
# districts (margin $>$ 5\\%); D Seats (Avg) = average Democratic seats per map;
# Min-Max D Seats = minimum and maximum Democratic seats across all maps in scenario;
# Maj-Min Dists = average number of majority-minority districts per map;
# Waste Disp. = waste disparity in percentage points (minority waste rate minus majority waste rate).
# Higher segregation reduces competitiveness by creating more homogeneous districts.
# \\end{minipage}
# \\end{table}")
# 
# # Print the LaTeX code
# cat(latex_code)



## Correct?
# district-level metrics
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

# map-level metrics
map_summary = df %>%
  mutate(
    dem_win_margin = (total_dem_votes - total_rep_votes) / total_population,
    dem_won = dem_win_margin > 0, 
    maj_min = (total_minority / total_population) > 0.5  # Using 0.5 for majority-minority
  ) %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    n_dem_seats = sum(dem_won),
    n_total_seats = n(),
    n_maj_min = sum(maj_min),
    .groups = "drop"
  ) %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    avg_dem_seats = mean(n_dem_seats),
    avg_maj_min = mean(n_maj_min),
    min_dem_seats = min(n_dem_seats),
    max_dem_seats = max(n_dem_seats),
    .groups = "drop"
  )

# Calculate waste disparity across all plans in each scenario, not average of district-level waste disparities 
scenario_summary = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    minority_waste_rate = sum(minority_wasted_total) / sum(total_minority) * 100,
    majority_waste_rate = sum(majority_wasted_total) / sum(total_majority) * 100,
    # Population-weighted disparity
    pop_weighted_waste_disparity = minority_waste_rate - majority_waste_rate,
    .groups = "drop"
  )

# Merge summaries
full_summary = district_summary %>%
  left_join(map_summary, by = c("seg_level", "redist_level")) %>%
  left_join(scenario_summary, by = c("seg_level", "redist_level")) %>%
  # Reorder seg_level factor to go Low -> Medium -> High
  mutate(seg_level = factor(seg_level, levels = c("Low", "Medium", "High"))) %>%
  arrange(seg_level, redist_level)

# Create more compact LaTeX table with proper multicolumn
latex_code = "\\begin{table}[!htbp]
\\centering
\\footnotesize 
\\setlength{\\tabcolsep}{3pt} 
\\caption{Electoral Competitiveness Across Segregation and Redistricting Scenarios}
\\label{tab:electoral_competitiveness}
\\begin{tabular}{@{}llrrrrrrrrr@{}} 
\\toprule
& & 
\\cmidrule(lr){3-7} \\cmidrule(lr){8-10} \\cmidrule(lr){11-11}
Seg. & Redist. & N & Comp. & SD & Safe D & Safe R & D Seats & Min-Max & Maj-Min & Waste \\\\
& & & (\\%) & (\\%) & (\\%) & (\\%) & (Avg) & D Seats & Dists & Disp. (pp) \\\\
\\midrule
"

# Add data rows
for(i in 1:nrow(full_summary)) {
  row = full_summary[i,]
  
  # Determine segregation label (only for first row of each segregation group)
  if(i %in% c(1, 4, 7)) {
    seg_label = as.character(row$seg_level)
  } else {
    seg_label = ""
  }
  
  # Format the row - now using pop_weighted_waste_disparity
  row_text = sprintf("%s & %s & %d & %.1f & %.1f & %.1f & %.1f & %.1f & %d-%d & %.1f & %.1f \\\\",
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
                     row$pop_weighted_waste_disparity)  # Changed this line
  
  latex_code = paste0(latex_code, row_text, "\n")
  
  # Add spacing between segregation groups
  if(i %in% c(3, 6)) {
    latex_code = paste0(latex_code, "\\addlinespace\n")
  }
}

# Complete the table
latex_code = paste0(latex_code, "\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.2cm}
\\scriptsize
\\textit{Note:} N = number of districts; Comp. = competitive districts (margin $<$ 5\\%); 
SD = standard deviation of win margins; Safe D/R = safe Democratic/Republican 
districts (margin $>$ 5\\%); D Seats (Avg) = average Democratic seats per map; 
Min-Max D Seats = minimum and maximum Democratic seats across all maps in scenario;
Maj-Min Dists = average number of majority-minority districts per map;
Waste Disp. = waste disparity in percentage points (minority waste rate minus majority waste rate), 
calculated as the population-weighted difference in waste rates. 
Higher segregation reduces competitiveness by creating more homogeneous districts.
\\end{minipage}
\\end{table}")

# Print the LaTeX code
cat(latex_code)









# Table with model and interaction model
reg_data = df %>%
  mutate(
    # Create error variables
    error_minority_er = abs(er_minority_resid * 100),
    error_minority_ei = abs(ei_minority_resid * 100),
    
    # Create dummy variables
    seg_medium = as.numeric(seg_level == "Medium"),
    seg_high = as.numeric(seg_level == "High"),
    redis_dem = as.numeric(redist_level == "Dem"),
    redis_rep = as.numeric(redist_level == "Rep"),
    
    # Margin
    dem_margin = (total_dem_votes - total_rep_votes) / total_population * 100,
    
    # District White pop % - State White pop %
    white_pop_diff = ((total_majority / total_population) - (752332 / 1003109)) * 100
  )

# Run separate regressions for ER and EI
fit_base_er = lm(
  error_minority_er ~ seg_medium + seg_high + redis_dem + redis_rep + dem_margin + white_pop_diff,
  data = reg_data
)
fit_base_ei = lm(
  error_minority_ei ~ seg_medium + seg_high + redis_dem + redis_rep + dem_margin + white_pop_diff,
  data = reg_data
)

fit_int_er = lm(
  error_minority_er ~ seg_medium + seg_high + redis_dem + redis_rep 
  + seg_medium:redis_dem + seg_medium:redis_rep + 
    + seg_high:redis_dem + seg_high:redis_rep 
  + dem_margin + white_pop_diff,
  data = reg_data
)
fit_int_ei = lm(
  error_minority_ei ~ seg_medium + seg_high + redis_dem + redis_rep 
  + seg_medium:redis_dem + seg_medium:redis_rep + 
    + seg_high:redis_dem + seg_high:redis_rep 
  + dem_margin + white_pop_diff,
  data = reg_data
)

fit_int_2_er = lm(
  error_minority_er ~ seg_medium + seg_high + redis_dem + redis_rep 
  + seg_medium:redis_dem + seg_medium:redis_rep + 
    + seg_high:redis_dem + seg_high:redis_rep 
  + dem_margin * white_pop_diff,
  data = reg_data
)
fit_int_2_ei = lm(
  error_minority_ei ~ seg_medium + seg_high + redis_dem + redis_rep 
  + seg_medium:redis_dem + seg_medium:redis_rep + 
    + seg_high:redis_dem + seg_high:redis_rep 
  + dem_margin*white_pop_diff,
  data = reg_data
)

# ==============================================================================
#                          REGRESSION RESULTS TABLE
# ==============================================================================

# Extract coefficients and standard errors from all models
coef_base_er = coef(fit_base_er)
coef_base_ei = coef(fit_base_ei)
coef_int_er = coef(fit_int_er)
coef_int_ei = coef(fit_int_ei)
coef_int_2_er = coef(fit_int_2_er)
coef_int_2_ei = coef(fit_int_2_ei)

# Get standard errors
se_base_er = summary(fit_base_er)$coefficients[, "Std. Error"]
se_base_ei = summary(fit_base_ei)$coefficients[, "Std. Error"]
se_int_er = summary(fit_int_er)$coefficients[, "Std. Error"]
se_int_ei = summary(fit_int_ei)$coefficients[, "Std. Error"]
se_int_2_er = summary(fit_int_2_er)$coefficients[, "Std. Error"]
se_int_2_ei = summary(fit_int_2_ei)$coefficients[, "Std. Error"]

# Get p-values for significance stars
pval_base_er = summary(fit_base_er)$coefficients[, "Pr(>|t|)"]
pval_base_ei = summary(fit_base_ei)$coefficients[, "Pr(>|t|)"]
pval_int_er = summary(fit_int_er)$coefficients[, "Pr(>|t|)"]
pval_int_ei = summary(fit_int_ei)$coefficients[, "Pr(>|t|)"]
pval_int_2_er = summary(fit_int_2_er)$coefficients[, "Pr(>|t|)"]
pval_int_2_ei = summary(fit_int_2_ei)$coefficients[, "Pr(>|t|)"]

# Function to add significance stars
add_stars = function(coef, pval) {
  stars = ""
  if (!is.na(pval)) {
    if (pval < 0.01) stars = "***"
    else if (pval < 0.05) stars = "**"
    else if (pval < 0.10) stars = "*"
  }
  return(paste0(sprintf("%.3f", coef), stars))
}

# Get model statistics
n_obs = nobs(fit_base_er)  # Should be same for all models
r2_base_er = summary(fit_base_er)$r.squared
r2_base_ei = summary(fit_base_ei)$r.squared
r2_int_er = summary(fit_int_er)$r.squared
r2_int_ei = summary(fit_int_ei)$r.squared
r2_int_2_er = summary(fit_int_2_er)$r.squared
r2_int_2_ei = summary(fit_int_2_ei)$r.squared
adj_r2_base_er = summary(fit_base_er)$adj.r.squared
adj_r2_base_ei = summary(fit_base_ei)$adj.r.squared
adj_r2_int_er = summary(fit_int_er)$adj.r.squared
adj_r2_int_ei = summary(fit_int_ei)$adj.r.squared
adj_r2_int_2_er = summary(fit_int_2_er)$adj.r.squared
adj_r2_int_2_ei = summary(fit_int_2_ei)$adj.r.squared

# Helper function to get coefficient with stars or empty string if not in model
get_coef_star = function(coef_vec, se_vec, pval_vec, var_name) {
  if (var_name %in% names(coef_vec)) {
    return(add_stars(coef_vec[var_name], pval_vec[var_name]))
  } else {
    return("")
  }
}

# Helper function to get SE or empty string if not in model
get_se = function(se_vec, var_name) {
  if (var_name %in% names(se_vec)) {
    return(sprintf("(%.3f)", se_vec[var_name]))
  } else {
    return("")
  }
}

# Generate LaTeX table
latex_table = "\\begin{table}[h]
\\centering
\\small 
\\setlength{\\tabcolsep}{3pt} 
\\caption{Predicting ER/EI Model Error}
\\label{table:prediction_errors}
\\begin{tabular}{lcccccc}
\\toprule
& \\multicolumn{2}{c}{Base Model} & \\multicolumn{2}{c}{Interaction Model 1} & \\multicolumn{2}{c}{Interaction Model 2} \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5} \\cmidrule(lr){6-7}
& ER & EI & ER & EI & ER & EI \\\\
\\midrule
\\textit{Baseline (Low, Neutral)} & & & & & & \\\\"

# Add Intercept as Baseline
latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "(Intercept)"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "(Intercept)"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "(Intercept)"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "(Intercept)"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "(Intercept)"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "(Intercept)"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "(Intercept)"), " & ",
                     get_se(se_base_ei, "(Intercept)"), " & ",
                     get_se(se_int_er, "(Intercept)"), " & ",
                     get_se(se_int_ei, "(Intercept)"), " & ",
                     get_se(se_int_2_er, "(Intercept)"), " & ",
                     get_se(se_int_2_ei, "(Intercept)"), " \\\\")

# Add Segregation Effects section
latex_table = paste0(latex_table, "\n\\midrule\n\\textit{Segregation Effects} & & & & & & \\\\")

# Add Medium segregation
latex_table = paste0(latex_table, "\n",
                     "Medium & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "seg_medium"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "seg_medium"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "seg_medium"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "seg_medium"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "seg_medium"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "seg_medium"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "seg_medium"), " & ",
                     get_se(se_base_ei, "seg_medium"), " & ",
                     get_se(se_int_er, "seg_medium"), " & ",
                     get_se(se_int_ei, "seg_medium"), " & ",
                     get_se(se_int_2_er, "seg_medium"), " & ",
                     get_se(se_int_2_ei, "seg_medium"), " \\\\")

# Add High segregation
latex_table = paste0(latex_table, "\n",
                     "High & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "seg_high"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "seg_high"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "seg_high"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "seg_high"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "seg_high"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "seg_high"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "seg_high"), " & ",
                     get_se(se_base_ei, "seg_high"), " & ",
                     get_se(se_int_er, "seg_high"), " & ",
                     get_se(se_int_ei, "seg_high"), " & ",
                     get_se(se_int_2_er, "seg_high"), " & ",
                     get_se(se_int_2_ei, "seg_high"), " \\\\")

# Add Redistricting Effects section
latex_table = paste0(latex_table, "\n\\midrule\n\\textit{Redistricting Effects} & & & & & & \\\\")

# Add Democratic redistricting
latex_table = paste0(latex_table, "\n",
                     "Democratic & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "redis_dem"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "redis_dem"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "redis_dem"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "redis_dem"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "redis_dem"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "redis_dem"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "redis_dem"), " & ",
                     get_se(se_base_ei, "redis_dem"), " & ",
                     get_se(se_int_er, "redis_dem"), " & ",
                     get_se(se_int_ei, "redis_dem"), " & ",
                     get_se(se_int_2_er, "redis_dem"), " & ",
                     get_se(se_int_2_ei, "redis_dem"), " \\\\")

# Add Republican redistricting
latex_table = paste0(latex_table, "\n",
                     "Republican & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "redis_rep"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "redis_rep"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "redis_rep"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "redis_rep"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "redis_rep"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "redis_rep"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "redis_rep"), " & ",
                     get_se(se_base_ei, "redis_rep"), " & ",
                     get_se(se_int_er, "redis_rep"), " & ",
                     get_se(se_int_ei, "redis_rep"), " & ",
                     get_se(se_int_2_er, "redis_rep"), " & ",
                     get_se(se_int_2_ei, "redis_rep"), " \\\\")

# Add Interactions section
latex_table = paste0(latex_table, "\n\\midrule\n\\textit{Interactions} & & & & & & \\\\")

# Add Medium × Dem interaction
latex_table = paste0(latex_table, "\n",
                     "Medium $\\times$ Dem & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "seg_medium:redis_dem"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "seg_medium:redis_dem"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "seg_medium:redis_dem"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "seg_medium:redis_dem"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "seg_medium:redis_dem"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "seg_medium:redis_dem"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "seg_medium:redis_dem"), " & ",
                     get_se(se_base_ei, "seg_medium:redis_dem"), " & ",
                     get_se(se_int_er, "seg_medium:redis_dem"), " & ",
                     get_se(se_int_ei, "seg_medium:redis_dem"), " & ",
                     get_se(se_int_2_er, "seg_medium:redis_dem"), " & ",
                     get_se(se_int_2_ei, "seg_medium:redis_dem"), " \\\\")

# Add Medium × Rep interaction
latex_table = paste0(latex_table, "\n",
                     "Medium $\\times$ Rep & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "seg_medium:redis_rep"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "seg_medium:redis_rep"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "seg_medium:redis_rep"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "seg_medium:redis_rep"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "seg_medium:redis_rep"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "seg_medium:redis_rep"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "seg_medium:redis_rep"), " & ",
                     get_se(se_base_ei, "seg_medium:redis_rep"), " & ",
                     get_se(se_int_er, "seg_medium:redis_rep"), " & ",
                     get_se(se_int_ei, "seg_medium:redis_rep"), " & ",
                     get_se(se_int_2_er, "seg_medium:redis_rep"), " & ",
                     get_se(se_int_2_ei, "seg_medium:redis_rep"), " \\\\")

# Add High × Dem interaction
latex_table = paste0(latex_table, "\n",
                     "High $\\times$ Dem & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "seg_high:redis_dem"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "seg_high:redis_dem"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "seg_high:redis_dem"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "seg_high:redis_dem"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "seg_high:redis_dem"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "seg_high:redis_dem"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "seg_high:redis_dem"), " & ",
                     get_se(se_base_ei, "seg_high:redis_dem"), " & ",
                     get_se(se_int_er, "seg_high:redis_dem"), " & ",
                     get_se(se_int_ei, "seg_high:redis_dem"), " & ",
                     get_se(se_int_2_er, "seg_high:redis_dem"), " & ",
                     get_se(se_int_2_ei, "seg_high:redis_dem"), " \\\\")

# Add High × Rep interaction
latex_table = paste0(latex_table, "\n",
                     "High $\\times$ Rep & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "seg_high:redis_rep"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "seg_high:redis_rep"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "seg_high:redis_rep"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "seg_high:redis_rep"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "seg_high:redis_rep"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "seg_high:redis_rep"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "seg_high:redis_rep"), " & ",
                     get_se(se_base_ei, "seg_high:redis_rep"), " & ",
                     get_se(se_int_er, "seg_high:redis_rep"), " & ",
                     get_se(se_int_ei, "seg_high:redis_rep"), " & ",
                     get_se(se_int_2_er, "seg_high:redis_rep"), " & ",
                     get_se(se_int_2_ei, "seg_high:redis_rep"), " \\\\")

# Add Dem Margin × White Pop Diff interaction (only for int_2 models)
latex_table = paste0(latex_table, "\n",
                     "Dem Margin $\\times$ White Pop Diff & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "dem_margin:white_pop_diff"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "dem_margin:white_pop_diff"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "dem_margin:white_pop_diff"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "dem_margin:white_pop_diff"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "dem_margin:white_pop_diff"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "dem_margin:white_pop_diff"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "dem_margin:white_pop_diff"), " & ",
                     get_se(se_base_ei, "dem_margin:white_pop_diff"), " & ",
                     get_se(se_int_er, "dem_margin:white_pop_diff"), " & ",
                     get_se(se_int_ei, "dem_margin:white_pop_diff"), " & ",
                     get_se(se_int_2_er, "dem_margin:white_pop_diff"), " & ",
                     get_se(se_int_2_ei, "dem_margin:white_pop_diff"), " \\\\")

# Add District Characteristics section
latex_table = paste0(latex_table, "\n\\midrule\n\\textit{District Characteristics} & & & & & & \\\\")

# Add Dem Margin
latex_table = paste0(latex_table, "\n",
                     "Dem Margin & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "dem_margin"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "dem_margin"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "dem_margin"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "dem_margin"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "dem_margin"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "dem_margin"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "dem_margin"), " & ",
                     get_se(se_base_ei, "dem_margin"), " & ",
                     get_se(se_int_er, "dem_margin"), " & ",
                     get_se(se_int_ei, "dem_margin"), " & ",
                     get_se(se_int_2_er, "dem_margin"), " & ",
                     get_se(se_int_2_ei, "dem_margin"), " \\\\")

# Add White Pop Diff
latex_table = paste0(latex_table, "\n",
                     "White Pop Diff & ", 
                     get_coef_star(coef_base_er, se_base_er, pval_base_er, "white_pop_diff"), " & ",
                     get_coef_star(coef_base_ei, se_base_ei, pval_base_ei, "white_pop_diff"), " & ",
                     get_coef_star(coef_int_er, se_int_er, pval_int_er, "white_pop_diff"), " & ",
                     get_coef_star(coef_int_ei, se_int_ei, pval_int_ei, "white_pop_diff"), " & ",
                     get_coef_star(coef_int_2_er, se_int_2_er, pval_int_2_er, "white_pop_diff"), " & ",
                     get_coef_star(coef_int_2_ei, se_int_2_ei, pval_int_2_ei, "white_pop_diff"), " \\\\")

latex_table = paste0(latex_table, "\n",
                     " & ", 
                     get_se(se_base_er, "white_pop_diff"), " & ",
                     get_se(se_base_ei, "white_pop_diff"), " & ",
                     get_se(se_int_er, "white_pop_diff"), " & ",
                     get_se(se_int_ei, "white_pop_diff"), " & ",
                     get_se(se_int_2_er, "white_pop_diff"), " & ",
                     get_se(se_int_2_ei, "white_pop_diff"), " \\\\")

# Add model statistics
latex_table = paste0(latex_table, "\n\\midrule")

latex_table = paste0(latex_table, "\n",
                     "Observations & ", n_obs, " & ", n_obs, " & ", n_obs, " & ", n_obs, " & ", n_obs, " & ", n_obs, " \\\\")

latex_table = paste0(latex_table, "\n",
                     "$R^2$ & ", sprintf("%.3f", r2_base_er), " & ", sprintf("%.3f", r2_base_ei), 
                     " & ", sprintf("%.3f", r2_int_er), " & ", sprintf("%.3f", r2_int_ei),
                     " & ", sprintf("%.3f", r2_int_2_er), " & ", sprintf("%.3f", r2_int_2_ei), " \\\\")

latex_table = paste0(latex_table, "\n",
                     "Adjusted $R^2$ & ", sprintf("%.3f", adj_r2_base_er), " & ", sprintf("%.3f", adj_r2_base_ei), 
                     " & ", sprintf("%.3f", adj_r2_int_er), " & ", sprintf("%.3f", adj_r2_int_ei),
                     " & ", sprintf("%.3f", adj_r2_int_2_er), " & ", sprintf("%.3f", adj_r2_int_2_ei), " \\\\")

# Close table
latex_table = paste0(latex_table, "\n\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.5cm}
\\footnotesize
\\textit{Note:} The dependent variable in all models is the absolute prediction error for minority vote share from ER or EI. The first panel shows baseline errors in the reference category of Low Segregation and Neutral Gerrymanders. IID standard errors are shown in parentheses. Significance levels: $^{*}p<0.10$, $^{**}p<0.05$, $^{***}p<0.01$. Note the sign flip on the coefficient for Dem. Margin between ER and EI. All else equal, ER does worse in heavily democratic districts while EI does better. The third model includes an interaction between Dem Margin and White Pop Diff to explore whether the effects of district characteristics vary together.
\\end{minipage}
\\end{table}")

# Print the LaTeX code
cat(latex_table)











# precinct level er/ei table

rm(list = ls())

# Set directories
setwd('~/Dropbox/RPV/Code/Simulation/Workflow/OLD/POPULATION_DENSITY/')
fig_dir = '~/Dropbox/RPV/Figures/Preliminary Analysis/TC/Precinct Figures/'

# Load the cleaned precinct-level results
precinct_data = readRDS('ER_EI_precinct_results_cleaned.rds')
precinct_data$district_id = precinct_data$district_id + 1
precinct_data$unique_id = paste0(precinct_data$map_id, '_', precinct_data$district_id, '_', precinct_data$precinct_id, '_', precinct_data$seg_level, '_', precinct_data$redist_level)


load('sy_er_ei_performance_precinct_summaries.RDA')


# Add correctly calculated precinct-level ei estimates
precinct_data = precinct_data %>%
  left_join(
    partial_results %>% 
      select(plan, scenario, district_id, precinct_id, ei.beta.w, ei.beta.b),
    by = c("map_id" = "plan",
           "seg_level" = "scenario",
           "district_id" = "district_id", 
           "precinct_id" = "precinct_id")
  ) %>%
  mutate(ei_minority_dem_share = ei.beta.b,
         ei_majority_dem_share = ei.beta.w, 
         ei_pred_minority_share_precinct = ei.beta.b,
         ei_pred_majority_share_precinct = ei.beta.w,
         er_pred_minority_share_precinct = er_pred_minority_share_precinct,
         er_pred_majority_share_precinct = er_pred_majority_share_precinct,
         true_minority_dem_share_precinct = true_minority_dem_share_precinct,
         true_majority_dem_share_precinct = true_majority_dem_share_precinct)


# Order scenarios as factors
precinct_data$seg_level = factor(precinct_data$seg_level, 
                                 levels = c('low', 'medium', 'high'), 
                                 labels = c('Low', 'Medium', 'High'))
precinct_data$redist_level = factor(precinct_data$redist_level, 
                                    levels = c('dem', 'middle', 'rep'), 
                                    labels = c('Dem', 'Neutral', 'Rep'))


# Add map-level democratic seatshare
precinct_data = precinct_data %>%
  group_by(map_id, seg_level, redist_level) %>%
  mutate(n_dem_seats = sum(prop_dem > 0.5),
         n_rep_seats = sum(prop_dem < 0.5)) 

# Filter to precincts with > 0 minority share
precinct_data = precinct_data %>% filter(!is.na(true_minority_dem_share_precinct) & true_minority_dem_share_precinct > 0)

# Create the summary tables
er_tab = precinct_data %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean(((er_pred_minority_share_precinct - true_minority_dem_share_precinct) * 100)^2, na.rm = TRUE)),
    rmse_maj = sqrt(mean(((er_pred_majority_share_precinct - true_majority_dem_share_precinct) * 100)^2, na.rm = TRUE)), 
    bias_min = mean((er_pred_minority_share_precinct - true_minority_dem_share_precinct) * 100, na.rm = TRUE),
    bias_maj = mean((er_pred_majority_share_precinct - true_majority_dem_share_precinct) * 100, na.rm = TRUE),
    mae_min = mean(abs(er_pred_minority_share_precinct - true_minority_dem_share_precinct) * 100, na.rm = TRUE),
    mae_maj = mean(abs(er_pred_majority_share_precinct - true_majority_dem_share_precinct) * 100, na.rm = TRUE),
    true_rpv = mean(true_rpv_precinct, na.rm = TRUE) * 100,
    pct_correct_rpv = mean(er_rpv_correct_precinct, na.rm = TRUE) * 100,
    pct_under = mean(er_pred_minority_share_precinct < true_minority_dem_share_precinct, na.rm = TRUE) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share_precinct - true_minority_dem_share_precinct) != sign(er_pred_majority_share_precinct - er_pred_minority_share_precinct), na.rm = TRUE) * 100
  ) %>%
  ungroup()

ei_tab = precinct_data %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    rmse_min = sqrt(mean(((ei_pred_minority_share_precinct - true_minority_dem_share_precinct) * 100)^2, na.rm = TRUE)),
    rmse_maj = sqrt(mean(((ei_pred_majority_share_precinct - true_majority_dem_share_precinct) * 100)^2, na.rm = TRUE)), 
    bias_min = mean((ei_pred_minority_share_precinct - true_minority_dem_share_precinct) * 100, na.rm = TRUE),
    bias_maj = mean((ei_pred_majority_share_precinct - true_majority_dem_share_precinct) * 100, na.rm = TRUE),
    mae_min = mean(abs(ei_pred_minority_share_precinct - true_minority_dem_share_precinct) * 100, na.rm = TRUE),
    mae_maj = mean(abs(ei_pred_majority_share_precinct - true_majority_dem_share_precinct) * 100, na.rm = TRUE),
    true_rpv = mean(true_rpv_precinct, na.rm = TRUE) * 100,
    pct_correct_rpv = mean(ei_rpv_correct_precinct, na.rm = TRUE) * 100,
    pct_under = mean(er_pred_minority_share_precinct < true_minority_dem_share_precinct, na.rm = TRUE) * 100,
    pct_sign_flips = mean(sign(true_majority_dem_share_precinct - true_minority_dem_share_precinct) != sign(er_pred_majority_share_precinct - er_pred_minority_share_precinct), na.rm = TRUE) * 100
  ) %>%
  ungroup()

# Format the tables
format_table = function(tab) {
  tab %>%
    mutate(
      across(where(is.numeric), ~sprintf("%.1f", .))
    ) %>%
    arrange(seg_level, redist_level)
}

er_fmt = format_table(er_tab)
ei_fmt = format_table(ei_tab)

latex_code = "\\begin{table}[!htbp]
\\centering
\\small 
\\setlength{\\tabcolsep}{3pt} 
\\caption{Precinct-Level erformance Comparison of Ecological Regression (ER) and Ecological Inference (EI)}
\\label{tab:er_ei_comparison_precinct}
\\begin{tabular}{@{}llcccccccc@{}} 
\\toprule
& & \\multicolumn{2}{c}{RMSE} & \\multicolumn{2}{c}{Bias} & \\multicolumn{4}{c}{Classification} \\\\
\\cmidrule(lr){3-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-10}
Segregation & Redistricting & Min. & Maj. & Min. & Maj. & True RPV & RPV Correct & Sign Flips & \\% Under \\\\
\\midrule
\\multicolumn{9}{l}{\\textit{Panel A: Ecological Regression (ER)}} \\\\ \\\\
"

# Add ER data rows
for(i in 1:nrow(er_fmt)) {
  row = er_fmt[i,]
  
  # Determine segregation label (only for first row of each group)
  if(i %in% c(1, 4, 7)) {
    seg_label = row$seg_level
  } else {
    seg_label = ""
  }
  
  # Clean redistricting label
  redist_label = row$redist_level
  
  # Add row with better spacing
  latex_code = paste0(latex_code,
                      seg_label, " & ", redist_label, " & ",
                      row$rmse_min, " & ", row$rmse_maj, " & ",
                      row$bias_min, " & ", row$bias_maj, " & ",
                      row$true_rpv, " & ", row$pct_correct_rpv, " & ",
                      row$pct_sign_flips, " & ", row$pct_under, " \\\\")
  
  latex_code = paste0(latex_code, "\n")
}

# Add midrule and EI section
latex_code = paste0(latex_code, "\\midrule
\\multicolumn{9}{l}{\\textit{Panel B: Ecological Inference (EI)}} \\\\ \\\\
")

# Add EI data rows
for(i in 1:nrow(ei_fmt)) {
  row = ei_fmt[i,]
  
  # Determine segregation label (only for first row of each group)
  if(i %in% c(1, 4, 7)) {
    seg_label = row$seg_level
  } else {
    seg_label = ""
  }
  
  # Clean redistricting label
  redist_label = row$redist_level
  
  # Add row
  latex_code = paste0(latex_code,
                      seg_label, " & ", redist_label, " & ",
                      row$rmse_min, " & ", row$rmse_maj, " & ",
                      row$bias_min, " & ", row$bias_maj, " & ",
                      row$true_rpv, " & ", row$pct_correct_rpv, " & ",
                      row$pct_sign_flips, " & ", row$pct_under, " \\\\")
  
  latex_code = paste0(latex_code, "\n")
}

# Complete the table with bottomrule and notes
latex_code = paste0(latex_code, "\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.5cm}
\\footnotesize
\\textit{Note:} Performance metrics for ecological regression (ER) and ecological inference (EI) across different levels of segregation (Low, Medium, High) and redistricting scenarios (Democratic-favoring, Neutral, Republican-favoring). RMSE = Root Mean Square Error; MAE = Mean Absolute Error; RPV = Racially Polarized Voting. Min. and Maj. refer to minority and majority groups, respectively. True RPV shows the actual proportion of racially polarized voting in the data. RPV Correct indicates the proportion of correctly identified RPV cases. Sign Flips shows the proportion of cases where the estimated difference between majority and minority Democratic vote share has the opposite sign from the true difference. \\% Under shows the proportion of cases where ER or EI under-predict district-level minority voteshares for the Democratic candidate. 
\\end{minipage}
\\end{table}")

# Print the LaTeX code
cat(latex_code)

# Save to file
writeLines(latex_code, "~/Dropbox/RPV/Figures/Preliminary Analysis/TC/Precinct Figures/er_ei_comparison_table.tex")












#  table with six columns: Pr(Represented by Copartisan), Pr(Reside in Competitive District) Pr(Wasted) by race
# with rows indicating segregation and redistricting level
summary_table = df %>%
  mutate(
    # Calculate copartisan representation
    minority_dem_copartisan = true_minority_dem_votes * dem_district,
    minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (1 - dem_district),
    minority_copartisan = minority_dem_copartisan + minority_rep_copartisan,
    
    majority_dem_copartisan = true_majority_dem_votes * dem_district,
    majority_rep_copartisan = (total_majority - true_majority_dem_votes) * (1 - dem_district),
    majority_copartisan = majority_dem_copartisan + majority_rep_copartisan,
    
    # Calculate competitive districts
    dem_margin = (total_dem_votes - total_rep_votes) / total_population,
    is_competitive = abs(dem_margin) < 0.05,
    
    # Calculate minority and majority in competitive districts
    minority_in_competitive = total_minority * is_competitive,
    majority_in_competitive = total_majority * is_competitive
  ) %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    # Pr(Represented by Copartisan)
    pr_minority_copartisan = sum(minority_copartisan) / sum(total_minority) * 100,
    pr_majority_copartisan = sum(majority_copartisan) / sum(total_majority) * 100,
    
    # Pr(Reside in Competitive District)
    pr_minority_competitive = sum(minority_in_competitive) / sum(total_minority) * 100,
    pr_majority_competitive = sum(majority_in_competitive) / sum(total_majority) * 100,
    
    # Pr(Wasted) - using pre-computed variables
    pr_minority_wasted = sum(minority_wasted_total) / sum(total_minority) * 100,
    pr_majority_wasted = sum(majority_wasted_total) / sum(total_majority) * 100,
    
    .groups = 'drop'
  ) %>%
  # Ensure proper ordering
  mutate(seg_level = factor(seg_level, levels = c("Low", "Medium", "High"))) %>%
  arrange(seg_level, redist_level)

# Format the table for LaTeX
latex_table = "\\begin{table}[!htbp]
\\centering
\\small 
\\setlength{\\tabcolsep}{3pt} 
\\caption{Electoral Outcomes by Race Across Segregation and Redistricting Scenarios}
\\label{tab:electoral_outcomes_by_race}
\\begin{tabular}{@{}llcccccc@{}} 
\\toprule
& & \\multicolumn{2}{c}{Pr(Represented by} & \\multicolumn{2}{c}{Pr(Reside in} & \\multicolumn{2}{c}{Pr(Wasted)} \\\\
& & \\multicolumn{2}{c}{Copartisan)} & \\multicolumn{2}{c}{Competitive District)} & \\multicolumn{2}{c}{} \\\\
\\cmidrule(lr){3-4} \\cmidrule(lr){5-6} \\cmidrule(lr){7-8}
Segregation & Redistricting & Min. & Maj. & Min. & Maj. & Min. & Maj. \\\\
\\midrule
"

# Add data rows
for(i in 1:nrow(summary_table)) {
  row = summary_table[i,]
  
  # Determine segregation label (only for first row of each group)
  if(i %in% c(1, 4, 7)) {
    seg_label = as.character(row$seg_level)
  } else {
    seg_label = ""
  }
  
  # Format the row
  row_text = sprintf("%s & %s & %.1f & %.1f & %.1f & %.1f & %.1f & %.1f \\\\",
                     seg_label,
                     row$redist_level,
                     row$pr_minority_copartisan,
                     row$pr_majority_copartisan,
                     row$pr_minority_competitive,
                     row$pr_majority_competitive,
                     row$pr_minority_wasted,
                     row$pr_majority_wasted)
  
  latex_table = paste0(latex_table, row_text, "\n")
  
  # Add spacing between segregation groups
  if(i %in% c(3, 6)) {
    latex_table = paste0(latex_table, "\\addlinespace\n")
  }
}

# Complete the table
latex_table = paste0(latex_table, "\\bottomrule
\\end{tabular}
\\begin{minipage}{\\textwidth}
\\vspace{0.3cm}
\\footnotesize
\\textit{Note:} All values are percentages. Pr(Represented by Copartisan) shows the probability that a voter is represented by someone from their preferred party. Pr(Reside in Competitive District) shows the probability of living in a district where the Democratic vote share is within 5 percentage points of 50\\%. Pr(Wasted) shows the probability that a voter's vote is wasted (either packed or cracked). Results are shown across different segregation levels (Low, Medium, High) and redistricting scenarios (Dem = Democratic-favoring, Neutral, Rep = Republican-favoring).
\\end{minipage}
\\end{table}")

# Print the LaTeX code
cat(latex_table)

# Also print the summary table to see the values
print(summary_table)

# Save to file if needed
writeLines(latex_table, "~/Dropbox/RPV/Figures/electoral_outcomes_by_race_table.tex")


























df_error = df %>%
  mutate(
    error_minority_er = abs(er_minority_resid) * 100,
    error_minority_ei = abs(ei_minority_resid) * 100,
    error_majority_er = abs(er_majority_resid) * 100,
    error_majority_ei = abs(ei_majority_resid) * 100,
    seg_medium = as.numeric(seg_level == "Medium"),
    seg_high = as.numeric(seg_level == "High"),
    redis_dem = as.numeric(redist_level == "Dem"),
    redis_rep = as.numeric(redist_level == "Rep"),
    
    # Margin
    dem_margin = (total_dem_votes - total_rep_votes) / total_population * 100,
    
    # District White pop % - State White pop %
    white_pop_diff = ((total_majority / total_population) - (752332 / 1003109)) * 100
  ) %>%
  select(
    seg_level, redist_level, map_id, district_id, 
    seg_medium, seg_high, redis_rep, redis_dem,
    true_rpv, er_rpv, ei_rpv, 
    true_beta_b = true_minority_dem_share,
    true_beta_w = true_majority_dem_share,
    per_minority = prop_minority, 
    dem_voteshare = prop_dem,
    dem_margin, white_pop_diff,
    error_minority_er, error_minority_ei,
    error_majority_er, error_majority_ei,
    er_rpv_correct, ei_rpv_correct
    ) %>%
  mutate(
    er_true_pos = 1 * (true_rpv == 1 & er_rpv == 1), 
    er_false_pos = 1 * (true_rpv == 0 & er_rpv == 1),
    er_true_neg = 1 * (true_rpv == 0 & er_rpv == 0),
    er_false_neg = 1 * (true_rpv == 1 & er_rpv == 0),
    ei_true_pos = 1 * (true_rpv == 1 & ei_rpv == 1), 
    ei_false_pos = 1 * (true_rpv == 0 & ei_rpv == 1),
    ei_true_neg = 1 * (true_rpv == 0 & ei_rpv == 0),
    ei_false_neg = 1 * (true_rpv == 1 & ei_rpv == 0)
    ) 



# Run separate regressions for ER and EI
err_min_er = lm(error_minority_er ~ seg_medium + seg_high 
                + redis_dem + redis_rep 
                + dem_margin + white_pop_diff
                + true_rpv, 
                data = df_error)

err_min_ei = lm(error_minority_ei ~ seg_medium + seg_high 
                + redis_dem + redis_rep 
                + dem_margin + white_pop_diff
                + true_rpv, 
                data = df_error)

err_maj_er = lm(error_majority_er ~ seg_medium + seg_high 
                + redis_dem + redis_rep 
                + dem_margin + white_pop_diff
                + true_rpv, 
                data = df_error)

err_maj_ei = lm(error_majority_ei ~ seg_medium + seg_high 
                + redis_dem + redis_rep 
                + dem_margin + white_pop_diff
                + true_rpv, 
                data = df_error)

class_er = glm(er_rpv_correct ~ seg_medium + seg_high 
               + redis_dem + redis_rep 
               + dem_margin + white_pop_diff
               + true_rpv, 
               family = binomial('logit'),
               data = df_error)

class_ei = glm(ei_rpv_correct ~ seg_medium + seg_high 
               + redis_dem + redis_rep 
               + dem_margin + white_pop_diff
               + true_rpv, 
               family = binomial('logit'),
               data = df_error)


stargazer::stargazer(class_er, class_ei, type = 'text')


df_error %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    er_true_pos = sum(er_true_pos) / n(),
    er_false_pos = sum(er_false_pos) / n(),
    er_true_neg = sum(er_true_neg) / n(),
    er_false_neg = sum(er_false_neg) / n(),
    ei_true_pos = sum(ei_true_pos) / n(),
    ei_false_pos = sum(ei_false_pos) / n(),
    ei_true_neg = sum(ei_true_neg) / n(),
    ei_false_neg = sum(ei_false_neg) / n(),
  ) 





# Run logit classification models
class_er = glm(er_rpv_correct ~ seg_medium + seg_high 
               + redis_dem + redis_rep 
               + dem_margin + white_pop_diff,
               # + true_rpv, 
               family = binomial('logit'),
               data = df_error)

class_ei = glm(ei_rpv_correct ~ seg_medium + seg_high 
               + redis_dem + redis_rep 
               + dem_margin + white_pop_diff,
               # + true_rpv, 
               family = binomial('logit'),
               data = df_error)

# Display results
stargazer::stargazer(class_er, class_ei, type = 'text')

# Extract coefficients with confidence intervals
coef_class_er = tidy(class_er, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(method = "ER")

coef_class_ei = tidy(class_ei, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(method = "EI")

# Combine coefficients
coef_class_df = bind_rows(coef_class_er, coef_class_ei) %>%
  mutate(
    term_clean = case_when(
      term == "seg_medium" ~ "Medium\nSegregation",
      term == "seg_high" ~ "High\nSegregation", 
      term == "redis_dem" ~ "Democratic\nGerrymander",
      term == "redis_rep" ~ "Republican\nGerrymander",
      term == 'dem_margin' ~ "Dem. Margin",
      term == 'white_pop_diff' ~ "District % White\n- State % White" #,
      # term == 'true_rpv' ~ "True RPV"
    )
  )

# Add reference category (baseline log-odds = 0)
reference_class_df = data.frame(
  term = "reference",
  estimate = 0,
  std.error = 0,
  statistic = NA,
  p.value = NA,
  conf.low = 0,
  conf.high = 0,
  method = c("ER", "EI"),
  term_clean = "Low Segregation,\nNeutral Gerrymander"
)

coef_class_df = bind_rows(reference_class_df, coef_class_df)

# Set factor levels for ordering
coef_class_df$term_clean = factor(coef_class_df$term_clean, 
                                  levels = c("True RPV",
                                             "District % White\n- State % White",
                                             "Dem. Margin",
                                             "High\nSegregation",
                                             "Medium\nSegregation", 
                                             "Democratic\nGerrymander",
                                             "Republican\nGerrymander",
                                             "Low Segregation,\nNeutral Gerrymander"))

# Create the coefficient plot for logit models
fig_logit = ggplot(coef_class_df, aes(x = term_clean, y = estimate, color = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 6) +
  geom_errorbar(data = filter(coef_class_df, term != "reference"),
                aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.3), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  labs(x = "", 
       y = "Change in Log-Odds of Correct Classification",
       color = "Method") +
  scale_color_brewer(palette = 'Dark2') +
  theme_custom() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Display the plot
fig_logit

# Save the plot
ggsave(paste0(fig_dir, "New_07_09/logit_classification_coefficients.pdf"), 
       fig_logit, width = 12, height = 10)

# Convert log-odds to probability changes at the mean
# This shows the average marginal effects

library(margins)

# Calculate average marginal effects for ER
ame_er = margins(class_er) %>%
  summary() %>%
  mutate(method = "ER")

# Calculate average marginal effects for EI
ame_ei = margins(class_ei) %>%
  summary() %>%
  mutate(method = "EI")

# Combine AMEs
ame_df = bind_rows(ame_er, ame_ei) %>%
  mutate(
    term_clean = case_when(
      factor == "seg_medium" ~ "Medium\nSegregation",
      factor == "seg_high" ~ "High\nSegregation", 
      factor == "redis_dem" ~ "Democratic\nGerrymander",
      factor == "redis_rep" ~ "Republican\nGerrymander",
      factor == 'dem_margin' ~ "Dem. Margin",
      factor == 'white_pop_diff' ~ "District % White\n- State % White"
    )
  )

# Add reference category
reference_class_df = data.frame(
  factor = "reference",
  AME = 0,
  SE = 0,
  z = NA,
  p = NA,
  lower = 0,
  upper = 0,
  method = c("ER", "EI"),
  term_clean = "Low Segregation,\nNeutral Gerrymander"
)

ame_df = bind_rows(reference_class_df, ame_df)

# Set factor levels
ame_df$term_clean = factor(ame_df$term_clean, 
                           levels = c("District % White\n- State % White",
                                      "Dem. Margin",
                                      "High\nSegregation",
                                      "Medium\nSegregation", 
                                      "Democratic\nGerrymander",
                                      "Republican\nGerrymander",
                                      "Low Segregation,\nNeutral Gerrymander"))

# Create AME plot
fig_ame = ggplot(ame_df, aes(x = term_clean, y = AME, color = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 6) +
  geom_errorbar(aes(ymin = lower, ymax = upper), 
                position = position_dodge(width = 0.3), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  labs(x = "", 
       y = "Average Marginal Effect on Probability of Correct Classification",
       color = "Method") +
  scale_color_brewer(palette = 'Dark2') +
  theme_custom() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Display AME plot
fig_ame

# Save AME plot
ggsave(paste0(fig_dir, "New_07_09/logit_ame_classification.pdf"), 
       fig_ame, width = 12, height = 10)




