# Precinct-Level Analysis of ER/EI Accuracy
# This script analyzes how well ER and EI perform at the precinct level

# Packages
library(fixest)
library(xtable)
library(viridis)
library(patchwork)
library(ggridges)
library(tidyverse)

rm(list = ls())

# Set directories
setwd('~/Dropbox/RPV/Code/Simulation/Workflow/OLD/POPULATION_DENSITY/')
fig_dir = '~/Dropbox/RPV/Figures/Preliminary Analysis/TC/Precinct Figures/'

# Set sampling parameters
SAMPLE_SIZE = 50000  # Number of precinct-map combinations to sample for plotting
SAMPLE_SEED = 123    # For reproducibility

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
  mutate(ei_minority_dem_share = ei.beta.b * 100,
         ei_majority_dem_share = ei.beta.w * 100, 
         ei_pred_minority_share_precinct = ei.beta.b * 100,
         ei_pred_majority_share_precinct = ei.beta.w * 100,
         er_pred_minority_share_precinct = er_pred_minority_share_precinct * 100,
         er_pred_majority_share_precinct = er_pred_majority_share_precinct * 100,
         true_minority_dem_share_precinct = true_minority_dem_share_precinct * 100,
         true_majority_dem_share_precinct = true_majority_dem_share_precinct * 100)


# Order scenarios as factors
precinct_data$seg_level = factor(precinct_data$seg_level, 
                                 levels = rev(c('low', 'medium', 'high')), 
                                 labels = rev(c('Low', 'Medium', 'High')))
precinct_data$redist_level = factor(precinct_data$redist_level, 
                                    levels = c('dem', 'middle', 'rep'), 
                                    labels = c('Dem', 'Neutral', 'Rep'))

# Sample data if needed for plotting efficiency
set.seed(SAMPLE_SEED)
if(nrow(precinct_data) > SAMPLE_SIZE) {
  precinct_sample = precinct_data %>%
    sample_n(SAMPLE_SIZE)
} else {
  precinct_sample = precinct_data
}

# ggplot theme
theme_custom = function() {
  theme_bw() +
    theme(
      axis.text = element_text(size = 10),
      axis.title = element_text(size = 11),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 11),
      strip.text = element_text(size = 10), 
      strip.background = element_rect(fill = "grey95", color = NA),
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    ) 
  
}

# ------------------------- Figure 1 & 2 -------------------------------- 
# Raw ER/EI estimates vs truth for majority and minority voteshares

# Prepare data for scatter plots
scatter_data = precinct_sample %>%
  select(
    seg_level, redist_level, 
    n_minority, n_majority,
    population, prop_minority_precinct,
    prop_dem_precinct,
    true_minority_dem_share_precinct, true_majority_dem_share_precinct,
    er_pred_minority_share_precinct, er_pred_majority_share_precinct,
    ei_pred_minority_share_precinct, ei_pred_majority_share_precinct
  ) %>%
  filter(!is.na(true_minority_dem_share_precinct) & !is.na(true_majority_dem_share_precinct))

# Reshape for plotting
er_scatter = scatter_data %>%
  select(-starts_with("ei_")) %>%
  rename(estimate_minority = er_pred_minority_share_precinct,
         estimate_majority = er_pred_majority_share_precinct) %>%
  mutate(Model = "ER")

ei_scatter = scatter_data %>%
  select(-starts_with("er_")) %>%
  rename(estimate_minority = ei_pred_minority_share_precinct,
         estimate_majority = ei_pred_majority_share_precinct) %>%
  mutate(Model = "EI")

combined_scatter = bind_rows(er_scatter, ei_scatter)

# Minority scatter plot
fig_1 = ggplot(combined_scatter, 
               aes(x = true_minority_dem_share_precinct, y = estimate_minority, color = Model)) +
  geom_point(alpha = 0.1, size = 2, shape = 21) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_grid(seg_level ~ redist_level) +
  labs(
    x = "True Minority Democratic Share",
    y = "Estimated Minority Democratic Share",
    color = "Model"
  ) +
  theme_custom() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = 'Dark2') +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100))

fig_1

ggsave(
  paste0(fig_dir, "raw_voteshares_minority.pdf"),
  fig_1,
  width = 10,
  height = 8
)

# Majority scatter plot
fig_2 = ggplot(combined_scatter, 
               aes(x = true_majority_dem_share_precinct, y = estimate_majority, color = Model)) +
  geom_point(alpha = 0.1, size = 2, shape = 21) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_grid(seg_level ~ redist_level) +
  labs(
    x = "True Majority Democratic Share",
    y = "Estimated Majority Democratic Share",
    color = "Model"
  ) +
  theme_custom() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = 'Dark2') +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100))

fig_2

ggsave(
  paste0(fig_dir, "raw_voteshares_majority.pdf"),
  fig_2,
  width = 10,
  height = 8
)













# Pct rpv correct by scenario with per-minority
rpv_correct_df = precinct_data %>%
  select(map_id, district_id, n_dem_seats, prop_minority_precinct, er_rpv_correct_precinct, ei_rpv_correct_precinct, seg_level, redist_level) %>%
  pivot_longer(cols = c(er_rpv_correct_precinct, ei_rpv_correct_precinct), names_to = 'model', values_to = 'correct') %>%
  mutate(model = ifelse(model == 'er_rpv_correct_precinct', 'ER', 'EI')) %>%
  mutate(
    prop_minority_precinct = prop_minority_precinct * 100,
    per_minority_bin = cut(prop_minority_precinct, 
                           breaks = c(seq(0, 75, by = 5), Inf),
                           labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                      "25-30", "30-35", "35-40", "40-45", "45-50", 
                                      "50-55", "55-60", "60-65", "65-70", "70-75", "75+"),
                           right = FALSE)
  ) %>%
  filter(!is.na(per_minority_bin)) %>%
  group_by(seg_level, redist_level, per_minority_bin, model) %>% 
  summarise(
    pct_correct = mean(correct) * 100,
    n_obs = n(),
    .groups = 'drop'
  )

# Create the plot
rpv_correct_p1 = ggplot(rpv_correct_df,
                        aes(x = per_minority_bin, y = pct_correct, color = model, group = model)) +
  geom_line(size = 1) +
  geom_point(shape = 21, size = 2, fill = "white") +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Minority Population (%)", 
       y = "Percent Correct", 
       color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

rpv_correct_p1





























# ------------------------- Figure 3 -------------------------------- 
# RPV Prediction Accuracy vs Precinct-level percent minority

fig_3_df = precinct_sample %>%
  filter(!is.na(prop_minority_precinct) & !is.na(true_rpv_precinct)) %>%
  select(prop_minority_precinct, er_rpv_correct_precinct, ei_rpv_correct_precinct, 
         seg_level, redist_level) %>%
  pivot_longer(cols = c(er_rpv_correct_precinct, ei_rpv_correct_precinct), 
               names_to = 'measure', 
               values_to = 'correct') %>%
  mutate(measure = ifelse(measure == 'er_rpv_correct_precinct', 'ER', 'EI'))

fig_3 = ggplot(fig_3_df, aes(x = prop_minority_precinct, y = correct, 
                             color = measure, fill = measure)) +
  geom_point(position = position_jitter(height = 0.025, width = 0), 
             alpha = 0.75, size = 1, pch = 1) +
  geom_smooth(alpha = 0.5) +
  scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  scale_fill_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  labs(x = 'Precinct-Level Minority Share of Population', 
       y = 'RPV Correctly Predicted',
       color = 'Model', fill = 'Model') +
  facet_grid(seg_level ~ redist_level) + 
  theme_bw()

print(fig_3)

ggsave(
  paste0(fig_dir, "rpv_accuracy_by_minority_share.pdf"),
  fig_3,
  width = 10,
  height = 8
)

# ------------------------- Part A: Precinct Assignment Analysis -------------------------

# Figure A1: How precincts of different racial compositions end up in districts of varying racial compositions

fig_a1_data = precinct_data %>%
  filter(!is.na(prop_minority)) %>%
  mutate(
    precinct_minority_bin = cut(prop_minority_precinct, 
                                breaks = c(0, 0.1, 0.3, 0.5, 0.7, 0.9, 1),
                                labels = c("0-10%", "10-30%", "30-50%", 
                                           "50-70%", "70-90%", "90-100%"))
  ) %>%
  drop_na()

fig_a1 = ggplot(fig_a1_data, 
                aes(x = prop_minority,  # District-level minority share
                    y = factor(seg_level, levels = c('Low', 'Medium', 'High')),
                    fill = precinct_minority_bin, 
                    color = precinct_minority_bin)) +
  stat_binline(bins = 50, alpha = 0.5) +
  facet_wrap( ~ redist_level) +
  labs(
    x = "District-Level Minority Share",
    y = "Density",
    fill = "Precinct\nMinority %",
    color = "Precinct\nMinority %"
  ) +
  theme_minimal() +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  theme(legend.position = "bottom")

print(fig_a1)

ggsave(paste0(fig_dir, "precinct_to_district_composition.pdf"), 
       fig_a1, width = 10, height = 8)

# ------------------------- Context Gap and Model Error -------------------------

# Calculate contextual mismatch for each precinct
mismatch_error_data = precinct_data %>%
  filter(!is.na(er_minority_error_precinct) & !is.na(prop_minority)) %>%
  mutate(
    # Calculate contextual mismatch
    contextual_mismatch = abs(prop_minority_precinct - prop_minority),
    
    # Create mismatch bins for smoother visualization
    mismatch_bin = cut(contextual_mismatch, 
                       breaks = seq(0, 1, 0.05),
                       include.lowest = TRUE,
                       labels = seq(0.025, 0.975, 0.05))
  ) %>%
  # Calculate average errors by mismatch bin
  group_by(seg_level, redist_level, mismatch_bin) %>%
  summarize(
    # Mean errors
    mean_er_minority_error = mean(er_minority_error_precinct, na.rm = TRUE),
    mean_ei_minority_error = mean(ei_minority_error_precinct, na.rm = TRUE),
    mean_er_majority_error = mean(er_majority_error_precinct, na.rm = TRUE),
    mean_ei_majority_error = mean(ei_majority_error_precinct, na.rm = TRUE),
    
    # Standard errors
    se_er_minority = sd(er_minority_error_precinct, na.rm = TRUE) / sqrt(n()),
    se_ei_minority = sd(ei_minority_error_precinct, na.rm = TRUE) / sqrt(n()),
    
    # Sample size
    n_precincts = n(),
    
    .groups = "drop"
  ) %>%
  mutate(
    mismatch_bin = as.numeric(as.character(mismatch_bin))
  ) %>%
  filter(n_precincts >= 10)  # Only show bins with sufficient data

# Reshape for plotting 
mismatch_long = mismatch_error_data %>%
  select(seg_level, redist_level, mismatch_bin, n_precincts,
         ER = mean_er_minority_error, EI = mean_ei_minority_error,
         ER_se = se_er_minority, EI_se = se_ei_minority) %>%
  pivot_longer(cols = c(ER, EI), names_to = "Model", values_to = "mean_error") %>%
  mutate(
    se = ifelse(Model == "ER", ER_se, EI_se)
  )

# Create the main figure
fig_mismatch = ggplot(mismatch_long, 
                      aes(x = mismatch_bin, y = mean_error, color = Model)) +
  geom_ribbon(aes(ymin = mean_error - 1.96 * se, ymax = mean_error + 1.96 * se, 
                  fill = Model), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  geom_point(aes(size = n_precincts), alpha = 0.7) +
  facet_grid(seg_level ~ redist_level) +
  labs(
    x = "Contextual Mismatch (|Precinct % Minority - District % Minority|)",
    y = "Mean Absolute Error in Minority Voting Estimates",
    # title = "How Precinct-District Mismatch Drives Estimation Errors",
    # subtitle = "Larger mismatch = precinct racial composition differs from its district",
    size = "N Precincts"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  scale_fill_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  scale_x_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
  scale_y_continuous(limits = c(0, 0.5)) +
  theme(legend.position = "bottom")

print(fig_mismatch)

ggsave(paste0(fig_dir, "mae_vs_precinct_district_difference.pdf"), 
       fig_mismatch, width = 10, height = 8)

# # Create a second figure showing the distribution of mismatch across scenarios
# mismatch_distribution = precinct_data %>%
#   filter(!is.na(prop_minority)) %>%
#   mutate(
#     contextual_mismatch = abs(prop_minority_precinct - prop_minority)
#   )
# 
# fig_mismatch_dist = ggplot(mismatch_distribution, 
#                            aes(x = contextual_mismatch, fill = seg_level)) +
#   geom_histogram(binwidth = 0.02, alpha = 0.7, position = "identity") +
#   facet_wrap(~ redist_level) +
#   labs(
#     x = "Contextual Mismatch",
#     y = "Number of Precincts",
#     fill = "Segregation",
#     title = "Distribution of Precinct-District Mismatch",
#     subtitle = "Higher segregation creates more extreme mismatches"
#   ) +
#   theme_minimal() +
#   scale_fill_viridis_d() +
#   scale_x_continuous(limits = c(0, 1))
# 
# print(fig_mismatch_dist)
# 
# ggsave(paste0(fig_dir, "mismatch_distribution.pdf"), 
#        fig_mismatch_dist, width = 10, height = 6)
# 
# # Summary statistics
# mismatch_summary = mismatch_distribution %>%
#   group_by(seg_level, redist_level) %>%
#   summarize(
#     mean_mismatch = mean(contextual_mismatch, na.rm = TRUE),
#     median_mismatch = median(contextual_mismatch, na.rm = TRUE),
#     p90_mismatch = quantile(contextual_mismatch, 0.9, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# print("\nContextual Mismatch Summary:")
# print(mismatch_summary)

# ------------------------- Additional Analysis: Error by Precinct Size -------------------------

# Analyze how precinct size affects estimation accuracy
size_error_data = precinct_data %>%
  filter(!is.na(er_minority_error_precinct)) %>%
  mutate(
    log_population = log10(population + 1),
    population_bin = cut(log_population, 
                         breaks = quantile(log_population, probs = seq(0, 1, 0.2)),
                         labels = c("Smallest 20%", "20-40%", "40-60%", "60-80%", "Largest 20%"),
                         include.lowest = TRUE)
  )

fig_size_error = size_error_data %>%
  group_by(seg_level, redist_level, population_bin) %>%
  summarize(
    ER = mean(er_minority_error_precinct, na.rm = TRUE),
    EI = mean(ei_minority_error_precinct, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  pivot_longer(cols = c(ER, EI), names_to = "Model", values_to = "mean_error") %>%
  ggplot(aes(x = population_bin, y = mean_error, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
  facet_grid(seg_level ~ redist_level) +
  labs(
    x = "Precinct Population Size",
    y = "Mean Absolute Error",
    title = "Estimation Error by Precinct Size"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(fig_size_error)

ggsave(paste0(fig_dir, "error_by_precinct_size.pdf"), 
       fig_size_error, width = 10, height = 8)













