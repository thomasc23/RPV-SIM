# Packages
library(fixest)
library(xtable)
library(viridis)
library(patchwork)
library(ggridges)
library(ggpattern)
library(broom)
library(tidyverse)

rm(list = ls())

setwd('~/Dropbox/RPV/Code/Simulation/Workflow/OLD/POPULATION_DENSITY/')
fig_dir = '~/Dropbox/RPV/Figures/Preliminary Analysis/TC/'

# Suppress dplyr messages
library(dplyr, warn.conflicts = FALSE)
options(dplyr.summarise.inform = FALSE)


# df = readRDS('ER_EI_results_cleaned.rds')
df = readRDS('ER_EI_results_final.rds')
head(df)

# Order scenarios as factors
df$seg_level = factor(df$seg_level, levels = c('high', 'medium', 'low'), labels = c('High', 'Medium', 'Low'))
df$redist_level = factor(df$redist_level, levels = c('dem', 'middle', 'rep'), labels = c('Dem', 'Neutral', 'Rep'))

# Add map-level democratic seatshare
df = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  mutate(n_dem_seats = sum(prop_dem > 0.5),
         n_rep_seats = sum(prop_dem < 0.5),
         n_maj_min = sum(prop_minority > 0.5)) 



# ggplot theme
theme_custom = function() {
  theme_bw() +
    theme(
      axis.text = element_text(size = 16),
      axis.title = element_text(size = 24),
      legend.text = element_text(size = 14),
      legend.title = element_text(size = 16),
      strip.text = element_text(size = 16), 
      strip.background = element_rect(fill = "grey95", color = NA),
      plot.title = element_text(size = 12, face = "bold"),
      legend.position = "bottom"
    ) 
  
}




# --------------- Figure 0 --------------------- 
# ER/EI Performance pooled across scenarios by group

district_er_coefficients = df %>%
  mutate(
    er_b1 = er_minority_dem_share,  # Minority 
    er_b0 = er_majority_dem_share,  # Majority
    
    ei_b1 = ei_minority_dem_share,  # Minority
    ei_b0 = ei_majority_dem_share,  # Majority
    
    # True coefficients
    true_b1 = true_minority_dem_share,
    true_b0 = true_majority_dem_share,
    
    # Aggregates
    true_dem_voteshare_district = prop_dem,
    true_per_minority_district = prop_minority,
    true_n_minority = total_minority, 
    true_population = total_population
  ) %>%
  select(map_id, district_id, seg_level, redist_level, n_dem_seats,
         true_dem_voteshare_district, true_per_minority_district,
         true_n_minority, true_population,
         er_b0, er_b1, ei_b0, ei_b1, true_b0, true_b1)


fig_0_df = district_er_coefficients %>%
  select(true_b1, true_b0, er_b1, ei_b1, er_b0, ei_b0) %>%
  mutate(id = row_number()) %>%  # Add identifier
  pivot_longer(cols = -id, 
               names_to = c("Type", "Coefficient"), 
               values_to = "Value",
               names_pattern = "(.*)_(b[01])") %>%
  pivot_wider(names_from = Type, 
              values_from = Value) %>%
  pivot_longer(cols = c(er, ei), 
               names_to = "Model", 
               values_to = "Estimate") %>%
  mutate(Group = ifelse(Coefficient == "b0", "District-Level Majority Voteshares", "District-Level Minority Voteshares"),
         Model = ifelse(Model == 'er', 'ER', 'EI')) %>%
  select(-id) 


fig_0 = ggplot(fig_0_df, aes(x = true, y = Estimate, color = Model)) + 
  geom_point(alpha = 0.5, pch = 1) +
  scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  facet_wrap(~ Group) + 
  labs(x = 'True Group-Specific Voteshare', y = 'Estimated Group-Specific Voteshare') +
  theme_bw()

fig_0

ggsave(
  filename = paste0(fig_dir, 'er_ei_vs_truth_pooled.pdf'),
  plot = fig_0,
  width = 8,
  height = 6,
  dpi = 300
)


# --------------- Figure 1 --------------------- 
# Curve with % minority population on x axis and % accuracy on y-axis for ER/EI/Survey 

fig_1_df = df %>%
  select(map_id, district_id, n_dem_seats, prop_minority, er_rpv_correct, ei_rpv_correct, seg_level, redist_level) %>%
  pivot_longer(cols = c(er_rpv_correct, ei_rpv_correct), names_to = 'model', values_to = 'correct') %>%
  mutate(model = ifelse(model == 'er_rpv_correct', 'ER', 'EI'))

fig_1 = ggplot(fig_1_df, aes(x = prop_minority, y = correct, color = model, fill = model)) +
  geom_point(position = position_jitter(height = 0.025, width = 0), 
             alpha = 0.1, size = 1, pch = 1) +
  geom_smooth(alpha = 0.5) +
  scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  scale_fill_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  labs(x = 'District-Level Minority Share of Population', y = 'RPV Correctly Predicted') +
  facet_grid(seg_level ~ redist_level) + 
  labs(color = 'Model', fill = 'Model') +
  theme_bw()

fig_1

ggsave(
  filename = paste0(fig_dir, 'er_ei_vs_minority_share.pdf'),
  plot = fig_1,
  width = 8,
  height = 6,
  dpi = 300
)





# Pct rpv correct by scenario with per-minority
rpv_correct_df = df %>%
  select(map_id, district_id, n_dem_seats, prop_minority, er_rpv_correct, ei_rpv_correct, seg_level, redist_level) %>%
  pivot_longer(cols = c(er_rpv_correct, ei_rpv_correct), names_to = 'model', values_to = 'correct') %>%
  mutate(model = ifelse(model == 'er_rpv_correct', 'ER', 'EI')) %>%
  mutate(
    prop_min = prop_minority * 100,
    per_minority_bin = cut(prop_min, 
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

# Unique district counts per bin
true_dist = df %>%
  mutate(
    prop_min = prop_minority * 100,
    per_minority_bin = cut(prop_min, 
                           breaks = c(seq(0, 75, by = 5), Inf),
                           labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                      "25-30", "30-35", "35-40", "40-45", "45-50", 
                                      "50-55", "55-60", "60-65", "65-70", "70-75", "75+"),
                           right = FALSE)
  ) %>%
  filter(!is.na(per_minority_bin)) %>%
  count(seg_level, redist_level, per_minority_bin)

# Scale histogram
true_dist_scaled = true_dist %>%
  group_by(seg_level, redist_level) %>%
  mutate(n_scaled = n / max(n) * 30) %>%  # Scale to 30% of 100
  ungroup()

max_n = max(true_dist$n)
scale_factor = 0.3  # Scale histogram 

rpv_correct_p1 = ggplot() +
  geom_hline(yintercept = 0, linetype = 1, alpha = 0.5, color = 'gray70') +
  # Histogram as background
  geom_col(data = true_dist %>% mutate(n_scaled = n / max_n * scale_factor),
           aes(x = per_minority_bin, y = n_scaled),
           fill = "gray70", alpha = 0.5) +
  # Lines on top
  geom_line(data = rpv_correct_df,
            aes(x = per_minority_bin, y = pct_correct, color = model, group = model),
            linewidth = 1) +
  geom_point(data = rpv_correct_df,
             aes(x = per_minority_bin, y = pct_correct, color = model),
             shape = 21,
             size = 2) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * max_n / scale_factor, 
                        name = "")
  ) +
  scale_x_discrete(breaks = c('0-5', '10-15', '20-25','30-35', '40-45', '50-55', '60-65', '70-75')) + 
  labs(x = "Minority Population (%)", y = "Percent Correct", color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank()) 


rpv_correct_p1


ggsave(
  filename = paste0(fig_dir, 'New_07_09/rpv_classification_pct_by_minority_pop_binned.pdf'),
  plot = rpv_correct_p1,
  width = 8,
  height = 6,
  dpi = 300
)


rpv_correct_df = df %>%
  select(map_id, district_id, n_dem_seats, true_overall_dem_share, er_rpv_correct, ei_rpv_correct, seg_level, redist_level) %>%
  pivot_longer(cols = c(er_rpv_correct, ei_rpv_correct), names_to = 'model', values_to = 'correct') %>%
  mutate(
    model = ifelse(model == 'er_rpv_correct', 'ER', 'EI'),
    dem_voteshare_pct = true_overall_dem_share * 100,
    dem_voteshare_bin = cut(dem_voteshare_pct, 
                            breaks = c(seq(0, 100, by = 5), Inf),
                            labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                       "25-30", "30-35", "35-40", "40-45", "45-50", 
                                       "50-55", "55-60", "60-65", "65-70", "70-75",
                                       "75-80", "80-85", "85-90", "90-95", "95-100", "100+"),
                            right = FALSE)
    ) %>%
  filter(!is.na(dem_voteshare_bin)) %>%
  group_by(seg_level, redist_level, dem_voteshare_bin, model) %>% 
  summarise(
    pct_correct = mean(correct) * 100,
    n_obs = n(),
    .groups = 'drop'
  )


# Get unique district counts per bin
true_dist = df %>%
  mutate(
    dem_voteshare_pct = true_overall_dem_share * 100,
    dem_voteshare_bin = cut(dem_voteshare_pct, 
                            breaks = c(seq(0, 100, by = 5), Inf),
                            labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                       "25-30", "30-35", "35-40", "40-45", "45-50", 
                                       "50-55", "55-60", "60-65", "65-70", "70-75",
                                       "75-80", "80-85", "85-90", "90-95", "95-100", "100+"),
                            right = FALSE)
  ) %>%
  filter(!is.na(dem_voteshare_bin)) %>%
  count(seg_level, redist_level, dem_voteshare_bin)

# Calculate scaling factor
max_n = max(true_dist$n)
scale_factor = 0.3  # Scale histogram to 30% of plot

rpv_correct_p2 = ggplot() +
  geom_hline(yintercept = 0, linetype = 1, alpha = 0.5, color = 'gray70') +
  # Histogram as background
  geom_col(data = true_dist %>% mutate(n_scaled = n / max_n * scale_factor),
           aes(x = dem_voteshare_bin, y = n_scaled),
           fill = "gray70", alpha = 0.5) +
  # Lines on top
  geom_line(data = rpv_correct_df,
            aes(x = dem_voteshare_bin, y = pct_correct, color = model, group = model),
            linewidth = 1) +
  geom_point(data = rpv_correct_df,
             aes(x = dem_voteshare_bin, y = pct_correct, color = model),
             shape = 21,
             size = 2) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * max_n / scale_factor, 
                        name = "")
  ) +
  scale_x_discrete(breaks = c('20-25','30-35', '40-45', '50-55', '60-65', '70-75', '80-85')) + 
  labs(x = "Democratic Vote Share (%)", 
       y = "Percent Correct", 
       color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  ylim(0, 100)+
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank()) 


rpv_correct_p2

ggsave(
  filename = paste0(fig_dir, 'New_07_09/rpv_classification_pct_by_dem_share_binned.pdf'),
  plot = rpv_correct_p2,
  width = 8,
  height = 6,
  dpi = 300
)




# Create RPV accuracy by democratic margin bins
rpv_margin_df = df %>%
  select(map_id, district_id, n_dem_seats, true_overall_dem_share, er_rpv_correct, ei_rpv_correct, seg_level, redist_level) %>%
  mutate(dem_margin = (true_overall_dem_share - (1 - true_overall_dem_share)) * 100) %>%
  pivot_longer(cols = c(er_rpv_correct, ei_rpv_correct), names_to = 'model', values_to = 'correct') %>%
  mutate(
    model = ifelse(model == 'er_rpv_correct', 'ER', 'EI'),
    dem_margin_bin = cut(dem_margin, 
                         breaks = c(-Inf, seq(-50, 50, by = 10), Inf),
                         labels = c("50+ (R)", "40-50 (R)", "30-40 (R)", "20-30 (R)", "10-20 (R)",
                                    "0-10 (R)", "0-10 (D)", "10-20 (D)", "20-30 (D)", "30-40 (D)",
                                    "40-50 (D)", "50+ (D)"),
                         right = FALSE)) %>%
  filter(!is.na(dem_margin_bin)) %>%
  group_by(seg_level, redist_level, dem_margin_bin, model) %>% 
  summarise(
    pct_correct = mean(correct) * 100,
    n_obs = n(),
    .groups = 'drop'
  )

# Get unique district counts per margin bin
true_dist_margin = df %>%
  mutate(
    dem_margin = (true_overall_dem_share - (1 - true_overall_dem_share)) * 100,
    dem_margin_bin = cut(dem_margin, 
                         breaks = c(-Inf, seq(-50, 50, by = 10), Inf),
                         labels = c("50+ (R)", "40-50 (R)", "30-40 (R)", "20-30 (R)", "10-20 (R)",
                                    "0-10 (R)", "0-10 (D)", "10-20 (D)", "20-30 (D)", "30-40 (D)",
                                    "40-50 (D)", "50+ (D)"),
                         right = FALSE)
  ) %>%
  filter(!is.na(dem_margin_bin)) %>%
  count(seg_level, redist_level, dem_margin_bin)

# Calculate scaling factor
max_n_margin = max(true_dist_margin$n)
scale_factor = 0.3  # Scale histogram to 30% of plot

# Create the plot
rpv_correct_p3 = ggplot() +
  geom_hline(yintercept = 0, linetype = 1, alpha = 0.5, color = 'gray70') +
  # Histogram as background
  geom_col(data = true_dist_margin %>% mutate(n_scaled = n / max_n * scale_factor),
           aes(x = dem_margin_bin, y = n_scaled),
           fill = "gray70", alpha = 0.5) +
  # Lines on top
  geom_line(data = rpv_margin_df,
            aes(x = dem_margin_bin, y = pct_correct, color = model, group = model),
            linewidth = 1) +
  geom_point(data = rpv_margin_df,
             aes(x = dem_margin_bin, y = pct_correct, color = model),
             shape = 21,
             size = 2) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * max_n_margin / scale_factor, 
                        name = "")
  ) +
  labs(x = "Democratic Win Margin (%)", 
       y = "Percent Correct", 
       color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  ylim(0, 100) +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank()) +
  labs(caption = "Gray bars show relative frequency of districts")

# Display the plot
rpv_correct_p3

# Save the plot
ggsave(
  filename = paste0(fig_dir, 'New_07_09/rpv_classification_pct_by_dem_margin_binned.pdf'),
  plot = rpv_correct_p3,
  width = 8,
  height = 6,
  dpi = 300
)












# --- RMSE Same figure as above ---

# RMSE 
rmse_df = df %>%
  select(map_id, district_id, seg_level, redist_level,
         true_overall_dem_share,
         er_minority_dem_share, ei_minority_dem_share,
         er_majority_dem_share, ei_majority_dem_share,
         true_minority_dem_share, true_majority_dem_share) %>%
  # Pivot for minority group
  pivot_longer(
    cols = c(er_minority_dem_share, ei_minority_dem_share),
    names_to = "model",
    names_prefix = c("_minority_dem_share"),
    values_to = "predicted_minority"
  ) %>%
  mutate(
    model = ifelse(grepl("er_", model), "ER", "EI"),
    error_minority = abs(predicted_minority - true_minority_dem_share) * 100
  ) %>%
  # Add majority predictions
  mutate(
    predicted_majority = ifelse(model == "ER", er_majority_dem_share, ei_majority_dem_share),
    error_majority = abs(predicted_majority - true_majority_dem_share) * 100
  ) %>%
  select(-er_majority_dem_share, -ei_majority_dem_share) %>%
  # Pivot errors to long format
  pivot_longer(
    cols = c(error_minority, error_majority),
    names_to = "group",
    names_prefix = "error_",
    values_to = "error"
  ) %>%
  mutate(
    dem_voteshare_pct = true_overall_dem_share * 100,
    dem_voteshare_bin = cut(dem_voteshare_pct, 
                            breaks = c(seq(0, 100, by = 5), Inf),
                            labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                       "25-30", "30-35", "35-40", "40-45", "45-50", 
                                       "50-55", "55-60", "60-65", "65-70", "70-75",
                                       "75-80", "80-85", "85-90", "90-95", "95-100", "100+"),
                            right = FALSE)
  ) %>%
  filter(!is.na(dem_voteshare_bin)) %>%
  group_by(seg_level, redist_level, dem_voteshare_bin, model, group) %>%
  summarise(
    rmse = sqrt(mean(error^2)),  # Calculate RMSE
    n_obs = n(),
    .groups = 'drop'
  )

# Get unique district counts per bin
true_dist = df %>%
  mutate(
    dem_voteshare_pct = true_overall_dem_share * 100,
    dem_voteshare_bin = cut(dem_voteshare_pct, 
                            breaks = c(seq(0, 100, by = 5), Inf),
                            labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                       "25-30", "30-35", "35-40", "40-45", "45-50", 
                                       "50-55", "55-60", "60-65", "65-70", "70-75",
                                       "75-80", "80-85", "85-90", "90-95", "95-100", "100+"),
                            right = FALSE)
  ) %>%
  filter(!is.na(dem_voteshare_bin)) %>%
  count(seg_level, redist_level, dem_voteshare_bin)

# Calculate scaling factor
max_n = max(true_dist$n)
scale_factor = 0.15  # Scale histogram to 15% of plot

# Create separate plots for minority and majority
rmse_minority_plot = ggplot() +
  geom_hline(yintercept = 0, linetype = 1, alpha = 0.5, color = 'gray70') +
  # Histogram as background
  geom_col(data = true_dist %>% mutate(n_scaled = n / max_n * scale_factor),
           aes(x = dem_voteshare_bin, y = n_scaled),
           fill = "gray70", alpha = 0.5) +
  # Lines on top
  geom_line(data = rmse_df %>% filter(group == "minority"),
            aes(x = dem_voteshare_bin, y = rmse, color = model, group = model),
            linewidth = 1) +
  geom_point(data = rmse_df %>% filter(group == "minority"),
             aes(x = dem_voteshare_bin, y = rmse, color = model),
             shape = 21,
             size = 2) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * max_n / (scale_factor), 
                        name = "")
  ) +
  scale_x_discrete(breaks = c('20-25','30-35', '40-45', '50-55', '60-65', '70-75', '80-85')) + 
  labs(x = "Democratic Vote Share (%)", 
       y = "RMSE (Minority)", 
       color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank())

rmse_majority_plot = ggplot() +
  geom_hline(yintercept = 0, linetype = 1, alpha = 0.5, color = 'gray70') +
  # Histogram as background
  geom_col(data = true_dist %>% mutate(n_scaled = n / max_n * scale_factor),
           aes(x = dem_voteshare_bin, y = n_scaled),
           fill = "gray70", alpha = 0.5) +
  # Lines on top
  geom_line(data = rmse_df %>% filter(group == "majority"),
            aes(x = dem_voteshare_bin, y = rmse, color = model, group = model),
            linewidth = 1) +
  geom_point(data = rmse_df %>% filter(group == "majority"),
             aes(x = dem_voteshare_bin, y = rmse, color = model),
             shape = 21,
             size = 2) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * max_n / (scale_factor),
                        name = "")
  ) +
  scale_x_discrete(breaks = c('20-25','30-35', '40-45', '50-55', '60-65', '70-75', '80-85')) + 
  labs(x = "Democratic Vote Share (%)", 
       y = "RMSE (Majority)", 
       color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank())

# Display plots
rmse_minority_plot
rmse_majority_plot


# Save the plots
ggsave(
  filename = paste0(fig_dir, 'New_07_09/rmse_minority_by_dem_share_binned.pdf'),
  plot = rmse_minority_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = paste0(fig_dir, 'New_07_09/rmse_majority_by_dem_share_binned.pdf'),
  plot = rmse_majority_plot,
  width = 8,
  height = 6,
  dpi = 300
)



# --- Bias ---

# RMSE 
bias_df = df %>%
  select(map_id, district_id, seg_level, redist_level,
         true_overall_dem_share,
         er_minority_dem_share, ei_minority_dem_share,
         er_majority_dem_share, ei_majority_dem_share,
         true_minority_dem_share, true_majority_dem_share) %>%
  # Pivot for minority group
  pivot_longer(
    cols = c(er_minority_dem_share, ei_minority_dem_share),
    names_to = "model",
    names_prefix = c("_minority_dem_share"),
    values_to = "predicted_minority"
  ) %>%
  mutate(
    model = ifelse(grepl("er_", model), "ER", "EI"),
    error_minority = (predicted_minority - true_minority_dem_share) * 100
  ) %>%
  # Add majority predictions
  mutate(
    predicted_majority = ifelse(model == "ER", er_majority_dem_share, ei_majority_dem_share),
    error_majority = (predicted_majority - true_majority_dem_share) * 100
  ) %>%
  select(-er_majority_dem_share, -ei_majority_dem_share) %>%
  # Pivot errors to long format
  pivot_longer(
    cols = c(error_minority, error_majority),
    names_to = "group",
    names_prefix = "error_",
    values_to = "error"
  ) %>%
  mutate(
    dem_voteshare_pct = true_overall_dem_share * 100,
    dem_voteshare_bin = cut(dem_voteshare_pct, 
                            breaks = c(seq(0, 100, by = 5), Inf),
                            labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                       "25-30", "30-35", "35-40", "40-45", "45-50", 
                                       "50-55", "55-60", "60-65", "65-70", "70-75",
                                       "75-80", "80-85", "85-90", "90-95", "95-100", "100+"),
                            right = FALSE)
  ) %>%
  filter(!is.na(dem_voteshare_bin)) %>%
  group_by(seg_level, redist_level, dem_voteshare_bin, model, group) %>%
  summarise(
    bias = mean(error),  # Calculate bias
    n_obs = n(),
    .groups = 'drop'
  )

# Get unique district counts per bin
true_dist = df %>%
  mutate(
    dem_voteshare_pct = true_overall_dem_share * 100,
    dem_voteshare_bin = cut(dem_voteshare_pct, 
                            breaks = c(seq(0, 100, by = 5), Inf),
                            labels = c("0-5", "5-10", "10-15", "15-20", "20-25", 
                                       "25-30", "30-35", "35-40", "40-45", "45-50", 
                                       "50-55", "55-60", "60-65", "65-70", "70-75",
                                       "75-80", "80-85", "85-90", "90-95", "95-100", "100+"),
                            right = FALSE)
  ) %>%
  filter(!is.na(dem_voteshare_bin)) %>%
  count(seg_level, redist_level, dem_voteshare_bin)

# Calculate scaling factor
max_n = max(true_dist$n)
scale_factor = 0.15  # Scale histogram to 15% of plot

# Create separate plots for minority and majority
bias_minority_plot = ggplot() +
  geom_hline(yintercept = 0, linetype = 1, alpha = 0.5, color = 'gray70') +
  # Histogram as background
  geom_col(data = true_dist %>% mutate(n_scaled = n / max_n * scale_factor),
           aes(x = dem_voteshare_bin, y = n_scaled),
           fill = "gray70", alpha = 0.5) +
  # Lines on top
  geom_line(data = bias_df %>% filter(group == "minority"),
            aes(x = dem_voteshare_bin, y = bias, color = model, group = model),
            linewidth = 1) +
  geom_point(data = bias_df %>% filter(group == "minority"),
             aes(x = dem_voteshare_bin, y = bias, color = model),
             shape = 21,
             size = 2) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * max_n / (scale_factor),
                        name = "")
  ) +
  scale_x_discrete(breaks = c('20-25','30-35', '40-45', '50-55', '60-65', '70-75', '80-85')) + 
  labs(x = "Democratic Vote Share (%)", 
       y = "Bias (Minority)", 
       color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank())

bias_majority_plot = ggplot() +
  geom_hline(yintercept = 0, linetype = 1, alpha = 0.5, color = 'gray70') +
  # Histogram as background
  geom_col(data = true_dist %>% mutate(n_scaled = n / max_n * scale_factor),
           aes(x = dem_voteshare_bin, y = n_scaled),
           fill = "gray70", alpha = 0.5) +
  # Lines on top
  geom_line(data = bias_df %>% filter(group == "majority"),
            aes(x = dem_voteshare_bin, y = bias, color = model, group = model),
            linewidth = 1) +
  geom_point(data = bias_df %>% filter(group == "majority"),
             aes(x = dem_voteshare_bin, y = bias, color = model),
             shape = 21,
             size = 2) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . * max_n / (scale_factor),
                        name = "")
  ) +
  scale_x_discrete(breaks = c('20-25','30-35', '40-45', '50-55', '60-65', '70-75', '80-85')) + 
  labs(x = "Democratic Vote Share (%)", 
       y = "Bias (Majority)", 
       color = "Model") +
  scale_color_brewer(palette = 'Dark2') +
  # ylim(0, 20) +  # Adjust based on your RMSE range
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.ticks.y.right = element_blank(),
        axis.text.y.right = element_blank())

# Display plots
bias_minority_plot
bias_majority_plot




# Save the plots
ggsave(
  filename = paste0(fig_dir, 'New_07_09/bias_minority_by_dem_share_binned.pdf'),
  plot = bias_minority_plot,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  filename = paste0(fig_dir, 'New_07_09/bias_majority_by_dem_share_binned.pdf'),
  plot = bias_majority_plot,
  width = 8,
  height = 6,
  dpi = 300
)






# Heatmap of RPV accuracy by district characteristics and scenario

rpv_correct_df = df %>%
  select(map_id, district_id, n_dem_seats, prop_minority, true_overall_dem_share, er_rpv_correct, ei_rpv_correct, seg_level, redist_level) %>%
  rename(
    per_minority = prop_minority,
    dem_voteshare = true_overall_dem_share
  ) %>%
  pivot_longer(cols = c(er_rpv_correct, ei_rpv_correct), names_to = 'model', values_to = 'correct') %>%
  mutate(model = ifelse(model == 'er_rpv_correct', 'ER', 'EI'))

# Heatmap of rpv errors for ER
rpv_correct_p3 = ggplot(rpv_correct_df %>% filter(correct == 0 & model == 'ER'), aes(x = per_minority, y = dem_voteshare)) +
  geom_hex(bins = 20, alpha = 0.5) +
  scale_fill_fermenter(palette = "Reds",
                       direction = 1,
                       n.breaks = 6) +
  labs(x = 'District-Level Minority Share of Population', 
       y = 'District-Level Democratic Voteshare',
       fill = '# Incorrect\nPredictions') +
  facet_grid(seg_level ~ redist_level) + 
  theme_custom()

rpv_correct_p3

ggsave(
  filename = paste0(fig_dir, 'New_07_09/rpv_errors_dem_vs_min_er.pdf'),
  plot = rpv_correct_p3,
  width = 8,
  height = 6,
  dpi = 300
)

# Heatmap of rpv errors for EI
rpv_correct_p4 = ggplot(rpv_correct_df %>% filter(correct == 0 & model == 'EI'), aes(x = per_minority, y = dem_voteshare)) +
  geom_hex(bins = 20, alpha = 0.5) +
  scale_fill_fermenter(palette = "Reds",
                       direction = 1,
                       n.breaks = 4) +
  labs(x = 'District-Level Minority Share of Population', 
       y = 'District-Level Democratic Voteshare',
       fill = '# Incorrect\nPredictions') +
  facet_grid(seg_level ~ redist_level) + 
  theme_custom()

rpv_correct_p4

ggsave(
  filename = paste0(fig_dir, 'New_07_09/rpv_errors_dem_vs_min_ei.pdf'),
  plot = rpv_correct_p4,
  width = 8,
  height = 6,
  dpi = 300
)




mae_df = df %>%
  select(map_id, district_id, n_dem_seats, prop_minority, 
         er_minority_resid, er_majority_resid, 
         ei_minority_resid, ei_majority_resid,
         seg_level, redist_level) %>%
  mutate(
    er_min_abs_error = abs(er_minority_resid),
    er_maj_abs_error = abs(er_majority_resid),
    ei_min_abs_error = abs(ei_minority_resid),
    ei_maj_abs_error = abs(ei_majority_resid)
  ) %>%
  pivot_longer(cols = c(er_min_abs_error, er_maj_abs_error, ei_min_abs_error, ei_maj_abs_error), 
               names_to = c('model', 'group'), 
               names_pattern = '(er|ei)_(min|maj)_abs_error',
               values_to = 'abs_error') %>%
  mutate(model = ifelse(model == 'er', 'ER', 'EI'), 
         group = ifelse(group == 'min', 'Minority', 'Majority'))


abs_error_p1 = ggplot(mae_df, aes(x = prop_minority, y = abs_error, fill = group, color = group)) +
  geom_point(alpha = 0.01, shape = 21, size = 2) +
  geom_smooth(method = 'loess') +
  facet_grid(seg_level ~ redist_level) + # , scales = 'free_y') +
  labs(x = 'District-Level Minority Share of Population', 
       y = 'District-Level Democratic Voteshare',
       color = 'Group', fill = 'Group') +
  scale_color_brewer(palette = 'Dark2') +
  scale_fill_brewer(palette = 'Dark2') +
  theme_custom()

abs_error_p1

ggsave(
  filename = paste0(fig_dir, 'New_07_09/rpv_error.pdf'),
  plot = abs_error_p1,
  width = 8,
  height = 6,
  dpi = 300
)

# Calculate quantiles by group
mae_summary = mae_df %>%
  group_by(prop_minority_bin = cut(prop_minority, breaks = seq(0, 1, by = 0.05)), 
           group, seg_level, redist_level) %>%
  summarise(
    median_error = median(abs_error),
    mean_error = mean(abs_error),
    q25 = quantile(abs_error, 0.25),
    q75 = quantile(abs_error, 0.75),
    q90 = quantile(abs_error, 0.90),
    prop_minority = mean(prop_minority)
  )

ggplot(mae_summary %>% filter(seg_level == 'High'), aes(x = prop_minority, y = median_error, color = group, fill = group)) +
  geom_ribbon(aes(ymin = q25, ymax = q75), alpha = 0.3) +
  geom_line(size = 1) +
  facet_wrap(~ redist_level) +
  labs(x = 'District-Level Minority Share of Population', 
       y = 'Median Absolute Error (with IQR)',
       color = 'Group', fill = 'Group') +
  scale_color_brewer(palette = 'Dark2') +
  scale_fill_brewer(palette = 'Dark2') +
  theme_custom()


# --------------- Figure 2a --------------------- 
# Minority wasted votes / efficiency gap (BAR CHART)

# Aggregate to map level for efficiency gap calculation
map_summary = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    # Sum up the pre-calculated wasted votes
    total_minority_dem_wasted = sum(minority_dem_wasted, na.rm = TRUE),
    total_minority_rep_wasted = sum(minority_rep_wasted, na.rm = TRUE),
    total_minority_wasted = sum(minority_wasted_total, na.rm = TRUE),
    
    # Total minority votes
    total_minority_votes = sum(total_minority),
    
    # Calculate minority-specific efficiency gap
    # Positive = Democratic advantage (more minority Rep votes wasted)
    minority_efficiency_gap = (total_minority_rep_wasted - total_minority_dem_wasted) / total_minority_votes,
    
    .groups = 'drop'
  ) %>%
  # Calculate proportion of minority votes wasted
  mutate(prop_minority_wasted = total_minority_wasted / total_minority_votes)

# Create summary for plotting
plot_summary = map_summary %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    # Mean and SE for wasted votes
    mean_prop_wasted = mean(prop_minority_wasted),
    se_prop_wasted = sd(prop_minority_wasted) / sqrt(n()),
    
    # Mean and SE for efficiency gap
    mean_efficiency_gap = mean(minority_efficiency_gap, na.rm = TRUE),
    se_efficiency_gap = sd(minority_efficiency_gap, na.rm = TRUE) / sqrt(n()),
    
    n_maps = n(),
    
    .groups = 'drop'
  )

# Panel 1: Proportion of minority votes wasted
p_wasted_min = ggplot(plot_summary, aes(x = redist_level, y = mean_prop_wasted, fill = redist_level)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_prop_wasted - 1.96*se_prop_wasted,
                    ymax = mean_prop_wasted + 1.96*se_prop_wasted),
                width = 0.2) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey40", "Rep" = "#b31529"),
                    labels = c("Democratic", "Neutral", "Republican")) +
  scale_x_discrete(labels = c("Dem", "Neutral", "Rep")) +
  labs(x = "", 
       y = "Proportion of Minority Votes Wasted",
       title = "Wasted Minority Votes") +
  facet_wrap(~ seg_level, nrow = 1, 
             labeller = labeller(seg_level = c("low" = "Low Seg.",
                                               "medium" = "Medium Seg.",
                                               "high" = "High Seg."))) +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Panel 2: Minority efficiency gap
p_gap_min = ggplot(plot_summary, aes(x = redist_level, y = mean_efficiency_gap, fill = redist_level)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_efficiency_gap - 1.96*se_efficiency_gap,
                    ymax = mean_efficiency_gap + 1.96*se_efficiency_gap),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey40", "Rep" = "#b31529"),
                    labels = c("Democratic", "Neutral", "Republican")) +
  scale_x_discrete(labels = c("Dem", "Neutral", "Rep")) +
  labs(x = "", 
       y = "Minority Efficiency Gap",
       title = "Minority Efficiency Gap",
       fill = "Gerrymander") +
  facet_wrap(~ seg_level, nrow = 1,
             labeller = labeller(seg_level = c("low" = "Low Seg.",
                                               "medium" = "Medium Seg.", 
                                               "high" = "High Seg."))) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combine
fig_2a_bar = p_wasted_min + p_gap_min + 
  plot_layout(guides = "collect") 

fig_2a_bar

# Save figure
ggsave(
  filename = paste0(fig_dir, 'minority_wasted_votes_efficiency_gap_bar.pdf'),
  plot = fig_2a_bar,
  width = 12,
  height = 5,
  dpi = 300
)


# --------------- Figure 2b --------------------- 
# Minority wasted votes / efficiency gap (SCATTER PLOT)

# Calculate map-level summaries (raw data for plotting)
map_summary = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    total_minority_dem_wasted = sum(minority_dem_wasted, na.rm = TRUE),
    total_minority_rep_wasted = sum(minority_rep_wasted, na.rm = TRUE),
    total_minority_wasted = sum(minority_wasted_total, na.rm = TRUE),
    total_minority_votes = sum(total_minority),
    minority_efficiency_gap = (total_minority_rep_wasted - total_minority_dem_wasted) / total_minority_votes,
    .groups = 'drop'
  ) %>%
  mutate(
    prop_minority_wasted = total_minority_wasted / total_minority_votes,
    # Add numeric version for x-axis
    redist_numeric = case_when(
      redist_level == "Dem" ~ 1,
      redist_level == "Neutral" ~ 2,
      redist_level == "Rep" ~ 3
    )
  )

# Create summary for plotting (means for lines)
plot_summary = map_summary %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    mean_prop_wasted = mean(prop_minority_wasted),
    mean_efficiency_gap = mean(minority_efficiency_gap, na.rm = TRUE),
    n_maps = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    redist_numeric = case_when(
      redist_level == "Dem" ~ 1,
      redist_level == "Neutral" ~ 2,
      redist_level == "Rep" ~ 3
    )
  )

# Panel 1: Proportion of minority votes wasted
p1_slope_min = ggplot() +
  # Raw data points with jitter
  geom_point(data = map_summary,
             aes(x = redist_numeric, y = prop_minority_wasted, color = seg_level),
             alpha = 0.1, size = 1.5,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean lines
  geom_line(data = plot_summary,
            aes(x = redist_numeric, y = mean_prop_wasted, 
                group = seg_level, color = seg_level),
            linewidth = 1.5,
            position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean points
  geom_point(data = plot_summary,
             aes(x = redist_numeric, y = mean_prop_wasted, color = seg_level),
             size = 4,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  scale_x_continuous(breaks = 1:3, labels = c("Dem", "Neutral", "Rep")) +
  scale_color_viridis_d(option = 'C', name = "Segregation",
                        labels = c("Low", "Medium", "High"),
                        breaks = c("Low", "Medium", "High")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Gerrymander", 
       y = "Proportion of Minority Votes Wasted",
       title = "Wasted Minority Votes") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Panel 2: Minority efficiency gap
p2_slope_min = ggplot() +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  # Raw data points with jitter
  geom_point(data = map_summary,
             aes(x = redist_numeric, y = minority_efficiency_gap, color = seg_level),
             alpha = 0.1, size = 1.5,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean lines
  geom_line(data = plot_summary,
            aes(x = redist_numeric, y = mean_efficiency_gap, 
                group = seg_level, color = seg_level),
            linewidth = 1.5,
            position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean points
  geom_point(data = plot_summary,
             aes(x = redist_numeric, y = mean_efficiency_gap, color = seg_level),
             size = 4,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  scale_x_continuous(breaks = 1:3, labels = c("Dem", "Neutral", "Rep")) +
  scale_color_viridis_d(option = 'C', name = "Segregation",
                        labels = c("Low", "Medium", "High"),
                        breaks = c("Low", "Medium", "High")) +
  labs(x = "Gerrymander", 
       y = "Minority Efficiency Gap",
       title = "Minority Efficiency Gap") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  # Add interpretation note
  annotate("text", x = 1, y = max(map_summary$minority_efficiency_gap) * 0.95, 
           label = "Positive = Dem advantage", 
           hjust = -0.5, vjust = 1, size = 3, fontface = "italic")

# Combine 
fig_2b_scatter = p1_slope_min + p2_slope_min + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

fig_2b_scatter

# Save figure
ggsave(
  filename = paste0(fig_dir, 'minority_wasted_votes_efficiency_gap_scatter.pdf'),
  plot = fig_2b_scatter,
  width = 12,
  height = 6,
  dpi = 300
)

# --------------- Figure 3a --------------------- 
# Majority wasted votes / efficiency gap (BAR CHART)

# Aggregate to map level for efficiency gap calculation
map_summary_majority = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    # Sum up the pre-calculated wasted votes
    total_majority_dem_wasted = sum(majority_dem_wasted, na.rm = TRUE),
    total_majority_rep_wasted = sum(majority_rep_wasted, na.rm = TRUE),
    total_majority_wasted = sum(majority_wasted_total, na.rm = TRUE),
    
    # Total majority votes
    total_majority_votes = sum(total_majority),
    
    # Calculate majority-specific efficiency gap
    # Positive = Democratic advantage (more majority Rep votes wasted)
    majority_efficiency_gap = (total_majority_rep_wasted - total_majority_dem_wasted) / total_majority_votes,
    
    .groups = 'drop'
  ) %>%
  # Calculate proportion of majority votes wasted
  mutate(prop_majority_wasted = total_majority_wasted / total_majority_votes)

# Create summary for plotting
plot_summary_majority = map_summary_majority %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    # Mean and SE for wasted votes
    mean_prop_wasted = mean(prop_majority_wasted),
    se_prop_wasted = sd(prop_majority_wasted) / sqrt(n()),
    
    # Mean and SE for efficiency gap
    mean_efficiency_gap = mean(majority_efficiency_gap, na.rm = TRUE),
    se_efficiency_gap = sd(majority_efficiency_gap, na.rm = TRUE) / sqrt(n()),
    
    n_maps = n(),
    
    .groups = 'drop'
  )

# Panel 1: Proportion of majority votes wasted
p_wasted_maj = ggplot(plot_summary_majority, aes(x = redist_level, y = mean_prop_wasted, fill = redist_level)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_prop_wasted - 1.96*se_prop_wasted,
                    ymax = mean_prop_wasted + 1.96*se_prop_wasted),
                width = 0.2) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey40", "Rep" = "#b31529"),
                    labels = c("Democratic", "Neutral", "Republican")) +
  scale_x_discrete(labels = c("Dem", "Neutral", "Rep")) +
  labs(x = "", 
       y = "Proportion of Majority Votes Wasted",
       title = "Wasted Majority Votes") +
  facet_wrap(~ seg_level, nrow = 1, 
             labeller = labeller(seg_level = c("low" = "Low Seg.",
                                               "medium" = "Medium Seg.",
                                               "high" = "High Seg."))) +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Panel 2: Majority efficiency gap
p_gap_maj = ggplot(plot_summary_majority, aes(x = redist_level, y = mean_efficiency_gap, fill = redist_level)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = mean_efficiency_gap - 1.96*se_efficiency_gap,
                    ymax = mean_efficiency_gap + 1.96*se_efficiency_gap),
                width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey40", "Rep" = "#b31529"),
                    labels = c("Democratic", "Neutral", "Republican")) +
  scale_x_discrete(labels = c("Dem", "Neutral", "Rep")) +
  labs(x = "", 
       y = "Majority Efficiency Gap",
       title = "Majority Efficiency Gap",
       fill = "Gerrymander") +
  facet_wrap(~ seg_level, nrow = 1,
             labeller = labeller(seg_level = c("low" = "Low Seg.",
                                               "medium" = "Medium Seg.", 
                                               "high" = "High Seg."))) +
  theme_bw() +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 11),
        axis.text.x = element_text(angle = 45, hjust = 1))

# Combine
fig_3a_bar = p_wasted_maj + p_gap_maj + 
  plot_layout(guides = "collect") 

fig_3a_bar

# Save figure
ggsave(
  filename = paste0(fig_dir, 'majority_wasted_votes_efficiency_gap_bar.pdf'),
  plot = fig_3a_bar,
  width = 12,
  height = 5,
  dpi = 300
)


# --------------- Figure 3b --------------------- 
# Majority wasted votes / efficiency gap (SCATTER PLOT)

# Calculate map-level summaries (raw data for plotting)
map_summary_majority = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    total_majority_dem_wasted = sum(majority_dem_wasted, na.rm = TRUE),
    total_majority_rep_wasted = sum(majority_rep_wasted, na.rm = TRUE),
    total_majority_wasted = sum(majority_wasted_total, na.rm = TRUE),
    total_majority_votes = sum(total_majority),
    majority_efficiency_gap = (total_majority_rep_wasted - total_majority_dem_wasted) / total_majority_votes,
    .groups = 'drop'
  ) %>%
  mutate(
    prop_majority_wasted = total_majority_wasted / total_majority_votes,
    # Add numeric version for x-axis
    redist_numeric = case_when(
      redist_level == "Dem" ~ 1,
      redist_level == "Neutral" ~ 2,
      redist_level == "Rep" ~ 3
    )
  )

# Create summary for plotting (means for lines)
plot_summary_majority = map_summary_majority %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    mean_prop_wasted = mean(prop_majority_wasted),
    mean_efficiency_gap = mean(majority_efficiency_gap, na.rm = TRUE),
    n_maps = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    redist_numeric = case_when(
      redist_level == "Dem" ~ 1,
      redist_level == "Neutral" ~ 2,
      redist_level == "Rep" ~ 3
    )
  )

# Panel 1: Proportion of majority votes wasted
p1_slope_maj = ggplot() +
  # Raw data points with jitter
  geom_point(data = map_summary_majority,
             aes(x = redist_numeric, y = prop_majority_wasted, color = seg_level),
             alpha = 0.1, size = 1.5,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean lines
  geom_line(data = plot_summary_majority,
            aes(x = redist_numeric, y = mean_prop_wasted, 
                group = seg_level, color = seg_level),
            linewidth = 1.5,
            position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean points
  geom_point(data = plot_summary_majority,
             aes(x = redist_numeric, y = mean_prop_wasted, color = seg_level),
             size = 4,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  scale_x_continuous(breaks = 1:3, labels = c("Dem", "Neutral", "Rep")) +
  scale_color_viridis_d(option = 'C', name = "Segregation",
                        labels = c("Low", "Medium", "High"),
                        breaks = c("Low", "Medium", "High")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Gerrymander", 
       y = "Proportion of Majority Votes Wasted",
       title = "Wasted Majority Votes") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank())

# Panel 2: Majority efficiency gap
p2_slope_maj = ggplot() +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  # Raw data points with jitter
  geom_point(data = map_summary_majority,
             aes(x = redist_numeric, y = majority_efficiency_gap, color = seg_level),
             alpha = 0.1, size = 1.5,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean lines
  geom_line(data = plot_summary_majority,
            aes(x = redist_numeric, y = mean_efficiency_gap, 
                group = seg_level, color = seg_level),
            size = 1.5,
            position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean points
  geom_point(data = plot_summary_majority,
             aes(x = redist_numeric, y = mean_efficiency_gap, color = seg_level),
             size = 4,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  scale_x_continuous(breaks = 1:3, labels = c("Dem", "Neutral", "Rep")) +
  scale_color_viridis_d(option = 'C', name = "Segregation",
                        labels = c("Low", "Medium", "High"),
                        breaks = c("Low", "Medium", "High")) +
  labs(x = "Gerrymander", 
       y = "Majority Efficiency Gap",
       title = "Majority Efficiency Gap") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  # Add interpretation note
  annotate("text", x = 1, y = max(map_summary_majority$majority_efficiency_gap) * 0.95, 
           label = "Positive = Dem advantage", 
           hjust = -0.5, vjust = 1, size = 3, fontface = "italic")

# Combine 
fig_3b_scatter = p1_slope_maj + p2_slope_maj + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

fig_3b_scatter

# Save figure
ggsave(
  filename = paste0(fig_dir, 'majority_wasted_votes_efficiency_gap_scatter.pdf'),
  plot = fig_3b_scatter,
  width = 12,
  height = 6,
  dpi = 300
)


# --------------- COMBINED FIGURE - BAR CHARTS --------------------- 
# 2x2 layout: minority on top, majority on bottom

# Adjust titles for combined figure
p_wasted_minority_combined = p_wasted_min + 
  labs(title = "Minority: Wasted Votes") +
  theme(legend.position = "none")

p_gap_minority_combined = p_gap_min + 
  labs(title = "Minority: Efficiency Gap") +
  theme(legend.position = "none")

p_wasted_majority_combined = p_wasted_maj + 
  labs(title = "Majority: Wasted Votes") +
  theme(legend.position = "none")

p_gap_majority_combined = p_gap_maj + 
  labs(title = "Majority: Efficiency Gap") +
  theme(legend.position = "none")

# Create 2x2 combined figure
combined_bar = (p_wasted_minority_combined | p_gap_minority_combined) /
  (p_wasted_majority_combined | p_gap_majority_combined) +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

combined_bar

# Save combined bar chart figure
ggsave(
  filename = paste0(fig_dir, 'combined_wasted_votes_efficiency_gap_bar.pdf'),
  plot = combined_bar,
  width = 14,
  height = 10,
  dpi = 300
)


# --------------- COMBINED FIGURE - SCATTER PLOTS --------------------- 
# 2x2 layout: minority on top, majority on bottom

# Adjust titles for combined figure
p1_slope_minority_combined = p1_slope_min + 
  labs(title = "Minority: Wasted Votes")

p2_slope_minority_combined = p2_slope_min + 
  labs(title = "Minority: Efficiency Gap")

p1_slope_majority_combined = p1_slope_maj + 
  labs(title = "Majority: Wasted Votes")

p2_slope_majority_combined = p2_slope_maj + 
  labs(title = "Majority: Efficiency Gap")

# Create 2x2 combined figure
combined_scatter = (p1_slope_minority_combined | p2_slope_minority_combined) /
  (p1_slope_majority_combined | p2_slope_majority_combined) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "Wasted Votes and Efficiency Gap by Group",
                  theme = theme(plot.title = element_text(size = 16, face = "bold"))) &
  theme(legend.position = "bottom")

combined_scatter

# Save combined scatter plot figure
ggsave(
  filename = paste0(fig_dir, 'combined_wasted_votes_efficiency_gap_scatter.pdf'),
  plot = combined_scatter,
  width = 14,
  height = 10,
  dpi = 300
)




# --------------- Figure 3b --------------------- 
# Wasted votes by racial group
# (minority wasted votes - majority wasted votes) / total votes

# Calculate map-level summaries with the new metric
map_summary_racial = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    # Minority wasted votes
    total_minority_wasted = sum(minority_wasted_total, na.rm = TRUE),
    total_minority_votes = sum(total_minority),
    
    # Majority wasted votes  
    total_majority_wasted = sum(majority_wasted_total, na.rm = TRUE),
    total_majority_votes = sum(total_majority),
    
    # Total votes
    total_votes = total_minority_votes + total_majority_votes,
    
    # New metric: racial vote inefficiency disparity
    racial_inefficiency_disparity = (total_minority_wasted - total_majority_wasted) / total_votes,
    
    # Also calculate proportion wasted for each group (for comparison)
    prop_minority_wasted = total_minority_wasted / total_minority_votes,
    prop_majority_wasted = total_majority_wasted / total_majority_votes,
    
    .groups = 'drop'
  ) %>%
  mutate(
    # Add numeric version for x-axis
    redist_numeric = case_when(
      redist_level == "Dem" ~ 1,
      redist_level == "Neutral" ~ 2,
      redist_level == "Rep" ~ 3
    )
  )

# Create summary for plotting (means for lines)
plot_summary_racial = map_summary_racial %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    mean_racial_disparity = mean(racial_inefficiency_disparity, na.rm = TRUE),
    mean_prop_minority_wasted = mean(prop_minority_wasted),
    mean_prop_majority_wasted = mean(prop_majority_wasted),
    n_maps = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    redist_numeric = case_when(
      redist_level == "Dem" ~ 1,
      redist_level == "Neutral" ~ 2,
      redist_level == "Rep" ~ 3
    )
  )

# Panel 1: Proportion of votes wasted by group (for comparison)
p1_racial = ggplot() +
  # Minority wasted votes
  geom_point(data = map_summary_racial,
             aes(x = redist_numeric, y = prop_minority_wasted, color = seg_level),
             alpha = 0.1, size = 1.5,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  geom_line(data = plot_summary_racial,
            aes(x = redist_numeric, y = mean_prop_minority_wasted, 
                group = seg_level, color = seg_level),
            linewidth = 1.5, linetype = "solid") +
  geom_point(data = plot_summary_racial,
             aes(x = redist_numeric, y = mean_prop_minority_wasted, color = seg_level),
             size = 4, shape = 16) +
  
  # Majority wasted votes (dashed lines)
  geom_point(data = map_summary_racial,
             aes(x = redist_numeric, y = prop_majority_wasted, color = seg_level),
             alpha = 0.05, size = 1.5,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  geom_line(data = plot_summary_racial,
            aes(x = redist_numeric, y = mean_prop_majority_wasted, 
                group = seg_level, color = seg_level),
            linewidth = 1.5, linetype = "dashed") +
  geom_point(data = plot_summary_racial,
             aes(x = redist_numeric, y = mean_prop_majority_wasted, color = seg_level),
             size = 4, shape = 17) +
  
  scale_x_continuous(breaks = 1:3, labels = c("Dem", "Neutral", "Rep")) +
  scale_color_viridis_d(option = 'C', name = "Segregation",
                        labels = c("Low", "Medium", "High"),
                        breaks = c("Low", "Medium", "High")) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Gerrymander", 
       y = "Proportion of Votes Wasted",
       title = "Vote Waste by Racial Group") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  # Add legend for line types
  annotate("text", x = 2.5, y = max(map_summary_racial$prop_minority_wasted) * 0.95, 
           label = "Solid = Minority\nDashed = Majority", 
           hjust = 0, vjust = 1, size = 3, fontface = "italic")

# Panel 2: Racial vote inefficiency disparity
p2_racial = ggplot() +
  # Zero reference line
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  # Raw data points with jitter
  geom_point(data = map_summary_racial,
             aes(x = redist_numeric, y = racial_inefficiency_disparity, color = seg_level),
             alpha = 0.1, size = 1.5,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean lines
  geom_line(data = plot_summary_racial,
            aes(x = redist_numeric, y = mean_racial_disparity, 
                group = seg_level, color = seg_level),
            linewidth = 1.5,
            position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  # Mean points
  geom_point(data = plot_summary_racial,
             aes(x = redist_numeric, y = mean_racial_disparity, color = seg_level),
             size = 4,
             position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.2)) +
  scale_x_continuous(breaks = 1:3, labels = c("Dem", "Neutral", "Rep")) +
  scale_color_viridis_d(option = 'C', name = "Segregation",
                        labels = c("Low", "Medium", "High"),
                        breaks = c("Low", "Medium", "High")) +
  labs(x = "Gerrymander", 
       y = "Racial Vote Inefficiency Disparity",
       title = "Racial Disparity in Vote Efficiency") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank()) +
  # Add interpretation note
  annotate("text", x = 1, y = max(map_summary_racial$racial_inefficiency_disparity) * 0.95, 
           label = "Positive = Minority disadvantage", 
           hjust = -0.5, vjust = 1, size = 3, fontface = "italic")

# Combine panels
fig_racial_disparity = p1_racial + p2_racial + 
  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

# Display the figure
fig_racial_disparity

# Print summary statistics for interpretation
cat("\nSummary of Racial Vote Inefficiency Disparity:\n")
plot_summary_racial %>%
  select(seg_level, redist_level, mean_racial_disparity) %>%
  pivot_wider(names_from = redist_level, values_from = mean_racial_disparity) %>%
  print()

cat("\nInterpretation: Positive values indicate minority votes are wasted at a higher rate than majority votes\n")




# --------------- Figure 4 --------------------- 
# Correlation between RPV and wasted votes

fig_4_df = df %>%
  select(
    map_id, district_id, seg_level, redist_level, n_dem_seats, er_rpv, ei_rpv,
    starts_with('true'), starts_with('total'), contains('wasted')
    )

fig_4 = ggplot(fig_4_df) +
  geom_point(aes(x = minority_wasted_total, y = true_rpv), alpha = 0.05, pch = 1) +
  geom_smooth(aes(x = minority_wasted_total, y = true_rpv, color = 'True', fill = 'True'), 
              method = "glm", 
              method.args = list(family = "binomial"),
              linewidth = 1.2) +
  geom_smooth(aes(x = minority_wasted_total, y = er_rpv, color = 'ER', fill = 'ER'), 
              method = "glm", 
              method.args = list(family = "binomial"),
              linewidth = 1.2) +
  geom_smooth(aes(x = minority_wasted_total, y = ei_rpv, color = 'EI', fill = 'EI'),
              method = "glm",
              method.args = list(family = "binomial"),
              linewidth = 1.2) +
  scale_color_manual(name = 'Model', values = c("EI" = "#adf1a0", "ER" = "#c3a4cf", 'True' = 'black')) +
  scale_fill_manual(name = 'Model', values = c("EI" = "#adf1a0", "ER" = "#c3a4cf", 'True' = 'black')) +
  labs(x = 'Total Minority Wasted Votes', y = 'RPV') +
  facet_grid(seg_level ~ redist_level) +
  theme_bw()

fig_4

ggsave(
  filename = paste0(fig_dir, 'rpv_vs_minority_wasted_votes.pdf'),
  plot = fig_4,
  width = 14,
  height = 10,
  dpi = 300
)


# --------------- Figure 5 --------------------- 
# majority minority districts

fig_5_df = df %>%
  select(map_id, district_id, n_dem_seats, n_maj_min, prop_minority, seg_level, redist_level)

fig_5 = ggplot(fig_5_df, aes(x = n_maj_min)) +
  geom_histogram(fill = 'black') +
  facet_grid(seg_level ~ redist_level) +
  labs(x = '# Majority-Minority Districts', y = 'Count') +
  theme_bw()

fig_5

ggsave(
  filename = paste0(fig_dir, 'num_maj_min_dist_by_scenario.pdf'),
  plot = fig_5,
  width = 14,
  height = 10,
  dpi = 300
)

# --------------- Figure 6 --------------------- 
# Correlation between majority minority districts and partisan balance (do you get more of these in republican maps?)

fig_6_df = df %>%
  select(map_id, district_id, n_dem_seats, n_maj_min, prop_minority, seg_level, redist_level)  

fig_6 = ggplot(fig_6_df, aes(x = n_maj_min, y = n_dem_seats)) +
  geom_point(position = position_jitter(width = 0.1, height = 0), alpha = 0.025, pch = 1) +
  geom_smooth(method = 'loess', se = FALSE, color = 'black') +
  labs(x = '# Majority-Minority Districts', y = '# Democratic Seats') +
  scale_color_viridis_d() +
  scale_y_continuous(limits = c(4, 14), breaks = 4:14) +
  # facet_grid(seg_level ~ redist_level) +
  theme_bw() 

fig_6

ggsave(
  filename = paste0(fig_dir, 'dem_seats_vs_majority_minority_districts.pdf'),
  plot = fig_6, 
  width = 8,
  height = 6,
  dpi = 300
)


fig_6a = fig_6_df %>%
  ggplot(aes(x = n_maj_min, y = n_dem_seats)) +
  geom_point(aes(color = redist_level), alpha = 0.75, size = 2, pch = 1, position = position_jitter(width = 0.1, height = 0.1)) +
  labs(x = '# Majority-Minority Districts', 
       y = '# Democratic Seats',
       color = 'Gerrymander') +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey40", "Rep" = "#b31529")) +
  facet_wrap(~ seg_level, nrow = 1, 
             labeller = labeller(seg_level = c("Low" = "Low Segregation",
                                               "Medium" = "Medium Segregation",
                                               "High" = "High Segregation"))) +
  theme_bw() +
  theme(legend.position = 'bottom')

fig_6a

ggsave(
  filename = paste0(fig_dir, 'dem_seats_vs_majority_minority_districts_by_seg_level.pdf'),
  plot = fig_6a, 
  width = 8,
  height = 6,
  dpi = 300
)

# --------------- Figure 7 --------------------- 
# % of the time black voters wind up in Republican districts

# Create dataset with all Republican districts (no averaging)
fig_7_df = df %>%
  filter(prop_dem < 0.5) %>%  # Keep only Republican districts
  select(map_id, district_id, prop_minority, seg_level, redist_level)

# Plot the distribution of minority proportions in Republican districts
fig_7 = ggplot(fig_7_df, aes(x = prop_minority, y = seg_level, fill = seg_level)) +
  stat_binline(bins = 50, alpha = 0.75, show.legend = FALSE) +
  facet_wrap(~ redist_level, scales = 'free_y',
             nrow = 1, 
             labeller = labeller(
               redist_level = c(
                 "Dem" = "Democratic Gerrymander",
                 "Neutral" = "Neutral",
                 "Rep" = "Republican Gerrymander"
               )
             )) +
  labs(x = '% Minority Voters in Republican Districts', y = 'Segregation Level') +
  theme_ridges() +
  scale_fill_viridis_d(
    option = 'C',
    name = "Segregation",
    labels = c("Low", "Medium", "High"),
    breaks = c("Low", "Medium", "High")
  ) +
  theme(
    strip.text = element_text(size = 11, margin = margin(3, 0, 3, 0)),
    strip.background = element_rect(fill = "grey90"),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
  )

fig_7

ggsave(
  filename = paste0(fig_dir, 'minority_share_in_rep_districts_by_scenario.pdf'),
  plot = fig_7, 
  width = 8,
  height = 6,
  dpi = 300
)


# --------------- Figure 8 --------------------- 
# True RPV by partisan balance (do we want to compare whether minority voters are worst off in only the most extreme Republican maps to the less extreme ones or split chambers?)


  



# --------------- Figure 9 --------------------- 
# MAE by scenario
# Calculate MAE and RPV accuracy by scenario
mae_summary = df %>%
  group_by(seg_level, redist_level, map_id) %>%
  summarise(
    # ER metrics
    mae_minority_er = mean(abs(er_minority_resid)),
    mae_majority_er = mean(abs(er_majority_resid)),
    rpv_accuracy_er = mean(er_rpv_correct),
    bias_minority_er = mean(er_minority_resid),
    bias_majority_er = mean(er_majority_resid),
    
    # EI metrics
    mae_minority_ei = mean(abs(ei_minority_resid)),
    mae_majority_ei = mean(abs(ei_majority_resid)),
    rpv_accuracy_ei = mean(ei_rpv_correct),
    bias_minority_ei = mean(ei_minority_resid),
    bias_majority_ei = mean(ei_majority_resid),
    
    .groups = 'drop'
  )

# Reshape for plotting
mae_long = mae_summary %>%
  pivot_longer(
    cols = -c(seg_level, redist_level, map_id),
    names_to = c("metric", "method"),
    names_pattern = "(.*)_(er|ei)",
    values_to = "value"
  ) %>%
  mutate(method = toupper(method))

# Calculate summary statistics with confidence intervals
mae_summary_ci = mae_long %>%
  group_by(seg_level, redist_level, method, metric) %>%
  summarise(
    n_maps = n(),
    mean_value = mean(value),
    lower_ci = quantile(value, 0.025),
    upper_ci = quantile(value, 0.975),
    .groups = 'drop'
  )

# Figure: MAE Comparison
mae_plot_data = mae_summary_ci %>%
  filter(metric %in% c("mae_minority", "mae_majority")) %>%
  mutate(
    group = ifelse(grepl("minority", metric), "Minority", "Majority"),
    seg_level = factor(seg_level, levels = c('Low', 'Medium', 'High')),
    redist_level = factor(redist_level, levels = c('Dem', 'Neutral', 'Rep'))
  )

fig_9 = ggplot(mae_plot_data, aes(x = seg_level, y = mean_value, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  geom_point(position = position_dodge(0.3), size = 4) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(0.3), 
                width = 0.2) +
  facet_grid(group ~ redist_level) +
  labs(title = "",
       x = "Segregation Level",
       y = "Mean Absolute Error",
       color = "Model") +
  scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

fig_9

ggsave(paste0(fig_dir, "mae_comparison.pdf"), fig_9, width = 12, height = 10)


# --------------- Figure 10 ---------------------
# RPV classification accuracy by scenario

rpv_plot_data = mae_summary_ci %>%
  filter(metric == "rpv_accuracy") %>%
  mutate(
    seg_level = factor(seg_level, levels = c('Low', 'Medium', 'High')),
    redist_level = factor(redist_level, levels = c('Dem', 'Neutral', 'Rep'))
  )

fig_10 = ggplot(rpv_plot_data, aes(x = seg_level, y = mean_value, color = method)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray") +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), 
                position = position_dodge(0.3), 
                width = 0.2) +
  facet_wrap(~ redist_level) +
  labs(x = "Segregation Level", 
       y = "Accuracy",
       color = "Method") +
  scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(0, 1)

fig_10

ggsave(paste0(fig_dir, "rpv_classification_cleaned.pdf"), fig_10, width = 12, height = 8)

# --------------- Figure 11 & 12 --------------------
# Scatter Plots of Raw Estimates

# Prepare data for scatter plots
scatter_data = df %>%
  select(seg_level, redist_level, 
         true_minority_dem_share, true_majority_dem_share,
         er_minority_dem_share, er_majority_dem_share,
         ei_minority_dem_share, ei_majority_dem_share) 

# Reshape for plotting
er_scatter = scatter_data %>%
  select(-starts_with("ei_")) %>%
  rename(estimate_minority = er_minority_dem_share,
         estimate_majority = er_majority_dem_share) %>%
  mutate(method = "ER")

ei_scatter = scatter_data %>%
  select(-starts_with("er_")) %>%
  rename(estimate_minority = ei_minority_dem_share,
         estimate_majority = ei_majority_dem_share) %>%
  mutate(method = "EI")

combined_scatter = bind_rows(er_scatter, ei_scatter)

# Minority scatter plot
fig_11 = ggplot(combined_scatter, 
                            aes(x = true_minority_dem_share * 100, y = estimate_minority * 100, color = method)) +
  geom_point(alpha = 0.1, size = 2, shape = 21) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_grid(seg_level ~ redist_level) + 
  # facet_grid(redist_level ~ factor(seg_level, levels = c('Low', 'Medium', 'High'))) +
  labs(
    x = "True Minority Democratic Share",
    y = "Estimated Minority Democratic Share",
    color = "Model"
  ) +
  theme_custom() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = 'Dark2') +
  # scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100))

fig_11

ggsave(paste0(fig_dir, "New_07_09/raw_voteshares_minority.pdf"), 
       fig_11, width = 10, height = 8)

# Majority scatter plot
fig_12 = ggplot(combined_scatter, 
                            aes(x = true_majority_dem_share * 100, y = estimate_majority * 100, color = method)) +
  geom_point(alpha = 0.1, size = 2, shape = 21) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  facet_grid(redist_level ~ factor(seg_level, levels = c('Low', 'Medium', 'High'))) +
  labs(
    x = "True Majority Democratic Share",
    y = "Estimated Majority Democratic Share",
    color = "Model"
  ) +
  theme_custom() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = 'Dark2') +
  # scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  coord_fixed(xlim = c(0, 100), ylim = c(0, 100))

fig_12

ggsave(paste0(fig_dir, "New_07_09/raw_voteshares_majority.pdf"), 
       fig_12, width = 10, height = 8)



# Facetted by dem margin
# Prepare data for scatter plots
scatter_data = df %>% 
  ungroup() %>%
  select(seg_level, redist_level, 
         true_minority_dem_share, true_majority_dem_share,
         er_minority_dem_share, er_majority_dem_share,
         ei_minority_dem_share, ei_majority_dem_share, 
         total_dem_votes, total_rep_votes, total_population) 

# Reshape for plotting
er_scatter = scatter_data %>%
  select(-starts_with("ei_")) %>%
  rename(estimate_minority = er_minority_dem_share,
         estimate_majority = er_majority_dem_share) %>%
  mutate(method = "ER")

ei_scatter = scatter_data %>%
  select(-starts_with("er_")) %>%
  rename(estimate_minority = ei_minority_dem_share,
         estimate_majority = ei_majority_dem_share) %>%
  mutate(method = "EI")

combined_scatter = bind_rows(er_scatter, ei_scatter) %>%
  mutate(dem_margin = (total_dem_votes - total_rep_votes) / total_population * 100)


min_est_margin = ggplot(combined_scatter,
                        aes(x = dem_margin, y = (estimate_minority - true_minority_dem_share) * 100, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(alpha = 0.1, size = 2, shape = 21) +
  facet_grid(redist_level ~ factor(seg_level, levels = c('Low', 'Medium', 'High'))) +
  labs(
    x = "Democratic Win Margin",
    y = "Estimated - True Minority Democratic Voteshare",
    color = "Model"
  ) +
  theme_custom() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = 'Dark2') 

min_est_margin

ggsave(paste0(fig_dir, "New_07_09/minority_est_vs_true_spread_vs_dem_margin.pdf"), 
       min_est_margin, width = 10, height = 8)


maj_est_margin = ggplot(combined_scatter,
                        aes(x = dem_margin, y = (estimate_majority - true_majority_dem_share) * 100, color = method)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey") +
  geom_point(alpha = 0.1, size = 2, shape = 21) +
  facet_grid(redist_level ~ factor(seg_level, levels = c('Low', 'Medium', 'High'))) +
  labs(
    x = "Democratic Win Margin",
    y = "Estimated - True Majority Democratic Voteshare",
    color = "Model"
  ) +
  theme_custom() +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_brewer(palette = 'Dark2') 

maj_est_margin

ggsave(paste0(fig_dir, "New_07_09/majority_est_vs_true_spread_vs_dem_margin.pdf"), 
       maj_est_margin, width = 10, height = 8)


# --------------- Figure 13 ---------------------
# Scenario effect on error

# Prepare data for regression
reg_data = df %>%
  mutate(
    # Create error variables
    error_minority_er = abs(er_minority_resid) * 100,
    error_minority_ei = abs(ei_minority_resid) * 100,
    
    # Create dummy variables
    seg_medium = as.numeric(seg_level == "Medium"),
    seg_high = as.numeric(seg_level == "High"),
    redis_dem = as.numeric(redist_level == "Dem"),
    redis_rep = as.numeric(redist_level == "Rep"),
    
    # Margin
    dem_margin = (total_dem_votes - total_rep_votes) / total_population * 100,
    
    # District White pop % - State White pop %
    white_pop_diff = ((total_majority / total_population) - (752332 / 1003109)) * 100,
    
    # True RPV type
    rpv_type = factor(case_when(
      true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5 ~ 'Type 1',
      true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5 ~ 'Type 2',
      TRUE ~ 'None'
    ), 
    levels = c('None', 'Type 1', 'Type 2')),
    
    rpv_type_1 = ifelse(true_minority_dem_share > 0.5 & true_majority_dem_share < 0.5, 1, 0),
    rpv_type_2 = ifelse(true_minority_dem_share < 0.5 & true_majority_dem_share > 0.5, 1, 0)
    
  )

# Run separate regressions for ER and EI
model_er = lm(error_minority_er ~ seg_medium + seg_high + redis_dem + redis_rep + dem_margin + white_pop_diff, 
              data = reg_data)
model_ei = lm(error_minority_ei ~ seg_medium + seg_high + redis_dem + redis_rep + dem_margin + white_pop_diff, 
              data = reg_data)

# Run separate regressions for ER and EI
model_er = lm(error_minority_er ~ seg_medium + seg_high 
                                  + redis_dem + redis_rep 
                                  + dem_margin + white_pop_diff
                                  + true_rpv, 
              data = reg_data)
model_ei = lm(error_minority_ei ~ seg_medium + seg_high 
                                  + redis_dem + redis_rep 
                                  + dem_margin + white_pop_diff
                                  + true_rpv, 
              data = reg_data)

stargazer::stargazer(model_er, model_ei, type = 'text')

# Extract coefficients
coef_er = tidy(model_er, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(method = "ER")

coef_ei = tidy(model_ei, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(method = "EI")

# Combine coefficients
coef_df = bind_rows(coef_er, coef_ei) %>%
  mutate(
    term_clean = case_when(
      term == "seg_medium" ~ "Medium\nSegregation",
      term == "seg_high" ~ "High\nSegregation", 
      term == "redis_dem" ~ "Democratic\nGerrymander",
      term == "redis_rep" ~ "Republican\nGerrymander",
      term == 'dem_margin' ~ "Dem. Margin",
      term == 'white_pop_diff' ~ "District % White\n- State % White"
    )
  )

# Add reference category
reference_df = data.frame(
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

coef_df = bind_rows(reference_df, coef_df)

# Set factor levels
coef_df$term_clean = factor(coef_df$term_clean, 
                            levels = c("District % White\n- State % White",
                                       "Dem. Margin",
                                       "High\nSegregation",
                                       "Medium\nSegregation", 
                                       "Democratic\nGerrymander",
                                       "Republican\nGerrymander",
                                       "Low Segregation,\nNeutral Gerrymander"))

# Create the effects plot
fig_13 = ggplot(coef_df, aes(x = term_clean, y = estimate, color = method)) +
  geom_point(position = position_dodge(width = 0.3), size = 6) +
  geom_errorbar(data = filter(coef_df, term != "reference"),
                aes(ymin = conf.low, ymax = conf.high), 
                position = position_dodge(width = 0.3), width = 0.2) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_flip() +
  labs(x = "", 
       y = "Change in Absolute Error",
       color = "Method") +
  scale_color_brewer(palette = 'Dark2') +
  # scale_color_manual(values = c("ER" = "#CC78BC", "EI" = "#029E73")) +
  theme_custom() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

fig_13

ggsave(paste0(fig_dir, "New_07_09/effects_on_error_with_controls.pdf"), fig_13, width = 12, height = 10)




# --------------- Wasted Votes Breakdown: Majority vs Minority and Surplus vs Losing --------------------- 

# Pivot to get all four vote categories
vote_breakdown = df %>%
  select(map_id, district_id, seg_level, redist_level, prop_minority,
         minority_wasted_surplus, minority_wasted_losing,
         majority_wasted_surplus, majority_wasted_losing,
         total_minority, total_majority) %>%
  pivot_longer(cols = c('minority_wasted_surplus', 'minority_wasted_losing',
                        'majority_wasted_surplus', 'majority_wasted_losing'),
               names_to = c('group', 'vote_type'),
               names_pattern = '(minority|majority)_wasted_(surplus|losing)',
               values_to = 'votes') %>%
  mutate(
    # Calculate percentages
    total = if_else(group == "minority", total_minority, total_majority),
    percent = votes / total * 100,
    seg_level = factor(seg_level, levels = c('High', 'Medium', 'Low')),
    # Create combined y variable
    seg_vote_combo = paste(seg_level, vote_type, sep = " - "),
    # Order the factor levels logically
    seg_vote_combo = factor(seg_vote_combo, 
                            levels = c("Low - surplus", "Low - losing",
                                       "Medium - surplus", "Medium - losing", 
                                       "High - surplus", "High - losing"),
                            labels = c("Low - Surplus", "Low - Losing",
                                       "Medium - Surplus", "Medium - Losing", 
                                       "High - Surplus", "High - Losing"))
  ) %>% 
  ungroup()

# Create the plot
wasted_breakdown_group_fig = ggplot(vote_breakdown, aes(x = percent, y = seg_vote_combo, fill = group)) + 
  stat_binline(alpha = 0.7, scale = 1) +
  facet_wrap(~ redist_level) +
  theme_bw() +
  scale_fill_manual(values = c("majority" = "#E74C3C",
                               "minority" = "#3498DB"),
                    labels = c("Majority", "Minority")) +
  labs(x = "Percent of Group's Votes",
       y = "Segregation Level - Vote Type",
       fill = "Group")

wasted_breakdown_group_fig

ggsave(paste0(fig_dir, "wasted_votes_by_type_scenario_and_group.pdf"), wasted_breakdown_group_fig, width = 12, height = 10)







# Pivot to get all eight vote categories (majority/minority × dem/rep × surplus/losing)
vote_breakdown = df %>%
  select(map_id, district_id, seg_level, redist_level, prop_minority,
         minority_dem_wasted_surplus, minority_dem_wasted_losing,
         minority_rep_wasted_surplus, minority_rep_wasted_losing,
         majority_dem_wasted_surplus, majority_dem_wasted_losing,
         majority_rep_wasted_surplus, majority_rep_wasted_losing,
         total_minority, total_majority) %>%
  pivot_longer(cols = c('minority_dem_wasted_surplus', 'minority_dem_wasted_losing',
                        'minority_rep_wasted_surplus', 'minority_rep_wasted_losing',
                        'majority_dem_wasted_surplus', 'majority_dem_wasted_losing',
                        'majority_rep_wasted_surplus', 'majority_rep_wasted_losing'),
               names_to = c('group', 'party', 'vote_type'),
               names_pattern = '(minority|majority)_(dem|rep)_wasted_(surplus|losing)',
               values_to = 'votes') %>%
  mutate(
    # Calculate percentages
    total = if_else(group == "minority", total_minority, total_majority),
    percent = votes / total * 100,
    seg_level = factor(seg_level, levels = c('High', 'Medium', 'Low')),
    # Create combined variables
    seg_vote_combo = paste(seg_level, vote_type, sep = " - "),
    seg_vote_combo = factor(seg_vote_combo, 
                            levels = c("Low - surplus", "Low - losing",
                                       "Medium - surplus", "Medium - losing", 
                                       "High - surplus", "High - losing"),
                            labels = c("Low - Surplus", "Low - Losing",
                                       "Medium - Surplus", "Medium - Losing", 
                                       "High - Surplus", "High - Losing")),
    # Create fill variable combining group and party
    group_party = paste(group, party, sep = "_")
  ) %>% 
  ungroup()

# Create combined group_party variable
vote_breakdown = vote_breakdown %>%
  mutate(group_party = paste(party, group, sep = "_"))

wasted_breakdown_party_fig = ggplot(vote_breakdown, aes(x = percent, y = seg_vote_combo, fill = group_party)) + 
  stat_binline(alpha = 0.7, scale = 1) +
  facet_wrap(~ redist_level) +
  theme_bw() +
  scale_fill_manual(values = c("dem_majority" = "#3498DB",      # Full blue for majority Dem
                               "dem_minority" = "#85C1E2",      # Light blue for minority Dem
                               "rep_majority" = "#E74C3C",      # Full red for majority Rep
                               "rep_minority" = "#F1948A"),     # Light red for minority Rep
                    labels = c("Dem Majority", "Dem Minority", 
                               "Rep Majority", "Rep Minority")) +
  labs(x = "Percent of Group's Votes",
       y = "Segregation Level - Vote Type",
       fill = "Group-Party")

wasted_breakdown_party_fig

ggsave(paste0(fig_dir, "wasted_votes_by_type_scenario_group_and_party.pdf"), wasted_breakdown_party_fig, width = 12, height = 10)


# Calculate net wasted votes (Republican wasted - Democrat wasted) for each group
efficiency_gap = df %>%
  mutate(
    minority_net_wasted = (minority_rep_wasted_surplus + minority_rep_wasted_losing) - 
      (minority_dem_wasted_surplus + minority_dem_wasted_losing),
    majority_net_wasted = (majority_rep_wasted_surplus + majority_rep_wasted_losing) - 
      (majority_dem_wasted_surplus + majority_dem_wasted_losing),
    minority_net_percent = minority_net_wasted / total_minority * 100,
    majority_net_percent = majority_net_wasted / total_majority * 100
  ) %>%
  select(seg_level, redist_level, minority_net_percent, majority_net_percent) %>%
  pivot_longer(cols = c(minority_net_percent, majority_net_percent),
               names_to = "group",
               values_to = "net_wasted_percent") %>%
  mutate(group = if_else(group == "minority_net_percent", "Minority", "Majority"))

ggplot(efficiency_gap, aes(x = net_wasted_percent, y = seg_level, color = group)) +
  geom_point(alpha = 0.1, position = position_jitterdodge(jitter.width = 0.25,
                                                          dodge.width = 0.75)) +
  facet_wrap(~ redist_level) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  theme_bw() +
  labs(x = "Net Wasted Votes (% of Group Total)\n← More Rep Wasted | More Dem Wasted →",
       y = "Segregation Level",
       fill = "Voter Group") +
  scale_color_manual(values = c("Minority" = "#8E44AD", "Majority" = "#27AE60"))



waste_comparison = df %>%
  mutate(
    minority_surplus_total = minority_dem_wasted_surplus + minority_rep_wasted_surplus,
    minority_losing_total = minority_dem_wasted_losing + minority_rep_wasted_losing,
    majority_surplus_total = majority_dem_wasted_surplus + majority_rep_wasted_surplus,
    majority_losing_total = majority_dem_wasted_losing + majority_rep_wasted_losing,
    minority_surplus_pct = minority_surplus_total / total_minority * 100,
    minority_losing_pct = minority_losing_total / total_minority * 100,
    majority_surplus_pct = majority_surplus_total / total_majority * 100,
    majority_losing_pct = majority_losing_total / total_majority * 100
  ) %>%
  select(seg_level, redist_level, ends_with("_pct")) %>%
  pivot_longer(cols = ends_with("_pct"),
               names_to = c("group", "type"),
               names_pattern = "(minority|majority)_(surplus|losing)_pct",
               values_to = "percent")

ggplot(waste_comparison, aes(x = seg_level, y = percent, color = group)) +
  geom_point(position = position_jitter(height = 0, width = 0.25), 
             alpha = 0.1, size = 1, pch = 1) +
  facet_grid(type ~ redist_level, scales = "free_y") +
  theme_bw() +
  labs(x = "Segregation Level",
       y = "Wasted Votes (% of Group Total)",
       fill = "Voter Group") +
  scale_color_manual(values = c("minority" = "#8E44AD", "majority" = "#27AE60"))


# Calculate ratio of minority to majority wasted vote rates
waste_ratio = df %>%
  mutate(
    minority_total_wasted = minority_wasted_total / total_minority,
    majority_total_wasted = majority_wasted_total / total_majority,
    waste_ratio = minority_total_wasted / majority_total_wasted,
    log_waste_ratio = log2(waste_ratio)  # Log scale for symmetry around 1
  )

ggplot(waste_ratio, aes(x = seg_level, y = log_waste_ratio, fill = redist_level)) +
  geom_violin(alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ redist_level) +
  theme_bw() +
  labs(x = "Segregation Level",
       y = "Log2(Minority Waste Rate / Majority Waste Rate)\n← Majority More Affected | Minority More Affected →") +
  scale_fill_manual(values = c("Dem" = "#3498DB", "Neutral" = "#95A5A6", "Rep" = "#E74C3C"))


# Question 1: How does partisan gerrymandering affect each group?
partisan_effect = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    minority_waste_rate = mean(minority_wasted_total / total_minority * 100),
    majority_waste_rate = mean(majority_wasted_total / total_majority * 100),
    .groups = 'drop'
  ) %>%
  pivot_longer(cols = ends_with("_rate"),
               names_to = "group",
               values_to = "waste_rate")

ggplot(partisan_effect, aes(x = redist_level, y = waste_rate, 
                            color = group, group = group)) +
  geom_line(size = 1.5) +
  geom_point(size = 3) +
  facet_wrap(~ seg_level) +
  theme_bw() +
  labs(x = "Redistricting Type",
       y = "Average Wasted Vote Rate (%)",
       color = "Voter Group",
       title = "How Partisan Gerrymandering Affects Waste Rates by Segregation Level")













# ------------- GENERAL ----------------

# --- Dem vote share vs minority population share density 
gen_p1 = ggplot(df, aes(prop_minority, prop_dem)) +
  coord_equal() + 
  geom_density_2d_filled(contour_var = "ndensity", alpha = 0.8, bins = 8) +  # specify bins
  facet_grid(seg_level ~ redist_level) +
  labs(y = 'Democratic Voteshare', x = 'Minority Population Share') +
  scale_fill_brewer(palette = 'Reds') + 
  theme_custom() +
  guides(fill = 'none')


gen_p1

ggsave(paste0(fig_dir, "New_07_09/dem_share_vs_minority_share_contour.pdf"), gen_p1, width = 12, height = 10)


# --- Dem vote share vs minority population share heatmap 
gen_p2 = ggplot(df, aes(prop_minority, prop_dem)) +
  coord_equal() + 
  stat_density_2d(aes(fill = after_stat(ndensity)), 
                  geom = "raster", 
                  contour = FALSE) +
  facet_grid(seg_level ~ redist_level) +
  labs(y = 'Democratic Voteshare', x = 'Minority Population Share') +
  scale_fill_distiller(palette = 'Reds', direction = 1) +  
  guides(fill = 'none') +
  theme_custom()

gen_p2

ggsave(paste0(fig_dir, "New_07_09/dem_share_vs_minority_share_heat.pdf"), gen_p2, width = 12, height = 10)


# --- Dem vote share vs minority population share hex 
gen_p3 = ggplot(df, aes(prop_minority, prop_dem)) +
  coord_equal() + 
  geom_hex() +
  facet_grid(seg_level ~ redist_level) +
  labs(y = 'Democratic Voteshare', x = 'Minority Population Share') +
  scale_fill_distiller(palette = 'Reds', direction = 1) +  
  guides(fill = 'none') +
  theme_custom()

gen_p3

ggsave(paste0(fig_dir, "New_07_09/dem_share_vs_minority_share_hex.pdf"), gen_p3, width = 12, height = 10)


# ------------- Efficiency Gap -------------


wasted_data = df %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise( # Take the mean waste disparity across districts in each map
    mean_waste_disparity = mean(waste_disparity, na.rm = TRUE),
    n_dem_seats = first(n_dem_seats),
    .groups = 'drop') %>%
  # Reverse seg_level
  mutate(seg_level = factor(
    seg_level,
    levels = c("Low", "Medium", "High"),
    labels = c("Low Segregation", "Medium Segregation", "High Segregation")
  ))

# --- Raw waste disparity at district level by scenario
wasted_disp_p1 = ggplot(df, aes(x = redist_level, y = waste_disparity)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1, shape = 21) +
  stat_summary(fun.data = mean_se, geom = "pointrange", 
               color = RColorBrewer::brewer.pal(3, name = 'Dark2')[2], size = 1) + 
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High")), 
             labeller = as_labeller(c(
               "Low" = "Low Segregation",
               "Medium" = "Medium Segregation", 
               "High" = "High Segregation"
             ))) +
  scale_x_discrete(labels = c("Democratic", "Neutral", "Republican")) +
  labs(x = "Gerrymander",
       y = "Wasted Vote Disparity (Minority - Majority)",
       color = 'District Winner') +
  theme_custom()

wasted_disp_p1

ggsave(paste0(fig_dir, "New_07_09/wasted_vote_disparity_raw_district.pdf"), wasted_disp_p1, width = 12, height = 10)


# --- Map-level average wasted vote disparity
wasted_disp_p2 = ggplot(wasted_data, aes(x = mean_waste_disparity, y = seg_level, color = redist_level, fill = redist_level)) +
  stat_binline(scale = 0.9, bins = 100, alpha = 0.8, draw_baseline = FALSE) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  labs(x = 'Mean Disparity in Wasted Votes', y = '', fill = 'Gerrymander', color = 'Gerrymander') +
  theme_custom()

wasted_disp_p2


ggsave(paste0(fig_dir, "New_07_09/wasted_vote_disparity_map_average_by_seg.pdf"), wasted_disp_p2, width = 12, height = 10)

# --- Map-level average wasted vote disparity by # republican seats
wasted_disp_p3 = ggplot(wasted_data, aes(x = 14 - n_dem_seats, y = mean_waste_disparity)) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'grey') +
  geom_jitter(width = 0.2, alpha = 0.3, size = 1, shape = 21) +
  stat_summary(fun.data = mean_se, geom = "pointrange", 
               color = RColorBrewer::brewer.pal(3, name = 'Dark2')[2], size = 1) + 
  scale_x_continuous(breaks = 0:10) +
  # scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  labs(x = 'Republican Seats', y = 'Mean Waste Disparity') +
  theme_custom()

wasted_disp_p3


ggsave(paste0(fig_dir, "New_07_09/wasted_vote_disparity_map_average_by_rep_seats.pdf"), wasted_disp_p3, width = 12, height = 10)





# ------------- Sign Flips ----------------


flip_data = df %>%
  mutate(
    er_sign_flip = sign(true_minority_dem_share - true_majority_dem_share) != sign(er_minority_dem_share - er_majority_dem_share),
    ei_sign_flip = sign(true_minority_dem_share - true_majority_dem_share) != sign(ei_minority_dem_share - ei_majority_dem_share)
  ) %>%
  select(map_id, district_id, seg_level, redist_level, n_dem_seats, prop_minority, prop_dem, er_sign_flip, ei_sign_flip) %>%
  pivot_longer(cols = c(er_sign_flip, ei_sign_flip),
               values_to = 'sign_flip',
               names_to = 'model',
               names_pattern = '(ei|er)_sign_flip')


# --- Vote share vs minority scatter by scenario color for sign flip
sign_p1 = ggplot(flip_data, aes(x = prop_minority, y = prop_dem, color = sign_flip)) +
  geom_point(alpha = 0.5, size = 2, shape = 21) +
  scale_color_brewer(palette = 'Dark2') +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = 'Minority Population Share', y = 'Democratic Voteshare', color = 'Sign Flip', fill = 'Sign Flip') +
  theme_custom()

sign_p1


ggsave(paste0(fig_dir, "New_07_09/sign_flips_by_scenario_scatter.pdf"), sign_p1, width = 12, height = 10)

# --- Vote share vs minority hex by scenario color for sign flip
sign_p2 = ggplot(flip_data, aes(x = prop_minority, y = prop_dem, color = sign_flip, fill = sign_flip)) +
  geom_hex(alpha = 0.5) +
  scale_color_brewer(palette = 'Dark2') +
  scale_fill_brewer(palette = 'Dark2') +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = 'Minority Population Share', y = 'Democratic Voteshare', color = 'Sign Flip', fill = 'Sign Flip') +
  theme_custom()

sign_p2


ggsave(paste0(fig_dir, "New_07_09/sign_flips_by_scenario_hex.pdf"), sign_p2, width = 12, height = 10)


# --- High seg only
high_seg = flip_data %>% filter(seg_level == 'High')

# dem voteshare vs percent minority high only facet by model
sign_p3 = ggplot(flip_data, aes(x = prop_minority * 100, y = prop_dem * 100, color = sign_flip)) +
  geom_point(alpha = 0.5, size = 2, shape = 21) +
  scale_color_brewer(palette = 'Dark2', direction = -1) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_wrap(~ model, labeller = labeller(model = c("ei" = "EI",
                                                    "er" = "ER"))) +
  labs(x = 'Minority Population Share', y = 'Democratic Voteshare', color = 'Sign Flip', fill = 'Sign Flip') +
  theme_custom()

sign_p3

ggsave(paste0(fig_dir, "New_07_09/sign_flips_high_seg.pdf"), sign_p3, width = 12, height = 10)


# ------------- True Spread vs Estimated Spread ----------------


spread_data = df %>%
  mutate(
    # True spread
    true_spread = true_minority_dem_share - true_majority_dem_share, 
    
    # Estimated spreads
    er_spread = er_minority_dem_share - er_majority_dem_share, 
    ei_spread = ei_minority_dem_share - ei_majority_dem_share,
    
    # Sign flips
    er_sign_flip = sign(true_spread) != sign(er_spread),
    ei_sign_flip = sign(true_spread) != sign(ei_spread),
    
    # Dem margin
    dem_margin = (total_dem_votes - total_rep_votes) / total_population

  )


# --- Scatter plot of true vs estimated spread for ER, colored by sign flip
spread_er_p1 = ggplot(spread_data, aes(x = true_spread, y = er_spread)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = er_sign_flip), alpha = 0.6, size = 2, shape = 21) +
  scale_color_brewer(palette = 'Dark2') +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = "True Spread",
       y = "ER Estimated Spread",
       color = "Sign Flip") +
  theme_custom() +
  theme(legend.position = "bottom")

spread_er_p1

ggsave(paste0(fig_dir, "New_07_09/er_pred_spread_vs_true_spread_color_sign_flip.pdf"), spread_er_p1, width = 12, height = 10)

# --- Scatter plot of true vs estimated spread for EI, colored by sign flip
spread_ei_p1 = ggplot(spread_data, aes(x = true_spread, y = ei_spread)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = ei_sign_flip), alpha = 0.6, size = 2, shape = 21) +
  scale_color_brewer(palette = 'Dark2') +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = "True Spread",
       y = "ER Estimated Spread",
       color = "Sign Flip") +
  theme_minimal() +
  theme(legend.position = "bottom")

spread_ei_p1

ggsave(paste0(fig_dir, "New_07_09/ei_pred_spread_vs_true_spread_color_sign_flip.pdf"), spread_ei_p1, width = 12, height = 10)



# --- Scatter plot of true vs estimated spread for ER, colored by RPV Correct
spread_er_p2 = ggplot(spread_data, aes(x = true_spread, y = er_spread)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = factor(er_rpv_correct)), alpha = 0.6, size = 2, shape = 21) +
  scale_color_brewer(palette = 'Dark2', direction = -1) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = "True Spread",
       y = "ER Estimated Spread",
       color = "RPV Correct") +
  theme_custom() +
  theme(legend.position = "bottom")

spread_er_p2

ggsave(paste0(fig_dir, "New_07_09/er_pred_spread_vs_true_spread_color_rpv_correct.pdf"), spread_er_p2, width = 12, height = 10)

# --- Scatter plot of true vs estimated spread for EI, colored by RPV Correct
spread_ei_p2 = ggplot(spread_data, aes(x = true_spread, y = ei_spread)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(aes(color = factor(ei_rpv_correct)), alpha = 0.6, size = 2, shape = 21) +
  scale_color_brewer(palette = 'Dark2', direction = -1) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = "True Spread",
       y = "ER Estimated Spread",
       color = "RPV Correct") +
  theme_custom() +
  theme(legend.position = "bottom")

spread_ei_p2

ggsave(paste0(fig_dir, "New_07_09/ei_pred_spread_vs_true_spread_color_rpv_correct.pdf"), spread_ei_p2, width = 12, height = 10)








# ----------- ER ESTIMATED VS TRUE SPREAD BY GROUP AND MODEL -------------------





# ------------- SPREAD SIZE VS PERCENT MINORITY ----------------

# --- True spread vs minority share colored by dem district
spread_min_p1 = ggplot(spread_data, aes(x = prop_minority, y = true_spread, color = factor(dem_district))) +
  geom_point(alpha = 0.6, size = 2, shape = 21) +
  scale_color_manual(values = c("1" = "#3498DB", "0" = "#E74C3C"), labels = c('1' = 'True', '0' = 'False')) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = "Minority Population Share",
       y = "True Spread",
       color = "Democratic Representative") +
  theme_custom() +
  theme(legend.position = "bottom")

spread_min_p1

ggsave(paste0(fig_dir, "New_07_09/spread_vs_pct_min_color_dem_seat.pdf"), spread_min_p1, width = 12, height = 10)


# --- True spread vs minority share colored by dem margin
spread_min_p2 = ggplot(spread_data, aes(x = prop_minority, y = true_spread, color = dem_margin)) +
  geom_point(size = 2, shape = 21) +
  scale_color_gradient2(low = "#E74C3C", mid = "#B19CD9", high = "#3498DB", midpoint = 0) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = "Minority Population Share",
       y = "True Spread",
       color = "Democratic Win Margin") +
  theme_custom() +
  theme(legend.position = "bottom") +
  guides(color = guide_colorbar(barwidth = 15, barheight = 0.5))

spread_min_p2

ggsave(paste0(fig_dir, "New_07_09/spread_vs_pct_min_color_dem_margin.pdf"), spread_min_p2, width = 12, height = 10)


# ------------- RPV VS GERRYMANDER ----------------

# --- Dem vote share vs minority population share scatter with dots colored by rpv
rpv_gerry_p1 = ggplot(df, aes(x = prop_minority, y = prop_dem, color = factor(true_rpv))) +
  geom_point(alpha = 0.3, size = 2, shape = 21) + 
  scale_color_brewer(palette = 'Dark2', direction = -1, labels = c('1' = 'True', '0' = 'False')) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  facet_grid(seg_level ~ redist_level) +
  labs(x = 'Minority Population Share', y = 'Democratic Voteshare', color = 'RPV') +
  theme_custom() +
  theme(legend.position = "bottom") 

rpv_gerry_p1

ggsave(paste0(fig_dir, "New_07_09/dem_share_vs_minority_share_color_rpv.pdf"), rpv_gerry_p1, width = 12, height = 10)


# --- Dem vote share vs minority population share density with dots for rpv districts
rpv_gerry_p2 = ggplot(df, aes(prop_minority, prop_dem)) +
  coord_equal() + 
  geom_density_2d_filled(contour_var = "ndensity", alpha = 0.8, bins = 8) +  # specify bins
  geom_point(data = filter(df, true_rpv == 1), 
             shape = 21, fill = "white", color = "black", 
             size = 1, alpha = 0.05) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  facet_grid(seg_level ~ redist_level) +
  labs(y = 'Democratic Voteshare', x = 'Minority Population Share') +
  scale_fill_brewer(palette = 'Reds') + 
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_custom() +
  guides(fill = 'none')

rpv_gerry_p2

ggsave(paste0(fig_dir, "New_07_09/dem_share_vs_minority_share_contour_with_rpv_points.pdf"), rpv_gerry_p2, width = 12, height = 10)


# --- 2D histogram 
rpv_gerry_p3 = ggplot(df, aes(prop_minority, prop_dem)) +
  coord_equal() + 
  geom_bin2d(data = filter(df, true_rpv == 0),
             aes(fill = "No"), 
             color = NA,
             bins = 15,
             alpha = 0.5) +
  geom_bin2d(data = filter(df, true_rpv == 1),
             aes(fill = "Yes"), 
             color = NA,
             bins = 15,
             alpha = 0.5) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  facet_grid(seg_level ~ redist_level) +
  labs(y = 'Democratic Voteshare', 
       x = 'Minority Population Share') +
  scale_fill_brewer(palette = 'Dark2', 
                    name = "RPV District", 
                    direction = -1) +
  guides(color = guide_legend(override.aes = list(alpha = 1))) +
  theme_custom() +
  theme(legend.position = "bottom")

rpv_gerry_p3

ggsave(paste0(fig_dir, "New_07_09/dem_share_vs_minority_share_2d_rpv_histogram.pdf"), rpv_gerry_p3, width = 12, height = 10)



# ------------- DEM WIN MARGINS VS GERRYMANDER ----------------

dem_margins = df %>%
  mutate(
    dem_win_margin = (total_dem_votes - total_rep_votes) / total_population,
    is_competitive = abs(dem_win_margin) < 0.05, 
  ) %>%
  select(unique_id, map_id, district_id, seg_level, redist_level, prop_minority, prop_dem, dem_win_margin, is_competitive) 


ggplot(dem_margins, aes(x = dem_win_margin, y = seg_level, fill = redist_level)) +
  stat_binline()

# Competitiveness summary - proportion of competitive districts
competitive_summary = dem_margins %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    pct_competitive = mean(is_competitive) * 100,
    n_districts = n(),
    avg_margin = mean(dem_win_margin),
    margin_sd = sd(dem_win_margin)
  )

comp_p1 = ggplot(competitive_summary,
       aes(x = factor(seg_level, levels = c('Low', "Medium", "High")), y = pct_competitive, fill = redist_level)) +
  geom_col(position = "dodge", alpha = 0.8) +
  scale_fill_manual(
    values = c(
      "Dem" = "#1065ab",
      "Neutral" = "grey60",
      "Rep" = "#b31529"
    ),
    name = 'Gerrymander'
  ) +
  labs(x = "Segregation Level", y = "% Competitive Districts (within ±5% margin)") +
  theme_custom() +
  theme(legend.position = "bottom")

comp_p1

ggsave(paste0(fig_dir, "New_07_09/cometetive_districts_pct_bar.pdf"), comp_p1, width = 12, height = 10)






# ------------- FRACTION OF MINORITIES REPRESENTED BY CO-PARTISAN ----------------


copartisan_data = df %>%
  mutate(
    minority_dem_copartisan = true_minority_dem_votes * dem_district,
    minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (1 - dem_district),
    minority_copartisan = minority_dem_copartisan + minority_rep_copartisan
  ) %>%
  # First, calculate fractions within each map
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    frac_minority_dem_copartisan = sum(minority_dem_copartisan) / sum(total_minority),
    frac_minority_rep_copartisan = sum(minority_rep_copartisan) / sum(total_minority),
    frac_minority_copartisan = sum(minority_copartisan) / sum(total_minority)
  ) %>%
  # Then, average across maps
  group_by(seg_level, redist_level) %>%
  summarise(
    mean_frac_minority_dem_copartisan = mean(frac_minority_dem_copartisan),
    mean_frac_minority_rep_copartisan = mean(frac_minority_rep_copartisan),
    mean_frac_minority_copartisan = mean(frac_minority_copartisan),
    sd_frac_minority_copartisan = sd(frac_minority_copartisan),
    n_maps = n()
  )

# View as a nice table
print(copartisan_data)






















# ------------- DEM WIN MARGINS / COMPETITIVE BALANCE VS GERRYMANDERS ----------------

# Create dem_margins data if not already created
dem_margins = df %>%
  mutate(
    dem_win_margin = (total_dem_votes - total_rep_votes) / total_population,
    is_competitive = abs(dem_win_margin) < 0.05,
    margin_category = case_when(
      dem_win_margin > 0.1 ~ "Safe Dem (>10%)",
      dem_win_margin > 0.05 ~ "Likely Dem (5-10%)",
      dem_win_margin > 0 ~ "Lean Dem (0-5%)",
      dem_win_margin > -0.05 ~ "Lean Rep (0-5%)",
      dem_win_margin > -0.1 ~ "Likely Rep (5-10%)",
      TRUE ~ "Safe Rep (>10%)"
    ),
    margin_category = factor(margin_category, levels = c(
      "Safe Dem (>10%)", "Likely Dem (5-10%)", "Lean Dem (0-5%)",
      "Lean Rep (0-5%)", "Likely Rep (5-10%)", "Safe Rep (>10%)"
    ))
    # seg_level = factor(seg_level, levels = c('High', 'Medium', 'Low'))
  )

# --- Distribution of win margins using density ridges
margin_p1 = ggplot(dem_margins, aes(x = dem_win_margin, y = seg_level, fill = redist_level)) +
  stat_binline(scale = 0.9, bins = 50, alpha = 0.8, draw_baseline = FALSE) +
  geom_vline(xintercept = c(-0.05, 0, 0.05), linetype = "dashed", alpha = 0.5) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  # scale_x_continuous(labels = scales::percent, limits = c(-0.5, 0.5)) +
  scale_y_discrete(limits = c('Low', 'Medium', 'High')) +
  labs(x = "Democratic Win Margin", y = "", fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

margin_p1

ggsave(paste0(fig_dir, "New_07_09/dem_win_margins_distribution_by_scenario.pdf"), margin_p1, width = 12, height = 10)

# --- Stacked bar chart showing distribution of margin categories
margin_summary = dem_margins %>%
  group_by(seg_level, redist_level, margin_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(seg_level, redist_level) %>%
  mutate(pct = n / sum(n) * 100)

margin_p2 = ggplot(margin_summary, aes(x = redist_level, y = pct, fill = margin_category)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(
    values = c(
      "Safe Dem (>10%)" = "#084594",
      "Likely Dem (5-10%)" = "#2171b5",
      "Lean Dem (0-5%)" = "#6baed6",
      "Lean Rep (0-5%)" = "#fc9272",
      "Likely Rep (5-10%)" = "#de2d26",
      "Safe Rep (>10%)" = "#860308"
    ),
    breaks = c(
      "Safe Dem (>10%)",
      "Likely Dem (5-10%)",
      "Lean Dem (0-5%)",
      "Lean Rep (0-5%)",
      "Likely Rep (5-10%)",
      "Safe Rep (>10%)"
    )
  ) +
  scale_x_discrete(labels = c("Democratic", "Neutral", "Republican")) +
  labs(x = "Gerrymander", y = "Percentage of Districts", fill = "Win Margin Category") +
  theme_custom() +
  theme(legend.position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))

margin_p2

ggsave(paste0(fig_dir, "New_07_09/map_competetiveness_by_district_type_stacked_bar.pdf"), margin_p2, width = 12, height = 10)


# --- Scatter plot: minority share vs win margin, sized by total population
margin_p3 = ggplot(dem_margins, aes(x = prop_minority, y = dem_win_margin)) +
  geom_hline(yintercept = c(-0.05, 0, 0.05), linetype = "dashed", alpha = 0.3) +
  geom_point(aes(size = total_population), alpha = 0.4, shape = 21) +
  geom_smooth(method = "loess", se = TRUE, color = "darkred", size = 1) +
  facet_grid(seg_level ~ redist_level) +
  scale_y_continuous(labels = scales::percent) +
  scale_size_continuous(range = c(1, 6), guide = "none") +
  labs(x = "Minority Population Share", y = "Democratic Win Margin") +
  theme_custom()

margin_p3

ggsave(paste0(fig_dir, "New_07_09/dem_margin_vs_minority_share_by_scenario_scatter.pdf"), margin_p3, width = 12, height = 10)

# ------------- FRACTION OF MINORITIES REPRESENTED BY CO-PARTISAN ----------------

# Calculate copartisan data at district level for more detailed viz
copartisan_district = df %>%
  mutate(
    minority_dem_copartisan = true_minority_dem_votes * dem_district,
    minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (1 - dem_district),
    minority_copartisan = minority_dem_copartisan + minority_rep_copartisan,
    frac_minority_copartisan_district = minority_copartisan / total_minority
  )

# Recalculate summary data with more detail
copartisan_summary = df %>%
  mutate(
    minority_dem_copartisan = true_minority_dem_votes * dem_district,
    minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (1 - dem_district),
    minority_copartisan = minority_dem_copartisan + minority_rep_copartisan
  ) %>%
  group_by(map_id, seg_level, redist_level) %>%
  summarise(
    frac_minority_dem_copartisan = sum(minority_dem_copartisan) / sum(total_minority),
    frac_minority_rep_copartisan = sum(minority_rep_copartisan) / sum(total_minority),
    frac_minority_copartisan = sum(minority_copartisan) / sum(total_minority),
    n_dem_seats = first(n_dem_seats),
    .groups = "drop"
  )

# --- Bar plot showing average fraction of minorities with co-partisan representation
copartisan_p1 = copartisan_summary %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    mean_frac = mean(frac_minority_copartisan),
    se_frac = sd(frac_minority_copartisan) / sqrt(n()),
    .groups = "drop"
  ) %>%
  ggplot(aes(x = factor(seg_level, levels = c("Low", "Medium", "High")), 
             y = mean_frac, fill = redist_level)) +
  geom_col(position = position_dodge(width = 0.9), alpha = 0.8) +
  geom_errorbar(aes(ymin = mean_frac - se_frac, ymax = mean_frac + se_frac),
                position = position_dodge(width = 0.9), width = 0.2) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Segregation Level", 
       y = "Fraction of Minorities with Co-Partisan Representative",
       fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

copartisan_p1

ggsave(paste0(fig_dir, "New_07_09/minority_copartisan_fraction_bar.pdf"), copartisan_p1, width = 12, height = 10)

# --- Density plot showing distribution across maps
copartisan_p2 = ggplot(copartisan_summary, aes(x = frac_minority_copartisan, fill = redist_level)) +
  geom_density(alpha = 0.6, adjust = 1.5) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_x_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(x = "Fraction of Minorities with Co-Partisan Representative", 
       y = "Density", 
       fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

copartisan_p2

ggsave(paste0(fig_dir, "New_07_09/minority_copartisan_fraction_density.pdf"), copartisan_p2, width = 12, height = 10)

# --- Scatter plot: number of dem seats vs fraction copartisan
copartisan_p3 = ggplot(copartisan_summary, aes(x = n_dem_seats, y = frac_minority_copartisan)) +
  geom_point(aes(color = redist_level), alpha = 0.6, size = 2) +
  geom_smooth(aes(color = redist_level), method = "lm", se = TRUE) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 0:14) +
  labs(x = "Number of Democratic Seats", 
       y = "Fraction of Minorities with Co-Partisan Representative",
       color = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

copartisan_p3

ggsave(paste0(fig_dir, "New_07_09/dem_seats_vs_minority_copartisan_scatter.pdf"), copartisan_p3, width = 12, height = 10)






# First, determine which party won each district
df = df %>%
  mutate(
    party_wins = ifelse(total_dem_votes > total_rep_votes, "Dem", "Rep"),
    # Calculate minority voters by party
    minority_dem_voters = total_minority * prop_minority * true_minority_dem_share,
    minority_rep_voters = total_minority * prop_minority * (1 - true_minority_dem_share),
    majority_dem_voters = total_majority * (1 - prop_minority) * true_majority_dem_share,
    majority_rep_voters = total_majority * (1 - prop_minority) * (1 - true_majority_dem_share)
  )

# Now calculate minority Democrat representation
minority_dem_representation = df %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    # Fraction of minority Democrats who have a Democratic representative
    minority_dem_copartisan = sum(minority_dem_voters * (party_wins == "Dem")) / sum(minority_dem_voters),
    # Also calculate for minority Republicans for comparison
    minority_rep_copartisan = sum(minority_rep_voters * (party_wins == "Rep")) / sum(minority_rep_voters),
    # Overall minority representation (any minority voter with their preferred party)
    minority_overall_copartisan = (sum(minority_dem_voters * (party_wins == "Dem")) + 
                                     sum(minority_rep_voters * (party_wins == "Rep"))) / 
      sum(total_minority),
    .groups = 'drop'
  )

# Plot specifically for minority Democrats
ggplot(minority_dem_representation, aes(x = seg_level, y = minority_dem_copartisan, fill = redist_level)) +
  geom_bar(stat = "identity", position = "dodge", alpha = 0.9) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Segregation Level",
    y = "% of Minority Democrats with Democratic Representative",
    fill = "Gerrymander",
    title = "Representation of Minority Democrats Across Gerrymandering Scenarios"
  ) +
  theme_custom()
 






















# District competitiveness
df = df %>%
  mutate(
    # Democratic margin for competitiveness
    dem_margin = (total_dem_votes - total_rep_votes) / total_population * 100,
    
    # District categories
    is_competitive = abs(dem_margin) < 5,  # Within 5% of 50%
    is_safe_dem = dem_margin > 10,  # More than 10% Democratic
    is_safe_rep = dem_margin < -10,  # More than 10% Republican
    
    # District type categories
    district_type = case_when(
      dem_margin > 10 ~ "Safe Dem (>10%)",
      dem_margin > 5 ~ "Likely Dem (5-10%)",
      dem_margin > -5 ~ "Competitive (±5%)",
      dem_margin > -10 ~ "Likely Rep (5-10%)",
      TRUE ~ "Safe Rep (>10%)"
    ),
    district_type = factor(district_type, levels = c(
      "Safe Dem (>10%)", "Likely Dem (5-10%)", "Competitive (±5%)",
      "Likely Rep (5-10%)", "Safe Rep (>10%)"
    ))
  )


# --- % of minority voters who end up in competitive (+/- 5% off 50%) districts vs safe Democratic districts (>10%)

# Calculate where minority voters end up
minority_distribution = df %>%
  group_by(seg_level, redist_level, district_type) %>%
  summarise(
    total_minority_in_type = sum(total_minority),
    .groups = 'drop'
  ) %>%
  group_by(seg_level, redist_level) %>%
  mutate(
    total_minority_scenario = sum(total_minority_in_type),
    pct = total_minority_in_type / total_minority_scenario * 100
  ) %>%
  select(seg_level, redist_level, district_type, pct)

# Create the plot
minority_dist_fig = ggplot(minority_distribution, 
                           aes(x = district_type, y = pct, fill = redist_level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  labs(x = "District Type", 
       y = "% of Minority Voters",
       fill = "Gerrymander") +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

minority_dist_fig

ggsave(paste0(fig_dir, "New_07_09/minority_voters_by_district_type.pdf"), 
       minority_dist_fig, width = 12, height = 8)


# --- % of majority voters who end up in competitive (+/- 5% off 50%) districts vs safe Democratic districts (>10%)

# Calculate where majority voters end up
majority_distribution = df %>%
  group_by(seg_level, redist_level, district_type) %>%
  summarise(
    total_majority_in_type = sum(total_majority),
    .groups = 'drop'
  ) %>%
  group_by(seg_level, redist_level) %>%
  mutate(
    total_majority_scenario = sum(total_majority_in_type),
    pct = total_majority_in_type / total_majority_scenario * 100
  ) %>%
  select(seg_level, redist_level, district_type, pct)

# Create the plot
majority_dist_fig = ggplot(majority_distribution, 
                           aes(x = district_type, y = pct, fill = redist_level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  labs(x = "District Type", 
       y = "% of Majority Voters",
       fill = "Gerrymander") +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

majority_dist_fig

ggsave(paste0(fig_dir, "New_07_09/majority_voters_by_district_type.pdf"), 
       majority_dist_fig, width = 12, height = 8)


# --- probability of copartisan representation for minority voters by minority share

# Calculate copartisan representation for minority voters
minority_copartisan_data = df %>%
  mutate(
    # Minority voters with copartisan representation
    minority_dem_copartisan = true_minority_dem_votes * dem_district,
    minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (1 - dem_district),
    minority_copartisan = minority_dem_copartisan + minority_rep_copartisan,
    
    # Fraction at district level
    frac_minority_copartisan = minority_copartisan / total_minority,
    
    # Create bins for minority share
    minority_bin = cut(prop_minority, 
                       breaks = seq(0, 1, by = 0.05),
                       include.lowest = TRUE,
                       labels = seq(0.025, 0.975, by = 0.05))
  ) %>%
  filter(!is.na(minority_bin))

# Calculate mean probability by bin
minority_copartisan_summary = minority_copartisan_data %>%
  group_by(seg_level, redist_level, minority_bin) %>%
  summarise(
    mean_copartisan = mean(frac_minority_copartisan, na.rm = TRUE),
    se_copartisan = sd(frac_minority_copartisan, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    minority_share = as.numeric(as.character(minority_bin))
  )

# Create the plot
minority_copartisan_fig = ggplot(minority_copartisan_summary, 
                                 aes(x = minority_share, y = mean_copartisan, 
                                     color = redist_level, fill = redist_level)) +
  geom_ribbon(aes(ymin = mean_copartisan - se_copartisan, 
                  ymax = mean_copartisan + se_copartisan), 
              alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "District Minority Share", 
       y = "Probability of Co-partisan Representation",
       color = "Gerrymander",
       fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

minority_copartisan_fig

ggsave(paste0(fig_dir, "New_07_09/minority_copartisan_probability_by_minority_share.pdf"), 
       minority_copartisan_fig, width = 12, height = 8)



# --- probability of copartisan representation for majority voters by minority share


# Calculate copartisan representation for majority voters
majority_copartisan_data = df %>%
  mutate(
    # Majority voters with copartisan representation
    majority_dem_copartisan = true_majority_dem_votes * dem_district,
    majority_rep_copartisan = (total_majority - true_majority_dem_votes) * (1 - dem_district),
    majority_copartisan = majority_dem_copartisan + majority_rep_copartisan,
    
    # Fraction at district level
    frac_majority_copartisan = majority_copartisan / total_majority,
    
    # Create bins for minority share (same as above)
    minority_bin = cut(prop_minority, 
                       breaks = seq(0, 1, by = 0.05),
                       include.lowest = TRUE,
                       labels = seq(0.025, 0.975, by = 0.05))
  ) %>%
  filter(!is.na(minority_bin))

# Calculate mean probability by bin
majority_copartisan_summary = majority_copartisan_data %>%
  group_by(seg_level, redist_level, minority_bin) %>%
  summarise(
    mean_copartisan = mean(frac_majority_copartisan, na.rm = TRUE),
    se_copartisan = sd(frac_majority_copartisan, na.rm = TRUE) / sqrt(n()),
    n_obs = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    minority_share = as.numeric(as.character(minority_bin))
  )

# Create the plot
majority_copartisan_fig = ggplot(majority_copartisan_summary, 
                                 aes(x = minority_share, y = mean_copartisan, 
                                     color = redist_level, fill = redist_level)) +
  geom_ribbon(aes(ymin = mean_copartisan - se_copartisan, 
                  ymax = mean_copartisan + se_copartisan), 
              alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "District Minority Share", 
       y = "Probability of Co-partisan Representation",
       color = "Gerrymander",
       fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

majority_copartisan_fig

ggsave(paste0(fig_dir, "New_07_09/majority_copartisan_probability_by_minority_share.pdf"), 
       majority_copartisan_fig, width = 12, height = 8)



# Combine voter distribution figures
combined_dist_fig = minority_dist_fig + majority_dist_fig + 
  plot_layout(ncol = 1) +
  plot_annotation(title = "Distribution of Voters Across District Types by Group",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave(paste0(fig_dir, "New_07_09/combined_voter_distribution_by_district_type.pdf"), 
       combined_dist_fig, width = 14, height = 12)

# Combine copartisan probability figures
combined_copartisan_fig = minority_copartisan_fig + majority_copartisan_fig + 
  plot_layout(ncol = 1) +
  plot_annotation(title = "Probability of Co-partisan Representation by District Minority Share",
                  theme = theme(plot.title = element_text(size = 16, face = "bold")))

ggsave(paste0(fig_dir, "New_07_09/combined_copartisan_probability_by_minority_share.pdf"), 
       combined_copartisan_fig, width = 14, height = 12)


# --- BEtter combined figures

# Combine minority and majority distribution data
combined_distribution = bind_rows(
  minority_distribution %>% mutate(group = "Minority"),
  majority_distribution %>% mutate(group = "Majority")
) %>%
  mutate(
    # Create interaction for dodging
    group_redist = interaction(group, redist_level)
  )


# Alternative version using ggpattern for better distinction
library(ggpattern)

combined_dist_pattern_fig = ggplot(combined_distribution, 
                                   aes(x = district_type, y = pct, 
                                       fill = redist_level,
                                       color = redist_level,
                                       pattern = group)) +
  geom_bar_pattern(stat = "identity", 
                   position = position_dodge(width = 0.9),
                   # color = "black", 
                   pattern_fill = "black",
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6,
                   alpha = 0.75) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_pattern_manual(values = c("Minority" = "stripe", "Majority" = "none")) +
  labs(x = "District Type", 
       y = "% of Voters",
       fill = "Gerrymander",
       color = 'Gerrymander',
       pattern = "Voter Group") +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom") +
  guides(
    # Override the fill legend to not show patterns
    fill = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "grey80"))
  )

combined_dist_pattern_fig

ggsave(paste0(fig_dir, "New_07_09/combined_percent_voters_by_district_type.pdf"), 
       combined_dist_pattern_fig, width = 14, height = 8)

# Combine copartisan probability data
combined_copartisan = bind_rows(
  minority_copartisan_summary %>% 
    mutate(group = "Minority", copartisan = mean_copartisan),
  majority_copartisan_summary %>% 
    mutate(group = "Majority", copartisan = mean_copartisan)
) %>%
  select(seg_level, redist_level, minority_share, group, copartisan, se_copartisan)

# Combined copartisan probability figure
combined_copartisan_fig = ggplot(combined_copartisan, 
                                 aes(x = minority_share, y = copartisan, 
                                     color = redist_level, 
                                     linetype = group)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_linetype_manual(values = c("Minority" = "solid", "Majority" = "dashed")) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = "District Minority Share", 
       y = "Probability of Co-partisan Representation",
       color = "Gerrymander",
       linetype = "Voter Group") +
  theme_custom() +
  theme(legend.position = "bottom") +
  guides(
    # Make linetype legend key wider 
    linetype = guide_legend(keywidth = unit(2, "cm"))
  )

combined_copartisan_fig

ggsave(paste0(fig_dir, "New_07_09/combined_copartisan_probability_by_minority_share.pdf"), 
       combined_copartisan_fig, width = 14, height = 8)









# average % minority voters with copartisan representation
overall_minority_copartisan = df %>%
  mutate(
    # Minority voters with copartisan representation
    minority_dem_copartisan = true_minority_dem_votes * dem_district,
    minority_rep_copartisan = (total_minority - true_minority_dem_votes) * (1 - dem_district),
    minority_copartisan = minority_dem_copartisan + minority_rep_copartisan
  ) %>%
  group_by(seg_level, redist_level) %>%
  summarise(
    # Sum across all districts within each scenario
    total_minority_copartisan = sum(minority_copartisan),
    total_minority_voters = sum(total_minority),
    # Calculate overall percentage
    pct_minority_copartisan = (total_minority_copartisan / total_minority_voters) * 100,
    .groups = 'drop'
  ) %>%
  # Select columns for pivot
  select(seg_level, redist_level, pct_minority_copartisan) %>%
  # Pivot wider for better readability
  pivot_wider(
    names_from = redist_level,
    values_from = pct_minority_copartisan
  ) %>%
  # Reorder segregation levels
  mutate(seg_level = factor(seg_level, levels = c("Low", "Medium", "High"))) %>%
  arrange(seg_level)

# Display the table with better formatting
print(overall_minority_copartisan)









# ------------- COMPETITIVE AND SAFE DEM ONLY ----------------

# Filter for just the two district types we want
minority_distribution_filtered = minority_distribution %>%
  filter(district_type %in% c("Competitive (±5%)", "Safe Dem (>10%)"))

majority_distribution_filtered = majority_distribution %>%
  filter(district_type %in% c("Competitive (±5%)", "Safe Dem (>10%)"))

# Create individual plots
minority_dist_simple_fig = ggplot(minority_distribution_filtered, 
                                  aes(x = district_type, y = pct, fill = redist_level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  labs(x = "District Type", 
       y = "% of Minority Voters",
       fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

minority_dist_simple_fig

ggsave(paste0(fig_dir, "New_07_09/minority_voters_competitive_safedem_only.pdf"), 
       minority_dist_simple_fig, width = 10, height = 6)

majority_dist_simple_fig = ggplot(majority_distribution_filtered, 
                                  aes(x = district_type, y = pct, fill = redist_level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  labs(x = "District Type", 
       y = "% of Majority Voters",
       fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

majority_dist_simple_fig

ggsave(paste0(fig_dir, "New_07_09/majority_voters_competitive_safedem_only.pdf"), 
       majority_dist_simple_fig, width = 10, height = 6)

# ------------- COMBINED SIMPLE VERSION ----------------

# Combine and filter data
combined_distribution_filtered = bind_rows(
  minority_distribution_filtered %>% mutate(group = "Minority"),
  majority_distribution_filtered %>% mutate(group = "Majority")
)


library(ggpattern)

combined_dist_simple_pattern = ggplot(combined_distribution_filtered, 
                                      aes(x = district_type, y = pct, 
                                          fill = redist_level, 
                                          color = redist_level,
                                          pattern = group)) +
  geom_bar_pattern(stat = "identity", 
                   position = position_dodge(width = 0.9),
                   alpha = 0.75,
                   # color = "black", 
                   pattern_fill = "black",
                   pattern_alpha = 0.75,
                   pattern_angle = 45,
                   pattern_density = 0.1,
                   pattern_spacing = 0.025,
                   pattern_key_scale_factor = 0.6) +
  facet_wrap(~ factor(seg_level, levels = c("Low", "Medium", "High"),
                      labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_color_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  scale_pattern_manual(values = c("Minority" = "stripe", "Majority" = "none")) +
  labs(x = "District Type", 
       y = "% of Voters",
       fill = "Gerrymander",
       color = "Gerrymander",
       pattern = "Voter Group") +
  theme_custom() +
  theme(legend.position = "bottom") +
  guides(
    fill = guide_legend(override.aes = list(pattern = "none")),
    pattern = guide_legend(override.aes = list(fill = "grey80"))
  )

combined_dist_simple_pattern

ggsave(paste0(fig_dir, "New_07_09/combined_voters_competitive_safedem_only.pdf"), 
       combined_dist_simple_pattern, width = 10, height = 6)

# ------------- ALTERNATIVE: SIDE-BY-SIDE COMPARISON ----------------

# Create a side-by-side comparison focusing on these two district types
comparison_data = combined_distribution_filtered %>%
  mutate(
    group_district = paste(group, district_type, sep = "\n")
  )

combined_dist_simple_dodge = ggplot(comparison_data, 
                                    aes(x = group, y = pct, fill = redist_level)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), alpha = 0.8) +
  facet_grid(district_type ~ factor(seg_level, levels = c("Low", "Medium", "High"),
                                    labels = c("Low Segregation", "Medium Segregation", "High Segregation"))) +
  scale_fill_manual(values = c("Dem" = "#1065ab", "Neutral" = "grey60", "Rep" = "#b31529")) +
  labs(x = "Voter Group", 
       y = "% of Voters",
       fill = "Gerrymander") +
  theme_custom() +
  theme(legend.position = "bottom")

combined_dist_simple_dodge

ggsave(paste0(fig_dir, "New_07_09/combined_voters_competitive_safedem_dodge.pdf"), 
       combined_dist_simple_dodge, width = 12, height = 8)








