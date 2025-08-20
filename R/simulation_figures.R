library(tidyverse)
library(patchwork)


# need emp_pop_min_joint_dists stored in global environment

seed_states = list(
  low = c('mt', 'ia', 'me', 'vt', 'wy'),
  medium = c('or', 'ut', 'mi', 'oh', 'ind', 'pa'),
  high = c('al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx')
)

# Combine dataframes by category from the list
combine_states = function(state_names, category_label, data_list) {
  map_dfr(state_names, function(state) {
    data_list[[state]] %>%
      mutate(category = category_label)
  })
}

# Create combined datasets for each category
low_data = combine_states(seed_states$low, "Low", emp_pop_min_joint_dists)
medium_data = combine_states(seed_states$medium, "Medium", emp_pop_min_joint_dists)
high_data = combine_states(seed_states$high, "High", emp_pop_min_joint_dists)


all_data = bind_rows(low_data, medium_data, high_data) %>%
  mutate(category = factor(category, levels = c("Low", "Medium", "High")))


set.seed(123)  

all_data_hex_sample = all_data %>%
  group_by(category) %>%
  sample_n(size = min(50000, n())) %>%
  ungroup()

p1 = ggplot(all_data_hex_sample, aes(x = population, y = per_minority)) +
  geom_hex(bins = 50) +
  facet_wrap(~category, scales = "free") +
  scale_x_log10(labels = scales::comma) +
  scale_fill_viridis_c(trans = "log10") +
  labs(x = "Population (log scale)", 
       y = "Proportion Minority",
       fill = "Count\n(log scale)") +
  theme_minimal()

ggsave(filename = "~/Dropbox/RPV/Figures/Simulation Figures/emp_pop_min_joint_dists_hex.pdf",
       plot = p1, width = 25, height = 12, units = 'cm')
