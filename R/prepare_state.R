state = 'TX'

library(sf)
library(rmapshaper)
library(patchwork)
library(RColorBrewer)

rm(list = ls())

tx16 = read_sf('data/States/TX/out/tx_16.geojson') 
tx20 = read_sf('data/States/TX/out/tx_20.geojson')
tx24 = read_sf('data/States/TX/out/tx_24.geojson')

tx_16_simple = ms_simplify(tx16, keep = 0.05, keep_shapes = TRUE)
tx_20_simple = ms_simplify(tx20, keep = 0.05, keep_shapes = TRUE)
tx_24_simple = ms_simplify(tx24, keep = 0.05, keep_shapes = TRUE)

tx_16_simple$hisp_share = tx_16_simple$cvap_hispanic / tx_16_simple$cvap_tot * 100
tx_20_simple$hisp_share = tx_20_simple$cvap_hispanic / tx_20_simple$cvap_tot * 100
tx_24_simple$hisp_share = tx_24_simple$cvap_hispanic / tx_24_simple$cvap_tot * 100




p16 = ggplot(tx_16_simple) +
  geom_sf(aes(fill = hisp_share), color = 'black', linewidth = 0.1) +
  scale_fill_distiller(palette = 'Greens', 
                       direction = 1,
                       name = 'Hispanic % of CVAP') +
  theme_void()

p20 = ggplot(tx_20_simple) +
  geom_sf(aes(fill = hisp_share), color = 'black', linewidth = 0.1) +
  scale_fill_distiller(palette = 'Greens', 
                       direction = 1,
                       name = 'Hispanic % of CVAP') +
  theme_void()

p24 = ggplot(tx_24_simple) +
  geom_sf(aes(fill = hisp_share), color = 'black', linewidth = 0.1) +
  scale_fill_distiller(palette = 'Greens', 
                       direction = 1,
                       name = 'Hispanic % of CVAP') +
  theme_void()



p16 + p20 + p24 + plot_layout(nrow = 1)





# Add plan info
library(sf)
library(dplyr)

# 0) Read districts and align CRS
pc2193 = read_sf("data/States/TX/Plans/PLANC2193/PLANC2193.shp") %>%
  rename(plan_2193 = District) %>%
  st_make_valid() %>%
  st_transform(3083)

tx24 = tx24 %>%
  st_make_valid() %>%
  st_transform(st_crs(pc2193))

# 1) One representative point per precinct (guaranteed on/inside polygon surface)
prec_pts = tx24 %>%
  st_point_on_surface() %>%
  select(prec_id)  # keep the join key

# 2) Spatial join: point within district polygon
pt_join = st_join(
  prec_pts,
  pc2193 %>% select(plan_2193),
  join = st_within,
  left = TRUE
)

# 3) If any points somehow miss (topology slivers, tiny gaps), use nearest district
if (anyNA(pt_join$plan_2193)) {
  miss = is.na(pt_join$plan_2193)
  nearest_idx = st_nearest_feature(pt_join[miss, ], pc2193)
  pt_join$plan_2193[miss] = pc2193$plan_2193[nearest_idx]
}

# 4) Attach assigned district back to the full precinct polygons
pr_assigned = tx24 %>%
  left_join(st_drop_geometry(pt_join), by = "prec_id")

# (Optional) sanity checks
stopifnot(!anyNA(pr_assigned$plan_2193))
stopifnot(nrow(pr_assigned) == nrow(tx24))

# 4) Compute precinct Dem 2-party share
pr_assigned = pr_assigned %>%
  mutate(
    two_party = rep_votes + dem_votes,
    dem_share = ifelse(two_party > 0, dem_votes / two_party * 100, NA_real_),
    pct_black = ifelse(cvap_black > 0 & cvap_tot > 0, cvap_black / cvap_tot * 100, NA_real_),
    pct_hisp = ifelse(cvap_hispanic > 0 & cvap_tot > 0, cvap_hispanic / cvap_tot * 100, NA_real_),
    pct_white = ifelse(cvap_white > 0 & cvap_tot > 0, cvap_white / cvap_tot * 100, NA_real_)
  )

plot_df = ms_simplify(pr_assigned, keep = 0.05, keep_shapes = TRUE)

# 5) Plot: precinct dem_share with thick district outlines
p_vote = ggplot() +
  geom_sf(
    data = plot_df,
    aes(fill = dem_share),
    color = NA
  ) +
  scale_fill_gradient2(
    name = "Dem. 2P Share",
    low = "#D32F2F",
    mid = "#F5F5F5",
    high = "#1976D2",
    midpoint = 50
  ) +
  # thick district outlines
  geom_sf(
    data = pc2193,
    fill = NA,
    color = "black",
    linewidth = 0.1
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_black = ggplot() +
  geom_sf(
    data = plot_df,
    aes(fill = dem_share),
    color = NA
  ) +
  scale_fill_gradient2(
    name = "% Black",
    low = "#beecbf",
    mid = "#25be28",
    high = "#0f4c10",
    midpoint = median(pr_assigned$pct_black, na.rm = TRUE)
  ) +
  # district outlines
  geom_sf(
    data = pc2193,
    fill = NA,
    color = "black",
    linewidth = 0.1
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_hisp = ggplot() +
  geom_sf(
    data = plot_df,
    aes(fill = dem_share),
    color = NA
  ) +
  scale_fill_gradient2(
    name = "% Hispanic",
    low = "#F3E5F5",
    mid = "#B38AE7",
    high = "#4A148C",
    midpoint = median(pr_assigned$pct_hisp, na.rm = TRUE)
  ) +
  # district outlines
  geom_sf(
    data = pc2193,
    fill = NA,
    color = "black",
    linewidth = 0.1
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

p_white = ggplot() +
  geom_sf(
    data = plot_df,
    aes(fill = dem_share),
    color = NA
  ) +
  scale_fill_gradient2(
    name = "% White",
    low = "#f6f6c9",
    mid = "#e2e24b",
    high = "#717126",
    midpoint = median(pr_assigned$pct_white, na.rm = TRUE)
  ) +
  # district outlines
  geom_sf(
    data = pc2193,
    fill = NA,
    color = "black",
    linewidth = 0.1  
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )





tx_fit = p_hisp + p_black + p_white + p_vote + plot_layout(ncol = 2, nrow = 2)
ggsave("Output/TEXAS/Figures/tx_race_vote_precinct.pdf", plot = tx_fit, width = 10, height = 7.5, units = "in")


