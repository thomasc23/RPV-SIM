state = 'TX'

library(sf)
library(rmapshaper)
library(patchwork)
library(RColorBrewer)
library(tidyverse)

rm(list = ls())

# tx16 = read_sf('data/States/TX/out/tx_16.shp') 
# tx20 = read_sf('data/States/TX/out/tx_20.shp')
tx24 = read_sf('data/States/TX/out/tx_24.shp')

# tx_16_simple = ms_simplify(tx16, keep = 0.05, keep_shapes = TRUE)
# tx_20_simple = ms_simplify(tx20, keep = 0.05, keep_shapes = TRUE)
tx_24_simple = ms_simplify(tx24, keep = 0.05, keep_shapes = TRUE)

# tx_16_simple$hisp_share = tx_16_simple$cvap_hispanic / tx_16_simple$cvap_tot * 100
# tx_20_simple$hisp_share = tx_20_simple$cvap_hispanic / tx_20_simple$cvap_tot * 100
tx_24_simple$hisp_share = tx_24_simple$cvap_hsp / tx_24_simple$cvap_tot * 100




# p16 = ggplot(tx_16_simple) +
#   geom_sf(aes(fill = hisp_share), color = 'black', linewidth = 0.1) +
#   scale_fill_distiller(palette = 'Greens', 
#                        direction = 1,
#                        name = 'Hispanic % of CVAP') +
#   theme_void()
# 
# p20 = ggplot(tx_20_simple) +
#   geom_sf(aes(fill = hisp_share), color = 'black', linewidth = 0.1) +
#   scale_fill_distiller(palette = 'Greens', 
#                        direction = 1,
#                        name = 'Hispanic % of CVAP') +
#   theme_void()

p24 = ggplot(tx_24_simple) +
  geom_sf(aes(fill = hisp_share), color = 'black', linewidth = 0.1) +
  scale_fill_distiller(palette = 'Greens', 
                       direction = 1,
                       name = 'Hispanic % of CVAP') +
  theme_void()


p24


ggplot(tx_24_simple) +
  geom_sf(aes(fill = (dem_votes) / (dem_votes + rep_votes) * 100), color = 'black', linewidth = 0.1) +
  scale_fill_gradient2(
    name = "Dem. 2P Share",
    low = "#D32F2F",
    mid = "#F5F5F5",
    high = "#1976D2",
    midpoint = 50
  ) +
  theme_void()


# p16 + p20 + p24 + plot_layout(nrow = 1)





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
    pct_black = ifelse(cvap_blk > 0 & cvap_tot > 0, cvap_blk / cvap_tot * 100, NA_real_),
    pct_hisp = ifelse(cvap_hsp > 0 & cvap_tot > 0, cvap_hsp / cvap_tot * 100, NA_real_),
    pct_white = ifelse(cvap_wht > 0 & cvap_tot > 0, cvap_wht / cvap_tot * 100, NA_real_)
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










library(tidylog)


setwd('~/Dropbox/RPV/Code/Simulation/')

# TX VTD Shapefile
tx_shp = read_sf('data/States/TX/Capitol Data Portal/vtds_24pg/VTDs_24PG.shp')

#---------------
# VTD - unique VTD identifier 
# --------------

tx_pop = read.table('data/States/TX/Capitol Data Portal/VTDs_24PG_Pop.txt', header = TRUE, sep = ',')
tx_pres = read_csv('data/States/TX/Capitol Data Portal/president.csv')

tx_out = tx_shp %>%
  select(VTDKEY, CNTYKEY, CNTYVTD, COLOR) %>%
  left_join(
    tx_pres %>% 
      mutate(
        dem_votes = HarrisD_24G_President, 
        rep_votes = TrumpR_24G_President
        ) %>%
      select(VTDKEY, dem_votes, rep_votes), 
    by = c('VTDKEY')
  ) %>%
  left_join(
    tx_pop %>%
      mutate(
        county_fips = CountyFIPS,
        cvap_tot    = vap,
        cvap_wht    = anglovap,
        cvap_blk    = blackvap,
        cvap_hsp    = hispvap,
        cvap_bh     = bhvap
      ) %>%
      select(
        county_fips, VTDKEY,
        cvap_tot, cvap_wht, cvap_blk, 
        cvap_hsp, cvap_bh
      ), 
    by = c('VTDKEY')
  ) %>% 
  mutate(prec_id = seq_along(CNTYVTD)) %>%
  select(prec_id, VTDKEY, county_fips, everything())

head(tx_out)

st_write(tx_out, 'data/States/TX/out/cdp_tx_24.shp')

hist(tx_out$cvap_hsp + tx_out$cvap_blk + tx_out$cvap_wht)

tx_out %>%
  st_drop_geometry() %>%
  select(VTDKEY, cvap_tot, cvap_wht, cvap_blk, cvap_hsp, cvap_bh) %>%
  pivot_longer(
    cols = c(cvap_wht, cvap_blk, cvap_hsp, cvap_bh),
    names_to = 'group',
    values_to = 'count',
    names_prefix = 'cvap_'
  ) %>%
  ggplot(
    aes(x = cvap_tot, y = count, color = group)
  ) +
  geom_point(shape = 1, alpha = 0.25) +
  facet_wrap(~ group) +
  theme_bw()
  


tx_out %>%
  st_drop_geometry() %>%
  group_by(VTDKEY) %>%
  summarise(
    across(c(cvap_wht, cvap_blk, cvap_hsp, cvap_bh), function(x) x / cvap_tot * 100)
  ) %>%
  pivot_longer(
    cols = c('cvap_blk', 'cvap_hsp', 'cvap_bh'),
    names_to = 'group',
    values_to = 'percent',
    names_prefix = 'cvap_'
  ) %>%
  ggplot(
    aes(x = cvap_wht, y = percent, color = group)
  ) +
  geom_point(shape = 1, alpha = 0.25) +
  facet_wrap(~ group) +
  theme_bw()


# 0) Read districts and align CRS
pc2193 = read_sf("data/States/TX/Plans/PLANC2193/PLANC2193.shp") %>%
  rename(plan_2193 = District) %>%
  st_make_valid() %>%
  st_transform(3083)

tx_out = tx_out %>%
  st_make_valid() %>%
  st_transform(st_crs(pc2193))

# 1) One representative point per precinct (guaranteed on/inside polygon surface)
prec_pts = tx_out %>%
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
pr_assigned = tx_out %>%
  left_join(st_drop_geometry(pt_join), by = "prec_id")

# (Optional) sanity checks
stopifnot(!anyNA(pr_assigned$plan_2193))
stopifnot(nrow(pr_assigned) == nrow(tx_out))

# 4) Compute precinct Dem 2-party share
pr_assigned = pr_assigned %>%
  mutate(
    two_party = rep_votes + dem_votes,
    dem_share = ifelse(two_party > 0, dem_votes / two_party * 100, NA_real_),
    pct_black = ifelse(cvap_tot > 0, cvap_blk / cvap_tot * 100, NA_real_),
    pct_hisp = ifelse(cvap_tot > 0, cvap_hsp / cvap_tot * 100, NA_real_),
    pct_white = ifelse(cvap_tot > 0, cvap_wht / cvap_tot * 100, NA_real_)
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
    aes(fill = pct_black),
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
    aes(fill = pct_hisp),
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
    aes(fill = pct_white),
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





tx_fig = p_hisp + p_black + p_white + p_vote + plot_layout(ncol = 2, nrow = 2)
ggsave("Output/TEXAS/Figures/tx_race_vote_precinct.pdf", plot = tx_fig, width = 10, height = 7.5, units = "in")











