# Read in CVAP x Census Block data from https://redistrictingdatahub.org/data/download-data/. 
# Will give us distributions that we can use to take draws from for precinct-level fraction 
# minority values. 

initial_env = ls()

cat("Getting joint distributions of population x percent minority at Census Block level...\n")

# high (25%+): AL, GA, LA, MD, MS, SC
# medium (10-25%): FL, IL, MI, NY, PA, TX
# low (<10%): MT, OR, UT, VT, WY


# high (25%+): AL, CA, FL, GA, IL, LA, MD, MS, NC, NY, SC, TX
# medium (10-25%): OR, UT, MI, OH, IN, PA
# low (< 10%) MT, IA, ME, VT, WY


# states = c('al','ca','fl','ga','il','la','md','mi','ms','mt','ny','or','pa','sc','tx','ut','vt','wy')
states = c(
  'al','ca','fl','ga','il','la','md', 'ms', 'nc', 'ny', 'sc', 'tx',
  'or', 'ut', 'mi', 'oh', 'in', 'pa',
  'mt', 'ia', 'me', 'vt', 'wy' 
  )

# Directory path
base_path = '~/Dropbox/RPV/Data/Census/State-Level CVAP/'

# Function to read data, create PER_MIN variable, and store in variable
read_CVAP_data = function(state) {
  file_path = paste0(base_path, state, '_cvap_2022_2020_b/', state, '_cvap_2022_2020_b.csv')
  data = read_csv(file_path, col_types = cols(.default = 'd', GEOID20 = 'c')) %>%
    dplyr::select(GEOID20, CVAP_TOT22, CVAP_BLK22, CVAP_WHT22) %>%
    filter(CVAP_TOT22 > 0) %>% # remove blocks with no population
    mutate(PER_MIN = CVAP_BLK22 / CVAP_TOT22) %>%
    # mutate(PER_MIN = 1 - (CVAP_WHT22 / CVAP_TOT22)) %>%
    mutate(CVAP_MAJ = CVAP_WHT22,
           CVAP_MIN = CVAP_BLK22,
           PER_MIN = ifelse(PER_MIN > 1, 1, PER_MIN),
           PER_MIN = ifelse(PER_MIN < 0, 0, PER_MIN)) %>% # Looked at documentation from redistricting data hub but not sure why some blocks have > 100% black population
    # filter(PER_MIN <= 1) # other option is to remove these blocks instead of truncating them
    # CURRENT SOLUTION TO ABOVE IS TO FIX IN 01_sim_population_tc.R function simulate_voters
    rename(precinct_id = GEOID20, 
           population = CVAP_TOT22, 
           cvap_maj = CVAP_MAJ,
           cvap_min = CVAP_MIN,
           per_minority = PER_MIN) %>%
    mutate(population = round(population * 27)) # scale size of blocks to proxy districts
  assign(paste0(state, '_blocks'), data, envir = .GlobalEnv) 
}

# Loop over the states and read the CSV files
lapply(states, read_CVAP_data)

# save joint distributions
emp_pop_min_joint_dists = list(
  al = al_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  ca = ca_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  fl = fl_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  ga = ga_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  ia = ia_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  il = il_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  ind = in_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  la = la_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  me = me_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  md = md_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  mi = mi_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  ms = ms_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  mt = mt_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  nc = nc_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  ny = ny_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  oh = oh_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  or = or_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  pa = pa_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  sc = sc_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  tx = tx_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  ut = ut_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  vt = vt_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority),
  wy = wy_blocks %>% dplyr::select(precinct_id, population, cvap_maj, cvap_min, per_minority)
)


rm(list = setdiff(ls(), c('emp_pop_min_joint_dists', initial_env)), envir = .GlobalEnv)

