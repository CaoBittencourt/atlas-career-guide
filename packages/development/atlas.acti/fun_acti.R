# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'bvls'
  , 'fastglm'
  , 'weights'
  # 'atlas.ftools' #Factor analysis tools
  , 'dplyr', 'tidyr', 'purrr' #Data wrangling
  # , 'vctrs' #Data wrangling
  # , 'modeest' #Mode
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTIONS] ---------------------------

# - ACTI estimator helper function --------------------------------------------------------

# - ACTI estimator --------------------------------------------------------

# - ACTI matching ---------------------------------------------------------
# fun_eqvl_equivalence_acti

# # [TEST] ------------------------------------------------------------------
# # - Data ------------------------------------------------------------------
# library(readr)
# library(tictoc)
# 
# read_rds(
#   'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
# ) -> efa_model
# 
# read_csv(
#   'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
# ) -> df_occupations
# 
# read_csv(
#   'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
# ) -> df_input
# 
# # - Atlas Career Type Indicator test --------------------------------------
