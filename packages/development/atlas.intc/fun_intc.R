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

# [FUNCTIONS] ------------------------------------------
# - Educational equivalence function -------------------------------
fun_intc_equivalence_education <- function(
    dbl_years_education
    , dbl_years_education_min
){
  
  # Arguments validation
  stopifnot(
    "'dbl_years_education' must be numeric." = 
      is.numeric(dbl_years_education)
  )
  
  stopifnot(
    "'dbl_years_education_min' must be numeric." = 
      is.numeric(dbl_years_education_min)
  )
  
  # Data wrangling
  rep(
    dbl_years_education
    , each = length(
      dbl_years_education_min
    )) -> dbl_years_education
  
  # Apply equivalence function to years of education
  fun_eqvl_equivalence(
    dbl_var = dbl_years_education
    , dbl_scale_ub = dbl_years_education_min
    , dbl_scaling = dbl_years_education_min
  ) -> dbl_eq_education
  
  rm(dbl_years_education)
  rm(dbl_years_education_min)
  
  # Truncate educational equivalence to [0,1]
  pmin(dbl_eq_education, 1) -> dbl_eq_education
  pmax(dbl_eq_education, 0) -> dbl_eq_education
  
  # Output
  return(dbl_eq_education)
  
}

# - Interchangeability function -------------------------------------------
fun_intc_interchangeability <- function(
    dbl_similarity
    , dbl_scaling = 1
    , dbl_years_education = NULL
    , dbl_years_education_min = NULL
){
  
  # Other arguments validation within helper functions
  stopifnot(
    "'dbl_similarity' must be a percentage." = 
      all(
        dbl_similarity >= 0,
        dbl_similarity <= 1
      )
  )
  
  # Data wrangling
  dbl_scaling[[1]] -> dbl_scaling
  
  # Apply equivalence function to similarity scores
  # fun_eqvl_equivalence_similarity
  fun_eqvl_equivalence(
    dbl_var = dbl_similarity
    , dbl_scaling = dbl_scaling
  ) -> dbl_interchangeability
  
  rm(dbl_similarity)
  
  # Apply equivalence function to years of education
  if(all(
    length(dbl_years_education),
    length(dbl_years_education_min)
  )){
    
    fun_eqvl_equivalence_education(
      dbl_years_education = 
        dbl_years_education
      , dbl_years_education_min = 
        dbl_years_education_min
    ) * 
      dbl_interchangeability -> 
      dbl_interchangeability
    
  }
  
  rm(dbl_years_education)
  rm(dbl_years_education_min)
  
  # Apply equivalence function to Atlas Career Type
  # fun_eqvl_equivalence_acti
  
  # Data wrangling
  
  # Output
  return(dbl_interchangeability)
  
}

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
# # - Interchangeability test 1 -----------------------------------------------
# fun_match_similarity(
#   df_data_rows =
#     df_occupations %>%
#     select(
#       occupation
#       , ends_with('.l')
#     )
#   , df_query_rows =
#     df_input
#   , chr_method = 'bvls'
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
# ) -> df_similarity
# 
# df_similarity$
#   df_similarity ->
#   df_similarity
# 
# tic()
# fun_match_interchangeability(
#   dbl_similarity =
#     df_similarity$
#     similarity
#   , dbl_scaling = 1
# ) %>% round(4)
# toc()
# 
# # - Interchangeability test 2 -----------------------------------------------
# fun_match_similarity(
#   df_data_rows =
#     df_occupations %>%
#     select(
#       occupation
#       , ends_with('.l')
#     )
#   , df_query_rows =
#     df_input
#   , chr_method = 'bvls'
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
# ) -> df_similarity
# 
# df_similarity$
#   df_similarity ->
#   df_similarity
# 
# tic()
# fun_match_interchangeability(
#   dbl_similarity =
#     df_similarity$
#     similarity
#   , dbl_scaling = 1
#   , dbl_years_education = 22
#   , dbl_years_education_min =
#     rep(25, nrow(df_similarity))
# ) %>% round(4)
# toc()
