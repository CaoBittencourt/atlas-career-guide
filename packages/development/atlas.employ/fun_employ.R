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

# [FUNCTIONS] -----------------------------------------------
# - Employability function -----------------------------------------
fun_employ_employability <- function(
    int_employment
    , dbl_interchangeability
){
  
  # Arguments validation
  stopifnot(
    "'int_employment' must be a numeric vector the same length as 'dbl_interchangeability'." =
      all(
        is.numeric(int_employment)
        , length(int_employment) == 
          nrow(cbind(dbl_interchangeability))
      )
  )
  
  stopifnot(
    "'dbl_interchangeability' must be a percentage." = 
      all(
        dbl_interchangeability >= 0,
        dbl_interchangeability <= 1
      )
  )
  
  # Data wrangling
  round(int_employment) -> int_employment
  
  # Percentage of interchangeable job posts
  sum(
    int_employment * 
      dbl_interchangeability
  ) / sum(int_employment) ->
    dbl_employability
  
  # Output
  return(dbl_employability)
  
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
# # - Employability test ----------------------------------------------------
# tic()
# 
# fun_match_employability(
#   int_employment =
#     df_occupations$
#     employment2
#   , dbl_interchangeability =
#     df_similarity$
#     similarity %>%
#     fun_match_interchangeability()
# )
# 
# toc()
