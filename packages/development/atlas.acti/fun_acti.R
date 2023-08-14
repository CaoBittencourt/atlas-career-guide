# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'bvls'
  , 'fastglm'
  , 'weights'
  # 'atlas.ftools' #Factor analysis tools
  , 'dplyr', 'tidyr', 'purrr' #Data wrangling
  , 'remotes'
  , 'kselection' #K-means clustering
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

# remotes::install_github('Van1yu3/SWKM')
library(SWKM)

# [FUNCTIONS] ---------------------------
# - Generalism function ---------------------------------------------------
fun_acti_generalism <- function(df_data){}

# - Create Career types -----------------------------------------------------
fun_acti_derive_types <- function(
    df_data
    , efa_model
    , dbl_weights = NULL
    , dbl_scale_lb
    , dbl_scale_up
    , dbl_discount = 0.25
    # , int_types = 16
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." =
      all(
        is.data.frame(df_data),
        df_data %>%
          map_lgl(is.numeric) %>%
          any()
      )
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." =
      any(
        is.null(dbl_weights),
        all(
          is.numeric(dbl_weights),
          length(dbl_weights) ==
            nrow(df_data)
        )
      )
  )
  
  # Data wrangling
  
  # Determine generalist vs specialist scope descriptor
  sapply(
    df_data / apply(df_data, 1, max)
    , fun_eqvl_equivalence
    ) %>% 
    rowMeans() -> 
    dbl_generalism
  
  # Determine high-mid-low level competency descriptor
  
  # Determine career type
  # Calculate factor scores
  atlas.ftools::fun_ftools_factor_scores(
    df_data = df_data
    , efa_model = efa_model
    , lgc_pivot = T
  ) -> df_factor_scores
  
  rm(df_data)
  
  # Calculate capital flexibility for each factor
  atlas.kcoef::fun_kcoef_kflex_macro_df(
    df_data = df_factor_scores
    , dbl_weights = dbl_weights
    , dbl_scale_lb = dbl_scale_lb
    , dbl_scale_ub = dbl_scale_ub
    , dbl_discount = dbl_discount
    , lgc_sample_variance = F
  ) -> df_kflex_macro
  
  # Define relevance as 1 - kflex_macro
  df_kflex_macro %>% 
    mutate(
      relevance = 
        1 - kflex_macro
    ) %>% 
    select(!kflex_macro) -> 
    df_kflex_macro
  
  # Join data frames
  df_factor_scores %>% 
    left_join(
      df_kflex_macro
    ) -> df_factor_scores
  
  rm(df_kflex_macro)
  
  # Truncate factor scores
  df_factor_scores
  
  # Remove useless factors
  
  # Classify type
  
  
  
  # Output
  
}

# - ACTI estimator helper function --------------------------------------------------------

# - ACTI estimator --------------------------------------------------------

# - ACTI matching ---------------------------------------------------------
# fun_eqvl_equivalence_acti

# # [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
) -> df_input

# - Atlas Career Type Indicator test --------------------------------------
fun_acti_derive_types

# dsds --------------------------------------------------------------------
library(readr)

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023.csv'
) -> df_occupations

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
) -> efa_model

library(atlas.kcoef)
library(atlas.ftools)

atlas.ftools::fun_ftools_factor_scores(
  df_data = df_occupations
  , efa_model = efa_model
  , lgc_pivot = F
) -> df_factor_scores

atlas.kcoef::fun_kcoef_kflex_macro_df(
  df_data = 
    df_occupations %>% 
    select(starts_with(
      'item_'
    ))
  , dbl_weights = 
    df_occupations$
    employment_variants
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_discount = 0.25
  , lgc_sample_variance = F
) -> df_kflex_macro

# df_kflex_macro %>%
#   mutate(
#     relevance = 
#       1 - kflex_macro
#   ) %>% 
#   select(!kflex_macro) -> 
#   df_kflex_macro

df_kflex_macro %>% 
  arrange(desc(
    relevance
  )) %>% 
  print(n = Inf)

rm(df_kflex_macro)

df_occupations %>% 
  select(starts_with(
    'item_'
  ), employment_variants
  ) %>% 
  group_by(
    row_number()
  ) %>% 
  slice(rep(
    1:n()
    , employment_variants
  ))
kselection::kselection(
  max_centers = 100
)
