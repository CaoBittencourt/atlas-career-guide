# [SETUP] -----------------------------------------------------------------
# - Parameters ------------------------------------------------------------
# # Language
# chr_language <- 'en'

# - Workspace -------------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

tryCatch(
  
  load('./atlas_career_type_image.RData')
  
  , error = function(e){
    
    source('./atlas_career_type_setup.R')
    
  }
  
)

# - Packages --------------------------------------------------------------
lapply(
  chr_profile
  , function(x){
    if(!require(
      x, character.only = T
    )){
      install.packages(x)
      require(x)
    }}
)

# - Data ------------------------------------------------------------------
# User data
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
) -> df_input

# df_occupations %>%
#   slice_sample(
#     n = 1
#   ) %>%
#   select(
#     occupation
#     , ends_with('.l')
#   ) -> df_input

# - Parameters --------------------------------------------------------------
# EFA parameters
# Sample weights
df_occupations$
  employment2 ->
  .dbl_weights

# Orthogonal rotations
.chr_rotation <- 'equamax'
# .chr_rotation <- 'varimax'
# .chr_rotation <- 'varimin'
# .chr_rotation <- 'quartimax'

# Oblique rotations
# .chr_rotation <- 'oblimin'
# .chr_rotation <- 'Promax'
# .chr_rotation <- 'promax'
# .chr_rotation <- 'bentlerQ'
# .chr_rotation <- 'cluster'

# Number of factors
# int_nfactors <- 15
.auto_select.nfactors <- T

# Minimum factor size
# Though generally there should be at least 3 items per factor,
# the intuition behind this particular model allows for single item factors.
# Indeed this model is estimated via factor scores, and thus even single "factor" factors are
# actually not single item factors.
# .int_min.factor_size <- 3
.int_min.factor_size <- 1

# MSAi
.remove_unacceptable_MSAi.items <- F

# Underloadings and crossloadings
.remove_under_loading.items <- F
.remove_cross_loading.items <- F
.dbl_under_loading.threshold <- 0.5
.dbl_cross_loading.threshold <- 0.2

# Diagrams and tests
.show_diagrams <- T
.show_results <- T

# [Derive career types from factor scores] ----------------------------------------------
# - Calculate relative factor scores ---------------------------------------------
fun_professional_type(
  df_data = df_occupations
  , efa_model = efa_model
) %>% 
  select(
    occupation
    , factor
    , factor_score
  ) %>% 
  pivot_wider(
    id_cols = occupation
    , names_from = factor
    , values_from = factor_score
  ) -> df_career_types


# fun_factor_scores(
#   df_data = df_occupations
#   , efa_model = efa_model
#   , lgc_pivot = T
# ) %>% 
#   select(
#     occupation
#     , starts_with('factor')
#   ) -> df_occupations_factors


# - EFA on factor scores ---------------------------------------------
fun_efa.bestmodel(
  .df_data.numeric = 
    df_career_types
  , .dbl_weights = 
    .dbl_weights
  , .chr_rotation =
    .chr_rotation
  , .auto_select.nfactors =
    .auto_select.nfactors
  # , .int_nfactors.vector =
  #   # .int_nfactors
  #   c(1, 2, 5)
  , .int_min.factor_size =
    .int_min.factor_size
  , .remove_unacceptable_MSAi.items =
    .remove_unacceptable_MSAi.items
  , .remove_under_loading.items =
    .remove_under_loading.items
  , .remove_cross_loading.items =
    .remove_cross_loading.items
  , .dbl_under_loading.threshold =
    .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold =
    .dbl_cross_loading.threshold
  , .show_diagrams =
    .show_diagrams
  , .show_results =
    .show_results
) -> list_efa_factor_scores

pca(
  df_career_types[-1]
  , 5
  )

list_efa_factor_scores$
  EFA.workflow$
  EFA$
  EFA.4factors$
  model %>% 
  fun_factor_loadings()

dsds























# - Output ----------------------------------------------
