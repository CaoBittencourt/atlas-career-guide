# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'rstudioapi' #Current file directory
  , 'readr' #Read data
  , 'tidyr', 'dplyr', 'stringr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.efa'
)

# Activate / install CRAN packages
lapply(
  chr_pkg
  , function(pkg){
    
    if(!require(pkg, character.only = T)){
      
      install.packages(pkg, dependencies = T)
      
    }
    
    require(pkg, character.only = T)
    
  }
)

# Activate / install Git packages
Map(
  function(git, profile){
    
    if(!require(git, character.only = T)){
      
      install_github(
        paste0(profile, '/', git)
        , dependencies = T
        , upgrade = F
        , force = T
      )
      
    }
    
    require(git, character.only = T)
    
  }
  , git = chr_git
  , profile = names(chr_git)
)

# chr_pkg <- c(
#   'devtools' #GitHub packages
# )

# - Data ------------------------------------------------------------------
# Occupations data frame
read_csv(
  '/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv'
) -> df_occupations

# [DATA] ------------------------------------------------------------------
# - Problematic items -------------------------------------------------
# These variables had to be removed from the model
c(
  # useless items:
  'abl_near_vision'
  
  , 'skl_service_orientation'
  
  # redundant items:
  , 'skl_speaking'
  # , 'skl_reading_comprehension'
  , 'skl_writing'

  # poorly clustering items:
  , 'abl_near_vision'
  , 'knw_food_production'
  , 'knw_law_and_government'
  , 'knw_foreign_language'
  , 'knw_public_safety_and_security'
  , 'knw_telecommunications'
  , 'abl_speech_recognition'
  
) -> chr_items_remove

unique(
  chr_items_remove
) -> chr_items_remove

# Number of items
120 - length(chr_items_remove)

# # - Item correlation ------------------------------------------------------
# df_occupations %>%
#   select(
#     starts_with('skl'),
#     starts_with('abl'),
#     starts_with('knw')
#   ) %>% 
#   fun_efa_correlations(
#     dbl_weights =
#       df_occupations$
#       employment_variants
#   ) -> mtx_correlations
# 
# diag(mtx_correlations) <- NA
# 
# mtx_correlations %>% 
#   as_tibble(
#     rownames = 'item'
#   ) %>% 
#   pivot_longer(
#     cols = -1,
#     names_to = 'correlate_item',
#     values_to = 'correlation'
#   ) %>% 
#   na.omit() ->
#   df_correlations
# 
# df_correlations %>% 
#   filter(
#     str_detect(
#       # item, 'food_production'
#       # item, 'near_vision'
#       # item, 'law'
#       # item, 'foreign_language'
#       item, 'public_safety'
#     )
#   ) %>% View
# 
# # 
# # diag(mtx_correlations) <- NA
# # 
# # mtx_correlations %>% 
# #   abs() %>% 
# #   rowMeans(na.rm = T) %>% 
# #   as_tibble(
# #     rownames = 'item'
# #   ) %>% 
# #   rename(
# #     mean_abs_r = 2
# #   ) %>% 
# #   arrange(desc(
# #     -mean_abs_r
# #   )) %>% 
# #   print(
# #     n = 120
# #   )

# - Item variance -----------------------------------------------------
df_occupations %>% 
  select(
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) %>% 
  reframe(across(
    .cols = everything()
    ,.fns = ~ .x %>% 
      wtd.var(
        df_occupations$
          employment_variants
      )
  )) %>%
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'variance'
  ) %>% 
  mutate(
    pct_variance = 
      variance / 
      sum(variance)
    , norm_rank = 
      variance / 
      max(variance)
    , problematic = 
      if_else(
        item %in% 
          chr_items_remove
        , T
        , F
      )
  ) -> df_variance

df_variance %>%
  arrange(desc(
    variance
  )) %>% 
  print(
    n = Inf
  )

df_variance %>% 
  filter(
    !problematic
  ) %>% 
  reframe(
    retained_variance = 
      sum(pct_variance)
  )

# - Variables of interest -----------------------------------
# Keep only employment levels (for sample weights), as well as skl, abl, knw
df_occupations %>%
  select(
    employment_variants
    , starts_with('skl')
    , starts_with('abl')
    , starts_with('knw')
    , -any_of(chr_items_remove)
  ) -> df_occupations_efa

# [MODEL] --------------------------------------------------------------
# - Estimate models ---------------------------------------------------------
# Run EFA with defined parameters
fun_efa_vfa(
  df_data = 
    df_occupations_efa[-1]
  # , int_factors = NULL
  , int_factors = c(10:14)
  # , chr_rotation = 'equamax'
  # , chr_rotation = c('oblimin', 'equamax', 'promax') #promax doesn't work
  , chr_rotation = c('oblimin', 'equamax')
  , dbl_weights = 
    df_occupations_efa$
    employment_variants
  , int_min_items_factor = 4
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa

# [EVALUATE] --------------------------------------------------------------
# - Evaluate models -------------------------------------------------------
# Factor adequacy tests
list_efa$adequacy_tests

# Optimal number of factors
list_efa$nfactors

# Reliability metrics
list_efa$reliability_metrics
list_efa$reliability_evaluation

# Overall model performance
list_efa$model_performance

# # - Interpret factors (with problematic items) -----------------------------------------------------
# list_efa$
#   loadings_long$
#   efa_equamax_10factors %>% 
#   split(.$factor)
# 
# # 10 factor model
# # f1 discernment
# # f2 dexterity
# # f3 arts_humanities
# # f4 business (shit)
# # f5 spatial_orientation/transportation
# # f6 engineering_tech
# # f7 health_science (foreign language, law, social perceptiveness, public safety, education and training, sociology, etc etc)
# # f8 robustness (food_production)
# # f9 intelligence (near_vision)
# # f10 management
# 
# # list_efa$
# #   loadings_long$
# #   efa_equamax_11factors %>% 
# #   split(.$factor)
# 
# # 10 factor model
# # f1 discernment
# # f2 perception/cognition (shit)
# # f3 arts_humanities
# # f4 business (SHIT)
# # f5 spatial_orientation/transportation
# # f6 engineering_tech
# # f7 health_science (foreign language, law, social perceptiveness, public safety, education and training, sociology, etc etc)
# # etc etc
# 
# # list_efa$
# #   loadings_long$
# #   efa_equamax_13factors %>% 
# #   split(.$factor)

# - Interpret factors (without problematic items) -----------------------------------------------------
list_efa$
  loadings_long$
  # efa_equamax_10factors %>%
  # efa_equamax_11factors %>%
  # efa_equamax_12factors %>%
  # efa_equamax_13factors %>%
  # efa_equamax_14factors %>%
  # efa_oblimin_10factors %>%
  # efa_oblimin_11factors %>%
  # efa_oblimin_12factors %>%
  efa_oblimin_13factors %>%
  # efa_oblimin_14factors %>%
  split(.$factor) %>% 
  map(print, n = Inf) %>% 
  invisible()

# 10 factor model
# f1 discernment
# f2 dexterity
# f3 arts_humanities
# f4 business (shit)
# f5 spatial_orientation/transportation
# f6 engineering_tech
# f7 health_science (foreign language, law, social perceptiveness, public safety, education and training, sociology, etc etc)
# f8 robustness (food_production)
# f9 intelligence (near_vision)
# f10 management

list_efa$
  loadings_long$
  efa_equamax_12factors %>% 
  split(.$factor) %>% 
  map(print, n = Inf) %>% 
  invisible()

# list_efa$
#   loadings_long$
#   efa_equamax_13factors %>%
#   split(.$factor) %>% 
#   map(print, n = Inf) %>% 
#   invisible()

# [PLOTS] -----------------------------------------------------------------
# - Plot results ---------------------------------------------------------
# 

# [CLEAR] -----------------------------------------------------------------
# - Keep only necessary variables --------------------------
# Variables to keep
c() -> chr_var_keep

# Remove everything else
rm(
  list =
    .GlobalEnv %>% 
    as.list() %>% 
    names() %>% 
    subset(!(
      .GlobalEnv %>% 
        as.list() %>% 
        names() %in% 
        chr_var_keep
    ))
)

# [EXPORT] ----------------------------------------------------------------
# - Working directory -----------------------------------------------------
# Set working directory to current file path
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Save .RData image --------------------------------------------------
# Save work space image
save.image('./image_file.RData')
