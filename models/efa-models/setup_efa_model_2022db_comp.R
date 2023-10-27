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
# - Variables of interest -----------------------------------
# Keep only employment levels (for sample weights), as well as skl, abl, knw
df_occupations %>%
  select(
    employment_variants
    , starts_with('skl')
    , starts_with('abl')
    , starts_with('knw')
  ) -> df_occupations

# [MODEL] --------------------------------------------------------------
# - Estimate models ---------------------------------------------------------
# Run EFA with defined parameters
fun_efa_vfa(
  df_data = 
    df_occupations[-1]
  , int_factors = NULL
  , chr_rotation = 'equamax'
  , dbl_weights = 
    df_occupations$
    employment_variants
  , int_min_items_factor = 4
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa

# [EVALUATE] --------------------------------------------------------------
# - Evaluate models -------------------------------------------------------
# Factor adequacy tests
# list_efa$adequacy_tests

# Optimal number of factors
# list_efa$nfactors

# Reliability metrics
# list_efa$reliability_metrics
# list_efa$reliability_evaluation

# Overall model performance
# list_efa$model_performance

# - Interpret factors -----------------------------------------------------
# list_efa$
#   loadings_long$
#   split(.$factor)

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

# list_efa$
#   loadings_long$
#   efa_equamax_11factors %>% 
#   split(.$factor)

# 10 factor model
# f1 discernment
# f2 perception/cognition (shit)
# f3 arts_humanities
# f4 business (SHIT)
# f5 spatial_orientation/transportation
# f6 engineering_tech
# f7 health_science (foreign language, law, social perceptiveness, public safety, education and training, sociology, etc etc)
# etc etc

# list_efa$
#   loadings_long$
#   efa_equamax_13factors %>% 
#   split(.$factor)

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
