# [SETUP] -----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'openxlsx' #Write excel
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# R profile to load packages at the start of the project
pkg -> chr_profile

# - Functions ---------------------------------------------------------------
# EFA
source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_efa.R')
c(chr_profile, pkg) -> chr_profile
# Factor scores
source('C:/Users/Cao/Documents/Github/atlas-research/functions/metrics/fun_factor_scores.R')
c(chr_profile, pkg) -> chr_profile

unique(chr_profile) -> chr_profile

# - Working directory -------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Data --------------------------------------------------------------------
# EFA model
read_rds(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/efa_model_equamax_15_factors.rds'
) -> efa_model

# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/atlas-research/data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

# [EXPORT] ----------------------------------------------------------------
# - Save workspace image --------------------------------------------------
save.image('atlas_career_type_image.RData')
