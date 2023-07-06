# [SETUP] -----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse',  #Data wrangling
  'Hmisc', #Weighted variance
  'openxlsx' #Export excel
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
# NOT IQ Function
source('C:/Users/Cao/Documents/Github/atlas-research/functions/metrics/fun_not_iq.R')
c(chr_profile, pkg) -> chr_profile
# Automated plotting
source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_plots.R')
c(chr_profile, pkg) -> chr_profile

unique(chr_profile) -> chr_profile

# - Working directory -------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Data --------------------------------------------------------------------
# Factor list
openxlsx::read.xlsx(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_questionnaire_atlas.complete_equamax_15_factors.xlsx'
) -> df_factors

# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas.complete_equamax_15_factors.csv'
) -> df_occupations

# [EXPORT] ----------------------------------------------------------------
# - Save workspace image --------------------------------------------------
save.image('sketch_not_iq_image.RData')


