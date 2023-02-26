# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  # 'labelled', 
  'tidyverse' #Data wrangling
  # , 'openxlsx' #Export excel
  # , 'blsR' #BLS API
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# DATA --------------------------------------------------------------------
# Occupations data frame
df_acronyms <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQjexMbCPDqyFYrx_yhQ6laGhSV7ZLEWlWnOTi8qF5mUECkAxVid7yWJM2ApT4g6A/pub?gid=204835218&single=true&output=csv')
