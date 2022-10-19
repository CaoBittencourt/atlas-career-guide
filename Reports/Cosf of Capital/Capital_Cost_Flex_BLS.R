# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# DATA --------------------------------------------------------------------
# Flexible capital
source('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Capital_Flex_BLS.R')

# Cost of capital
source('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Capital_Cost_BLS.R')

# -------- COST OF CAPITAL FLEXIBLITY ---------------------------------------------
df_kcost.long
df_kflex.long
