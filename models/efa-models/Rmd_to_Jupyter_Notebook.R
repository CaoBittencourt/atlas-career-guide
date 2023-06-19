# DEV TOOLS -----------------------------------------------------------------
pkg <- c(
  'devtools' 
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# RMD TO JUPYTER NOTEBOOK PACKAGE -----------------------------------------
devtools::install_github("mkearney/rmd2jupyter")

# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models/')

# FILE NAME ---------------------------------------------------------------
Rmd_KNN.matching <- './KNN_Matching_Notebook.Rmd'

# CONVERT MARKDOWN TO JUPYTER NOTEBOOK ------------------------------------
rmd2jupyter::rmd2jupyter(Rmd_KNN.matching)

