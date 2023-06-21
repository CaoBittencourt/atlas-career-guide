# [SETUP] -----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'psych' #Factor Analysis
  , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
  , 'jsonify' #Work with JSON (faster than jsonlite)
  , 'ggthemes' #Data visualization
  , 'tidyverse', 'stringi', 'english' #Data wrangling
  , 'tinytex' #LaTeX
  , 'modeest' #Mode
  , 'knitr' #Knitr
  , 'readxl' #Import excel (use other package?)
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Install TinyTex
if(!tinytex::is_tinytex()){
  tinytex::install_tinytex()
}

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# R profile to load packages at the start of the project
pkg -> chr_profile

# - Functions ---------------------------------------------------------------
# KNN matching
source('C:/Users/Cao/Documents/Github/atlas-research/old-functions/KNN_Matching.R')
# KNN matching
source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_knn.R')
c(chr_profile, pkg) -> chr_profile
# Capital flexiblity
source('C:/Users/Cao/Documents/Github/atlas-research/functions/metrics/fun_kflex.R')
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
save.image('sketch_matching_assessment_image.RData')


