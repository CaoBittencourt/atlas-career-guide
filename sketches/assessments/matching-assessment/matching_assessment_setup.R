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
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/KNN_Matching.R')
c(chr_profile, pkg) -> chr_profile
# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
c(chr_profile, pkg) -> chr_profile
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
c(chr_profile, pkg) -> chr_profile
# Dynamic text
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Dynamic_text.R')
c(chr_profile, pkg) -> chr_profile
# Capital flexibility
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')
c(chr_profile, pkg) -> chr_profile

unique(chr_profile) -> chr_profile

# - Working directory -------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Parameters --------------------------------------------------------------
# Scales
seq_scale.1_5 <- seq(0,1,.25)
seq_scale.1_6 <- round(seq(0, 0.9, 1/6), 2)
seq_scale.1_7 <- c(.33, .33 + .17/2, .50, .50 + .17/2, .67, .67 + .17/2)
seq_scale.1_8 <- round(seq(0,1,1/7), 2)

# Recommendation cutoff
dbl_recommended.cutff <- 0.67

# Colors
list(
  'green' = '#4AF7B0'
  , 'purple1' = '#753AF9'
  , 'purple2' = '#301866'
  , 'purple3' = '#3854FB'
  , 'blue1' = '#56D0F5'
  , 'blue2' = '#ABF4D4'
  , 'blue3' = '#43DED1'
  , 'blue4' = '#182766'
  , 'red' = '#CE3527'
  , 'black' = '#212121'
  , 'grey' = '#D4D5D8'
) -> list_atlas.pal

# - Data --------------------------------------------------------------------
# Factor list
openxlsx::read.xlsx(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_questionnaire_atlas.complete_equamax_15_factors.xlsx'
) -> df_factors

# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas.complete_equamax_15_factors.csv'
) -> df_occupations

# DEFAULT TEXTS FOR IMPUTATION
map(
  excel_sheets('./matching_assessment_texts.xlsx')
  , ~ read_excel('./matching_assessment_texts.xlsx', sheet = .x)
) -> list_df_text

names(list_df_text) <- excel_sheets('./matching_assessment_texts.xlsx')

# Remove carriage returns
list_df_text %>%
  map(function(df){
    
    df %>% 
      mutate(across(
        where(is.character)
        , ~ str_remove_all(.x, "\r") %>% 
          str_remove_all("\\\\n") %>% 
          str_replace_all("\n", "  \n")
      ))
    
  }) -> list_df_text

# Section list
list_df_text$sections$text %>% 
  as.list() -> list_sections

names(list_sections) <- list_df_text$sections$section

# [EXPORT] ----------------------------------------------------------------
# - Save workspace image --------------------------------------------------
save.image('matching_assessment_image.RData')


