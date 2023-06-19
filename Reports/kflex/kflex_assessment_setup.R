# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  # , 'openxlsx' #Export excel
  , 'readxl' #Export excel
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

# - Functions -------------------------------------------------------------
# Capital flexiblity
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_kflex.R')
c(chr_profile, pkg) -> chr_profile
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
c(chr_profile, pkg) -> chr_profile
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')
c(chr_profile, pkg) -> chr_profile
# Dictionary evaluation
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_dictionary.R')
c(chr_profile, pkg) -> chr_profile
# Dynamic text imputation
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_dynamic_text.R')
c(chr_profile, pkg) -> chr_profile

unique(chr_profile) -> chr_profile

# - Parameters ------------------------------------------------------------
# Scale bounds
.dbl_scale.lb <- 0
.dbl_scale.ub <- 100

# Discount factor
.dbl_discount <- 0.25

# - Data ------------------------------------------------------------------
# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas.complete_equamax_15_factors.csv'
) -> df_occupations

# # Dynamic texts
# map(
#   excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/kflex_assessment_texts.xlsx')
#   , ~ read_excel('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/kflex_assessment_texts.xlsx', sheet = .x)
# ) -> list_df_text
# 
# names(list_df_text) <- excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/kflex_assessment_texts.xlsx')
# 
# # Remove carriage returns
# list_df_text %>%
#   map(function(df){
#     
#     df %>% 
#       mutate(across(
#         where(is.character)
#         , ~ str_remove_all(.x, "\r") %>% 
#           str_remove_all("\\\\n") %>% 
#           str_replace_all("\n", "  \n")
#       ))
#     
#   }) -> list_df_text
# 
# # Remove reference dictionary
# list_df_text$
#   dictionary <- NULL
# 
# # Filter text by language
# list_df_text %>% 
#   map(
#     ~ .x %>% 
#       filter(
#         language ==
#           chr_language
#       )
#   ) -> list_df_text

# [RESULTS] ----------------------------------------------
# - Estimate capital flexibility for each item ---------------------------------------------
fun_kflex.df(
  .df_data =
    df_occupations %>% 
    select(
      ends_with('.l')
    )
  , .dbl_weights = 
    df_occupations$
    employment2 / 
    min(
      df_occupations$
        employment2
    )
  , .dbl_scale.lb = 
    .dbl_scale.lb
  , .dbl_scale.ub = 
    .dbl_scale.ub
  , .dbl_discount = 
    .dbl_discount
) -> df_kflex.items

# - Estimate capital flexibility for each occupation -----------------------------------------------------------
fun_kflex.aggregate(
  .df_data =
    df_occupations
  , .df_kflex.items = 
    df_kflex.items
) -> df_occupations.kflex

# [EXPORT] ----------------------------------------------------------------
# - Save workspace image --------------------------------------------------
save.image('kflex_assessment_image.RData')
