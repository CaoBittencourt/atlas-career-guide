# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
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
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.comp.R')

# Employment data frame
df_employment <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2VjvaVX0WrPJcuTtfYL5E4yZ6OmijSL961ytjRtxCPHb2JInjOKSHqq-pGg_m7g/pub?gid=873564137&single=true&output=csv')

# -------- DATA --------------------------------------------
# EMPLOYMENT DATA FRAME ---------------------------------------------------
df_employment %>% 
rename(
  code = OCC_CODE
  , employment = TOT_EMP
) %>% 
  select(
    code
    , employment
  ) %>% 
  group_by(code) %>%
  summarise(
    employment = max(employment)
  ) %>% 
  right_join(
    df_occupations.comp
  ) %>% 
  mutate(
    employment2 = employment / code.variants
    , .after = employment
  ) -> df_occupations.comp

# # MISSING OCCUPATIONS -----------------------------------------------------
# df_occupations.comp %>% 
#   filter(
#     code %in% 
#       setdiff(
#         df_occupations.comp$code
#         , df_employment$OCC_CODE
#       )
#   ) %>% 
#   select(occupation) %>% view
# 
# setdiff(
#   df_employment$OCC_CODE
#   , df_occupations.comp$code
# )

# POPULATION-WEIGHTED OCCUPATIONS DATA FRAME -------------------------------------------------------
df_occupations.comp %>% 
  drop_na() %>% 
  mutate(
    employment2 = employment2 / min(employment2, na.rm = T)
    , employment2 = round(employment2)
  ) %>% 
  group_by(occupation) %>%
  slice(rep(1:n(), first(employment2))) %>% 
  ungroup() -> df_occupations.pop.comp

# -------- EXPORT ----------------------------------------------------
# # XLSX --------------------------------------------------------------------
# df_occupations.pop.comp %>% 
#   select(
#     code
#     , code.variants
#     , occupation
#     , employment
#     , annual_wage_2021
#   ) %>% 
#   write.xlsx(
#     'df_occupations.pop.comp.xlsx'
#   )
