# [SETUP] ----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'dplyr', 'tidyr', 'readr',  'stringr' #Data wrangling
  , 'devtools' #Github packages
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

install_github('CaoBittencourtFerreira/atlas.misc')

library(atlas.misc)

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Data --------------------------------------------------------------------
# Occupations data frame
df_occupations <- read_csv(
  'C:/Users/Cao/Documents/Github/atlas-research/data/df_occupations_2023_3q.csv'
)

# Employment data frame
df_employment <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2VjvaVX0WrPJcuTtfYL5E4yZ6OmijSL961ytjRtxCPHb2JInjOKSHqq-pGg_m7g/pub?gid=873564137&single=true&output=csv')

# [DATA] --------------------------------------------
# - Occupations data frame -------------------------------------------------------
fun_misc_rename_df(
  df_occupations
) -> df_occupations

df_occupations

df_occupations %>%
  mutate(code = substr(code, 1, 7)) %>%
  group_by(code) %>%
  mutate(
    code.variants = n()
    , .after = code
  ) %>%
  ungroup() %>%
  select(
    occupation
    , code
    , code.variants
    , career_cluster
    , entry_level_education
    , typical_on_the_job_training
    , annual_wage_2021
    , projected_growth_2020.2030
  ) %>%
  mutate(
    across(
      .cols = ends_with('.l') #Using recommended levels
      # .cols = ends_with(c('.l', '.i'))
      ,.fns = function(x){x/100}
    )
  ) -> df_occupations

# - Years of education ---------------------------------------------
df_occupations %>% 
  mutate(
    .after = entry_level_education
    , education_years = recode(
      entry_level_education
      , "Bachelor's degree" = 17 + 4
      , "Postsecondary nondegree award" = 17 + 4 + 2
      , "High school diploma or equivalent" = 17
      , "Master's degree" = 17 + 5
      , "Associate's degree" = 17 + 2
      , "No formal educational credential" = 14
      , "Some college, no degree" = 17 + 2
      , "Doctoral or professional degree" = 17 + 7
    )
  ) -> df_occupations

# - Employment data frame -------------------------------------------------
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
  reframe(
    employment = max(employment)
  ) %>% 
  right_join(
    df_occupations
  ) %>% 
  mutate(
    .after = employment
    , employment_variants = 
      employment / 
      code.variants
  ) -> df_occupations

# [EXPORT] ----------------------------------------------------
# # XLSX --------------------------------------------------------------------
# df_occupations %>% 
#   select(
#     code
#     , code.variants
#     , occupation
#     , annual_wage_2021
#   ) %>% 
#   write.xlsx(
#     'df_occupations.xlsx'
#   )
