# [SETUP] ----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  # 'labelled', 
  'dplyr', 'tidyr', 'readr',  'stringr' #Data wrangling
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

# - Data --------------------------------------------------------------------
# Occupations data frame
# df_occupations <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')
df_occupations <- read.csv(
  'C:/Users/Cao/Documents/Github/atlas-research/data/df_occupations_2023.csv'
  , sep = ';'
  , check.names = T
)

# Employment data frame
df_employment <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2VjvaVX0WrPJcuTtfYL5E4yZ6OmijSL961ytjRtxCPHb2JInjOKSHqq-pGg_m7g/pub?gid=873564137&single=true&output=csv')

# [DATA] --------------------------------------------
# # - Occupations data frame -------------------------------------------------------
# df_occupations %>%
# rename_with(
#     .cols = everything()
#     , .fn = ~ .x %>% 
#       str_to_lower() %>%
#       str_replace_all(
#         '\\.'
#         , '_'
#       ) %>% 
#       str_replace_all(
#         'cluster$'
#         , 'cluster_id'
#       ) %>%
#       str_replace_all(
#         'cluster_1'
#         , 'cluster' 
#       ) %>% 
#       str_replace_all(
#         'wage_change__'
#         , 'wage_growth'
#       ) %>% 
#       str_replace_all(
#         'wage_growth_rate'
#         , 'wage_growth_desc'
#       ) %>% 
#       str_remove_all(
#         'required_'
#       ) %>% 
#       str_replace_all(
#         'level_of_education'
#         , 'education_id'
#       )
#   ) %>%
#   View
#   
# 
# df_occupations %>%
#   select(which(
#     str_detect(
#       names(.)
#       , '.[0-9]'
#     )
#   )) %>%
#   
#   rename_with(
#     .cols = everything()
#     , .fn = str_to_lower
#   )
# 
# df_occupations %>% 
#   mutate(code = substr(code, 1, 7)) %>% 
#   group_by(code) %>% 
#   mutate(
#     code.variants = n()
#     , .after = code
#   ) %>%
#   ungroup() %>% 
#   select(
#     occupation
#     , code
#     , code.variants
#     , career_cluster
#     , entry_level_education
#     , typical_on_the_job_training
#     , annual_wage_2021
#     , projected_growth_2020.2030
#   ) %>% 
#   mutate(
#     across(
#       .cols = ends_with('.l') #Using recommended levels
#       # .cols = ends_with(c('.l', '.i'))
#       ,.fns = function(x){x/100}
#     )
#   ) -> df_occupations

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
