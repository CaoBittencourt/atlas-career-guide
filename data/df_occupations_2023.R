# [SETUP] ----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'dplyr', 'tidyr', 'readr',  'stringr' #Data wrangling
  , 'openxlsx' #Open excel
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

# # Employment data frame (2021)
# df_employment <- read_csv(
#   'https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2VjvaVX0WrPJcuTtfYL5E4yZ6OmijSL961ytjRtxCPHb2JInjOKSHqq-pGg_m7g/pub?gid=873564137&single=true&output=csv'
# )

# Employment data frame (2022)
df_employment_2022 <- read.xlsx(
  'C:/Users/Cao/Documents/Github/atlas-research/data/employment/bls-oes-2022/national_M2022_dl.xlsx'
)

# Missing occupations (2022)
df_employment_2022_na <- read_csv(
  'C:/Users/Cao/Documents/Github/atlas-research/data/employment/bls-oes-2022/national_M2022_na.csv'
)

df_employment_2022 <- as_tibble(df_employment_2022)

# [DATA] --------------------------------------------
# - Occupations data frame -------------------------------------------------------
# Column names
fun_misc_rename_df(
  df_occupations
) -> df_occupations

df_occupations %>%
  rename_with(
    .cols = 1:17
    , .fn = ~ .x %>%
      str_remove_all('required_') %>%
      str_remove_all('related_') %>%
      str_replace_all('level_of_', 'id_') %>%
      str_replace('mean$', 'wage_mean') %>%
      str_replace('compensation', 'wage')
  ) %>%
  rename_with(
    .cols = starts_with(c(
      'zone'
      , 'work_experience'
      , 'on_site_'
      , 'on_the_job'
    ))
    , .fn = ~ paste0('id_', .x)
  ) %>% 
  rename_with(
    .cols = c(
      'realistic',
      'investigative',
      'artistic',
      'social',
      'enterprising',
      'conventional'
    )
    , .fn = ~ paste0('holland_', .x)
  ) %>%
  rename_with(
    .cols = seq(match(
      'oral_comprehension'
      , names(df_occupations)
    ), ncol(df_occupations))
    , .fn = ~ paste0('item_', .x)
  ) %>% 
  rename_with(
    .cols = c(
      'achievement',
      'independence',
      'recognition',
      'relationships',
      'support',
      'working_conditions'
    )
    , .fn = ~ paste0('value_', .x)
  ) %>% 
  rename_with(
    .cols = 
      seq(
        max(which(str_detect(
          names(.)
          , 'value_'
        ))) + 1
        , min(which(str_detect(
          names(.)
          , 'item_'
        ))) - 1 
      )
    , .fn = ~ paste0('style_', .x)
  ) %>% 
  rename(
    occupation = title
  ) -> df_occupations

# Data types
df_occupations %>% 
  mutate(
    wage_mean = 
      wage_mean %>% 
      str_remove_all(',') %>%
      str_remove_all('\\$') %>% 
      as.numeric()
    , across(
      .cols = starts_with('id_')
      ,.fns = as.integer
    )
  ) -> df_occupations

# SOC code variants
df_occupations %>%
  mutate(
    .before = 1
    , soc_code = substr(
      code, 1, 7
    )
  ) %>%
  group_by(soc_code) %>%
  mutate(
    nvariants = n()
    , .after = code
  ) %>%
  ungroup() -> 
  df_occupations

# # - Years of education ---------------------------------------------
# # sort(unique(df_occupations$id_education)) * 2 + 1
# df_occupations %>% 
#   mutate(
#     .after = id_education
#     , education_years = recode(
#       id_education
#       , "Bachelor's degree" = 17 + 4
#       , "Postsecondary nondegree award" = 17 + 4 + 2
#       , "High school diploma or equivalent" = 17
#       , "Master's degree" = 17 + 5
#       , "Associate's degree" = 17 + 2
#       , "No formal educational credential" = 14
#       , "Some college, no degree" = 17 + 2
#       , "Doctoral or professional degree" = 17 + 7
#     )
#   ) -> df_occupations

# - Employment data frame -------------------------------------------------
df_employment_2022 %>% 
  rename(
    soc_code = OCC_CODE
    , employment = TOT_EMP
  ) %>%
  select(
    soc_code
    , employment
  ) %>% 
  group_by(soc_code) %>%
  reframe(
    employment = 
      max(employment)
  ) %>% 
  bind_rows(
    df_employment_2022_na %>% 
      select(-occupation)
  ) -> df_employment_2022

rm(df_employment_2022_na)

df_employment_2022 %>% 
  right_join(
    df_occupations
    , multiple = 'all'
  ) %>%
  mutate(
    .after = employment
    , employment_variants = 
      employment / 
      nvariants
  ) -> df_occupations

rm(df_employment_2022)

# - Imput NA -------------------------------------------------------------
df_occupations %>%
  filter(is.na(
    employment
  ))

df_occupations %>%
  filter(if_any(
    .cols = everything()
    ,.fns = is.na
  )) %>% 
  pull(soc_code)

df_occupations %>% 
  group_by(cluster) %>%
  mutate(
    wage_mean = if_else(
      is.na(wage_mean)
      , weighted.mean(
        wage_mean
        , employment_variants
        , na.rm = T
      )
      , wage_mean
    )
  ) -> df_occupations

# [EXPORT] ----------------------------------------------------
# - Write new csv file ----------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

write_csv(
  df_occupations
  , file = './df_occupations_2023.csv'
)