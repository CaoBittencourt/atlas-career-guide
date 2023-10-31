# [SETUP] ----------------------------------------------------------
# - Packages ----------------------------------------------------------------
chr_pkg <- c(
  'dplyr', 'tidyr', 'readr', 'stringr', 'purrr' #Data wrangling
  , 'openxlsx' #Open excel
  , 'devtools' #Github packages
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.misc'
)

# Activate / install CRAN packages
lapply(
  chr_pkg
  , function(pkg){
    
    if(!require(pkg, character.only = T)){
      
      install.packages(pkg)
      
    }
    
    require(pkg, character.only = T)
    
  }
)

# Activate / install Git packages
Map(
  function(git, profile){
    
    if(!require(git, character.only = T)){
      
      install_github(
        paste0(profile, '/', git)
        , upgrade = F
        , force = T
      )
      
    }
    
    require(git, character.only = T)
    
  }
  , git = chr_git
  , profile = names(chr_git)
)

# chr_pkg <- c(
#   'devtools' #GitHub packages
# )


# - Data --------------------------------------------------------------------
# Occupations data frame
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vROh3DoPWCg9Nj8aK1VBQdNlGKk5Z9QOBnIAOSQ44UHMcLJwNEt6m1Pfha7E3f8pb346g-EO3RTRDxo/pub?gid=1060672467&single=true&output=csv'
) -> df_occupations

# Employment data frame (2022)
df_employment_2022 <- read.xlsx(
  '/home/Cao/Storage/github/atlas-research/data/employment/bls-oes-2022/national_M2022_dl.xlsx'
)

# Missing occupations (2022)
df_employment_2022_na <- read_csv(
  '/home/Cao/Storage/github/atlas-research/data/employment/bls-oes-2022/national_M2022_na.csv'
)

# # Education, experience codes
# df_education <- read.xlsx(
#   '/home/Cao/Storage/github/atlas-research/data/education/id_education_experience.xlsx'
# )
# 
# df_education <- as_tibble(df_education)

# [DATA] --------------------------------------------
# - Occupations data frame ------------------------------------------------------
# Standardize names
df_occupations %>% 
  names() %>%
  str_to_lower() %>%
  str_remove_all(
    'basic_|cross_functional_'
  ) -> chr_names

chr_names[
  str_starts(
    chr_names,
    'work_context|work_styles'
  )
] %>% 
  paste0('.l') -> 
  chr_names[
    str_starts(
      chr_names,
      'work_context|work_styles'
    )
  ]

chr_names %>% 
  str_split(
    '\\.'
    , n = 5
  ) -> chr_names

chr_names %>% 
  map_chr(
    ~ .x[c(
      1, 
      length(.x) - 1, 
      length(.x)
    )] %>%
      na.omit() %>% 
      unique() %>% 
      paste0(
        collapse = '.'
      )
  ) -> chr_names

chr_names %>% 
  fun_misc_str_standard() -> 
  chr_names

chr_names %>% 
  str_replace_all(
    'skills_',
    'skl_'
  ) %>% 
  str_replace_all(
    'abilities_',
    'abl_'
  ) %>% 
  str_replace_all(
    'knowledge_',
    'knw_'
  ) %>% 
  str_replace_all(
    'work_context_',
    'ctx_'
  ) %>% 
  str_replace_all(
    'work_activities_',
    'act_'
  ) %>%
  str_replace_all(
    'work_styles_',
    'stl_'
  ) %>% 
  str_replace_all(
    'work_values_',
    'val_'
  ) -> chr_names

df_occupations %>% 
  set_names(
    chr_names
  ) -> df_occupations

# Drop importance levels
df_occupations %>% 
  select(
    -ends_with('_i')
  ) -> df_occupations

# Remove suffix
df_occupations %>% 
  rename_with(
    .fn = ~ str_remove_all(
      .x, '_l$'
    )
  ) -> df_occupations

# SOC code variants
df_occupations %>% 
  mutate(
    id_soc_code = substr(
      id_soc_code
      , 1, 7
    )
  ) %>% 
  group_by(
    id_soc_code
  ) %>% 
  mutate(
    soc_code_variants = n()
    , .after = id_soc_code
  ) %>% 
  ungroup() -> 
  df_occupations

# - Years of education ---------------------------------------------
# # Education ID inconsistency
# df_occupations %>%
#   group_by(
#     education,
#     id_education
#   ) %>%
#   tally() %>%
#   arrange(desc(
#     id_education
#   ))

# Resolve education ID inconsistency
df_occupations %>% 
  mutate(
    id_education = 
      if_else(
        education == "Master's degree"
        , 2, id_education
      )
    , id_education =
      8 - id_education + 1
  ) -> df_occupations

# Years of education
df_occupations %>% 
  mutate(
    .after = education
    , education_years = 
      case_match(
        id_education
        , 1 ~ 14
        , 2 ~ 17
        , 3 ~ 17 + 1
        , 4 ~ 17 + 1.5
        , 5 ~ 17 + 2
        , 6 ~ 17 + 4
        , 7 ~ 17 + 4 + 1
        , 8 ~ 17 + 5
        , 9 ~ 17 + 5 + 1
        , 10 ~ 17 + 6
        , 11 ~ 17 + 7
        , 12 ~ 17 + 7 + 1
        , .default = 14
      )
  ) -> df_occupations

# - Employment data frame -------------------------------------------------
df_employment_2022 %>%  
  as_tibble() ->
  df_employment_2022

df_employment_2022 %>% 
  rename(
    id_soc_code = OCC_CODE
    , employment = TOT_EMP
  ) %>%
  select(
    id_soc_code
    , employment
  ) %>% 
  group_by(id_soc_code) %>%
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
  ) -> df_occupations

rm(df_employment_2022)

# Employment per variant
df_occupations %>% 
  mutate(
    .after = employment
    , employment_variants = 
      employment / 
      soc_code_variants
    , employment_norm = 
      employment_variants / 
      min(employment_variants)
    , employment_norm = 
      ceiling(employment_norm)
  ) -> df_occupations

# - Relocate --------------------------------------------------------------
df_occupations %>% 
  relocate(
    id_soc_code
    , occupation
  ) -> df_occupations

# [EXPORT] ----------------------------------------------------
# - Write new csv file ----------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

write_csv(
  df_occupations
  , file = './occupations/df_occupations_2022.csv'
)
