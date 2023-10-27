# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'rstudioapi' #Current file directory
  , 'readr' #Read data
  , 'tidyr', 'dplyr', 'stringr' #Data wrangling
  , 'writexl' #Write xlsx
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.efa',
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

# - Data ------------------------------------------------------------------
# Occupations data frame
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vROh3DoPWCg9Nj8aK1VBQdNlGKk5Z9QOBnIAOSQ44UHMcLJwNEt6m1Pfha7E3f8pb346g-EO3RTRDxo/pub?gid=1060672467&single=true&output=csv'
) -> df_occupations

# [DATA] ------------------------------------------------------------------
# - Data wrangling ------------------------------------------------------
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

# - Select competencies (skl, abl, knw) -----------------------------------
# Keep only categories of interest
df_occupations %>%
  select(
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) -> df_occupations_efa

# [MODEL] --------------------------------------------------------------
# - Parameters ------------------------------------------------------------
# 

# - Estimate models ---------------------------------------------------------
# Run EFA with defined parameters

# [PLOTS] -----------------------------------------------------------------
# - Plot results ---------------------------------------------------------
# 

# [CLEAR] -----------------------------------------------------------------
# - Keep only necessary variables --------------------------
# Variables to keep
c() -> chr_var_keep

# Remove everything else
rm(
  list =
    .GlobalEnv %>% 
    as.list() %>% 
    names() %>% 
    subset(!(
      .GlobalEnv %>% 
        as.list() %>% 
        names() %in% 
        chr_var_keep
    ))
)

# [EXPORT] ----------------------------------------------------------------
# - Working directory -----------------------------------------------------
# Set working directory to current file path
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Save .RData image --------------------------------------------------
# Save work space image
save.image('./image_file.RData')
