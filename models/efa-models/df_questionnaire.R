# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'rstudioapi' #Current file directory
  , 'readr', 'openxlsx' #Read data
  , 'tidyr', 'dplyr', 'stringr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.notiq',
  'CaoBittencourt' = 'atlas.acti',
  'CaoBittencourt' = 'atlas.match',
  'CaoBittencourt' = 'atlas.plot'
)

# Activate / install CRAN packages
lapply(
  chr_pkg
  , function(pkg){
    
    if(!require(pkg, character.only = T)){
      
      install.packages(pkg, dependencies = T)
      
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
        , dependencies = T
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
  '/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv'
) -> df_occupations

# Questionnaire
df_questionnaire <- read_csv('/home/Cao/Storage/github/atlas-research/data/efa/questionnaire_efa_equamax_14factors_60items.csv')

df_questionnaire_full <- read_csv('/home/Cao/Storage/github/atlas-research/data/efa/questionnaire_efa_equamax_14factors_114items.csv')

# EFA model
efa_model <- read_rds('/home/Cao/Storage/github/atlas-research/data/efa/efa_equamax_14factors.rds')

# My questionnaire
df_input <- read.xlsx('/home/Cao/Storage/github/atlas-research/models/efa-models/questionnaire.xlsx')


# [DATA] ------------------------------------------------------------------
# - Data wrangling --------------------------------------------------------
# Keep capabilities to calculate competency and NOT IQ
df_input %>% 
  select(
    !preference
  ) -> df_capabilities

# Profile = Capabilities vs preferences
df_input %>%
  transmute(
    item = item,
    profile = pmin(capability, preference)
  ) -> df_profile

rm(df_input)

# Pivot
df_profile %>% 
  pivot_wider(
    names_from = 'item',
    values_from = 'profile'
  ) %>% 
  mutate(
    .before = 1
    , occupation = 'Cao'
  ) -> df_profile_rows

rm(df_profile)

# [MATCHING] -----------------------------------------------------------------
# - Matching model ---------------------------------------
fun_match_similarity(
  df_data_rows = df_occupations,
  df_query_rows = df_profile_rows,
  chr_method = 'logit',
  dbl_scale_ub = 100,
  dbl_scale_lb = 0,
  chr_id_col = 'occupation',
  lgc_sort = T
)[[1]] -> df_match

df_match %>% 
  select(
    occupation,
    similarity
  ) %>% 
  print(
    n = 30
  )

# [NOT IQ] ---------------------------------------------------------------
# - Parameters ------------------------------------------------------------
# Setup proxies for IQ
# df_questionnaire %>% select(factor_name, item) %>% print(n = Inf)
c(
  'mathematics',
  'analytical_skills'
) -> chr_notiq_factors


# Calculate descriptive statistics for IQ proxies
df_questionnaire_full %>% 
  filter(
    factor_name %in% 
      chr_notiq_factors
  ) %>% 
  pull(
    item
  ) -> chr_notiq_items

df_occupations %>% 
  select(all_of(
    chr_notiq_items
  )) %>% 
  
  
  # - NOT IQ model ----------------------------------------------------------


atlas.notiq::fun_notiq_quotient(
  
)

# [ACTI] ------------------------------------------------------------------
# - Generality ------------------------------------------------------------
# Full capabilities
atlas.gene::fun_gene_generality(
  dbl_profile = 
    df_capabilities %>% 
    pull(capability)
  , dbl_scale_lb = 0
)

# Preferred profile
atlas.gene::fun_gene_generality(
  dbl_profile = 
    df_profile_rows[-1] %>% 
    as.numeric()
  , dbl_scale_lb = 0
)

# - Competence ------------------------------------------------------------
# Full capabilities
atlas.comp::fun_comp_competence(
  dbl_profile = 
    df_capabilities %>% 
    pull(capability)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

# Preferred profile
atlas.comp::fun_comp_competence(
  dbl_profile = 
    df_profile_rows[-1] %>% 
    as.numeric()
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

# - ACTI model ------------------------------------------------------------
# Full capabilities
atlas.acti::fun_acti_type(
  df_data = 
    df_capabilities %>% 
    select(
      item,
      capability
    ) %>% 
    pivot_wider(
      names_from = 'item',
      values_from = 'capability'
    ) %>% 
    mutate(
      occupation = 'Cao'
    )
  , efa_model = efa_model
  , chr_factor_labels = 
    df_questionnaire$
    factor_abbv %>% 
    unique()
  , chr_id_col = 
    'occupation'
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
) -> df_acti_capabilities

# Preferred profile
atlas.acti::fun_acti_type(
  df_data = df_profile_rows
  , efa_model = efa_model
  , chr_factor_labels = 
    df_questionnaire$
    factor_abbv %>% 
    unique()
  , chr_id_col = 
    'occupation'
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
) -> df_acti_preferred

df_acti_capabilities
df_acti_preferred

# - ACTI molecule ---------------------------------------------------------
# Full capabilities
df_acti_capabilities %>% 
  fun_acti_plot_molecule() ->
  plt_acti_capabilities

# Preferred profile
df_acti_preferred %>% 
  fun_acti_plot_molecule() ->
  plt_acti_preferred

plt_acti_capabilities
plt_acti_preferred
# dsds --------------------------------------------------------------------
atlas.acti::fun_acti_type(
  df_data = 
    df_occupations %>% 
    filter(str_detect(
      occupation,
      'Statisticians|Economists|Mathematicians|Business Intelligence'
    ))
  , efa_model = efa_model
  , chr_factor_labels = 
    df_questionnaire$
    factor_abbv %>% 
    unique()
  , chr_id_col = 
    'occupation'
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
) -> df_acti_stats

df_acti_stats %>%
  fun_acti_plot_molecule() ->
  plt_acti_stats

plt_acti_stats$`Business Intelligence Analysts`
plt_acti_stats$Economists
plt_acti_stats$`Environmental Economists`
plt_acti_stats$Mathematicians
plt_acti_stats$Statisticians

df_acti_stats %>% 
  group_by(
    id_profile
  ) %>% 
  slice(1)
