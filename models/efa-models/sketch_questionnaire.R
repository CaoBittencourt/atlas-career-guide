# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'rstudioapi' #Current file directory
  , 'readr', 'openxlsx' #Read data
  , 'tidyr', 'dplyr', 'stringr' #Data wrangling
  , 'Hmisc' #Weighted var
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.notiq',
  'CaoBittencourt' = 'atlas.acti',
  'CaoBittencourt' = 'atlas.match',
  'CaoBittencourt' = 'atlas.intc',
  'CaoBittencourt' = 'atlas.employ',
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
df_input <- read.xlsx('/home/Cao/Storage/github/atlas-research/data/questionnaires/questionnaire_Cao.xlsx')


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
  lgc_sort = F
)[[1]] -> df_match

# - Interchangeability ----------------------------------------------------
df_match %>% 
  mutate(
    interchangeablity = 
      df_match$
      similarity %>%
      fun_intc_interchangeability(
        dbl_years_education = 21
        , dbl_years_education_min = 
          df_occupations$
          education_years
      ) %>% 
      round(4)
  ) -> df_match

df_match %>% 
  select(
    occupation,
    education_years,
    similarity,
    interchangeablity
  ) %>% 
  arrange(desc(
    interchangeablity
  )) %>% 
  print(
    n = 30
  )

df_match %>% 
  select(
    occupation,
    education_years,
    similarity,
    interchangeablity
  ) %>% 
  arrange(
    interchangeablity
  ) %>% 
  print(
    n = 30
  )

# - Employability ---------------------------------------------------------
fun_employ_employability(
  int_employment = 
    df_occupations$
    employment_variants
  , dbl_interchangeability = 
    df_match$
    interchangeablity
)

# [NOT IQ] ---------------------------------------------------------------
# - Parameters ------------------------------------------------------------
# Setup proxies for IQ
# df_questionnaire %>% select(factor_name, item) %>% print(n = Inf)
c(
  'mathematics',
  'analytical_skills'
  # , 'rhetoric'
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
  select(
    employment_variants,
    any_of(chr_notiq_items)
  ) %>%
  pivot_longer(
    cols = -1,
    names_to = 'item',
    values_to = 'item_score'
  ) %>% 
  reframe(
    notiq_sd = sqrt(wtd.var(
      x = item_score,
      weights = employment_variants
    )),
    notiq_mean = wtd.mean(
      x = item_score,
      weights = employment_variants
    )
  ) -> df_notiq_stats

df_notiq_stats

# - NOT IQ model ----------------------------------------------------------
df_capabilities %>%
  filter(
    factor_name %in% 
      chr_notiq_factors
  ) %>% 
  pull(
    capability
  ) %>%
  fun_notiq_quotient(
    dbl_proxy_mean = 
      df_notiq_stats$
      notiq_mean,
    dbl_proxy_sd = 
      df_notiq_stats$
      notiq_sd,
    dbl_iq_mean = 100,
    dbl_iq_sd = 15
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
