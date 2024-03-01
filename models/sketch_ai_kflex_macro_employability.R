# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr', 'openxlsx' #Read and write data
  , 'tidyr', 'dplyr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.match',
  'CaoBittencourt' = 'atlas.intc',
  'CaoBittencourt' = 'atlas.employ',
  'CaoBittencourt' = 'atlas.fstatics',
  'CaoBittencourt' = 'atlas.kcoef'
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

# EFA model
read_rds(
  '/home/Cao/Storage/github/atlas-research/data/efa/efa_equamax_14factors.rds'
) -> efa_model

# Factor names
read_csv(
  '/home/Cao/Storage/github/atlas-research/data/efa/questionnaire_efa_equamax_14factors_60items.csv'
) %>% 
  select(
    factor,
    factor_name
  ) %>% 
  unique() -> 
  df_factor_names

# - Parameters --------------------------------------------------
# AI factor impact
set_names(
  c(
    'factor1' = 0,
    'factor2' = 0,
    'factor3' = -17,
    'factor4' = 0,
    'factor5' = -17/2,
    'factor6' = 0,
    'factor7' = 0,
    'factor8' = -17/2,
    'factor9' = 0,
    'factor10' = 0,
    'factor11' = 0,
    'factor12' = -33,
    'factor13' = -17/2,
    'factor14' = 0
  )
  , df_factor_names$
    factor_name
) -> dbl_impact_factors

# [DATA] ------------------------------------------------------------------
# - Matching data frame ---------------------------------------------------
# Select only competencies
df_occupations %>%
  select(
    occupation,
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) -> df_matching

# [MODEL] --------------------------------------------------------------
# - Estimate AI impact ----------------------------------------------------
# Factor analytic comparative statics with exogenous AI impact
fun_fstatics_impact(
  df_data = 
    df_occupations
  , dbl_weights = 
    df_occupations$
    employment_variants
  , efa_model = 
    efa_model
  , dbl_factors_impact = 
    dbl_impact_factors
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_immune_lb = 0
  , dbl_immune_ub = 33
  , lgc_aggregate = T
) -> list_ai_impact

list_ai_impact$
  factors_impact %>% 
  inner_join(
    df_factor_names
  ) -> 
  list_ai_impact$
  factors_impact

# - Estimate capital macro-flex (items) -------------------------------------------
df_occupations %>% 
  select(
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
  ) %>% 
  fun_kcoef_kflex_macro_df(
    dbl_weights = 
      df_occupations$
      employment_variants
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
  ) %>% 
  arrange(desc(
    kflex_macro
  )) -> df_kflex_macro_items

# - Estimate capital macro-flex (occupations) -------------------------------------------
# Aggregate capital macro-flex scores by occupation
df_occupations %>% 
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'item'
    , values_to = 'item_score'
  ) %>% 
  inner_join(
    df_kflex_macro_items
  ) %>% 
  group_by(across(c(
    !where(is.numeric)
    , -item
  ))) %>% 
  reframe(
    kflex_macro = 
      sum(
        kflex_macro * 
          item_score
      ) / sum(item_score)
  ) -> df_kflex_macro_agg

# - Estimate similarity matrix --------------------------------------------
# Occupations vs occupations matching
fun_match_similarity(
  df_data_rows = 
    df_occupations %>% 
    select(
      occupation,
      starts_with('skl_'),
      starts_with('abl_'),
      starts_with('knw_')
    )
  , df_query_rows = 
    df_occupations %>% 
    select(
      occupation,
      starts_with('skl_'),
      starts_with('abl_'),
      starts_with('knw_')
    )
  , chr_method = 'bvls'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 
    'occupation'
  , lgc_sort = F
) -> list_matching

# - Estimate interchangeability matrix ------------------------------------
# Occupations vs occupations interchangeability
list_matching$
  list_similarity %>%
  map(
    ~ fun_intc_interchangeability(.x)
    , dbl_similarity = 
      list_matching$
      mtx_similarity
    , dbl_scaling = 1
    , dbl_years_education = 
      df_occupations$
      education_years
    , dbl_years_education_min = 14
  ) -> list_interchangeability

list_interchangeability %>% 
  bind_cols() %>%
  as.matrix() ->
  mtx_interchangeability

mtx_interchangeability %>% 
  mutate(
    .before = 1
    , comparison_occupation = 
      colnames(
        mtx_interchangeability
      )
  ) -> mtx_interchangeability

colnames(
  mtx_interchangeability
) -> 
  rownames(
    mtx_interchangeability
  ) 

# - Estimate employability ------------------------------------------------
# Occupations' employability coefficients
mtx_interchangeability %>% 
  t() %>% 
  as_tibble(
    rownames =
      'occupation'
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'comparison_occupation'
    , values_to = 'interchangeability'
  ) %>% 
  inner_join(
    df_occupations %>% 
      select(
        occupation,
        employment_variants
      )
    , by = c(
      'comparison_occupation' = 
        'occupation'
    )
  ) %>% 
  group_by(
    occupation
  ) %>% 
  reframe(
    employability = 
      fun_employ_employability(
        int_employment = 
          employment_variants
        , dbl_interchangeability = 
          interchangeability
      ) 
  ) -> df_employability

# [EXPORT] ----------------------------------------------------------------
# - AI impact -------------------------------------------------------------
# Factors
openxlsx::write.xlsx(
  x = list_ai_impact$factors_impact
  , file = 'ai_factor_impact.xlsx'
)

# Items
openxlsx::write.xlsx(
  x = list_ai_impact$items_impact
  , file = 'ai_items_impact.xlsx'
)

# Occupations
openxlsx::write.xlsx(
  x = list_ai_impact$aggregate_impact
  , file = 'ai_occupations_impact.xlsx'
)

# Overall
openxlsx::write.xlsx(
  x = list_ai_impact$overall_impact
  , file = 'ai_market_impact.xlsx'
)

# - Macro-flex ------------------------------------------------------------
# Items
openxlsx::write.xlsx(
  x = df_kflex_macro_items
  , file = 'kflex_macro_items.xlsx'
)

# Occupations
openxlsx::write.xlsx(
  x = df_kflex_macro_agg
  , file = 'kflex_macro_occupations.xlsx'
)

# # - Similarity matrix -----------------------------------------------------
# openxlsx::write.xlsx(
#   x = list_matching$mtx_similarity
#   , file = 'similarity_matrix.xlsx'
# )
# 
# # - Interchangeability matrix -----------------------------------------------------
# openxlsx::write.xlsx(
#   x = mtx_interchangeability
#   , file = 'interchangeability_matrix.xlsx'
# )

# - Employability ---------------------------------------------------------
# Occupations
openxlsx::write.xlsx(
  x = df_employability
  , file = 'employability_occupations.xlsx'
)