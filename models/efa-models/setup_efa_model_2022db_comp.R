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
  'CaoBittencourt' = 'atlas.efa',
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

# [DATA] ------------------------------------------------------------------
# - Problematic items -------------------------------------------------
# These variables had to be removed from the model
# c(
#   # useless items:
#   'abl_near_vision' #low variance, poor clustering, bullshit item => drop
#   
#   , 'skl_service_orientation' #low variance, poor clustering, bullshit item => drop
#   
#   # redundant items:
#   # , 'skl_speaking' #low variance, ok clustering, bullshit item => drop or rename
#   # , 'skl_reading_comprehension' #low variance, ok clustering, bullshit item => drop or rename
#   , 'skl_writing' #low variance, ok clustering, bullshit item => drop or rename
#   
#   # poorly clustering items:
#   , 'abl_near_vision' #low variance, poor clustering, bullshit item => drop
#   , 'knw_food_production' #mid variance, poor clustering, bullshit item => drop
#   , 'knw_law_and_government' #mid variance, poor clustering, not bullshit => drop
#   , 'knw_foreign_language' #low variance, poor clustering, bullshit item => drop
#   , 'knw_public_safety_and_security' #mid variance, poor clustering, convoluted item => drop
#   , 'knw_telecommunications' #mid-low variance, poor clustering, not bullshit => drop
#   , 'abl_speech_recognition' #low variance, poor clustering, bullshit item => drop
#   , 'knw_computers_and_electronics' #mid-high variance, poor clustering, not bullshit => drop
#   
# ) -> chr_items_remove

# # working with oblimin 13
# c(
#   # drop to round off
#   # , 'skl_speaking' #low variance, ok clustering, bullshit item => drop or rename
#   # , 'skl_reading_comprehension' #low variance, ok clustering, bullshit item => drop or rename
#   # 'skl_writing' #low variance, ok clustering, bullshit item => drop or rename
#   
#   # have to go
#    'knw_telecommunications' #mid-low variance, poor clustering, not bullshit => drop
#   , 'abl_speech_recognition' #low variance, poor clustering, bullshit item => drop
#   , 'knw_computers_and_electronics' #mid-high variance, poor clustering, not bullshit => drop
#   , 'knw_public_safety_and_security' #mid variance, poor clustering, convoluted item => drop
#   
#   # absolutely have to go
#   , 'abl_near_vision' #low variance, poor clustering, bullshit item => drop
#   , 'skl_service_orientation' #low variance, poor clustering, bullshit item => drop
#   , 'knw_food_production' #mid variance, poor clustering, bullshit item => drop
#   , 'knw_law_and_government' #mid variance, poor clustering, not bullshit => drop
#   , 'knw_foreign_language' #low variance, poor clustering, bullshit item => drop
# ) -> chr_items_remove

# # working with equamax 14
# c(
#   # drop to round off
#   # 'skl_reading_comprehension' #low variance, ok clustering, bullshit item => drop or rename
#   # , 'skl_speaking' #low variance, ok clustering, bullshit item => drop or rename
#   # 'skl_writing' #low variance, ok clustering, bullshit item => drop or rename
#   
#   # have to go
#   # 'knw_telecommunications' #mid-low variance, poor clustering, bullshit item => drop
#   # , 'abl_speech_recognition' #low variance, poor clustering, bullshit item => drop
#   # , 'knw_computers_and_electronics' #mid-high variance, poor clustering, not bullshit => drop
# 
#   # absolutely have to go
#   'abl_near_vision' #low variance, poor clustering, bullshit item => drop
#   , 'skl_service_orientation' #low variance, poor clustering, bullshit item => drop
#   , 'knw_food_production' #mid variance, poor clustering, bullshit item => drop
#   , 'knw_law_and_government' #mid variance, poor clustering, not bullshit => drop
#   , 'knw_foreign_language' #low variance, poor clustering, bullshit item => drop
#   , 'knw_public_safety_and_security' #mid variance, poor clustering, convoluted item => drop
# ) -> chr_items_remove

# working with equamax 14
c(
  # drop to round off
  # 'skl_reading_comprehension' #low variance, ok clustering, bullshit item => drop or rename
  # , 'skl_speaking' #low variance, ok clustering, bullshit item => drop or rename
  # 'skl_writing' #low variance, ok clustering, bullshit item => drop or rename
  
  # have to go
  # , 'knw_telecommunications' #mid-low variance, poor clustering, bullshit item => drop
  # 'abl_speech_recognition' #low variance, poor clustering, bullshit item => drop
  # , 'knw_computers_and_electronics' #mid-high variance, poor clustering, not bullshit => drop
  
  # absolutely have to go
  'abl_near_vision' #low variance, poor clustering, bullshit item => drop
  , 'skl_service_orientation' #low variance, poor clustering, bullshit item => drop
  , 'knw_food_production' #mid variance, poor clustering, bullshit item => drop
  , 'knw_law_and_government' #mid variance, poor clustering, not bullshit => drop
  , 'knw_foreign_language' #low variance, poor clustering, bullshit item => drop
  , 'knw_public_safety_and_security' #mid variance, poor clustering, convoluted item => drop
) -> chr_items_remove

unique(
  chr_items_remove
) -> chr_items_remove

# Number of items
120 - length(chr_items_remove)

# - Problematic items distribution ----------------------------------------
for(item in chr_items_remove){
  
  df_occupations %>%
     fun_plot.density(aes(
      x = !!sym(item)
      , weight = employment_variants
    )
    , .list_axis.x.args = list(
      limits = c(-25,125)
    )
    , .fun_format.x = number_format(1)
    , .list_labs = list(
      y = NULL
    )) %>%
    plot()
  
}

# - Item correlation ------------------------------------------------------
df_occupations %>%
  select(
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) %>%
  fun_efa_correlations(
    dbl_weights =
      df_occupations$
      employment_variants
  ) -> mtx_correlations

diag(mtx_correlations) <- NA

mtx_correlations %>%
  as_tibble(
    rownames = 'item'
  ) %>%
  pivot_longer(
    cols = -1,
    names_to = 'correlate_item',
    values_to = 'correlation'
  ) %>%
  na.omit() ->
  df_correlations

df_correlations %>% 
  group_by(
    item
  ) %>% 
  reframe(
    mean_abs_r = mean(abs(
      correlation
    ), na.rm = T),
    max_r = max(
      correlation
      , na.rm = T),
    min_r = min(
      correlation
      , na.rm = T)
  ) %>% 
  arrange(desc(
    mean_abs_r
  )) -> df_mean_r

df_mean_r %>% 
  print(
    n = Inf
  )


# - Item variance -----------------------------------------------------
df_occupations %>% 
  select(
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) %>% 
  reframe(across(
    .cols = everything()
    ,.fns = ~ .x %>% 
      wtd.var(
        df_occupations$
          employment_variants
      )
  )) %>%
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'variance'
  ) %>% 
  mutate(
    pct_variance = 
      variance / 
      sum(variance)
    , norm_rank = 
      variance / 
      max(variance)
    , problematic = 
      if_else(
        item %in% 
          chr_items_remove
        , T
        , F
      )
  ) -> df_variance

df_variance %>% 
  right_join(
    df_mean_r
  ) -> df_items

df_items %>%
  arrange(desc(
    variance * (1 - mean_abs_r)
  )) %>% 
  print(
    n = Inf
  )

df_items %>% 
  filter(
    !problematic
  ) %>% 
  reframe(
    retained_variance = 
      sum(pct_variance)
  )

# - Variables of interest -----------------------------------
# Keep only employment levels (for sample weights), as well as skl, abl, knw
df_occupations %>%
  select(
    employment_variants
    , starts_with('skl')
    , starts_with('abl')
    , starts_with('knw')
    , -any_of(chr_items_remove)
  ) -> df_occupations_efa

# [MODEL] --------------------------------------------------------------
# - Estimate models ---------------------------------------------------------
# Run EFA with defined parameters
fun_efa_vfa(
  df_data = 
    df_occupations_efa[-1]
  # , int_factors = NULL
  # , int_factors = c(10:14)
  , int_factors = c(13, 14)
  # , chr_rotation = 'equamax'
  # , chr_rotation = c('oblimin', 'equamax', 'promax') #promax doesn't work
  , chr_rotation = c('oblimin', 'equamax')
  , dbl_weights = 
    df_occupations_efa$
    employment_variants
  , int_min_items_factor = 4
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa

# [EVALUATE] --------------------------------------------------------------
# - Evaluate models -------------------------------------------------------
# Factor adequacy tests
list_efa$adequacy_tests

# Optimal number of factors
list_efa$nfactors

# Reliability metrics
list_efa$reliability_metrics
list_efa$reliability_evaluation

# Overall model performance
list_efa$model_performance

# Factor correlation
list_efa$factor_correlations

# - Interpret factors -----------------------------------------------------
# Equamax
list_efa$
  loadings_long$
  # efa_equamax_10factors %>%
  # efa_equamax_11factors %>%
  # efa_equamax_12factors %>%
  # efa_equamax_13factors %>%
  efa_equamax_14factors %>%
  split(.$factor) %>%
  map(print, n = Inf) %>%
  invisible()

# Oblimin
list_efa$
  loadings_long$
  # efa_oblimin_10factors %>%
  # efa_oblimin_11factors %>%
  # efa_oblimin_12factors %>%
  # efa_oblimin_13factors %>%
  efa_oblimin_14factors %>%
  split(.$factor) %>% 
  map(print, n = Inf) %>% 
  invisible()

# - Choose model ----------------------------------------------------------
chr_efa_model <- 'efa_equamax_14factors'

# - Name factors ----------------------------------------------------------
c(
  'factor1' = 'rhetoric',
  'factor2' = 'engineering',
  'factor3' = 'health_science',
  'factor4' = 'spatial_abilities',
  'factor5' = 'management',
  'factor6' = 'arts_and_humanities',
  'factor7' = 'mathematics',
  'factor8' = 'business',
  'factor9' = 'perception',
  'factor10' = 'robustness',
  'factor11' = 'mechanical_skills',
  'factor12' = 'administrative_skills',
  'factor13' = 'analytical_skills',
  'factor14' = 'dexterity'
) %>% 
  as_tibble(
    rownames = 'factor'
  ) %>% 
  rename(
    factor_name = 2
  ) %>% 
  mutate(
    factor_abbv = c(
      'Rt', 'Eg', 'Hs',
      'Sp', 'Mn', 'Ah',
      'Mt', 'Bs', 'Pc',
      'Rb', 'Mc', 'Ad',
      'An', 'Dx'
    )
  ) -> df_factors

df_factors

# [QUESTIONNAIRES] -------------------------------------------------------
# - Estimate variance-weighted questionnaires -----------------------------
fun_efa_top_items(
  df_data = 
    df_occupations_efa[-1]
  , dbl_weights = 
    df_occupations_efa$
    employment_variants
  , efa_model = 
    list_efa$
    models[[
      chr_efa_model
    ]]
  , int_items_total_vector = 
    seq(60, 114, length.out = 3)
  # ncol(df_occupations_efa[-1]) * 
  # c(1, 0.5, 0.25)
  , lgc_uneven_factors = T
  , int_min_items_factor = 4
) -> list_questionnaires

list_questionnaires %>% 
  map(
    ~ right_join(df_factors, .x)
  ) -> list_questionnaires

list_questionnaires[[1]] %>% 
  print(n = Inf)

# [EXPORT] ----------------------------------------------------------------
# - Working directory -----------------------------------------------------
# Set working directory to current file path
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Write rds file (EFA model) --------------------------------------------------------
list_efa$
  models[[
    chr_efa_model
  ]] %>%
  write_rds(
    file = paste0(
      chr_efa_model,
      '.rds'
    )
  )

list_efa$
  models[[
    chr_efa_model
  ]] %>%
  write_rds(
    file = paste0(
      '/home/Cao/Storage/github/atlas-research/data/efa/',
      chr_efa_model,
      '.rds'
    )
  )

# - Write xlsx file (questionnaires) -------------------------------------------------------
map2(
  .x = list_questionnaires
  , .y = names(list_questionnaires)
  , .f =
    ~ .x %>%
    write.xlsx(
      file = paste0('./', 'questionnaire_', .y, '.xlsx')
    )
) %>% 
  invisible()

map2(
  .x = list_questionnaires
  , .y = names(list_questionnaires)
  , .f =
    ~ .x %>%
    write.xlsx(
      file = paste0('./', 'questionnaire_', .y, '.xlsx')
    )
) %>% 
  invisible()

# - Write csv file (questionnaires) -------------------------------------------------------
map2(
  .x = list_questionnaires
  , .y = names(list_questionnaires)
  , .f =
    ~ .x %>%
    write_csv(
      file = paste0('./', 'questionnaire_', .y, '.csv')
    )
) %>% 
  invisible()

map2(
  .x = list_questionnaires
  , .y = names(list_questionnaires)
  , .f =
    ~ .x %>%
    write_csv(
      file = paste0('/home/Cao/Storage/github/atlas-research/data/efa/', 'questionnaire_', .y, '.csv')
    )
) %>% 
  invisible()
