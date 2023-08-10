# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'dplyr', 'tidyr', 'readr' #Data wrangling
  , 'devtools' #Github packages
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# install_github('CaoBittencourtFerreira/atlas.efa')

library(atlas.efa)

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Data ------------------------------------------------------------------
# Occupations data frame (2023, 3Q)
read_csv(
  'C:/Users/Cao/Documents/Github/atlas-research/data/df_occupations_2023.csv'
) -> df_occupations

# [DATA] ------------------------------------------------------------------
# - Problematic items -----------------------------------------------------
# c(
#   'item_near_vision'
#   , 'item_installation'
#   , 'item_foreign_language'
#   , 'item_duration_of_typical_work_week'
#   # , 'item_telephone'
#   # , 'item_electronic_mail'
#   , 'item_letters_and_memos'
#   , 'item_food_production'
#   , 'item_structured_versus_unstructured_work'
#   , 'item_establishing_and_maintaining_interpersonal_relationships'
#   # , 'item_communicating_with_supervisors_peers_or_subordinates'
#   , 'item_speaking'
#   , 'item_writing'
#   , 'item_reading_comprehension'
#   , 'item_degree_of_automation'
#   , 'item_work_schedules'
#   , 'item_indoors_environmentally_controlled'
#   , 'item_impact_of_decisions_on_co_workers_or_company_results'
#   , 'item_spend_time_sitting'
#   , 'item_law_and_government'
#   , 'item_public_safety_and_security'
# ) -> chr_items_remove1

# c(
#   'item_near_vision'
#   , 'item_foreign_language'
#   , 'item_duration_of_typical_work_week'
#   # , 'item_telephone'
#   # , 'item_electronic_mail'
#   , 'item_letters_and_memos'
#   , 'item_food_production'
#   , 'item_structured_versus_unstructured_work'
#   , 'item_establishing_and_maintaining_interpersonal_relationships'
#   , 'item_communicating_with_supervisors_peers_or_subordinates'
#   , 'item_speaking'
#   , 'item_writing'
#   , 'item_reading_comprehension'
#   , 'item_degree_of_automation'
#   , 'item_work_schedules'
#   , 'item_indoors_environmentally_controlled'
#   , 'item_impact_of_decisions_on_co_workers_or_company_results'
#   , 'item_spend_time_sitting'
#   , 'item_law_and_government'
#   , 'item_public_safety_and_security'
# ) -> chr_items_remove1

# c(
#   'item_near_vision'
#   , 'item_foreign_language'
#   , 'item_duration_of_typical_work_week'
#   # , 'item_telephone'
#   # , 'item_electronic_mail'
#   , 'item_letters_and_memos'
#   , 'item_food_production'
#   , 'item_structured_versus_unstructured_work'
#   , 'item_establishing_and_maintaining_interpersonal_relationships'
#   , 'item_communicating_with_supervisors_peers_or_subordinates'
#   , 'item_speaking'
#   # , 'item_writing'
#   , 'item_reading_comprehension'
#   , 'item_level_of_competition'
#   , 'item_degree_of_automation'
#   , 'item_work_schedules'
#   , 'item_indoors_environmentally_controlled'
#   , 'item_impact_of_decisions_on_co_workers_or_company_results'
#   , 'item_spend_time_sitting'
#   , 'item_law_and_government'
#   , 'item_public_safety_and_security'
# ) -> chr_items_remove1

c(
  'item_near_vision'
  , 'item_foreign_language'
  , 'item_duration_of_typical_work_week'
  # , 'item_telephone'
  # , 'item_electronic_mail'
  , 'item_letters_and_memos'
  , 'item_food_production'
  , 'item_structured_versus_unstructured_work'
  , 'item_establishing_and_maintaining_interpersonal_relationships'
  , 'item_communicating_with_supervisors_peers_or_subordinates'
  # , 'item_speaking'
  # , 'item_writing'
  , 'item_freedom_to_make_decisions'
  , 'item_reading_comprehension'
  , 'item_level_of_competition'
  , 'item_degree_of_automation'
  , 'item_work_schedules'
  , 'item_indoors_environmentally_controlled'
  , 'item_impact_of_decisions_on_co_workers_or_company_results'
  , 'item_spend_time_sitting'
  , 'item_law_and_government'
  , 'item_public_safety_and_security'
) -> chr_items_remove1

c(
  'item_near_vision'
  , 'item_foreign_language'
  , 'item_duration_of_typical_work_week'
  , 'item_law_and_government'
  , 'item_food_production'
  , 'item_structured_versus_unstructured_work'
  # , 'item_degree_of_automation'
  # , 'item_work_schedules'
  # , 'item_spend_time_sitting'
  # , 'item_public_safety_and_security'
) -> chr_items_remove2

# - Data wrangling --------------------------------------------------------
df_occupations %>% 
  select(
    employment_variants
    , starts_with('item_')
    , -any_of(chr_items_remove1)
  ) -> df_occupations_efa1

df_occupations %>%
  select(
    employment_variants
    , starts_with(c(
      'item_', 
      'style',
      'value_',
      'holland_'
    ))
    , -any_of(chr_items_remove2)
  ) -> df_occupations_efa2

df_occupations %>%
  select(
    employment_variants
    , starts_with(c(
      'item_', 
      'style',
      'holland_'
    ))
    # , -any_of(chr_items_remove3)
  ) -> df_occupations_efa3

# [EFA 1 - Only items] -------------------------------------------------------------------
# - Vectorized EFA ---------------------------
atlas.efa::fun_efa_vfa(
  df_data = 
    df_occupations_efa1[-1]
  # , int_factors = NULL
  , int_factors = c(14, 15, 16, 20)
  , chr_rotation = 'equamax'
  # , chr_rotation = c('oblimin', 'equamax')
  # , chr_rotation = c('quartimax', 'equamax')
  , dbl_weights = 
    df_occupations_efa1$
    employment_variants
  , int_min_items_factor = 3
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa1

list_efa1$model_performance
list_efa1$model_performance %>% arrange(desc(performance))
list_efa1$reliability_metrics
list_efa1$reliability_evaluation
list_efa1$factor_correlations %>% map( ~ .x$suggested_rotation)
list_efa1$factor_correlations %>% map( ~ .x$redundant_factors)
list_efa1$loadings_long
list_efa1$adequacy_tests
list_efa1$nfactors

list_efa1$
  model_performance %>% 
  arrange(desc(
    performance
  )) %>% 
  select(model) %>% 
  map(
    ~ list_efa1$
      loadings_long[.x]
  ) -> list_efa1_loadings

list_efa1$
  loadings_long[
    list_efa1$
      model_performance %>% 
      arrange(desc(
        performance
      )) %>% 
      pull(model)
  ] -> list_efa1_loadings

list_efa1_loadings[[1]] %>% 
  group_by(factor) %>% 
  arrange(
    desc(loading)
    , .by_group = T
  ) %>% 
  ungroup() %>%
  print(n = nrow(.))

# 1 social skills
# 2 engineering
# 3 health science
# 4 management
# 5 transportation
# 6 administrative
# 7 sales and marketing
# 8 perception
# 9 job hazards
# 10 analytical skills
# 11 intelligence
# 12 dexterity
# 13 arts and humanities
# 14 team work
# 15 discernment / systems skills
# 16 industrial
# 17 creativity
# 18 mechanical skills
# 19 teaching
# 20 strength / robustness

# Factor 1 team work / social skills
# Factor 2 dexterity / manual skills / mechanical
# Factor 3 health science
# Factor 4 sales and marketing / persuasion
# Factor 5 transportation / spatial skills (or orientation)
# Factor 6 administrative 
# Factor 7 arts and humanities
# odd item: 'item_developing_objectives_and_strategies'
# Factor 8 management
# Factor 9 engineering / building
# Factor 10 analytical skills
# Factor 11 intelligence
# Factor 12 robustness
# odd item: 'item_installation'
# Factor 13 job hazards
# Factor 14 discernment / systems skills / systemic thinking

list_efa1_loadings[[2]] %>% 
  group_by(factor) %>% 
  arrange(
    desc(loading)
    , .by_group = T
  ) %>% 
  ungroup() %>%
  print(n = nrow(.))

# Factor 1 industrial / production and processing
# Factor 2 dexterity / manual skills / mechanical
# Factor 3 health science
# Factor 4 sales and marketing / persuasion
# Factor 5 transportation / spatial skills (or orientation)
# Factor 6 administrative 
# Factor 7 arts and humanities
# Factor 8 management
# Factor 9 engineering / building
# Factor 10 analytical skills
# Factor 11 intelligence
# Factor 12 robustness
# Factor 13 job hazards / building and construction
# semi-odd item: 'item_installation'
# Factor 14 discernment
# Factor 15 team work

# - Questionnaires --------------------------------------------------------
# [EFA 2 - Items, Holland, Styles, Values] -------------------------------------------------------------------
# - Vectorized EFA ---------------------------
atlas.efa::fun_efa_vfa(
  df_data = 
    df_occupations_efa2[-1]
  , int_factors = NULL
  , chr_rotation = c('oblimin', 'equamax')
  , dbl_weights = 
    df_occupations_efa2$
    employment_variants
  , int_min_items_factor = 3
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa2

list_efa2$model_performance %>% print(n = nrow(.))
list_efa2$model_performance %>% arrange(desc(performance)) %>% print(n = nrow(.))
list_efa2$reliability_metrics
list_efa2$reliability_evaluation
list_efa2$factor_correlations %>% map( ~ .x$suggested_rotation)
list_efa2$factor_correlations %>% map( ~ .x$redundant_factors)
list_efa2$loadings_long
list_efa2$adequacy_tests
list_efa2$nfactors

list_efa2$
  model_performance %>% 
  arrange(desc(
    performance
  )) %>% 
  select(model) %>% 
  map(
    ~ list_efa2$
      loadings_long[.x]
  ) -> list_efa2_loadings

list_efa2$
  loadings_long[
    list_efa2$
      model_performance %>% 
      arrange(desc(
        performance
      )) %>% 
      pull(model)
  ] -> list_efa2_loadings

list_efa2_loadings[[1]] %>% print(n = nrow(.))

# - Questionnaires --------------------------------------------------------
# [EFA 3 - Items, Holland, Styles] -------------------------------------------------------------------
# - Vectorized EFA ---------------------------
atlas.efa::fun_efa_vfa(
  df_data = 
    df_occupations_efa3[-1]
  , int_factors = NULL
  , chr_rotation = c('oblimin', 'equamax')
  , dbl_weights = 
    df_occupations_efa3$
    employment_variants
  , int_min_items_factor = 3
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa3

list_efa3$model_performance %>% print(n = nrow(.))
list_efa3$model_performance %>% arrange(desc(performance)) %>% print(n = nrow(.))
list_efa3$reliability_metrics
list_efa3$reliability_evaluation
list_efa3$factor_correlations %>% map( ~ .x$suggested_rotation)
list_efa3$factor_correlations %>% map( ~ .x$redundant_factors)
list_efa3$loadings_long
list_efa3$adequacy_tests
list_efa3$nfactors

list_efa3$
  model_performance %>% 
  arrange(desc(
    performance
  )) %>% 
  select(model) %>% 
  map(
    ~ list_efa3$
      loadings_long[.x]
  ) -> list_efa3_loadings

list_efa3$
  loadings_long[
    list_efa3$
      model_performance %>% 
      arrange(desc(
        performance
      )) %>% 
      pull(model)
  ] -> list_efa3_loadings

list_efa3_loadings[[1]] %>% print(n = nrow(.))

# - Questionnaires --------------------------------------------------------
# [EXPORT] ----------------------------------------------------------------
# - New EFA model (RDS) ---------------------------------------------------
# - New questionnaires (xlsx, csv) ----------------------------------------





