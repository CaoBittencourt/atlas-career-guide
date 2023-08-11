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

# install_github('CaoBittencourtFerreira/atlas.ftools')
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
# # - Problematic items -----------------------------------------------------
# c(
#   # Poorly measured
#   'item_duration_of_typical_work_week'
#   , 'item_structured_versus_unstructured_work'
#   , 'item_level_of_competition'
#   , 'item_degree_of_automation'
#   , 'item_impact_of_decisions_on_co_workers_or_company_results'
#   , 'item_work_schedules'
#   , 'item_frequency_of_decision_making'
#   # Poor clustering
#   , 'item_food_production'
#   , 'item_near_vision'
#   , 'item_foreign_language'
#   , 'item_law_and_government'
#   , 'item_establishing_and_maintaining_interpersonal_relationships'
#   , 'item_public_safety_and_security'
#   # , 'item_visual_color_discrimination'
#   
#   # , 'item_installation'
#   
#   # Redundant
#   , 'item_speaking'
#   , 'item_writing'
#   , 'item_reading_comprehension'
#   , 'item_indoors_environmentally_controlled'
#   , 'item_spend_time_sitting'
#   #, 'item_communicating_with_supervisors_peers_or_subordinates'
#   
#   # To downsize to 200 items
#   # , 'item_letters_and_memos'
#   # , 'item_telephone'
#   # , 'item_electronic_mail'
#   
# ) -> chr_items_remove
# 
# (218 - length(chr_items_remove)) / c(1, 2, 4)
# 
# # c(
# #   # Poorly measured
# #   'item_duration_of_typical_work_week'
# #   , 'item_structured_versus_unstructured_work'
# #   , 'item_level_of_competition'
# #   , 'item_degree_of_automation'
# #   # , 'item_impact_of_decisions_on_co_workers_or_company_results'
# #   , 'item_work_schedules'
# #   # Poor clustering
# #   , 'item_food_production'
# #   , 'item_near_vision'
# #   , 'item_foreign_language'
# #   , 'item_law_and_government'
# #   # , 'item_establishing_and_maintaining_interpersonal_relationships'
# #   # , 'item_public_safety_and_security'
# #   # , 'item_installation'
# #   
# #   # Redundant
# #   , 'item_speaking'
# #   , 'item_writing'
# #   , 'item_reading_comprehension'
# #   , 'item_indoors_environmentally_controlled'
# #   , 'item_spend_time_sitting'
# #   
# #   # To downsize to 200 items
# #   # , 'item_letters_and_memos'
# #   # , 'item_telephone'
# #   # , 'item_electronic_mail'
# #   # , 'item_communicating_with_supervisors_peers_or_subordinates'
# #   
# # ) -> chr_items_remove
# 
# # c(
# #   'item_near_vision'
# #   , 'item_installation'
# #   , 'item_foreign_language'
# #   , 'item_duration_of_typical_work_week'
# #   # , 'item_telephone'
# #   # , 'item_electronic_mail'
# #   , 'item_letters_and_memos'
# #   , 'item_food_production'
# #   , 'item_structured_versus_unstructured_work'
# #   , 'item_establishing_and_maintaining_interpersonal_relationships'
# #   # , 'item_communicating_with_supervisors_peers_or_subordinates'
# #   , 'item_speaking'
# #   , 'item_writing'
# #   , 'item_reading_comprehension'
# #   , 'item_degree_of_automation'
# #   , 'item_work_schedules'
# #   , 'item_indoors_environmentally_controlled'
# #   , 'item_impact_of_decisions_on_co_workers_or_company_results'
# #   , 'item_spend_time_sitting'
# #   , 'item_law_and_government'
# #   , 'item_public_safety_and_security'
# # ) -> chr_items_remove
# 
# # c(
# #   'item_near_vision'
# #   , 'item_foreign_language'
# #   , 'item_duration_of_typical_work_week'
# #   # , 'item_telephone'
# #   # , 'item_electronic_mail'
# #   , 'item_letters_and_memos'
# #   , 'item_food_production'
# #   , 'item_structured_versus_unstructured_work'
# #   , 'item_establishing_and_maintaining_interpersonal_relationships'
# #   , 'item_communicating_with_supervisors_peers_or_subordinates'
# #   , 'item_speaking'
# #   , 'item_writing'
# #   , 'item_reading_comprehension'
# #   , 'item_degree_of_automation'
# #   , 'item_work_schedules'
# #   , 'item_indoors_environmentally_controlled'
# #   , 'item_impact_of_decisions_on_co_workers_or_company_results'
# #   , 'item_spend_time_sitting'
# #   , 'item_law_and_government'
# #   , 'item_public_safety_and_security'
# # ) -> chr_items_remove
# 
# # c(
# #   'item_near_vision'
# #   , 'item_foreign_language'
# #   , 'item_duration_of_typical_work_week'
# #   # , 'item_telephone'
# #   # , 'item_electronic_mail'
# #   , 'item_letters_and_memos'
# #   , 'item_food_production'
# #   , 'item_structured_versus_unstructured_work'
# #   , 'item_establishing_and_maintaining_interpersonal_relationships'
# #   , 'item_communicating_with_supervisors_peers_or_subordinates'
# #   , 'item_speaking'
# #   # , 'item_writing'
# #   , 'item_reading_comprehension'
# #   , 'item_level_of_competition'
# #   , 'item_degree_of_automation'
# #   , 'item_work_schedules'
# #   , 'item_indoors_environmentally_controlled'
# #   , 'item_impact_of_decisions_on_co_workers_or_company_results'
# #   , 'item_spend_time_sitting'
# #   , 'item_law_and_government'
# #   , 'item_public_safety_and_security'
# # ) -> chr_items_remove
# 
# # c(
# #   'item_near_vision'
# #   , 'item_foreign_language'
# #   , 'item_duration_of_typical_work_week'
# #   # , 'item_telephone'
# #   # , 'item_electronic_mail'
# #   , 'item_letters_and_memos'
# #   , 'item_food_production'
# #   , 'item_structured_versus_unstructured_work'
# #   , 'item_establishing_and_maintaining_interpersonal_relationships'
# #   , 'item_communicating_with_supervisors_peers_or_subordinates'
# #   # , 'item_speaking'
# #   # , 'item_writing'
# #   , 'item_freedom_to_make_decisions'
# #   , 'item_reading_comprehension'
# #   , 'item_level_of_competition'
# #   , 'item_degree_of_automation'
# #   , 'item_work_schedules'
# #   , 'item_indoors_environmentally_controlled'
# #   , 'item_impact_of_decisions_on_co_workers_or_company_results'
# #   , 'item_spend_time_sitting'
# #   , 'item_law_and_government'
# #   , 'item_public_safety_and_security'
# # ) -> chr_items_remove

# - Problematic items -----------------------------------------------------
c(
  # poorly measured items:
  'item_work_schedules'
  , 'item_time_pressure'
  , 'item_level_of_competition'
  , 'item_duration_of_typical_work_week'
  , 'item_frequency_of_decision_making'
  , 'item_degree_of_automation'
  
  # poorly clustering items:
  , 'item_law_and_government'
  , 'item_public_safety_and_security'
  # , 'item_establishing_and_maintaining_interpersonal_relationships'
  , 'item_food_production'
  , 'item_level_of_competition'
  , 'item_duration_of_typical_work_week'
  # , item_time_sharing
  
  # # redundant:
  # , 'item_reading_comprehension'
  # , 'item_speaking'
  
  # useless items:
  , 'item_near_vision'
) -> chr_items_remove

unique(
  chr_items_remove
) -> chr_items_remove

length(chr_items_remove)

# - Questionnaire items ---------------------------------------------------
218 - length(chr_items_remove)

(218 - length(chr_items_remove)) / 
  c(1, 2, 4)

# - Retained variance -----------------------------------------------------
df_occupations %>% 
  select(starts_with(
    'item'
  )) %>% 
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
  arrange(desc(
    variance
  )) %>% 
  mutate(
    pct_variance = 
      variance / 
      sum(variance)
    , rank = row_number()
    , norm_rank = 
      variance / 
      max(variance)
  ) -> df_variance

df_variance %>% 
  filter(
    item %in%
      chr_items_remove
  ) %>% 
  rename(
    problematic_item = item
  )

df_variance %>%
  filter(!(
    item %in%
      chr_items_remove
  )) %>% 
  pull(pct_variance) %>% 
  sum() %>% 
  setNames(
    'retained_variance'
  )

# - Problematic items distribution ----------------------------------------
library(ggplot2)

for(i in 1:length(chr_items_remove)){
  plot(
    df_occupations %>%
      ggplot(aes(
        x = !!sym(chr_items_remove[i])
        , weight = employment_variants
      )) +
      geom_density() + 
      scale_x_continuous(
        limits = c(0, 100)
      )
  )
}

# - Data wrangling --------------------------------------------------------
df_occupations %>% 
  select(
    employment_variants
    , starts_with('item_')
    , -any_of(chr_items_remove)
  ) -> df_occupations_efa

# [EFA] -------------------------------------------------------------------
# - Vectorized EFA ---------------------------
atlas.efa::fun_efa_vfa(
  df_data =
    df_occupations_efa[-1]
  # , int_factors = NULL
  , int_factors = c(14, 15, 16)
  # , int_factors = c(14, 15, 16, 20)
  , chr_rotation = 'equamax'
  # Use only equamax
  # , chr_rotation = c('equamax', 'promax')
  # , chr_rotation = c('oblimin', 'equamax')
  # , chr_rotation = c('quartimax', 'equamax')
  , dbl_weights = 
    df_occupations_efa$
    employment_variants
  , int_min_items_factor = 3
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa

# - Evaluate results --------------------------------------------------------------
list_efa$model_performance %>% arrange(desc(performance))
list_efa$reliability_metrics %>% map(~ min(.x$nitems))
list_efa$reliability_metrics
list_efa$reliability_evaluation
list_efa$factor_correlations %>% map( ~ .x$suggested_rotation)
list_efa$factor_correlations %>% map( ~ .x$redundant_factors)
list_efa$loadings_long
list_efa$adequacy_tests
list_efa$nfactors

list_efa$
  model_performance %>%
  arrange(desc(
    performance
  )) %>%
  select(model) %>%
  map(
    ~ list_efa$
      loadings_long[.x]
  ) -> list_efa_loadings

list_efa$
  loadings_long[
    list_efa$
      model_performance %>%
      arrange(desc(
        performance
      )) %>%
      pull(model)
  ] -> list_efa_loadings

for(df in 1:length(list_efa_loadings)){
  print(
    list_efa_loadings[[df]]
    , n = nrow(list_efa_loadings[[df]])
  )
}

# ctrl --

# all 218 items
# almost good

# poorly clustering items:
# item_law_and_government
# item_public_safety_and_security
# item_establishing_and_maintaining_interpersonal_relationships
# item_food_production
# item_level_of_competition
# item_duration_of_typical_work_week

# suboptimal clustering:
# item_programming
# interesting enough,
# item_foreign_language is clustering correctly

# 210 items


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
# [EXPORT] ----------------------------------------------------------------
# - New EFA model (RDS) ---------------------------------------------------
# - New questionnaires (xlsx, csv) ----------------------------------------





