# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'dplyr', 'tidyr', 'readr' #Data wrangling
  , 'atlas.efa' #Github packages
  , 'writexl' #Export to excel
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# devtools::install_github('CaoBittencourtFerreira/atlas.ftools')
# devtools::install_github('CaoBittencourtFerreira/atlas.efa')

source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_plots.R')

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
c(
  # poorly measured items:
  'item_work_schedules'
  , 'item_time_pressure'
  , 'item_level_of_competition'
  , 'item_structured_versus_unstructured_work'
  , 'item_duration_of_typical_work_week'
  , 'item_frequency_of_decision_making'
  , 'item_degree_of_automation'
  , 'item_consequence_of_error'
  , 'item_impact_of_decisions_on_co_workers_or_company_results'
  , 'item_freedom_to_make_decisions'
  # useless items:
  # , 'item_near_vision'
  , 'item_spend_time_sitting'
  , 'item_indoors_environmentally_controlled'
  , 'item_face_to_face_discussions'
  
  # , 'item_responsible_for_others_health_and_safety'
  
  # , 'item_selective_attention'
  
  # , 'item_work_with_work_group_or_team'
  
  # , 'item_communicating_with_supervisors_peers_or_subordinates'
  # or remove or rename Explaining ideas? Presenting ideas?
  
  # poorly clustering items:
  , 'item_law_and_government'
  , 'item_public_safety_and_security'
  , 'item_establishing_and_maintaining_interpersonal_relationships'
  , 'item_food_production'
  # , 'item_monitoring'
  , 'item_multitasking' #rename to multi-tasking
  # # redundant:
  # , 'item_reading_comprehension' #rename to item_literacy / item_written_whatever according to factor
  # # , 'item_speaking' #rename to item_oral_whatever according to factor
  # # , 'item_writing' #rename to item_written_whatever according to factor
) -> chr_items_remove

# c(
#   # poorly measured items:
#   'item_work_schedules'
#   , 'item_time_pressure'
#   , 'item_level_of_competition'
#   , 'item_structured_versus_unstructured_work'
#   , 'item_duration_of_typical_work_week'
#   , 'item_frequency_of_decision_making'
#   , 'item_degree_of_automation'
#   , 'item_consequence_of_error'
#   , 'item_impact_of_decisions_on_co_workers_or_company_results'
#   , 'item_freedom_to_make_decisions'
#   # useless items:
#   , 'item_near_vision'
#   , 'item_spend_time_sitting'
#   , 'item_indoors_environmentally_controlled'
#   , 'item_face_to_face_discussions'
#   
#   # , 'item_work_with_work_group_or_team'
#   
#   # , 'item_communicating_with_supervisors_peers_or_subordinates'
#   # or remove or rename Explaining ideas? Presenting ideas?
#   
#   # poorly clustering items:
#   , 'item_law_and_government'
#   , 'item_public_safety_and_security'
#   , 'item_establishing_and_maintaining_interpersonal_relationships'
#   , 'item_food_production'
#   # , 'item_monitoring'
#   # , 'item_multitasking' #rename to multi-tasking
#   # # redundant:
#   # , 'item_reading_comprehension' #rename to item_literacy / item_written_whatever according to factor
#   # # , 'item_speaking' #rename to item_oral_whatever according to factor
#   # # , 'item_writing' #rename to item_written_whatever according to factor
# ) -> chr_items_remove

unique(
  chr_items_remove
) -> chr_items_remove

length(chr_items_remove)

# # - Problematic items -----------------------------------------------------
# c(
#   # poorly measured items:
#   'item_work_schedules'
#   , 'item_time_pressure'
#   , 'item_level_of_competition'
#   , 'item_structured_versus_unstructured_work'
#   , 'item_duration_of_typical_work_week'
#   , 'item_frequency_of_decision_making'
#   , 'item_degree_of_automation'
#   , 'item_consequence_of_error'
#   , 'item_impact_of_decisions_on_co_workers_or_company_results'
#   
#   # rename or remove item_communicating_with_supervisors_peers_or_subordinates
#   # Explaining ideas? Presenting ideas?
#   
#   # , rename to autonomy 'item_freedom_to_make_decisions'
#   
#   # poorly clustering items:
#   , 'item_law_and_government'
#   , 'item_public_safety_and_security'
#   , 'item_establishing_and_maintaining_interpersonal_relationships'
#   , 'item_food_production'
#   , 'item_level_of_competition'
#   , 'item_duration_of_typical_work_week'
#   , 'item_time_sharing'
#   , 'item_frequency_of_decision_making'
#   # redundant:
#   , 'item_reading_comprehension'
#   # , 'item_speaking'
#   # , 'item_writing'
#   , 'item_spend_time_sitting'
#   # useless items:
#   , 'item_near_vision'
#   , 'item_spend_time_sitting'
#   , 'item_indoors_environmentally_controlled'
# ) -> chr_items_remove
# 
# unique(
#   chr_items_remove
# ) -> chr_items_remove
# 
# length(chr_items_remove)

# - Questionnaire items ---------------------------------------------------
218 - length(chr_items_remove)

(218 - length(chr_items_remove)) / 
  c(1, 2, 4)

# - Item variance -----------------------------------------------------
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
  arrange(desc(
    variance
  )) %>% 
  print(
    n = Inf
  )

# - Retained variance -----------------------------------------------------
df_variance %>%
  filter(!problematic) %>% 
  pull(pct_variance) %>% 
  sum() %>% 
  setNames(
    'retained_variance'
  )

# - Correlation-adjusted retained variance ----------------------------------
df_occupations %>% 
  select(starts_with(
    'item_'
  )) %>%
  fun_efa_correlations(
    dbl_weights = 
      df_occupations$
      employment_variants
  ) -> mtx_correlations

mtx_correlations *
  lower.tri(
    mtx_correlations
    , diag = T
  ) -> mtx_correlations_abs

mtx_correlations_abs %>% 
  abs() -> mtx_correlations_abs

mtx_correlations_abs * 
  df_variance$variance ->
  mtx_retained_covariance

mtx_correlations_abs * 
  df_variance$variance *
  !df_variance$problematic ->
  mtx_retained_covariance2

c(
  'retained_covariance' =
    sum(mtx_retained_covariance2) /
    sum(mtx_retained_covariance)
)

# # - Problematic items distribution ----------------------------------------
# for(var in chr_items_remove){
# 
#   df_occupations %>%
#     fun_plot.density(aes(
#       x = !!sym(var)
#       , weight = employment_variants
#     )
#     , .list_axis.x.args = list(
#       limits = c(-25,125)
#     )
#     , .fun_format.x = number_format(1)
#     , .list_labs = list(
#       y = NULL
#     )) %>%
#     plot()
# 
# }

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
  # , int_factors = 12
  , int_factors = 14
  # , int_factors = c(14, 15, 16)
  # , int_factors = c(7, 14, 15, 16)
  , chr_rotation = 'equamax'
  , dbl_weights = 
    df_occupations_efa$
    employment_variants
  , int_min_items_factor = 4
  , lgc_remove_low_msai_items = T
  # , lgc_adequacy_testing = F
  # , lgc_optimal_nfactors = F
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa

# - Evaluate results --------------------------------------------------------------
list_efa$model_performance %>% arrange(desc(performance))
list_efa$reliability_metrics %>% map(~ min(.x$nitems))
list_efa$reliability_metrics
list_efa$reliability_evaluation
list_efa$factor_correlations %>% map(~ .x$suggested_rotation)
list_efa$factor_correlations %>% map(~ .x$redundant_factors)
list_efa$loadings_long
list_efa$adequacy_tests
list_efa$nfactors

list_efa$
  loadings_long[
    list_efa$
      model_performance %>%
      filter(
        evaluation != 
          'unacceptable'
      ) %>%
      arrange(desc(
        performance
      )) %>%
      pull(model)
  ] -> list_efa_loadings

for(df in 1:length(list_efa_loadings)){
  print(
    list_efa_loadings[[df]]
    , n = Inf
  )
}

# 12 factor model
# f1 discernment
# f2 mechanical skills
# f3 health science
# f4 business / management
# f5 transportation *
# f6 administrative
# f7 sales & marketing
# f8 arts & humanities
# f9 building / engineering / science and engineering [S(T)E(M)]
# f10 analytical skills
# f11 intelligence / mathematics and technology [(S)T(E)M]
# f12 robustness

# 12 factor model
# f1 discernment
# f2 mechanical skills
# f3 health science
# f4 management
# f5 transportation *
# f6 administrative
# f7 sales & marketing
# f8 arts & humanities
# f9 engineering
# f10 analytical skills
# f11 intelligence
# f12 robustness

# 14 factor model
# f1 discernment
# f2 engineering
# f3 health science
# f4 business
# f5 transportation
# f6 administrative
# f7 sales & marketing / social skills
# f8 arts & humanities
# f9 job hazards / jeopardy / risk
# f10 analytical skills
# f11 intelligence
# f12 robustness
# f13 industrial *
# f14 mechanical skills

# list_efa_loadings$
#   EFA_equamax_14factors %>% 
#   print(n = Inf)

# - Questionnaires --------------------------------------------------------
fun_efa_top_items(
  df_data = 
    df_occupations_efa[-1]
  , dbl_weights = 
    df_occupations_efa$
    employment_variants
  , efa_model = 
    list_efa$
    models$
    # EFA_equamax_12factors
    EFA_equamax_14factors
  , int_items_total_vector = 
    ncol(df_occupations_efa[-1]) * 
    c(1, 0.5, 0.25)
  , lgc_uneven_factors = T
  , int_min_items_factor = 4
) -> list_questionnaires

# [EXPORT] ----------------------------------------------------------------
# - Set working directory -------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - New EFA model (RDS) ---------------------------------------------------
write_rds(
  x = list_efa$models$EFA_equamax_14factors
  , file = 'efa_equamax_14factors.rds'
)

# - New questionnaires (xlsx, csv) ----------------------------------------
map2(
  .x = list_questionnaires
  , .y = names(list_questionnaires)
  , .f =
    ~ .x %>%
    writexl::write_xlsx(
      path = paste0('./', 'questionnaire_', .y, '.xlsx')
    )
)
