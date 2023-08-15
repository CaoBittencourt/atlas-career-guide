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
  # poorly clustering items:
  , 'item_law_and_government'
  , 'item_public_safety_and_security'
  # , 'item_establishing_and_maintaining_interpersonal_relationships'
  , 'item_food_production'
  , 'item_level_of_competition'
  , 'item_duration_of_typical_work_week'
  , 'item_time_sharing'
  , 'item_frequency_of_decision_making'
  # redundant:
  , 'item_reading_comprehension'
  , 'item_speaking'
  , 'item_writing'
  # useless items:
  , 'item_near_vision'
  , 'item_indoors_environmentally_controlled'
) -> chr_items_remove

unique(
  chr_items_remove
) -> chr_items_remove

length(chr_items_remove)

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

# - Problematic items distribution ----------------------------------------
for(var in chr_items_remove){
  
  df_occupations %>%
    fun_plot.density(aes(
      x = !!sym(var)
      , weight = employment_variants
    )
    , .list_axis.x.args = list(
      limits = c(0,100)
    )
    , .fun_format.x = number_format(1)
    , .list_labs = list(
      y = NULL
    )) %>% 
    plot()
  
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
  , int_factors = c(14, 15, 16)
  , chr_rotation = 'equamax'
  , dbl_weights = 
    df_occupations_efa$
    employment_variants
  # , int_min_items_factor = 3
  , int_min_items_factor = 5
  , lgc_remove_low_msai_items = T
  # , lgc_adequacy_testing = T
  # , lgc_optimal_nfactors = T
  , lgc_adequacy_testing = F
  , lgc_optimal_nfactors = F
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
      ) %>% ○
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





