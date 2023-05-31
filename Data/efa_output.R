# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Data --------------------------------------------------------------------
# Occupations data frame
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.R')
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')

# - Functions ---------------------------------------------------------------
# EFA
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/auto_efa_fa.R')

# - Parameters --------------------------------------------------------------
# EFA parameters
# Sample weights
df_occupations %>% 
  pull(
    employment2
  ) -> .dbl_weights

# Orthogonal rotations
.chr_rotation <- 'equamax'
# .chr_rotation <- 'varimax'
# .chr_rotation <- 'varimin'
# .chr_rotation <- 'quartimax'

# Oblique rotations
# .chr_rotation <- 'oblimin'
# .chr_rotation <- 'Promax'
# .chr_rotation <- 'promax'
# .chr_rotation <- 'bentlerQ'
# .chr_rotation <- 'cluster'

# Number of factors
# Models tested: from 1 to 22+ factors
.int_nfactors <- 15
# .auto_select.nfactors <- T
.auto_select.nfactors <- F

# Minimum factor size
.int_min.factor_size <- 3

.remove_unacceptable_MSAi.items <- F
# Underloadings and crossloadings
.remove_under_loading.items <- F
.remove_cross_loading.items <- F
.dbl_under_loading.threshold <- 0.5
.dbl_cross_loading.threshold <- 0.2

# Diagrams and tests
.show_diagrams <- T
.show_results <- T

# [DATA] -----------------------------------------------------
# - Problematic items -----------------------------------------------------
list(
  'near_vision.l'
  , 'foreign_language.l'
  , 'duration_of_typical_work_week.l'
  # , 'telephone.l'
  # , 'electronic_mail.l'
  , 'letters_and_memos.l'
  , 'food_production.l'
  , 'structured_versus_unstructured_work.l'
  , 'establishing_and_maintaining_interpersonal_relationships.l'
  , 'communicating_with_supervisors_peers_or_subordinates.l'
  , 'speaking.l'
  , 'writing.l'
  , 'reading_comprehension.l'
  , 'degree_of_automation.l'
  , 'work_schedules.l'
  , 'indoors_environmentally_controlled.l'
  , 'impact_of_decisions_on_co_workers_or_company_results.l'
  , 'spend_time_sitting.l'
  , 'law_and_government.l'
) -> list_items.remove

# - Select items for EFA --------------------------------------------------
df_occupations %>%
  select(
    occupation
    , ends_with('.l')
  ) %>%
  select(
    !list_items.remove %>%
      flatten_chr()
  ) -> df_occupations.efa

# [EFA] -----------------------
# - Run factor analysis on the whole data frame ---------------------------
fun_efa.bestmodel(
  .df_data.numeric = 
    df_occupations.efa
  , .dbl_weights = 
    .dbl_weights
  , .chr_rotation =
    .chr_rotation
  , .auto_select.nfactors =
    .auto_select.nfactors
  , .int_nfactors.vector =
    .int_nfactors
  , .int_min.factor_size =
    .int_min.factor_size
  , .remove_unacceptable_MSAi.items =
    .remove_unacceptable_MSAi.items
  , .remove_under_loading.items =
    .remove_under_loading.items
  , .remove_cross_loading.items =
    .remove_cross_loading.items
  , .dbl_under_loading.threshold =
    .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold =
    .dbl_cross_loading.threshold
  , .show_diagrams =
    .show_diagrams
  , .show_results =
    .show_results
) -> list_efa.equamax.15

# - Factor names ----------------------------------------------------------
tibble(
  factor = 
    paste0('factor', seq(1,.int_nfactors))
  , factor.name = 
    c(
      'discernment'
      , 'mechanical skills'
      , 'health science'
      , 'transportation / vehicle operation / operation'
      , 'management'
      , 'social skills'
      , 'analytical skills'
      , 'business'
      , 'dexterity'
      , 'administrative'
      , 'building'
      , 'intelligence'
      , 'industrial / job hazards'
      , 'arts & humanities'
      , 'robustness'
    )
) -> df_factor.names

list_efa.equamax.15$
  best.model$
  loadings.long.factors %>%
  full_join(df_factor.names) %>%
  group_by(factor) %>% 
  mutate(
    nitems = n()
  ) %>% 
  ungroup() %>% 
  select(
    factor
    , factor.name
    , nitems
    , item
    , loading
  ) %>%
  ungroup() -> df_loadings.factors

# - Top items -------------------------------------------------------------
# Top items selection
list(
  'atlas.mini' = 0.25
  , 'atlas.pro' = 0.5
  , 'atlas.complete' = 1
) %>%
  map(
    ~ df_occupations.efa %>% 
      select(ends_with('.l')) %>% 
      ncol() * .x
  ) %>% 
  map(
    ~ fun_efa.topitems(
      .df_data.numeric = 
        df_occupations.efa
      , .dbl_weights = 
        .dbl_weights
      , .efa_model = 
        list_efa.equamax.15$
        EFA.workflow$
        EFA$
        EFA.15factors$
        model
      , .int_n.items.total = .x
      , .lgc_uneven.factors = T
      , .int_min.factor_size = 3
    ) %>% 
      full_join(df_factor.names) %>% 
      relocate(
        factor
        , factor.name
        , factor.items
        , item
        , everything()
      )
  ) -> list_questionnaires
