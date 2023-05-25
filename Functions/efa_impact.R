# [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# pkg <- c(
#   # 'labelled', 
#   'tidyverse' #Data wrangling
#   # , 'openxlsx' #Export excel
#   # , 'blsR' #BLS API
# )
# 
# # Activate / install packages
# lapply(pkg, function(x)
#   if(!require(x, character.only = T))
#   {install.packages(x); require(x)})
# 
# # Package citation
# # lapply(pkg, function(x)
# #   {citation(package = x)})

# - Data --------------------------------------------------------------------
# Occupations data frame
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.R')
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')
# Acronyms
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_acronyms.R')

# - Functions ---------------------------------------------------------------
# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# Capital flexibility
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')
# EFA
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/auto_efa_fa.R')
# KNN matching
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/KNN_Matching.R')

# - Parameters --------------------------------------------------------------
# Colors
list(
  'green' = '#4AF7B0'
  , 'purple1' = '#753AF9'
  , 'purple2' = '#301866'
  , 'purple3' = '#3854FB'
  , 'blue1' = '#56D0F5'
  , 'blue2' = '#ABF4D4'
  , 'blue3' = '#43DED1'
  , 'blue4' = '#182766'
  , 'red' = '#CE3527'
  
  , 'abilities' = '#C92618'
  , 'knowledge' = '#FF9E1F'
  , 'skills' = '#50915D'
  
  , 'black' = '#212121'
  , 'grey' = '#D4D5D8'
) -> list_pal.atlas

# EFA parameters
# Oblique rotation
.chr_rotation <- 'oblimin'
# .chr_rotation <- 'promax'
# Orthogonal rotation
# .chr_rotation <- 'varimax'
# .chr_rotation <- 'quartimax'

# Number of factors
.auto_select.nfactors <- T

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

# [FUNCTION] EFA-BASED IMPACT ANALYSIS ----------------------------------------------
fun_efa.impact <- function(
    
  # Data
  .df_data
  # Factor loadings
  , .efa_model
  # Factor impact
  , .df_factors.impact
  # Scale truncation
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
  # Impact truncation
  , .dbl_impact.lb = -Inf
  , .dbl_impact.ub = Inf
  # Aggregate results
  , .lgc_aggregate = F
  
){
  
  # Arguments validation
  stopifnot(
    "'.efa_model' must be a factor analysis object." =
      any(
        str_to_lower(class(
          .efa_model
        )) == 'factanal'
        , str_to_lower(class(
          .efa_model
        )) == 'fa'
      )
  )
  
  stopifnot(
    "'.df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(.df_data)
        , any(
          loadings(.efa_model)[,] %>%
            rownames() %in%
            names(.df_data)
        )))
  
  stopifnot(
    "'.df_factors.impact' must be a data frame in long format containing factor impact evaluations." =
      all(
        is.data.frame(.df_factors.impact)
        , c('factor', 'factor.impact') %in% 
          names(.df_factors.impact)
        , loadings(.efa_model)[,] %>%
          colnames() %in%
          .df_factors.impact$factor
        , is.numeric(
          .df_factors.impact$factor.impact
        )
      ))
  
  stopifnot(
    "'.dbl_scale.lb' must be numeric." =
      is.numeric(.dbl_scale.lb)
  )
  
  stopifnot(
    "'.dbl_scale.ub' must be numeric." =
      is.numeric(.dbl_scale.ub)
  )
  
  stopifnot(
    "'.dbl_impact.lb' must be numeric." =
      is.numeric(.dbl_impact.lb)
  )
  
  stopifnot(
    "'.dbl_impact.lb' must be numeric." =
      is.numeric(.dbl_impact.lb)
  )
  
  # Data wrangling
  .dbl_scale.lb[[1]] -> dbl_scale.lb
  .dbl_scale.ub[[1]] -> dbl_scale.ub
  
  .dbl_impact.lb[[1]] -> dbl_impact.lb
  .dbl_impact.ub[[1]] -> dbl_impact.ub
  
  .df_data %>% 
    select(
      !where(is.numeric)
      , any_of(
        loadings(.efa_model)[,] %>%
          rownames()
      )) %>%
    pivot_longer(
      cols = is.numeric
      , names_to = 'item'
      , values_to = 'item.score'
    ) %>% 
    mutate(
      item.score = pmax(
        item.score
        , dbl_scale.lb
      )
      , item.score = pmin(
        item.score
        , dbl_scale.ub
      )
    ) -> df_data.long
  
  loadings(.efa_model)[,] %>% 
    as_tibble(rownames = 'item') %>%
    pivot_longer(
      cols = !item
      , names_to = 'factor'
      , values_to = 'factor.loading'
    ) -> df_loadings.long
  
  df_data.long %>%
    full_join(df_loadings.long) %>% 
    full_join(.df_factors.impact) %>% 
    mutate(
      factor.impact = pmax(
        factor.impact
        , -dbl_scale.ub
      )
      , factor.impact = pmin(
        factor.impact
        , dbl_scale.ub
      )
    ) -> df_loadings.long
  
  # Calculate net impact
  df_loadings.long %>%
    group_by(
      across(c(
        -where(is.numeric)
        , -factor
      ))) %>%
    reframe(
      item.score = item.score
      , item.impact = 
        sum(
          factor.loading *
            factor.impact
        )
    ) %>% 
    group_by(
      across(c(
        -where(is.numeric)
      ))) %>%
    slice(1) %>%
    ungroup() %>% 
    mutate(
      .after = item.score
      , item.score2 =
        item.score +
        item.impact
      , item.score2 = pmax(
        item.score2
        , dbl_scale.lb
      )
      , item.score2 = pmin(
        item.score2
        , dbl_scale.ub
      ) 
      , item.impact.rate =
        if_else(
          item.score == 0
          , (item.score2 - item.score) / 
            dbl_scale.ub
          , (item.score2 - item.score) / 
            item.score
        )
      , item.impact.rate = pmax(
        item.impact.rate
        , dbl_impact.lb
      )
      , item.impact.rate = pmin(
        item.impact.rate
        , dbl_impact.ub
      )
    ) -> df_impact
  
  # Aggregate results
  df_impact.items <- NULL
  df_impact.agg <- NULL
  df_impact.all <- NULL
  
  if(.lgc_aggregate){
    
    df_impact %>% 
      group_by(
        across(c(
          -where(is.numeric)
          , -item
        ))) %>% 
      mutate(
        .before = item.score 
        , item.weight = 
          item.score / 
          sum(item.score)
      ) %>% 
      ungroup() -> df_impact.weighted
    
    df_impact.weighted %>% 
      group_by(
        across(c(
          -where(is.numeric)
          , -item
        ))) %>%
      reframe(across(
        .cols = is.numeric
        # ,.fns = mean
        ,.fns = ~ weighted.mean(
          .x, item.weight
        )
      )) -> df_impact.agg
    
    # df_impact.weighted %>%
    #   group_by(item) %>%
    #   reframe(across(
    #     .cols = is.numeric
    #     # ,.fns = mean
    #     ,.fns = ~ weighted.mean(
    #       .x, item.weight
    #     )
    #   )) -> df_impact.items
    
    df_impact %>%
      group_by(item) %>%
      reframe(across(
        .cols = is.numeric
        ,.fns = mean
      )) -> df_impact.items
    
    df_impact.weighted %>%
      reframe(across(
        .cols = is.numeric
        # ,.fns = mean
        ,.fns = ~ weighted.mean(
          .x, item.weight
        )
      )) -> df_impact.all
    
    # df_impact %>%
    #   reframe(across(
    #     .cols = is.numeric
    #     ,.fns = mean
    #   )) -> df_impact.all
    
  }
  
  # Output
  return(compact(list(
    'individual.impact' = df_impact
    , 'items.impact' = df_impact.items
    , 'aggregate.impact' = df_impact.agg
    , 'overall.impact' = df_impact.all
    , 'factors.impact' = .df_factors.impact
    , 'data' = df_data.long
    , 'scale.lb' = dbl_scale.lb
    , 'scale.ub' = dbl_scale.ub
    , 'impact.lb' = dbl_impact.lb
    , 'impact.ub' = dbl_impact.ub
  )))
  
}

# [EFA] RUN FACTOR ANALYSIS ON THE WHOLE DATA FRAME -----------------------
# Occupations data frame on a 0 to 100 scale
# df_occupations %>%
#   # select(
#   #   ends_with('.l')
#   # ) %>%
#   mutate(across(
#     .cols = ends_with('.l')
#     ,.fns = ~ .x * 100
#   )) -> df_occupations

# [x] Try the fa() function from the psych package
# [ ] Try competencies-only model and a
# [ ] work-related-only (context, activities) model
map(
  set_names(
    c(
      'oblimin'
      , 'Promax'
      , 'promax'
      # , 'simplimax' [no]
      , 'bentlerQ'
      , 'cluster'
    )
    , c(
      'oblimin'
      , 'Promax'
      , 'promax'
      # , 'simplimax' [no]
      , 'bentlerQ'
      , 'cluster'
    )
  )
  , ~ 
    fun_efa.bestmodel(
      .df_data.numeric =
        df_occupations %>%
        select(ends_with('.l')) #%>% 
      # select(1:120)
      , .dbl_weights = 
        df_occupations %>%
        pull(employment2)
      , .auto_select.nfactors = T
      , .int_min.factor_size =
        .int_min.factor_size
      , .chr_rotation = .x
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
    ) 
) -> list_efa.oblique

list_efa.oblique %>%
  map(~ .x$best.models.evaluation) %>% 
  bind_rows(.id = 'rotation') %>% view

list_efa.orthogonal %>%
  map(~ .x$best.models.evaluation) %>% 
  bind_rows(.id = 'rotation') %>% view

list_efa.oblique %>% 
  list_flatten() %>% 
  map(
    ~ .x$EFA
  ) %>%
  list_flatten() %>% 
  map(
    ~ .x$suggested.rotation
  ) %>% 
  compact() %>%
  bind_rows(
    .id = 'model'
  ) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'model'
    , values_to = 'suggested.rotation'
  ) %>% 
  mutate(
    .before = 1 
    , rotation = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,1]
    , model = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,2]
  ) -> df_rotation.oblique

list_efa.oblique %>% 
  list_flatten() %>% 
  map(
    ~ .x$EFA
  ) %>%
  list_flatten() %>% 
  map(
    ~ .x$suggested.rotation
  ) %>% 
  compact() %>%
  bind_rows(
    .id = 'model'
  ) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'model'
    , values_to = 'suggested.rotation'
  ) %>% 
  mutate(
    .before = 1 
    , rotation = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,1]
    , model = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,2]
  ) -> df_rotation.oblique

list_efa.orthogonal %>% 
  list_flatten() %>% 
  map(
    ~ .x$EFA
  ) %>%
  list_flatten() %>% 
  map(
    ~ .x$suggested.rotation
  ) %>% 
  compact() %>%
  bind_rows(
    .id = 'model'
  ) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'model'
    , values_to = 'suggested.rotation'
  ) %>% 
  mutate(
    .before = 1 
    , rotation = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,1]
    , model = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,2]
  ) -> df_rotation.orthogonal

sum(
  nrow(df_rotation.oblique)
  , nrow(df_rotation.orthogonal)
)

df_rotation.oblique %>% 
  drop_na() %>%
  group_by(suggested.rotation) %>%
  tally()

df_rotation.oblique %>% 
  drop_na() %>% 
  filter(
    suggested.rotation ==
      'oblique'
  )

list_efa.oblique %>% 
  list_flatten() %>% 
  map(
    ~ .x$EFA
  ) %>%
  list_flatten() %>% 
  map(
    ~ .x$loadings.long.factors
  ) %>% 
  compact() %>% 
  bind_rows(
    .id = 'model'
  ) -> df_items.oblique

df_items.oblique %>% 
  mutate(
    .after = 1 
    , rotation = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,1]
    , nfactors = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,2] %>% 
      parse_number()
  ) %>%
  filter(
    rotation == 'Promax'
    # rotation == 'promax' [no]
    # rotation == 'oblimin' [ok]
    # rotation == 'oblimin'
    # rotation == 'bentlerQ'
    # , nfactors == 16
    # , nfactors == 14
    , nfactors == 15
  ) %>% 
  select(1:6) %>%
  view


df_items.oblique %>% 
  mutate(
    model = 
      model %>% 
      str_remove_all(
        '_EFA.workflow_EFA'
      )
  ) %>% 
  select(1:4) %>% 
  openxlsx::write.xlsx(file = 'df_items.oblique2.xlsx')

map(
  set_names(
    c(
      'varimax'
      , 'quartimax'
      , 'equamax'
    )
    , c(
      'varimax'
      , 'quartimax'
      , 'equamax'
    )
  )
  , ~ 
    fun_efa.bestmodel(
      .df_data.numeric =
        df_occupations %>%
        select(ends_with('.l')) #%>% 
      # select(1:120)
      , .dbl_weights = 
        df_occupations %>%
        pull(employment2)
      , .auto_select.nfactors = T
      , .int_min.factor_size =
        .int_min.factor_size
      , .chr_rotation = .x
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
    ) 
) -> list_efa.orthogonal

list_efa.orthogonal %>%
  map(~ .x$best.models.evaluation) %>% 
  bind_rows(.id = 'rotation') %>% view

list_efa.orthogonal %>% 
  list_flatten() %>% 
  map(
    ~ .x$EFA
  ) %>%
  list_flatten() %>% 
  map(
    ~ .x$loadings.long.factors
  ) %>% 
  compact() %>% 
  bind_rows(
    .id = 'model'
  ) -> df_items.orthogonal

# df_occupations$near_vision.l
# df_occupations$foreign_language.l
# df_occupations$duration_of_typical_work_week.l
# df_occupations$telephone.l
# df_occupations$electronic_mail.l
# df_occupations$letters_and_memos.l
# df_occupations$food_production.l

# list(
#   'near_vision.l'
#   # , 'foreign_language.l'
#   , 'duration_of_typical_work_week.l'
#   # , 'telephone.l'
#   , 'electronic_mail.l'
#   # , 'letters_and_memos.l'
#   # , 'food_production.l'
#   , 'structured_versus_unstructured_work.l'
#   # , 'establishing_and_maintaining_interpersonal_relationships.l'
#   # , 'communicating_with_supervisors_peers_or_subordinates.l'
#   # , 'speaking.l'
#   # , 'writing.l'
#   # , 'reading_comprehension.l'
#   # , 'degree_of_automation.l'
#   , 'work_schedules.l'
#   , 'indoors_environmentally_controlled.l'
#   # , 'impact_of_decisions_on_co_workers_or_company_results.l'
#   , 'spend_time_sitting.l'
# ) -> list_items.remove

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

list_items.remove %>%
  flatten_chr() %>% 
  writeClipboard()
  set_names(
    list_items.remove %>%
      flatten_chr()
  ) %>% 
  map_df(
    ~ df_occupations %>%
      select(ends_with('.l')) %>% 
      weights::wtd.cors(
        weight = 
          df_occupations %>%
          pull(employment2)
      ) %>% 
      as_tibble(
        rownames = 'item'
      ) %>% 
      filter(item == .x) %>% 
      pivot_longer(
        cols = -item
        , names_to = 'items'
        , values_to = 'correlation'
      ) %>% 
      filter(items != .x) %>%
      arrange(desc(
        correlation
      ))
  ) -> df_items.problematic

df_items.problematic %>% 
  filter(str_detect(
    item, 'law'
  )) %>% 
  mutate(correlation = round(correlation, 4)) %>% view

df_items.problematic %>% 
  group_by(item) %>% 
  slice(1,n()) %>%
  ungroup() %>% view

df_occupations %>% 
  select(contains(
    'comprehension'
  )) %>%
  names

df_occupations %>% 
  select(contains(
    'expression'
  )) %>%
  names

df_occupations %>% 
  select(contains(
    'read'
  )) %>%
  names

weights::wtd.cors(
  x = df_occupations$deductive_reasoning.l
  , y = df_occupations$inductive_reasoning.l
  , weight = df_occupations$employment2
)

df_occupations %>%
  select(ends_with('.l')) %>%
  select(
    !list_items.remove %>%
      flatten_chr()
  ) %>%
  fun_efa.nfactors()

fun_efa.bestmodel(
  .df_data.numeric =
    df_occupations %>%
    select(ends_with('.l')) %>%
    select(
      !list_items.remove %>%
        flatten_chr()
    )
  , .dbl_weights = 
    df_occupations %>%
    pull(employment2)
  # , .int_nfactors.vector = c(4,5,8,seq(13,16))
  , .int_nfactors.vector = seq(10,16)
  , .auto_select.nfactors = F
  # .auto_select.nfactors
  , .int_min.factor_size =
    .int_min.factor_size
  # , .chr_rotation = 'varimin' [no]
  , .chr_rotation = 'equamax'
  # , .chr_rotation = 'varimax' [no]
  # , .chr_rotation = 'quartimax' [no]
  # , .chr_rotation = 'oblimin' [no]
  # , .chr_rotation = 'Promax' [no]
  # , .chr_rotation = 'promax' [no]
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
) -> list_efa.equamax

list_efa.equamax$
  best.models.evaluation %>%
  view

map(
  list_efa.equamax$
    EFA.workflow$
    EFA
  , ~ .x$loadings.long.factors
) -> dsds

map_chr(
  dsds
  , ~ .x %>% 
    filter(str_detect(item, 'medicine')) %>%
    # filter(str_detect(item, 'law')) %>%
    pull(factor) %>% 
    as.character()
) -> lalala

Map(
  function(ds,la){
    
    ds %>%
      filter(factor == la) %>%
      select(item, factor, loading)
    
  }
  , ds = dsds
  , la = lalala
) -> dsdsds

dsdsds %>% 
  map(
    ~ .x %>%
      pull(item) %>%
      str_detect('philosophy|theology|sociology') %>%
      # str_detect('philosophy|theology|sociology|law') %>%
      any()
  )

dsds$
  # EFA.10factors %>%
  # EFA.6factors [no] %>% 
  # EFA.12factors [no] %>%
  # EFA.13factors [no] %>%
  # EFA.14factors [no] %>%
  # EFA.15factors [no] %>%
  # EFA.16factors [no] %>%
  # EFA.17factors [no] %>%
  
  # EFA.12factors [no] %>%
  # EFA.13factors [no] %>%
# EFA.14factors [no] %>%
# EFA.15factors %>%
# EFA.16factors %>%

# oblimin
# EFA.11factors [no] %>%
# EFA.12factors [no] %>%
# EFA.13factors [no] %>%
# EFA.14factors %>%
# EFA.15factors %>%
# EFA.16factors %>%

#equamax no gov
# EFA.12factors %>%
# EFA.13factors [no] %>%
# EFA.14factors [ok] %>%
EFA.15factors #[this one] %>%
# EFA.16factors %>%
# EFA.16factors %>%
select(1:3) %>% 
  group_by(factor) %>%
  mutate(
    .before = factor
    , nitems = n()
  ) -> df_factors

df_factors %>% view

# df_factors %>% 
#   filter(str_detect(
#     item, 'law'
#   ))

# df_factors %>% 
#   split(.$factor) %>% 
#   pluck(4) %>% 
#   print(n = nrow(.))

df_factors %>%
  split(.$factor)

df_factors %>% 
  # slice(1:3) %>%
  # ungroup() %>%
  split(.$factor) %>% 
  set_names(
    c(
      'discernment'
      , 'mechanical skills'
      , 'health science'
      , 'transportation / operation'
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
  ) %>% 
  bind_rows(.id = 'factor.name') %>% 
  openxlsx::write.xlsx(file = 'df_efa.equamax.15factors.xlsx')

map2(
  .x = dsds
  , .y = lalala
  ~ .x %>% 
    ungroup() %>% 
    filter(factor == .y) 
)

list_efa.equamax$
  EFA.workflow$
  EFA$
  EFA.5factors$
  loadings.long.factors %>% 
  filter(str_detect(item, 'medicine')) %>% 
  pull(factor)

list_efa.equamax.13_17$
  EFA.workflow$
  EFA %>% 
  map(
    ~ .x$removed.items
  )

# fun_efa.mfa(
#   .df_data.numeric =
#     df_occupations %>%
#     select(ends_with('.l'))
#   , .dbl_weights = 
#     df_occupations %>%
#     pull(employment2)
#   , .int_nfactors.vector = seq(20,23)
#   , .chr_rotation = 'equamax'
#   # , .chr_rotation = 'oblimin'
#   , .remove_unacceptable_MSAi.items =
#     .remove_unacceptable_MSAi.items
#   , .remove_under_loading.items =
#     .remove_under_loading.items
#   , .remove_cross_loading.items =
#     .remove_cross_loading.items
#   , .dbl_under_loading.threshold =
#     .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold =
#     .dbl_cross_loading.threshold
#   , .show_diagrams =
#     .show_diagrams
#   , .show_results =
#     .show_results
# ) -> list_efa.20_23.equamax

list_efa.orthogonal$
  equamax$
  EFA.workflow$
  EFA$
  EFA.15factors$
  loadings %>% 
  filter(
    item == 'establishing_and_maintaining_interpersonal_relationships.l'
  ) %>% 
  # rowwise() %>% 
  mutate(
    loading.max = max(c_across(-item))
  )

list_efa.equamax.13_16$
  best.models.evaluation %>% 
  view

list_efa.equamax.13_16$
  best.model$
  n.factors

list_efa.equamax.13_17$
  EFA.workflow$
  EFA$
  EFA.13factors$
  # EFA.14factors$
  # EFA.15factors$
  # EFA.16factors$
  loadings.long.factors %>% 
  select(1:3) %>%
  group_by(factor) %>% 
  mutate(
    .before = factor
    , nitems = n()
  ) %>% 
  ungroup() %>%
  view

# list_efa.20_23.equamax$
#   EFA$
#   EFA.22factors$
#   loadings.long.factors %>% 
#   select(1:3) %>%
#   group_by(factor) %>% 
#   mutate(
#     .before = factor
#     , nitems = n()
#   ) %>% 
#   ungroup() %>%
#   view

df_items.orthogonal %>% 
  mutate(
    .after = 1 
    , rotation = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,1]
    , nfactors = 
      str_split_fixed(
        model
        , '_EFA.workflow_EFA.'
        , n = 2
      )[,2] %>% 
      parse_number()
  ) %>%
  filter(
    # rotation == 'quartimax' [no]
    # rotation == 'equamax' [ok]
    rotation == 'equamax'
    # rotation == 'varimax' [no]
    , nfactors == 16
  ) %>% 
  select(1:6) %>% view

list_efa.equamax.13_16$
  EFA.workflow$
  EFA$
  EFA.13factors$
  loadings.long.factors %>% 
  select(1:3) %>%
  group_by(factor) %>% 
  mutate(
    factor.items = n()
  ) %>% 
  ungroup() %>% 
  view


fun_efa.topitems(
  .df_data.numeric = 
    list_efa.equamax.13_16$
    EFA.workflow$
    EFA$
    EFA.15factors$
    data
  , .dbl_weights =
    df_occupations$
    employment2
  , .efa_model = 
    list_efa.equamax.13_16$
    EFA.workflow$
    EFA$
    EFA.15factors$
    model
  , .int_n.items.total = 60
  , .lgc_uneven.factors = T
  , .int_min.factor_size = 3
) %>% 
  arrange(desc(
    factor.items
  )) %>%
  view

df_items.orthogonal %>% 
  mutate(
    model = 
      model %>% 
      str_remove_all(
        '_EFA.workflow_EFA'
      )
  ) %>% 
  select(1:4) %>% 
  openxlsx::write.xlsx(file = 'df_items.orthogonal2.xlsx')

# fun_efa.bestmodel(
#   .df_data.numeric =
#     df_occupations %>%
#     select(ends_with('.l')) %>% 
#     select(1:120)
#   , .dbl_weights = 
#     df_occupations %>%
#     pull(employment2)
#   , .int_nfactors.vector = c(3,5,8,10)
#   , .auto_select.nfactors = F
#   , .int_min.factor_size =
#     .int_min.factor_size
#   , .chr_rotation =
#     'promax'
#     # 'oblimin' [no]
#     # 'simplimax' [no]
#     # .chr_rotation
#     # 'equamax'
#   , .remove_unacceptable_MSAi.items =
#     .remove_unacceptable_MSAi.items
#   , .remove_under_loading.items =
#     .remove_under_loading.items
#   , .remove_cross_loading.items =
#     .remove_cross_loading.items
#   , .dbl_under_loading.threshold =
#     .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold =
#     .dbl_cross_loading.threshold
#   , .show_diagrams =
#     .show_diagrams
#   , .show_results =
#     .show_results
# ) -> list_efa.models

list_efa.models$
  all.models.evaluation %>% view 

list_efa.models$
  best.models.evaluation %>% view

list_efa.models$EFA.workflow$EFA$EFA.10factors$n.factors

list_efa.models$EFA.workflow$EFA$EFA.1factor$loadings.long.factors
list_efa.models$EFA.workflow$EFA$EFA.10factor$loadings.long.factors

list_efa.rotations$
  oblimin$
  best.models.evaluation %>% view

fun_efa.topitems(
  .df_data.numeric = 
    df_occupations %>% 
    select(ends_with('.l')) %>% 
    select(1:120)
  , .efa_model = 
    list_efa.rotations$
    oblimin$
    EFA.workflow$
    EFA$
    EFA.2factors$
    model
  , .dbl_weights = 
    df_occupations %>% 
    pull(employment2)
  # , .int_n.items.total = 120
  , .lgc_uneven.factors = T
  , .int_n.items.total = 60
  , .int_min.factor_size = 3
) -> df_questionnaire

df_questionnaire %>% tally

fun_KNN.matching(
  .df_data.numeric = 
    df_occupations %>% 
    select(
      occupation
      , df_questionnaire %>%
        pull(item)
    )
  , .vec_query.numeric = 
    df_input.user %>% 
    select(
      df_questionnaire %>%
        pull(item)
    )
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = 0.17
) %>% 
  select(
    rank
    , occupation
    , similarity
  ) %>% 
  print(n = 20)

list_efa.models$
  EFA.workflow$
  EFA$
  EFA.10factor$
  reliability.evaluation %>% 
  view

list(
  # 'MR1' = 'Discernment'
  'MR1' = 'Problem Solving'
  , 'MR2' = 'Dexterity'
  , 'MR3' = '(Health) Science'
  # , 'MR4' = 'Business'
  , 'MR4' = 'Office'
  , 'MR5' = 'Perception'
  , 'MR6' = 'Building'
  # , 'MR7' = 'Intelligence'
  , 'MR7' = 'Mathematics'
  , 'MR8' = 'Physical Fitness'
  , 'MR9' = 'Arts & Humanities'
  , 'MR10' = 'Management'
)


df_questionnaire %>% 
  ungroup() %>% 
  mutate(
    item.purity.norm = 
      item.purity - 
      item.purity.norm
    , item.
  ) %>% view

fun_efa.impact(
  .df_data = 
    df_occupations %>%
    select(
      occupation
      , ends_with('.l')
    ) %>% 
    select(1:121)
  , .df_factors.impact = 
    tibble(
      'factor' = 
        loadings(
          list_efa.models$
            EFA.workflow$
            EFA$
            EFA.2factors$
            model
        )[,] %>% 
        colnames()
      , 'factor.impact' = c(
        'Discernment' = (-17/4)
        , 'Technical Skills' = -50
      )
    )
  , .efa_model = 
    list_efa.models$
    EFA.workflow$
    EFA$
    EFA.2factors$
    model
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
  , .dbl_impact.lb = -Inf
  , .dbl_impact.ub = Inf
  , .lgc_aggregate = T
) -> list_efa.impact

list_efa.impact$individual.impact %>% view
list_efa.impact$items.impact %>% view
list_efa.impact$aggregate.impact %>% view
list_efa.impact$overall.impact %>% view

list_efa.impact %>% 
  mutate(
    factor.impact.wgt = 
      factor.impact * 
      factor.loading
  ) %>% view
group_by(item) %>% 
  reframe(
    item.score = 
      item.score %>% 
      unique()
    , factor.impact.wgt = 
      factor.impact.wgt %>% 
      sum()
  )

list_efa.models$
  EFA.workflow$
  EFA$
  EFA.2factors$
  loadings %>% 
  arrange(desc(
    factor2
  )) %>%
  print(n = nrow(.))


list_efa.impact$individual.impact %>% view
list_efa.impact$factors.impact

fun_top.items.multi.workflow(
  .df_data.numeric =
    df_occupations %>%
    select(ends_with('.l'))
  , .int_n.items.total = 15 * 10
  , .auto_select.nfactors = F
  , .int_nfactors.vector = c(1,15)
  , .chr_rotation = 'promax'
  , .remove_unacceptable_MSAi.items = F
  , .remove_under_loading.items = F
  , .remove_cross_loading.items = F
  , .show_diagrams = F
  , .show_results = F
) -> list_efa.models


dsds$sufficient.loadings
dsds$reliability.evaluation



loadings(dsds)[,] %>%
  as_tibble(
    rownames = 'item'
  ) %>%
  set_names(
    c(
      'item'
      , loadings(dsds) %>%
        # as.matrix() %>%
        colnames() %>%
        str_extract(
          '[[:digit:]]+'
        ) %>%
        paste0('Factor',.)
    )
  ) %>%
  relocate(
    item
    , str_sort(
      names(.)
      , numeric = T
    )
  )




# Run EFA on occupations data frame
fun_best.model.workflow(
  .df_data.numeric =
    df_occupations
  , .chr_rotation =
    .chr_rotation
  , .auto_select.nfactors =
    .auto_select.nfactors
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
) -> list_efa.models

fun_EFA(
  .df_data.numeric =
    df_occupations
  , .chr_rotation =
    .chr_rotation
  # , .int_nfactors = 21
  , .int_nfactors = 15
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
    , .show_results
) -> list_efa.15factors


fun_nfactors.selection(
  df_occupations
) -> df_factors.recommended

fun_EFA.multi(
  .df_data.numeric =
    df_occupations
  , .auto_select.nfactors = F
  , .int_nfactors.vector =
    df_factors.recommended$Factors.Suggested
  , .chr_rotation =
    .chr_rotation
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
    , .show_results
) -> list_efa

list_efa.models$EFA.workflow$reliability.metrics %>% view
list_efa.models$EFA.workflow$reliability.evaluation %>% view

# DSDS --------------------------------------------------------------------
# P.S.:
# partial replacement ~ 50, not 83!
# 83 == structural unemployment
# Auxiliary level ~ 25
tibble(
  'label' = 
    c(
      'Complete replacement'
      , 'Severe replacement'
      , 'Very high replacement'
      , 'Partial replacement'
      , 'Medium impact'
      , 'Auxiliary impact'
      , 'No impact'
    )
  , 'desc' = 
    c(
      'Unemployed'
      , 'Monitor machines'
      , 'Prepare machines'
      , 'Setup machines'
      , 'Reallocate time'
      , 'Increase speed'
      , 'Business as usual'
    )
  , 'impact' = round(seq(-100, 0, length.out = 7))
) %>% 
  mutate(
    .before = 1
    , interval = 
      findInterval(
        impact
        , impact
      )) %>% 
  mutate(
    .after = impact
    , hours.saved = 
      (-(impact / 100)) * 8
    , hours.worked = 
      8 - hours.saved
    , across(
      .cols = starts_with('hours.')
      ,.fns = ~ 
        paste(
          floor(.x)
          , round((
            .x - floor(.x)
          ) * 60)
          , sep = 'h'
        )
    )
  ) -> df_labels

view(df_labels)

tibble(
  'factor' = 
    loadings(
      EFA_skill.1$EFA.workflow$EFA$EFA.2Factors$model
    )[,] %>% 
    colnames()
  , 'factor.impact' = c(
    'Discernment' = -17/4
    , 'Technical Skills' = -50
  )
) -> df_impact

df_impact %>% 
  mutate(
    .after = 1
    , factor.name = 
      c(
        'Discernment'
        , 'Technical Skills'
      )
  ) %>% 
  mutate(
    impact.interval = 
      findInterval(
        factor.impact
        , df_labels$impact
      )) %>% 
  left_join(
    df_labels
    , by = c(
      'impact.interval' = 'interval'
    )
  ) %>% view

fun_efa.impact(
  .df_data = 
    df_occupations %>% 
    select(
      occupation
      , ends_with('.l')
    ) %>% 
    mutate(across(
      .cols = is.numeric
      ,.fns = ~ .x * 100
    ))
  , .efa_model = EFA_skill.1$EFA.workflow$EFA$EFA.2Factors$model
  , .df_factors.impact = df_impact
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
  , .dbl_impact.lb = -Inf
  , .dbl_impact.ub = Inf
  , .lgc_aggregate = T
) -> dsds

dsds$individual.impact
dsds$items.impact
dsds$aggregate.impact
dsds$overall.impact

list_efa.impact -> dsds

dsds$items.impact %>% 
  fun_plot.histogram(aes(
    x = item.impact.rate
  )
  , .list_axis.x.args = list(
    limits = c(-1, 1)
    , breaks = c(
      -round(seq(0,1,1/6), 2)
      , round(seq(0,1,1/6), 2)
    )
  )
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'Average item impact'
  )
  )

dsds$individual.impact %>% 
  fun_plot.histogram(aes(
    x = item.impact.rate
  )
  , .list_axis.x.args = list(
    limits = c(-1, 1)
    , breaks = c(
      -round(seq(0,1,1/6), 2)
      , round(seq(0,1,1/6), 2)
    )
  )
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'Individual impact (items vs occupations)'
  )
  )

dsds$aggregate.impact %>% 
  fun_plot.histogram(aes(
    x = item.impact.rate
  )
  , .list_axis.x.args = list(
    limits = c(-1, 1)
    , breaks = c(
      -round(seq(0,1,1/6), 2)
      , round(seq(0,1,1/6), 2)
    )
  )
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'Aggregate impact (occupations)'
  )
  )

sum(
  ncol(df_occupations.numeric.ablt)
  , ncol(df_occupations.numeric.skill)
  , ncol(df_occupations.numeric.know)
) * .3

sum(
  ncol(df_occupations.numeric.context)
  , ncol(df_occupations.numeric.activities)
)

dsds$aggregate.impact %>% 
  select(
    occupation
    , item.impact.rate
  ) %>% 
  full_join(
    df_occupations.pop
  ) %>% 
  fun_plot.histogram(aes(
    x = item.impact.rate
  )
  , .list_axis.x.args = list(
    limits = c(-1, 1)
    , breaks = c(
      -round(seq(0,1,1/6), 2)
      , round(seq(0,1,1/6), 2)
    )
  )
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'Aggregate impact (occupations)'
  )
  )
