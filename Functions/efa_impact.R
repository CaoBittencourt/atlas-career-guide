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
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_EFA.R')
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/auto_efa_fa.R')

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
.chr_rotation <- 'promax'
# quartimax

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
fun_EFA.impact <- function(
    
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
    "'.efa_model' must be a 'factanal' object returned from the factanal function." =
      str_to_lower(class(
        .efa_model
      )) == 'factanal'
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

# # [EFA] RUN FACTOR ANALYSIS ON THE WHOLE DATA FRAME -----------------------
# # Occupations data frame on a 0 to 100 scale
# df_occupations.pop %>%
#   # select(
#   #   ends_with('.l')
#   # ) %>% 
#   mutate(across(
#     .cols = ends_with('.l')
#     ,.fns = ~ .x * 100
#   )) -> df_occupations.pop
# 
# # [x] Try the fa() function from the psych package
# # [ ] Try competencies-only model and a 
# # [ ] work-related-only (context, activities) model
# 
# fun_best.model.top.items.workflow(
#   .df_data.numeric = 
#     df_occupations %>% 
#     select(ends_with('.l'))
#   , .auto_select.nfactors = T
#   , .int_min.factor_size = 
#     .int_min.factor_size
#   , .int_n.items.total = 60
#   , .chr_rotation =
#     .chr_rotation
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
# 
# list_efa.models$all.models.reliability
# list_efa.models$EFA.workflow$EFA$reliability.evaluation %>% 
#   filter(items.Min >= 3) %>% view
# 
# 
# fun_top.items.multi.workflow(
#   .df_data.numeric = 
#     df_occupations %>% 
#     select(ends_with('.l'))
#   , .int_n.items.total = 15 * 10
#   , .auto_select.nfactors = F
#   , .int_nfactors.vector = c(1,15)
#   , .chr_rotation = 'promax'
#   , .remove_unacceptable_MSAi.items = F
#   , .remove_under_loading.items = F
#   , .remove_cross_loading.items = F
#   , .show_diagrams = F
#   , .show_results = F
# ) -> list_efa.models
# 
# 
# dsds$sufficient.loadings
# dsds$reliability.evaluation
# 
# 
# 
# loadings(dsds)[,] %>% 
#   as_tibble(
#     rownames = 'item'
#   ) %>% 
#   set_names(
#     c(
#       'item'
#       , loadings(dsds) %>%
#         # as.matrix() %>%
#         colnames() %>% 
#         str_extract(
#           '[[:digit:]]+'
#         ) %>%
#         paste0('Factor',.)
#     )
#   ) %>% 
#   relocate(
#     item
#     , str_sort(
#       names(.)
#       , numeric = T
#     )
#   )
# 
# 
# 
# 
# # Run EFA on occupations data frame
# fun_best.model.workflow(
#   .df_data.numeric = 
#     df_occupations
#   , .chr_rotation = 
#     .chr_rotation
#   , .auto_select.nfactors = 
#     .auto_select.nfactors
#   , .int_min.factor_size = 
#     .int_min.factor_size
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
# 
# fun_EFA(
#   .df_data.numeric = 
#     df_occupations
#   , .chr_rotation = 
#     .chr_rotation
#   # , .int_nfactors = 21
#   , .int_nfactors = 15
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
#     , .show_results 
# ) -> list_efa.15factors
# 
# 
# fun_nfactors.selection(
#   df_occupations
# ) -> df_factors.recommended
# 
# fun_EFA.multi(
#   .df_data.numeric = 
#     df_occupations
#   , .auto_select.nfactors = F
#   , .int_nfactors.vector =
#     df_factors.recommended$Factors.Suggested
#   , .chr_rotation =
#     .chr_rotation
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
#     , .show_results 
# ) -> list_efa
# 
# list_efa.models$EFA.workflow$reliability.metrics %>% view
# list_efa.models$EFA.workflow$reliability.evaluation %>% view

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

fun_EFA.impact(
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
