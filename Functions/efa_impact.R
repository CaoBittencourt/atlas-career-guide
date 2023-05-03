# [SETUP] -----------------------------------------------------------------
# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')
# ACRONYMS
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_acronyms.R')
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')
# Capital flexibility
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')

library(Hmisc)
# library(ids)
library(stringi)

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

colorRampPalette(c(
  list_pal.atlas$purple2
  , list_pal.atlas$green
)) -> fun_gradient

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
      reframe(across(
        .cols = is.numeric
        ,.fns = mean
      )) -> df_impact.agg
    
    df_impact %>% 
      group_by(item) %>% 
      reframe(across(
        .cols = is.numeric
        ,.fns = mean
      )) -> df_impact.items
    
    df_impact %>% 
      reframe(across(
        .cols = is.numeric
        ,.fns = mean
      )) -> df_impact.all
    
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

# DSDS --------------------------------------------------------------------
fun_EFA.impact(
  .df_data = 
    df_occupations %>% 
    select(
      occupation
      , ends_with('.l')
    )
  , .efa_model = EFA_skill.1$EFA.workflow$EFA$EFA.2Factors$model
  , .df_factors.impact = df_impact
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
  , .dbl_impact.lb = -Inf
  , .dbl_impact.ub = Inf
  , .lgc_aggregate = T
) -> dsds

dsds$individual.impact
dsds$items.impact %>% view
dsds$aggregate.impact
dsds$overall.impact

dsds$factors.loadings %>% 
  mutate(
    item2_item = 
      if_else(
        item.score == 0
        , item.score2 / 100
        , item.score2 / item.score
      )
  ) %>%
  group_by(occupation) %>% 
  reframe(
    dsds = mean(item.impact)
    , lalala = mean(item2_item) - 1
  ) -> lalala

qplot(lalala$dsds, xlim = c(-1,1))
qplot(lalala$lalala, xlim = c(-1,1))

dsds$factors.loadings %>% 
  group_by(
    occupation
    , item
  ) %>% 
  slice(1) %>% 
  ungroup() %>% 
  group_by(occupation) %>% 
  reframe(
    dsds1 = mean(
      if_else(
        item.score == 0
        , item.score2 / 100
        , item.score2 / item.score
      )
    ) - 1
    , dsds2 = mean(item.impact)
  )

tibble(
  'factor' = 
    loadings(EFA_skill.1$EFA.workflow$EFA$EFA.2Factors$model)[,] %>% 
    colnames()
  , 'factor.impact' = 
    runif(
      loadings(EFA_skill.1$EFA.workflow$EFA$EFA.2Factors$model)[,] %>% 
        ncol()
      , -50
      , 25
    )
) -> df_impact

dsds$data
dsds$scale.lb
dsds$scale.ub
dsds$impact.lb
dsds$impact.ub
dsds$factors.impact
dsds$factors.loadings