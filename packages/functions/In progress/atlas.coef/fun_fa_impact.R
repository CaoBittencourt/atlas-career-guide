# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  # 'tidyverse' #Data wrangling
  'dplyr', 'tidyr' #Data wrangling
  # , 'atlas.ftools' #Factor analysis tools
  # , 'stats'
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTION] ----------------------------------------------
# - Factor-Analytic Comparative Statics ------------------------------------
fun_coef_fstatics <- function(
    # Data
  df_data
  # Sample weights
  , dbl_weights = NULL
  # Factor loadings
  , efa_model
  # Factor impact
  , dbl_factors_impact
  # Scale truncation
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  # Impact truncation
  , dbl_impact_lb = -Inf
  , dbl_impact_ub = Inf
  # Immunity truncation
  , dbl_immune_lb = 0
  , dbl_immune_ub = 17
  # Aggregate results
  , lgc_aggregate = F
  
){
  
  # Arguments validation
  stopifnot(
    "'efa_model' must be a factor analysis object." =
      any(
        str_to_lower(class(
          efa_model
        )) == 'factanal'
        , str_to_lower(class(
          efa_model
        )) == 'fa'
        , str_to_lower(class(
          efa_model
        )) == 'principal'
      )
  )
  
  stopifnot(
    "'df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(df_data)
        , any(
          loadings(efa_model)[,] %>%
            rownames() %in%
            names(df_data)
        )))
  
  stopifnot(
    "'dbl_weights' must be a vector of sample weights the same length as 'df_data'." =
      any(
        is.null(dbl_weights)
        , all(
          is.numeric(dbl_weights)
          , length(dbl_weights) ==
            df_data %>%
            nrow()
        )
      ))
  
  stopifnot(
    "'dbl_factors_impact' must be a vector of expected impact on each factor." =
      all(
        is.numeric(dbl_factors_impact)
        , length(dbl_factors_impact) ==
          loadings(efa_model)[,] %>%
          ncol()
      ))
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_impact_lb' must be numeric." =
      is.numeric(dbl_impact_lb)
  )
  
  stopifnot(
    "'dbl_impact_lb' must be numeric." =
      is.numeric(dbl_impact_lb)
  )
  
  stopifnot(
    "'dbl_immune_lb' must be numeric." =
      is.numeric(dbl_immune_lb)
  )
  
  stopifnot(
    "'dbl_immune_ub' must be numeric." =
      is.numeric(dbl_immune_ub)
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  dbl_impact_lb[[1]] -> dbl_impact_lb
  dbl_impact_ub[[1]] -> dbl_impact_ub
  
  dbl_immune_lb[[1]] -> dbl_immune_lb
  dbl_immune_ub[[1]] -> dbl_immune_ub
  
  df_data %>%
    select(
      !where(is.numeric)
      , any_of(
        loadings(efa_model)[,] %>%
          rownames()
      )) %>%
    pivot_longer(
      cols = is.numeric
      , names_to = 'item'
      , values_to = 'item_score'
    ) %>%
    mutate(
      item_score = pmax(
        item_score
        , dbl_scale_lb
      )
      , item_score = pmin(
        item_score
        , dbl_scale_ub
      )
    ) -> df_data_long
  
  loadings(efa_model)[,] %>%
    as_tibble(
      rownames = 'item'
    ) %>%
    set_names(
      c(
        'item'
        , loadings(efa_model)[,] %>%
          colnames() %>%
          str_extract(
            '[[:digit:]]+'
          ) %>%
          paste0('factor',.)
      )
    ) %>%
    relocate(
      item
      , str_sort(
        names(.)
        , numeric = T
      )
    ) %>%
    pivot_longer(
      cols = !item
      , names_to = 'factor'
      , values_to = 'factor_loading'
    ) -> df_loadings_long
  
  dbl_factors_impact %>%
    as_tibble() %>%
    set_names(
      'factor_impact'
    ) %>%
    mutate(
      .before = 1
      , factor =
        df_loadings_long %>%
        pull(factor) %>%
        unique()
    ) -> df_factors_impact
  
  df_loadings_long %>%
    full_join(
      df_factors_impact
    ) %>%
    mutate(
      factor_impact = pmax(
        factor_impact
        , -dbl_scale_ub
      )
      , factor_impact = pmin(
        factor_impact
        , dbl_scale_ub
      )
    ) %>%
    group_by(
      item = factor(item)
    ) %>%
    reframe(
      item_impact =
        sum(
          factor_loading *
            factor_impact
        )
      , item_impact = pmax(
        item_impact
        , dbl_impact_lb
      )
      , item_impact = pmin(
        item_impact
        , dbl_impact_ub
      )
    ) -> df_impact_items
  
  # Calculate net impact
  df_data_long %>%
    full_join(
      df_impact_items
    ) %>%
    drop_na() %>%
    mutate(
      .after = item_score
      , item_score2 =
        if_else(
          dplyr::between(
            item_score
            , dbl_immune_lb
            , dbl_immune_ub
          )
          , true =
            item_score +
            pmin(item_impact, 0)
          , false =
            item_score +
            item_impact
        )
      , item_score2 = pmax(
        item_score2
        , dbl_scale_lb
      )
      , item_score2 = pmin(
        item_score2
        , dbl_scale_ub
      )
    ) %>%
    mutate(
      .after = item_impact
      , item_impact_rate =
        if_else(
          item_score == 0
          , (item_score2 - item_score) /
            dbl_scale_ub
          , (item_score2 - item_score) /
            item_score
        )
    ) -> df_impact
  
  # Aggregate results
  df_impact_agg <- NULL
  df_impact_all <- NULL
  
  if(lgc_aggregate){
    
    df_impact %>%
      group_by(
        across(c(
          -where(is.numeric)
          , -item
        )
        , .fns = fct_inorder
        )) %>%
      reframe(
        aggregate_impact =
          sum(item_score2) /
          sum(item_score)
        , aggregate_impact =
          aggregate_impact - 1
      ) -> df_impact_agg
    
    if(!length(dbl_weights)){
      
      rep(1, nrow(df_data)) -> dbl_weights
      
    }
    
    df_impact_agg %>%
      mutate(
        weight =
          dbl_weights
      ) -> df_impact_agg
    
    df_impact_agg %>%
      reframe(
        aggregate_impact =
          weighted.mean(
            aggregate_impact
            , weight
          )
      ) -> df_impact_all
    
  }
  
  # Output
  return(compact(list(
    'factors_impact' = df_factors_impact
    , 'items_impact' = df_impact_items
    , 'aggregate_impact' = df_impact_agg
    , 'overall_impact' = df_impact_all
    , 'individual_impact' = df_impact
    , 'weights' = dbl_weights
    , 'scale_lb' = dbl_scale_lb
    , 'scale_ub' = dbl_scale_ub
    , 'impact_lb' = dbl_impact_lb
    , 'impact_ub' = dbl_impact_ub
    , 'immune_lb' = dbl_immune_lb
    , 'immune_ub' = dbl_immune_ub
  )))
  
}
