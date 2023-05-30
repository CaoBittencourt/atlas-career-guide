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

# [FUNCTION] ----------------------------------------------
# EFA-based exogenous impact analysis ------------------------------------
fun_efa.impact <- function(

  # Data
  .df_data
  # Sample weights
  , .dbl_weights = NULL
  # Factor loadings
  , .efa_model
  # Factor impact
  , .dbl_factors.impact
  # Scale truncation
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
  # Impact truncation
  , .dbl_impact.lb = -Inf
  , .dbl_impact.ub = Inf
  # Immunity truncation
  , .dbl_immune.lb = 0
  , .dbl_immune.ub = 17
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
    "'.dbl_weights' must be a vector of sample weights the same length as '.df_data'." =
      any(
        is.null(.dbl_weights)
        , all(
          is.numeric(.dbl_weights)
          , length(.dbl_weights) ==
            .df_data %>%
            nrow()
        )
      ))

  stopifnot(
    "'.dbl_factors.impact' must be a vector of expected impact on each factor." =
      all(
        is.numeric(.dbl_factors.impact)
        , length(.dbl_factors.impact) ==
          loadings(.efa_model)[,] %>%
          ncol()
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

  stopifnot(
    "'.dbl_immune.lb' must be numeric." =
      is.numeric(.dbl_immune.lb)
  )

  stopifnot(
    "'.dbl_immune.ub' must be numeric." =
      is.numeric(.dbl_immune.ub)
  )

  # Data wrangling
  .dbl_scale.lb[[1]] -> dbl_scale.lb
  .dbl_scale.ub[[1]] -> dbl_scale.ub

  .dbl_impact.lb[[1]] -> dbl_impact.lb
  .dbl_impact.ub[[1]] -> dbl_impact.ub

  .dbl_immune.lb[[1]] -> dbl_immune.lb
  .dbl_immune.ub[[1]] -> dbl_immune.ub

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
    as_tibble(
      rownames = 'item'
    ) %>%
    set_names(
      c(
        'item'
        , loadings(.efa_model)[,] %>%
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
      , values_to = 'factor.loading'
    ) -> df_loadings.long

  .dbl_factors.impact %>%
    as_tibble() %>%
    set_names(
      'factor.impact'
    ) %>%
    mutate(
      .before = 1
      , factor =
        df_loadings.long %>%
        pull(factor) %>%
        unique()
    ) -> df_factors.impact

  df_loadings.long %>%
    full_join(df_factors.impact) %>%
    mutate(
      factor.impact = pmax(
        factor.impact
        , -dbl_scale.ub
      )
      , factor.impact = pmin(
        factor.impact
        , dbl_scale.ub
      )
    ) %>%
    group_by(item) %>%
    reframe(
      item.impact =
        sum(
          factor.loading *
            factor.impact
        )
      , item.impact = pmax(
        item.impact
        , dbl_impact.lb
      )
      , item.impact = pmin(
        item.impact
        , dbl_impact.ub
      )
    ) -> df_impact.items

  # Calculate net impact
  df_data.long %>%
    full_join(
      df_impact.items
    ) %>%
    mutate(
      .after = item.score
      , item.score2 =
        if_else(
          dplyr::between(
            item.score
            , dbl_immune.lb
            , dbl_immune.ub
          )
          , true =
            item.score +
            pmin(item.impact, 0)
          , false =
            item.score +
            item.impact
        )
      , item.score2 = pmax(
        item.score2
        , dbl_scale.lb
      )
      , item.score2 = pmin(
        item.score2
        , dbl_scale.ub
      )
    ) %>%
    mutate(
      .after = item.impact
      , item.impact.rate =
        if_else(
          item.score == 0
          , (item.score2 - item.score) /
            dbl_scale.ub
          , (item.score2 - item.score) /
            item.score
        )
    ) -> df_impact

  # Aggregate results
  df_impact.agg <- NULL
  df_impact.all <- NULL

  if(.lgc_aggregate){

    df_impact %>%
      group_by(across(c(
        -where(is.numeric)
        , -item
      ))) %>%
      reframe(
        aggregate.impact =
          sum(item.score2) /
          sum(item.score)
        , aggregate.impact =
          aggregate.impact - 1
      ) -> df_impact.agg

    if(!length(.dbl_weights)){

      rep(1, nrow(.df_data)) -> .dbl_weights

    }

    df_impact.agg %>%
      reframe(
        aggregate.impact =
          weighted.mean(
            aggregate.impact
            , .dbl_weights
          )
      ) -> df_impact.all

  }

  # Output
  return(compact(list(
    'factors.impact' = df_factors.impact
    , 'items.impact' = df_impact.items
    , 'aggregate.impact' = df_impact.agg
    , 'overall.impact' = df_impact.all
    , 'individual.impact' = df_impact
    , 'weights' = .dbl_weights
    , 'scale.lb' = dbl_scale.lb
    , 'scale.ub' = dbl_scale.ub
    , 'impact.lb' = dbl_impact.lb
    , 'impact.ub' = dbl_impact.ub
  )))

}
