# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'psych' #Factor scores
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTION] ----------------------------------------------
# - Factor loadings function ----------------------------------------------
fun_factor_loadings <- function(efa_model){
  
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
  
  # Get factor loadings
  loadings(efa_model)[,] %>%
    as_tibble(
      rownames = 'item'
    ) -> df_loadings
  
  # Data wrangling
  df_loadings %>%
    set_names(
      c(
        'item'
        , df_loadings[-1] %>% 
          names() %>% 
          # , loadings(efa_model)[,] %>%
          #   colnames() %>%
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
    ) -> df_loadings
  
  # Output
  return(df_loadings)
  
}

# - Factor scores function ------------------------------------
fun_factor_scores <- function(
    
  # Data
  df_data
  # Factor loadings
  , efa_model
  # Aggregate results
  , lgc_pivot = F
  
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(df_data)
        , any(
          loadings(efa_model)[,] %>%
            rownames() %in%
            names(df_data)
        )))
  
  # Data wrangling
  fun_factor_loadings(
    efa_model
  ) %>%
    pivot_longer(
      cols = -item
      , names_to = 'factor'
      , values_to = 'factor_loading'
    ) %>%
    group_by(item) %>%
    filter(
      factor_loading ==
        max(factor_loading)
    ) %>%
    select(
      factor
      , item
    ) %>%
    ungroup() ->
    df_factors
  
  # Factor keys data frame
  df_data %>%
    select(any_of(
      df_factors$item
    )) -> df_data_factors
  
  df_factors %>% 
    filter(
      item %in% names(
        df_data_factors
      )
    ) -> df_factors
  
  df_data %>%
    select(!any_of(
      df_factors$item
    )) -> df_data
  
  # Create factor keys list
  df_factors %>%
    split(.$factor) %>% 
    map(~ pull(.x[-1])) %>% 
    c() -> list_factors
  
  rm(df_factors)
  
  # Score items
  if(nrow(df_data_factors) == 1){
    
    scoreVeryFast(
      keys = list_factors
      , items = rbind(
        df_data_factors, 
        df_data_factors
      )
      , totals = F
    ) %>% 
      as_tibble() %>%
      slice_head(n = 1) -> 
      df_factor_scores
    
  } else {
    
    scoreVeryFast(
      keys = list_factors
      , items = df_data_factors
      , totals = F
    ) %>% 
      as_tibble() -> 
      df_factor_scores
    
  }
  
  # Add id columns to data
  df_data %>% 
    bind_cols(
      df_factor_scores
    ) %>% 
    relocate(
      !starts_with('factor')
      , str_sort(
        names(.)
        , numeric = T
      )
    ) -> df_factor_scores
  
  # Aggregate results
  if(lgc_pivot){
    
    df_factor_scores %>% 
      pivot_longer(
        cols = starts_with('factor')
        , names_to = 'factor'
        , values_to = 'factor_score'
      ) -> df_factor_scores
    
  }
  
  # Output
  return(df_factor_scores)
  
}

