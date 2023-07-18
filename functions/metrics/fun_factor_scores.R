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
  
  stopifnot(
    "'efa_model' must be a factor analysis object." =
      any(
        str_to_lower(class(
          efa_model
        )) == 'factanal'
        , str_to_lower(class(
          efa_model
        )) == 'fa'
      )
  )
  
  # Data wrangling
  # df_data %>% 
  #   select dsds -> df_data_factors
  
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
    ) %>% 
    group_by(factor) %>% 
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
  
  # Create factor keys list
  # df_factors %>% 
  #   dsds -> list_factors
  rm(df_factors)
  
  # Score items
  scoreVeryFast(
    keys = list_factors
    , items = df_data
    , totals = F
  ) -> df_factor_scores
  
  # Add id columns to data
  
  # Aggregate results
  if(lgc_pivot){
    
    # dsds -> 
    #   df_factor_scores
    
  }
  
  # Output
  return(df_factor_scores)
  
}
