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
  
  df_data %>%
    select(any_of(
      df_factors$item
    )) -> df_data_factors
  
  df_data %>%
    select(!any_of(
      df_factors$item
    )) -> df_data
  
  # Are all items required to calculate factor scores?
  # Create factor keys list
  df_factors %>%
    split(.$factor) %>% 
    map(~ .x[-1]) %>% 
    c() -> list_factors
  
  rm(df_factors)
  
  return(list(
    list_factors
    ,df_data_factors
  ))
  stop()
  
  # Score items
  scoreVeryFast(
    keys = list_factors
    , items = df_data_factors
    , totals = F
  ) -> df_factor_scores
  
  return(df_factor_scores)
  stop()
  
  # Add id columns to data
  df_data %>% 
    bind_cols(
      df_factor_scores
    ) -> df_factor_scores
  
  # Aggregate results
  if(lgc_pivot){
    
    # dsds -> 
    #   df_factor_scores
    
  }
  
  # Output
  return(df_factor_scores)
  
}

# dsds --------------------------------------------------------------------
# read_rds(
#   "C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds"
# ) -> efa_model
fun_factor_scores(
  df_input,
  efa_model
) -> dsds

dsds[[1]]
dsds[[2]]

# loadings(efa_model) %>% 
#   ... %>% 
#   pivot_long() %>% 
#   ... %>% 
#   recode(
#     !max = 0
#     max = 1
#   ) -> df_factor_keys

?scoreVeryFast(
  keys = dsds[[1]]
  , items = dsds[[2]]
  , totals = F
)
