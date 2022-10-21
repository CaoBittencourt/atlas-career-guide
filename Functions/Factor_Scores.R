# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'psych' #Factor Analysis
  , 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# ------- FUNCTIONS -----------------------------------------------------------
# FACTOR SCORES FUNCTION -----------------------------------------------------------
fun_factor.scores <- function(
    
  .df_data.numeric
  , .list_factor.keys
  , .lgc_totals = F
  , .lgc_pivot.long = F
  
  ){
  
  # Data frame
  if(!is.data.frame(.df_data.numeric)){
    
    stop("'.df_data.numeric' must be a data frame.")
    
  }
  
  # List
  if(!is.list(.list_factor.keys)){
    
    stop("'.list_factor.keys' must be a data frame.")
    
  }
  
  # Logical
  if(!(isTRUE(.lgc_totals) | !isTRUE(.lgc_totals))){
    
    stop("'.lgc_totals' must be a either TRUE or FALSE.")
    
  }
  
  if(!(isTRUE(.lgc_pivot.long) | !isTRUE(.lgc_pivot.long))){
    
    stop("'.lgc_pivot.long' must be a either TRUE or FALSE.")
    
  }
  
  
  # Select only numeric data
  .df_data.numeric %>% 
    select(where(is.numeric)) -> .df_data.numeric
  
  
  # Factor scores
  map(
    .list_factor.keys
    , function(scales){
      
      psych::scoreVeryFast(
        keys = scales
        , items = .df_data.numeric
        , totals = .lgc_totals #Average scores
      ) %>% 
        as_tibble() -> df_temp
      
      if(nrow(.df_data.numeric) == 1){
        
        df_temp %>% 
          colMeans() -> df_temp
        
      }
      
      return(df_temp)
      
    } 
  ) %>% 
    flatten_df() -> df_factor.scores

    
  # Pivot data
  if(.lgc_pivot.long){
    
    # Factor names data frame
    list_factors %>%
      bind_rows(
        .id = 'competency'
      ) %>%
      pivot_longer(
        cols = -competency
        , names_to = 'factor'
        , values_to = 'item'
      ) %>%
      drop_na() %>%
      ungroup() -> df_factors.names
    
    # Pivot and join to factor names
    .df_data.numeric %>% 
      pivot_longer(
        cols = everything()
        , names_to = 'item'
        , values_to = 'score'
      ) %>% 
      full_join(
        df_factors.names
      ) -> df_data.long
    
    # Add factor scores
    df_factor.scores %>% 
      pivot_longer(
        cols = everything()
        , names_to = 'factor'
        , values_to = 'factor.score'
      ) %>% 
      full_join(
        df_data.long
      ) -> df_data.long
    
  } else { 
    
    df_data.long <- NULL
    
    }
  
  
  # Output
  return(compact(list(
    'factor.scores' = df_factor.scores
    , 'factor.scores.long' = df_data.long
  )))
  
}
