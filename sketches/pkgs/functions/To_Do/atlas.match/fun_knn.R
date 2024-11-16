# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
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

# [FUNCTIONS] ---------------------------------------------
# - KNN Alpha Function ---------------------------------------------------
fun_knn.alpha <- function(
    .df_data
    , .df_query
    , .dbl_scale.ub = 100
    , .lgc_sort = F
){
  
  # Arguments validation
  stopifnot(
    "'.df_data' must be a data frame." =
      is.data.frame(.df_data)
  )
  
  stopifnot(
    "'.df_query' must be a data frame." =
      is.data.frame(.df_query)
  )
  
  stopifnot(
    "'.dbl_scale.ub' must be numeric." =
      is.numeric(.dbl_scale.ub)
  )
  
  stopifnot(
    "'.lgc_sort' must be either TRUE or FALSE." =
      all(
        is.logical(.lgc_sort)
        , !is.na(.lgc_sort)
      )
  )
  
  # Data wrangling
  .dbl_scale.ub[[1]] -> .dbl_scale.ub
  
  .df_query %>%
    select(where(
      is.numeric
    )) -> .df_query
  
  .df_data[names(
    .df_query
  )] -> df_data.numeric
  
  # Euclidean distance
  .df_query[rep(
    1, nrow(df_data.numeric)
  ),] -> .df_query
  
  df_data.numeric - 
    .df_query -> df_dist
  
  abs(df_dist) -> df_dist
  
  # Normalize by the scale's upper bound
  df_dist / .dbl_scale.ub -> df_dist
  
  # Weigh distances by attribute relevance (alpha)
  df_dist *
    df_data.numeric /
    rowSums(
      df_data.numeric
    ) -> df_dist
  
  # Add Euclidean distances to original data frame
  .df_data %>%
    mutate(
      distance = rowSums(df_dist)
      , similarity = 1 - distance
    ) -> .df_data
  
  # Sort data frame
  if(.lgc_sort){
    
    .df_data %>% 
      arrange(
        distance
      ) -> .df_data
    
  }
  
  # Output
  return(.df_data)
  
}


