# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'Hmisc' #Weighted variance
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTIONS] -------------------------------------------------------------
# - Bootstrapping normality testing ---------------------------------------
fun_normality_test <- function(
    
  dbl_variable
  , dbl_weights = NULL
  , dbl_scale_ub = NULL
  , dbl_scale_lb = NULL
  , int_iterations = 100
  
){
  
  # Arguments validation
  
  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_variable[
    !is.na(dbl_variable)
  ] -> dbl_variable
  
  dbl_weights[
    !is.na(dbl_weights)
  ] -> dbl_weights
  
  if(!length(dbl_weights)){
    
    rep(1, length(dbl_variable)) ->
      dbl_weights
    
  }
  
  dbl_weights[
    1:length(dbl_variable)
  ] -> dbl_weights
  
  round(
    int_iterations
  ) -> int_iterations
  
  pmax(
    int_iterations, 1
  ) -> int_iterations
  
  # Aggregate and arrange unique data
  tibble(
    x = dbl_variable
    , w = dbl_weights
  ) %>% 
    group_by(x) %>% 
    reframe(
      x = first(x)
      , w = sum(w)
    ) %>% 
    arrange(x) ->
    df_data
  
  # Estimate mean and standard deviation
  df_data %>% 
    reframe(
      wgt.mean = 
        weighted.mean(x, w)
      , wgt.sd = sqrt(wtd.var(x, w))
      , n = n()
    ) -> df_data_stats
  
  # Bootstrap normal distributions
  replicate(
    int_iterations
    , expr = {
      sort(
        rnorm(
          df_data_stats$n
          , df_data_stats$wgt.mean
          , df_data_stats$wgt.sd
        ))
    }
  ) %>% 
    as_tibble() -> 
    df_normal_bootstrap
  
  # Apply bounds
  if(all(
    length(dbl_scale_ub)
    , length(dbl_scale_lb)
  )){
    
    df_normal_bootstrap[
      df_normal_bootstrap < 
        dbl_scale_lb
    ] <- dbl_scale_lb
    
    df_normal_bootstrap[
      df_normal_bootstrap > 
        dbl_scale_ub
    ] <- dbl_scale_ub
    
  }
  
  # Compare original data vs normal distributions
  df_normal_bootstrap - 
    df_data$x -> 
    df_normal_similarity
  
  abs(df_normal_similarity) -> 
    df_normal_similarity
  
  df_normal_similarity / 
    (max(df_data$x) - min(df_data$x)) -> 
    df_normal_similarity
  
  # Average similarity
  1 - 
    weighted.mean(
      rowMeans(df_normal_similarity)
      , df_data$w
    ) -> dbl_normal_similarity
  
  # Output
  return(dbl_normal_similarity)
  
}

# # dsds --------------------------------------------------------------------
# df_occupations %>% 
#   reframe(across(
#     .cols = ends_with('.l')
#     ,.fns = 
#       ~ .x %>% 
#       fun_normality_test(
#         dbl_weights = 
#           employment2
#         , dbl_scale_ub = 100
#         , dbl_scale_lb = 0
#         , int_iterations = 100
#       )
#   )) %>% 
#   pivot_longer(
#     cols = everything()
#     , names_to = 'item'
#     , values_to = 'item_normality'
#   ) -> df_items_normality_testing
# 
# df_items_normality_testing %>% 
#   arrange(item_normality) %>% 
#   view
