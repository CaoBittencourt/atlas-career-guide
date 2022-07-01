# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'TruncatedNormal' #Generate truncated normal correlated data
  , 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# # MASS vs dplyr conflicts
# select <- dplyr::select

# SIMULATED DATA FUNCTION ---------------------------------------------------------------
fun_simulate.tmvnorm <- function(
    # Basic
    df_data.numeric 
    , int_n.simulations = 100
    , chr_observations.name = 'Subject'
    # Truncated multivariate normal distribution parameters
    , dbl_lower.bound = 0
    , dbl_upper.bound = 1
    
){
  
  # Get numeric data only
  df_data.numeric %>% 
    select(where(is.numeric)) -> df_data.numeric
  
  # Generic names for each observation
  chr_observations <- paste(chr_observations.name, 1:int_n.simulations)
  names(chr_observations) <- chr_observations
  
  # Get the first line from the correlation matrix
  # In order to keep the original relationship between variables
  df_data.numeric %>% 
    cov() -> mtx_cov
  
  df_data.numeric %>% 
    cor() %>% 
    as_tibble() %>%
    slice(1) -> df_correlations
  
  # Simulate normal distributions in accordance with the correlation matrix
  # Mean for each variable
  df_data.numeric %>%
    summarise(across(
      .fns = mean
    )) %>% 
    as.numeric() -> dbl_mean
  
  # Multivariate truncated normal distribution
  rtmvnorm(
    n = int_n.simulations
    , mu = dbl_mean
    , sigma = mtx_cov
    , lb = rep(dbl_lower.bound, length(dbl_mean))
    , ub = rep(dbl_upper.bound, length(dbl_mean))
  ) -> mtx_tmvnorm
  
  colnames(mtx_tmvnorm) <- colnames(df_data.numeric)
  
  mtx_tmvnorm %>%
    as_tibble() -> df_simulations
  
  # Add the names of each observation
  df_simulations %>% 
    mutate(
      !!sym(chr_observations.name) := chr_observations
      , .before = names(.)[1]
    ) %>%
    return(.)
  
}



