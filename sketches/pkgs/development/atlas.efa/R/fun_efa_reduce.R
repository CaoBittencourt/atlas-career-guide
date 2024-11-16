fun_efa_retained_variance <- function(
    mtx_correlations_abs
    , dbl_variance
    , lgc_keep_item = T
){
  
  # Original covariance
  mtx_correlations_abs * 
    dbl_variance ->
    mtx_retained_covariance
  
  # Covariance after removing items
  mtx_correlations_abs * 
    dbl_variance *
    lgc_keep_item ->
    mtx_retained_covariance2
  
  c(
    'retained_covariance' =
      sum(mtx_retained_covariance2) /
      sum(mtx_retained_covariance)
  ) -> pct_retained_covariance
  
  # Output
  return(pct_retained_covariance)
  
}

fun_efa_reduce <- function(
    mtx_correlations
    , dbl_variance
    , pct_retained_variance = 0.95
    , int_retained_items = NULL
){
  
  # Arguments validation
  
  # Data wrangling
  if(!length(int_retained_items)){
    
    nrow(mtx_correlations) -> 
      int_retained_items
    
  } else {
    
    int_retained_items[[1]] ->
      int_retained_items
    
    ceiling(
      int_retained_items
    ) -> int_retained_items
    
  }
  
  # Helper functions
  fun_efa_correlations_abs <- function(mtx_correlations){
    
    # Get the lower triangle 
    # with diagonal
    mtx_correlations *
      lower.tri(
        mtx_correlations
        , diag = T
      ) -> mtx_correlations
    
    # Absolute correlations
    abs(mtx_correlations) -> 
      mtx_correlations
    
    # Output
    return(mtx_correlations)
    
  }
  
  # Absolute value lower triangle correlation matrix
  fun_efa_correlations_abs(
    mtx_correlations
  ) -> mtx_correlations_abs
  
  rm(mtx_correlations)
  
  # Original covariance
  mtx_correlations_abs * 
    dbl_variance ->
    mtx_original_covariance
  
  # Initialize 'lgc_keep_item' vector
  rep(T, nrow(mtx_correlations_abs)) -> 
    lgc_keep_item
  
  # Initialize correlation-adjusted retained variance
  pct_retained_covariance <- 1
  
  # Initialize correlation-adjusted retained variance matrix
  mtx_retained_covariance <- mtx_original_covariance
  
  # Remove items until threshold is reached
  while(
    pct_retained_covariance > pct_retained_variance
  ){
    
    # Remove items
    lgc_keep_item[
      which(
        mtx_retained_covariance == 
          min(mtx_retained_covariance, na.rm = T)
        , arr.ind = T
      )[[1]]
    ] <- NA
    
    mtx_retained_covariance *
      lgc_keep_item ->
      mtx_retained_covariance
    
    # Recalculate retained covariance
    c(
      'pct_retained_covariance' =
        sum(mtx_retained_covariance, na.rm = T) /
        sum(mtx_original_covariance, na.rm = T)
    ) -> pct_retained_covariance
   
    if(sum(lgc_keep_item, na.rm = T) == int_retained_items){
      break
    }
     
  }
  
  rm(mtx_original_covariance)
  rm(mtx_retained_covariance)
  
  lgc_keep_item[is.na(
    lgc_keep_item
  )] <- F
  
  # Remaining items
  rownames(mtx_correlations_abs)[
    lgc_keep_item
  ] -> chr_items_keep
  
  rownames(mtx_correlations_abs)[
    !lgc_keep_item
  ] -> chr_items_drop
  
  # Output
  return(list(
    'items_keep' = chr_items_keep,
    'items_drop' = chr_items_drop,
    'adjusted_retained_variance' = pct_retained_covariance
  ))
  
}

fun_efa_reduce(
  mtx_correlations = 
    mtx_correlations
  , dbl_variance = 
    df_variance$
    variance
  , pct_retained_variance = 0.95
  , int_retained_items = 200
) -> list_retained_variance

list_retained_variance$items_keep
list_retained_variance$items_drop
list_retained_variance$adjusted_retained_variance
