# PERCENTAGE WITHIN N STARDARD DEVIATIONS FUNCTION ------------------------
fun_within.sd <- function(
    
  .dbl_var
  , .dbl_n = 3
  , .dbl_ub = NULL
  , .dbl_lb = NULL
  
){
  
  # Numeric
  if(!is.vector(.dbl_var, mode = 'numeric')){
    
    stop("'.dbl_var' must be a numeric vector.")
    
  }
  
  if(!is.vector(.dbl_n, mode = 'numeric')){
    
    stop("'.dbl_n' must be a numeric vector.")
    
  }
  
  if(
    length(.dbl_ub)
    & !is.vector(.dbl_ub, mode = 'numeric')
    ){
    
    stop("'.dbl_ub' must be numeric.")
    
  }
  
  if(
    length(.dbl_lb)
    & !is.vector(.dbl_lb, mode = 'numeric')
    ){
    
    stop("'.dbl_lb' must be numeric.")
    
  }
  
  # Coerce N into one element
  .dbl_n[[1]] -> .dbl_n
  
  # Mean and standard deviation
  sd(.dbl_var) -> dbl_sd
  
  mean(.dbl_var) -> dbl_mean
  
  
  # Percent within N standard deviations
  length(
    .dbl_var[
      .dbl_var <= min(dbl_mean + (.dbl_n * dbl_sd), .dbl_ub)
      & .dbl_var >= max(dbl_mean - (.dbl_n * dbl_sd), .dbl_lb)
    ]
  ) -> int_len
  
  int_len / length(.dbl_var) -> pct_within.sd
  
  
  # Output
  return(pct_within.sd)
  
}