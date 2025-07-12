# CAPITAL FLEXIBILITY FUNCTION --------------------------------------------
fun_capital.flex <- function(
    .dbl_var
    , .dbl_lb = 0
    , .dbl_ub = 1
    , .dbl_lambda = 0.25
    , .lgc_sample = F
){
  
  # Check if var is numeric
  if(all(is.na(.dbl_var))){
    
    return(NA)
    
  } else if(!is.numeric(.dbl_var)){
    
    stop("'.dbl_var' must be a numeric vector.")
    
  }
  
  if(
    length(.dbl_lb) != 1
    | !is.numeric(.dbl_lb)
  ){
    
    stop("'.dbl_lb' must be a numeric variable of length 1.")
    
  }
  
  if(
    length(.dbl_ub) != 1
    | !is.numeric(.dbl_ub)
  ){
    
    stop("'.dbl_ub' must be a numeric variable of length 1.")
    
  }
  
  if(
    length(.dbl_ub) != 1
    | !is.numeric(.dbl_ub)
    | !(.dbl_lambda >= 0 & .dbl_lambda <= 1)
  ){
    
    stop("'.dbl_lambda' must be a numeric variable between 0 and 1.")
    
  }
  
  # Logical
  if(!(
    is.logical(.lgc_sample) &
    !is.na(.lgc_sample)
  )){
    
    stop("'.lgc_sample' must be either TRUE or FALSE.")
    
  }
  
  # Prevent division by zero by increasing the scale by 1
  if(.dbl_ub == 0){
    
    .dbl_lb + 1 -> .dbl_lb
    .dbl_ub + 1 -> .dbl_ub
    .dbl_var + 1 -> .dbl_var
    
  }
  
  # Calculate upper limit for standard deviation
  sd(c(.dbl_lb, .dbl_ub), na.rm = T) -> dbl_std.u
  
  if(!.lgc_sample){
    # Population standard deviation
    dbl_std.u / sqrt(2) -> dbl_std.u
    
  }
  
  # Calculate standard deviation
  # Sample standard deviation
  sd(.dbl_var, na.rm = T) -> dbl_std
  
  if(!.lgc_sample){
    # Population standard deviation
    sqrt((length(.dbl_var) - 1) / length(.dbl_var)) * dbl_std -> dbl_std
    
  }
  
  # Calculate Mode
  unique(mlv(x = .dbl_var, method = 'shorth')) -> dbl_mode
  
  # Calculate capital flexibility score
  # .dbl_lambda +
  #   (1 - .dbl_lambda) * (dbl_mode / .dbl_ub) +
  #   -.dbl_lambda * sqrt(dbl_std / dbl_std.u) +
  #   -.dbl_lambda * (1 - (dbl_mode / .dbl_ub)) *
  #   (1 - 2 * sqrt(dbl_std / dbl_std.u)) -> dbl_kflex
  
  .dbl_lambda +
    (1 - .dbl_lambda) * (dbl_mode / .dbl_ub) +
    -.dbl_lambda * (dbl_std / dbl_std.u) +
    -.dbl_lambda * (1 - (dbl_mode / .dbl_ub)) *
    (1 - 2 * (dbl_std / dbl_std.u)) -> dbl_kflex
  
  
  # Output
  return(dbl_kflex)
  
}

