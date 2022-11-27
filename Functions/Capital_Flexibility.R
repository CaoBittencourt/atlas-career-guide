# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'modeest' #Mode
  # 'KbMvtSkew' #Bowley skewness
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# # CAPITAL FLEXIBILITY FUNCTION --------------------------------------------
# fun_capital.flex <- function(.dbl_var){
#   
#   # Check if var is numeric
#   if(all(is.na(.dbl_var))){
#     
#     return(NA)
#     
#   } else if(!is.numeric(.dbl_var)){
#     
#     stop("'.dbl_var' must be a numeric vector.")
#     
#   }
#   
#   # Calculate stardard deviation
#   sd(.dbl_var, na.rm = T) -> std
#   
#   
#   # Calculate skewness E [-1,1]
#   (
#     mean(.dbl_var, na.rm = T)
#     - median(.dbl_var, na.rm = T)
#   ) / sd(.dbl_var, na.rm = T) -> sk
#   
#   # # If standard deviation approximates 0, skewness explodes
#   # if(std == 0){
#   #   
#   #   mean(.dbl_var, na.rm = T) - median(.dbl_var, na.rm = T) -> sk
#   #   
#   # } else if(sk > 1){
#   #   
#   #   sk <- 1
#   #   
#   # } else if(sk < -1){
#   #   
#   #   sk <- -1
#   #   
#   # }
#   
#   # Metric does not yield expected results when everything is 0
#   # kflex(0,0,...,0) should be 0
#   # Metric also does not yield excepected when every value is 1
#   # kflex(1,1,...,1) should be 1
#   
#   # Calculate capital flexibility score
#   # Adjust skewness by stardard deviation
#   # Convert skewness to a 0 to 1 metric
#   (1 - sk * (1 - std)) / 2 -> kflex
#   
#   # Output
#   return(kflex)
#   
# }

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
  
  # Calculate stardard deviation
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


# # CAPITAL FLEXIBILITY FUNCTION --------------------------------------------
# fun_capital.flex <- function(
    #     .dbl_var
#     , .dbl_lb = 0
#     , .dbl_ub = 1
#     , .dbl_lambda = 0.25
#     , .lgc_sample = F
# ){
#   
#   # Check if var is numeric
#   if(all(is.na(.dbl_var))){
#     
#     return(NA)
#     
#   } else if(!is.numeric(.dbl_var)){
#     
#     stop("'.dbl_var' must be a numeric vector.")
#     
#   }
#   
#   if(
#     length(.dbl_lb) != 1
#     | !is.numeric(.dbl_lb)
#   ){
#     
#     stop("'.dbl_lb' must be a numeric variable of length 1.")
#     
#   }
#   
#   if(
#     length(.dbl_ub) != 1
#     | !is.numeric(.dbl_ub)
#   ){
#     
#     stop("'.dbl_ub' must be a numeric variable of length 1.")
#     
#   }
#   
#   if(
#     length(.dbl_ub) != 1
#     | !is.numeric(.dbl_ub)
#     | !(.dbl_lambda >= 0 & .dbl_lambda <= 1)
#   ){
#     
#     stop("'.dbl_lambda' must be a numeric variable between 0 and 1.")
#     
#   }
#   
#   # Logical
#   if(!(
#     is.logical(.lgc_sample) &
#     !is.na(.lgc_sample)
#   )){
#     
#     stop("'.lgc_sample' must be either TRUE or FALSE.")
#     
#   }
#   
#   
#   # Prevent division by zero by increasing the scale by 1
#   if(.dbl_ub == 0){
#     
#     .dbl_var + 1 -> .dbl_var
#     .dbl_lb + 1 -> .dbl_lb
#     .dbl_ub + 1 -> .dbl_ub
#     
#   }
#   
#   
#   # Normalize to percentage scale
#   .dbl_var / .dbl_ub -> .dbl_var
#   .dbl_lb / .dbl_ub -> .dbl_lb
#   .dbl_ub / .dbl_ub -> .dbl_ub
#   
#   
#   # Sample standard deviation
#   sd(.dbl_var, na.rm = T) -> dbl_std
#   
#   
#   if(!.lgc_sample){
#     # Population standard deviation
#     sqrt(
#       (length(.dbl_var) - 1) / length(.dbl_var)
#     ) * dbl_std -> dbl_std
#     
#   }
#   
#   
#   # Calculate Mode
#   unique(mlv(x = .dbl_var, method = 'shorth')) -> dbl_mode
#   
#   
#   # Calculate capital flexibility score
#   if(.lgc_sample){
#     
#     .dbl_lambda +
#       (1 - .dbl_lambda) * dbl_mode +
#       -.dbl_lambda * (1.414214 * dbl_std) +
#       -.dbl_lambda * (1 - dbl_mode) *
#       (1 - 2 * (1.414214 * dbl_std)) -> dbl_kflex
#     
#   } else {
#     
#     .dbl_lambda +
#       (1 - .dbl_lambda) * dbl_mode +
#       -.dbl_lambda * (2 * dbl_std) +
#       -.dbl_lambda * (1 - dbl_mode) *
#       (1 - 2 * (2 * dbl_std)) -> dbl_kflex
#     
#   }
#   
#   
#   # Output
#   return(dbl_kflex)
#   
# }
# 
