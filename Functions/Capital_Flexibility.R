# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'KbMvtSkew' #Bowley skewness
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# CAPITAL FLEXIBILITY FUNCTION --------------------------------------------
fun_capital.flex <- function(.dbl_var){
  
  # Check if var is numeric
  if(all(is.na(.dbl_var))){
    
    return(NA)
    
  } else if(!is.numeric(.dbl_var)){
    
    stop("'.dbl_var' must be a numeric vector.")
    
  }
  
  # Calculate stardard deviation
  sd(.dbl_var, na.rm = T) -> std
  
  
  # Calculate skewness E [-1,1]
  (
    mean(.dbl_var, na.rm = T)
    - median(.dbl_var, na.rm = T)
  ) / sd(.dbl_var, na.rm = T) -> sk
  
  # # If standard deviation approximates 0, skewness explodes
  # if(std == 0){
  #   
  #   mean(.dbl_var, na.rm = T) - median(.dbl_var, na.rm = T) -> sk
  #   
  # } else if(sk > 1){
  #   
  #   sk <- 1
  #   
  # } else if(sk < -1){
  #   
  #   sk <- -1
  #   
  # }
  
  # Metric does not yield expected results when everything is 0
  # kflex(0,0,...,0) should be 0
  # Metric also does not yield excepected when every value is 1
  # kflex(1,1,...,1) should be 1
  
  # Calculate capital flexibility score
  # Adjust skewness by stardard deviation
  # Convert skewness to a 0 to 1 metric
  (1 - sk * (1 - std)) / 2 -> kflex
  
  # Output
  return(kflex)
  
}
