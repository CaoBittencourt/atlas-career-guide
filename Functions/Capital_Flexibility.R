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

    stop(".dbl_var must be a numeric vector.")

  }

  # Calculate variance
  var(.dbl_var, na.rm = T) -> vr

  # Calculate skewness E [-1,1]
  (
    mean(.dbl_var, na.rm = T)
    - median(.dbl_var, na.rm = T)
  ) / sd(.dbl_var, na.rm = T) -> sk

  # Calculate capital flexibility score
  # Adjust skewness by variance
  # Convert skewness to a 0 to 1 metric
  # (1 - sk * (1 - vr)) / 2 -> kflex
  (1 - sk) * (1 - vr) / 2 -> kflex

  # Output
  return(kflex)

}

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
#     stop(".dbl_var must be a numeric vector.")
#     
#   }
#   
#   # Calculate variance
#   var(.dbl_var, na.rm = T) -> vr
#   
#   # Calculate skewness E [-1,1]
#   # quantile(.dbl_var, probs = c(.25, .50, .75)) -> Q
#   # 
#   # (Q[3] + Q[1] - 2 * Q[2]) / (Q[3] - Q[1]) -> sk
#   # 
#   # Calculate capital flexibility score
#   # Adjust skewness by variance
#   # Convert skewness to a 0 to 1 metric
#   # (1 - sk * (1 - vr)) / 2 -> kflex
#   (1 - sk) * (1 - vr) / 2 -> kflex
#   
#   # Output
#   return(kflex)
#   
# }
