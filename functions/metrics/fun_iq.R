# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
# [FUNCTION] --------------------------------------------------------------
# # - IQ Estimator ----------------------------------------------------------
# fun_iq <- function(
#     
#   dbl_scores
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
#   , int_interval = 7
#   , dbl_proxy_mean
#   , dbl_proxy_sd
#   , dbl_iq_mean = 100
#   , dbl_iq_sd = 15
#   
# ){
#   
#   # Arguments Validation
#   stopifnot(
#     "'dbl_scores' must be numeric." = 
#       is.numeric(dbl_scores)
#   )
#   
#   stopifnot(
#     "'dbl_scale_ub' must be numeric." = 
#       is.numeric(dbl_scale_ub)
#   )
#   
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." = 
#       is.numeric(dbl_scale_lb)
#   )
#   
#   stopifnot(
#     "'int_interval' must be an integer." = 
#       is.numeric(int_interval)
#   )
#   
#   stopifnot(
#     "'dbl_proxy_mean' must be numeric." = 
#       is.numeric(dbl_proxy_mean)
#   )
#   
#   stopifnot(
#     "'dbl_proxy_sd' must be numeric." = 
#       is.numeric(dbl_proxy_sd)
#   )
#   
#   stopifnot(
#     "'dbl_iq_mean' must be numeric." = 
#       is.numeric(dbl_proxy_mean)
#   )
#   
#   stopifnot(
#     "'dbl_iq_sd' must be numeric." = 
#       is.numeric(dbl_iq_sd)
#   )
#   
#   # Data Wrangling
#   dbl_scale_ub[[1]] -> dbl_scale_ub
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   dbl_proxy_mean[[1]] -> dbl_proxy_mean
#   dbl_proxy_sd[[1]] -> dbl_proxy_sd
#   dbl_iq_mean[[1]] -> dbl_iq_mean
#   dbl_iq_sd[[1]] -> dbl_iq_sd
#   round(int_interval) -> int_interval
#   
#   seq(
#     dbl_scale_lb
#     , dbl_scale_ub
#     , length.out = 
#       int_interval
#   ) -> dbl_scale
#   
#   findInterval(
#     dbl_proxy_mean
#     , dbl_scale
#   ) -> int_proxy_mean
#   
#   mean(c(
#     dbl_scale[int_proxy_mean],
#     dbl_scale[int_proxy_mean + 1]
#   )) -> dbl_mid_point
#   
#   diff(c(
#     dbl_scale[int_proxy_mean],
#     dbl_scale[int_proxy_mean + 1]
#   )) -> dbl_scale_sd
#   
#   # Estimate IQ
#   dbl_iq_mean * 
#     dbl_mid_point / 
#     dbl_proxy_mean + 
#     (
#       mean(dbl_scores) / 
#         dbl_mid_point
#       - 1 
#     ) *
#     dbl_iq_sd * 
#     dbl_scale_sd / 
#     dbl_proxy_sd -> 
#     dbl_iq
#   
#   # Output
#   return(dbl_iq)
#   
# }

# - IQ Estimator 2 ----------------------------------------------------------
fun_iq2 <- function(
    
  dbl_scores
  , dbl_proxy_mean
  , dbl_proxy_sd
  , dbl_iq_mean = 100
  , dbl_iq_sd = 15
  
){
  
  # Arguments Validation
  stopifnot(
    "'dbl_scores' must be numeric." = 
      is.numeric(dbl_scores)
  )
  
  stopifnot(
    "'dbl_proxy_mean' must be numeric." = 
      is.numeric(dbl_proxy_mean)
  )
  
  stopifnot(
    "'dbl_proxy_sd' must be numeric." = 
      is.numeric(dbl_proxy_sd)
  )
  
  stopifnot(
    "'dbl_iq_mean' must be numeric." = 
      is.numeric(dbl_proxy_mean)
  )
  
  stopifnot(
    "'dbl_iq_sd' must be numeric." = 
      is.numeric(dbl_iq_sd)
  )
  
  # Data Wrangling
  dbl_proxy_mean[[1]] -> dbl_proxy_mean
  dbl_proxy_sd[[1]] -> dbl_proxy_sd
  dbl_iq_mean[[1]] -> dbl_iq_mean
  dbl_iq_sd[[1]] -> dbl_iq_sd
  
  # Estimate IQ
  dbl_iq_mean +
    dbl_iq_sd * (
      mean(dbl_scores) -
        dbl_proxy_mean
    ) / dbl_proxy_sd -> 
    dbl_iq
  
  pmax(dbl_iq, 0) ->
    dbl_iq
  
  # Output
  return(dbl_iq)
  
}