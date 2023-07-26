# [FUNCTION] --------------------------------------------------------------
# - Logistic function -----------------------------------------------------
fun_logistic <- function(
    .mtx_x
    , .dbl_mid.point = 50
    , .dbl_scaling.factor = 0.25
    , .dbl_displacement.factor = 1
    , .dbl_logistic.ub = 1
    , .dbl_scale.ub = 100
    , .dbl_scale.lb = 0
    , .lgc_normalize = T
){
  
  # Arguments validation
  stopifnot(
    "'.mtx_x' must be a numeric vector." =
      is.numeric(.mtx_x)
  )
  
  stopifnot(
    "'.dbl_mid.point' must be a numeric." =
      is.numeric(.dbl_mid.point)
  )
  
  stopifnot(
    "'.dbl_scaling.factor' must be a numeric." =
      is.numeric(.dbl_scaling.factor)
  )
  
  stopifnot(
    "'.dbl_displacement.factor' must be a numeric." =
      is.numeric(.dbl_displacement.factor)
  )
  
  stopifnot(
    "'.lgc_normalize' must be either TRUE or FALSE." =
      all(
        is.logical(.lgc_normalize)
        , !is.na(.lgc_normalize)
      )
  )
  
  stopifnot(
    "'.dbl_logistic.ub' must be a numeric." =
      is.numeric(.dbl_logistic.ub)
  )
  
  stopifnot(
    "'.dbl_scale.ub' must be a numeric." =
      is.numeric(.dbl_scale.ub)
  )
  
  stopifnot(
    "'.dbl_scale.lb' must be a numeric." =
      is.numeric(.dbl_scale.lb)
  )
  
  # Coerce into length == 1
  .dbl_mid.point[[1]] -> .dbl_mid.point
  .dbl_scaling.factor[[1]] -> .dbl_scaling.factor
  .dbl_displacement.factor[[1]] -> .dbl_displacement.factor
  .dbl_logistic.ub[[1]] -> .dbl_logistic.ub
  .dbl_scale.ub[[1]] -> .dbl_scale.ub
  .dbl_scale.lb[[1]] -> .dbl_scale.lb
  
  # Normalize
  .mtx_x /
    (
      .dbl_scale.ub - 
        .dbl_scale.lb
    ) -> .mtx_x
  
  .dbl_mid.point /
    (
      .dbl_scale.ub - 
        .dbl_scale.lb
    ) -> .dbl_mid.point
  
  # Logistic approximation
  .dbl_logistic.ub /
    (
      1 +
        .dbl_displacement.factor *
        exp(
          .dbl_scaling.factor *
            (.dbl_mid.point - .mtx_x)
        )
    ) -> dbl_logistic
  
  if(.lgc_normalize){
    
    dbl_logistic +
      .mtx_x *
      (.dbl_logistic.ub - 
         max(dbl_logistic)) -
      (1 - .mtx_x) *
      min(dbl_logistic) ->
      dbl_logistic
    
  }
  
  # Output
  return(dbl_logistic)
  
}
