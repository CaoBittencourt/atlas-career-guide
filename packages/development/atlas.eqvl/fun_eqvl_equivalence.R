# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
# No packages required
# [FUNCTIONS] ------------------------------------------
# - Equivalence function ------------------------------------
fun_eqvl_equivalence <- function(
    dbl_var
    , dbl_scale_ub = NULL
    , dbl_scaling = 1
){
  
  # Argument validation
  stopifnot(
    "'dbl_var' must be a numeric vector or matrix." =
      is.numeric(dbl_var)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be either NULL or numeric." =
      any(
        is.numeric(dbl_scale_ub)
        , is.null(dbl_scale_ub)
      )
  )
  
  stopifnot(
    "'dbl_scaling' must be numeric." =
      is.numeric(dbl_scaling)
  )
  
  # Normalize data to percentage scale
  if(!all(
    max(dbl_var, na.rm = T) <= 1,
    min(dbl_var, na.rm = T) >= 0
  )){
    
    # Normalize by upper bound, if any
    if(length(dbl_scale_ub)){
      
      dbl_var / 
        dbl_scale_ub -> 
        dbl_var
      
    } else { 
      
      # Normalize by maxima
      dbl_var / 
        max(
          dbl_var
          , na.rm = T
        ) -> dbl_var
      
    }
    
  }
  
  # Calculate equivalence
  dbl_var ^ 
    (
      (1 / dbl_var) ^ 
        (dbl_scaling / dbl_var)
    ) -> dbl_equivalence
  
  # Output
  return(dbl_equivalence)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Test ------------------------------------------------------------------
# fun_eqvl_equivalence(runif(1, 0, 1))