# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
# No packages required

# [FUNCTIONS] ------------------------------------------
# - Equivalence function ------------------------------------
fun_eqvl_equivalence <- function(
    dbl_var
    , dbl_scale_lb = NULL
    , dbl_scale_ub = NULL
    , dbl_scaling = 1
){

  # Argument validation
  stopifnot(
    "'dbl_var' must be a numeric vector or matrix." =
      is.numeric(dbl_var)
  )

  stopifnot(
    "'dbl_scale_lb' must be either NULL or numeric." =
      any(
        is.numeric(dbl_scale_lb)
        , is.null(dbl_scale_lb)
      )
  )

  stopifnot(
    "'dbl_scale_ub' must be either NULL or numeric." =
      any(
        is.numeric(dbl_scale_ub)
        , is.null(dbl_scale_ub)
      )
  )

  stopifnot(
    "'dbl_scaling' must be a non-negative number." =
      all(
        is.numeric(dbl_scaling)
        , dbl_scaling >= 0
      )
  )

  # Normalize data to percentage scale
  if(all(
    length(dbl_scale_lb),
    length(dbl_scale_ub)
  )){

    # Normalize by scale bounds
    dbl_var / (
      dbl_scale_ub[[1]] -
        dbl_scale_lb[[1]]
    ) -
      dbl_scale_lb[[1]] / (
        dbl_scale_ub[[1]] -
          dbl_scale_lb[[1]]
      ) -> dbl_var

  } else if(!all(
    max(dbl_var, na.rm = T) <= 1,
    min(dbl_var, na.rm = T) >= 0
  )){

    # Normalize by maxima
    dbl_var /
      max(
        dbl_var
        , na.rm = T
      ) -> dbl_var

  }

  dbl_var[all(
    dbl_var <= 1,
    dbl_var >= 0
  )] -> dbl_var

  # Calculate equivalence
  dbl_var ^ (
    (1 / dbl_var) ^ (
      dbl_scaling[[1]] /
        dbl_var
    )
  ) -> dbl_equivalence

  # Truncate NA's
  dbl_equivalence[
    is.na(dbl_equivalence)
  ] <- 0

  # Output
  return(dbl_equivalence)

}

# # [TEST] ------------------------------------------------------------------
# # - Test ------------------------------------------------------------------
# fun_eqvl_equivalence(runif(1, 0, 1))
