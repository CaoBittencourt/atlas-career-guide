# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
# No packages required

# [FUNCTION] --------------------------------------------------------------
# - NOT IQ estimator ----------------------------------------------------------
fun_notiq_quotient <- function(
    dbl_proxy_scores
    , dbl_proxy_mean
    , dbl_proxy_sd
    , dbl_iq_mean = 100
    , dbl_iq_sd = 15
){

  # Arguments Validation
  stopifnot(
    "'dbl_proxy_scores' must be numeric." =
      is.numeric(dbl_proxy_scores)
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

  # Estimate NOT IQ
  dbl_iq_mean +
    dbl_iq_sd * (
      mean(dbl_proxy_scores) -
        dbl_proxy_mean
    ) / dbl_proxy_sd ->
    dbl_not_iq

  pmax(dbl_not_iq, 0) ->
    dbl_not_iq

  # Output
  return(dbl_not_iq)

}

# # [TEST] ------------------------------------------------------------------
# # - NOT IQ ----------------------------------------------------------------
# fun_notiq_quotient(
#   dbl_proxy_scores =
#     pmin(pmax(
#       rnorm(30, mean = 33, sd = 14.8)
#       , 0), 100)
#   , dbl_proxy_mean = 33
#   , dbl_proxy_sd = 14.8
# )
