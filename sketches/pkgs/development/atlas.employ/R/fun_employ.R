# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# No packages required

# [FUNCTIONS] -----------------------------------------------
# - Employability function -----------------------------------------
fun_employ_employability <- function(
    int_employment
    , dbl_interchangeability
){

  # Arguments validation
  stopifnot(
    "'int_employment' must be a numeric vector the same length as 'dbl_interchangeability'." =
      all(
        is.numeric(int_employment)
        , length(int_employment) ==
          nrow(cbind(dbl_interchangeability))
      )
  )

  stopifnot(
    "'dbl_interchangeability' must be a percentage." =
      all(
        dbl_interchangeability >= 0,
        dbl_interchangeability <= 1
      )
  )

  # Data wrangling
  round(int_employment) -> int_employment

  # Percentage of interchangeable job posts
  sum(
    int_employment *
      dbl_interchangeability
  ) / sum(int_employment) ->
    dbl_employability

  # Output
  return(dbl_employability)

}

# # [TEST] ------------------------------------------------------------------
# # - Employability test ----------------------------------------------------
# fun_employ_employability(
#   int_employment =
#     ceiling(runif(10, 1000, 250000))
#   , dbl_interchangeability =
#     runif(10, 0, 1)
# )
