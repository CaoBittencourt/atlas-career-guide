# # [SETUP] ----------------------------------------------------------------
# # - Packages ---------------------------------------------------------------
# pkg <- c(
#   'modeest' #Mode
#   , 'Hmisc' #Weighted variance
# )
#
# # Activate / install packages
# lapply(pkg, function(x)
#   if(!require(x, character.only = T))
#   {install.packages(x); require(x)})
#
# # Package citation
# # lapply(pkg, function(x)
# #   {citation(package = x)})

# [FUNCTIONS] -------------------------------------------------------------
# # - Sd-adjusted mode function --------------------------------------------
# fun_skew_sdmode <- function(
    #     dbl_var
#     , dbl_weights = NULL
#     , dbl_scale_lb
#     , dbl_scale_ub
#     , lgc_sample_variance = F
# ){
#
#   # Arguments validation
#   stopifnot(
#     "'dbl_var' must be numeric." =
#       is.numeric(dbl_var)
#   )
#
#   stopifnot(
#     "'dbl_weights' must be either NULL or a numeric vector the same length as 'dbl_var'." =
#       any(
#         all(
#           is.numeric(dbl_weights)
#           , length(dbl_weights) ==
#             nrow(cbind(dbl_var))
#         )
#         , is.null(dbl_weights)
#       )
#   )
#
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." =
#       is.numeric(dbl_scale_lb)
#   )
#
#   stopifnot(
#     "'dbl_scale_ub' must be numeric." =
#       is.numeric(dbl_scale_ub)
#   )
#
#   stopifnot(
#     "'lgc_sample_variance' must be either TRUE or FALSE." =
#       all(
#         is.logical(lgc_sample_variance)
#         , !is.na(lgc_sample_variance)
#       )
#   )
#
#   # Coerce bounds to one element
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   dbl_scale_ub[[1]] -> dbl_scale_ub
#
#   # Prevent division by zero by increasing the scale by 1
#   if(dbl_scale_ub == 0){
#
#     dbl_scale_lb + 1 -> dbl_scale_lb
#     dbl_scale_ub + 1 -> dbl_scale_ub
#     dbl_var + 1 -> dbl_var
#
#   }
#
#   # Coerce the data into the bounds
#   pmax(dbl_var, dbl_scale_lb) -> dbl_var
#   pmin(dbl_var, dbl_scale_ub) -> dbl_var
#
#   # Calculate upper limit for standard deviation
#   sd(c(
#     dbl_scale_lb
#     , dbl_scale_ub
#   )) -> dbl_sd_ub
#
#   if(!lgc_sample_variance){
#
#     # Population standard deviation
#     dbl_sd_ub / sqrt(2) -> dbl_sd_ub
#
#   }
#
#   # Minimal weights
#   if(length(dbl_weights)){
#
#     dbl_weights /
#       min(
#         dbl_weights
#         , na.rm = T
#       ) -> dbl_weights
#
#     round(
#       dbl_weights
#     ) -> dbl_weights
#
#   }
#
#   # Check if 'dbl_var' is named
#   lgc_named <- F
#
#   if(any(
#     length(colnames(dbl_var)),
#     length(names(dbl_var))
#   )){
#
#     lgc_named <- T
#
#   }
#
#   # Data wrangling
#   cbind(
#     dbl_var
#   ) -> dbl_var
#
#   # Calculate standard deviation
#   # Sample standard deviation
#   vapply(
#     as.data.frame(dbl_var)
#     , function(x){sqrt(wtd.var(x, dbl_weights))}
#     , FUN.VALUE = numeric(1)
#   ) -> dbl_sd
#
#   dbl_sd[is.na(
#     dbl_sd
#   )] <- 0
#
#   # Population standard deviation
#   if(!lgc_sample_variance){
#
#     dbl_sd *
#       sqrt((
#         nrow(dbl_var) - 1
#       ) / nrow(dbl_var)
#       ) -> dbl_sd
#
#   }
#
#   # Normalize sd
#   dbl_sd / dbl_sd_ub -> dbl_sd
#
#   rm(dbl_sd_ub)
#
#   # If weights are provided, repeat each element 'dbl_weights' times
#   if(length(dbl_weights)){
#
#     dbl_var[rep(
#       1:nrow(dbl_var)
#       , times = dbl_weights
#     ), ] -> dbl_var
#
#   }
#
#   # Calculate mode
#   vapply(
#     as.data.frame(dbl_var)
#     , function(x){mlv(x, method = 'shorth')}
#     , FUN.VALUE = numeric(1)
#   ) -> dbl_mode
#
#   rm(dbl_var)
#
#   # Normalize mode
#   dbl_mode / dbl_scale_ub -> dbl_mode
#
#   rm(dbl_scale_ub)
#
#   # Calculate sd-adjusted mode
#   dbl_sd +
#     (1 - dbl_sd) * dbl_mode +
#     -dbl_sd * dbl_sd +
#     -dbl_sd * (1 - dbl_mode) *
#     (1 - 2 * dbl_sd) ->
#     dbl_skew
#
#   rm(dbl_mode)
#   rm(dbl_sd)
#
#   # If named, keep names
#   if(!lgc_named){
#
#     as.numeric(
#       dbl_skew
#     ) -> dbl_skew
#
#   }
#
#   # Output
#   return(dbl_skew)
#
# }

# - Sd-adjusted mode function --------------------------------------------
fun_skew_sdmode <- function(
    dbl_var
    , dbl_weights = NULL
    , dbl_scale_lb
    , dbl_scale_ub
    , lgc_sample_variance = F
){

  # Arguments validation
  stopifnot(
    "'dbl_var' must be numeric." =
      is.numeric(dbl_var)
  )

  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as 'dbl_var'." =
      any(
        all(
          is.numeric(dbl_weights)
          , length(dbl_weights) ==
            nrow(cbind(dbl_var))
        )
        , is.null(dbl_weights)
      )
  )

  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )

  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )

  stopifnot(
    "'lgc_sample_variance' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_sample_variance)
        , !is.na(lgc_sample_variance)
      )
  )

  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  dbl_scale_ub[[1]] -> dbl_scale_ub

  # Check if 'dbl_var' is named
  lgc_named <- F

  if(any(
    length(colnames(dbl_var)),
    length(names(dbl_var))
  )){

    lgc_named <- T

  }

  cbind(
    dbl_var
  ) -> dbl_var

  dbl_var / (
    dbl_scale_ub -
      dbl_scale_lb
  ) -
    dbl_scale_lb / (
      dbl_scale_ub -
        dbl_scale_lb
    ) -> dbl_var

  rm(dbl_scale_lb)
  rm(dbl_scale_ub)

  # Calculate standard deviation
  # Sample standard deviation
  vapply(
    as.data.frame(dbl_var)
    , function(x){sqrt(wtd.var(
      x, dbl_weights, na.rm = T
    ))}
    , FUN.VALUE = numeric(1)
  ) -> dbl_sd

  # Normalize sd
  # Calculate upper limit for standard deviation
  sd(c(
    rep(
      0, (1 + floor((nrow(dbl_var) - 1) / 2))
    ),
    rep(
      1, nrow(dbl_var) - (1 + floor((nrow(dbl_var) - 1) / 2))
    )
  )) -> dbl_sd_ub

  if(!lgc_sample_variance){

    # Population standard deviation
    dbl_sd_ub *
      sqrt((
        nrow(dbl_var) - 1
      ) / nrow(dbl_var)
      ) -> dbl_sd_ub

    dbl_sd *
      sqrt((
        nrow(dbl_var) - 1
      ) / nrow(dbl_var)
      ) -> dbl_sd

  }

  dbl_sd / dbl_sd_ub -> dbl_sd

  rm(dbl_sd_ub)
  rm(lgc_sample_variance)

  # If weights are provided, repeat each element 'dbl_weights' times
  if(length(dbl_weights)){

    # Minimal weights
    dbl_weights
    min(
      dbl_weights
      , na.rm = T
    ) -> dbl_weights

    round(
      dbl_weights
    ) -> dbl_weights

    dbl_var[rep(
      1:nrow(dbl_var)
      , times = dbl_weights
    ), ] -> dbl_var

  }

  # Calculate mode
  vapply(
    as.data.frame(dbl_var)
    , function(x){mlv(x, method = 'shorth')}
    , FUN.VALUE = numeric(1)
  ) -> dbl_mode

  rm(dbl_var)
  rm(dbl_weights)

  # Calculate sd-adjusted mode
  0.5 * dbl_sd +
    (1 - 0.5 * dbl_sd) * dbl_mode +
    -0.5 * dbl_sd * dbl_sd +
    -0.5 * dbl_sd * (1 - dbl_mode) *
    (1 - 2 * dbl_sd) ->
    dbl_skew

  rm(dbl_mode)
  rm(dbl_sd)

  # If named, keep names
  if(!lgc_named){

    as.numeric(
      dbl_skew
    ) -> dbl_skew

  }

  rm(lgc_named)

  # Output
  return(dbl_skew)

}

# # - Skewness-centered data ----------------------------------------------
# fun_skew_center_data <- function(
#     dbl_var
#     , dbl_weights = NULL
#     , dbl_scale_lb
#     , dbl_scale_ub
#     , lgc_sample_variance = F
#     , lgc_truncate = T
# ){
#
#   # Arguments validation
#   stopifnot(
#     "'dbl_var' must be numeric." =
#       is.numeric(dbl_var)
#   )
#
#   stopifnot(
#     "'dbl_weights' must be either NULL or a numeric vector the same length as 'dbl_var'." =
#       any(
#         all(
#           is.numeric(dbl_weights)
#           , length(dbl_weights) ==
#             nrow(cbind(dbl_var))
#         )
#         , is.null(dbl_weights)
#       )
#   )
#
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." =
#       is.numeric(dbl_scale_lb)
#   )
#
#   stopifnot(
#     "'dbl_scale_ub' must be numeric." =
#       is.numeric(dbl_scale_ub)
#   )
#
#   stopifnot(
#     "'lgc_sample_variance' must be either TRUE or FALSE." =
#       all(
#         is.logical(lgc_sample_variance)
#         , !is.na(lgc_sample_variance)
#       )
#   )
#
#   # Data wrangling
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   dbl_scale_ub[[1]] -> dbl_scale_ub
#
#   # # Call 'fun_skew_sdmode'
#   # call_kflex_macro <- match.call()
#   #
#   # call_kflex_macro[[1]] <- as.name('fun_skew_sdmode')
#   #
#   # # Run 'fun_skew_sdmode' with try catch
#   # tryCatch(
#   #   expr = {return(eval.parent(call_kflex_macro))}
#   #   , error = function(e){return(NA)}
#   # ) -> dbl_skew
#
#   fun_skew_sdmode(
#     dbl_var = dbl_var
#     , dbl_weights =
#       dbl_weights
#     , dbl_scale_lb =
#       dbl_scale_lb
#     , dbl_scale_ub =
#       dbl_scale_ub
#     , lgc_sample_variance =
#       lgc_sample_variance
#   ) -> dbl_skew
#
#   rm(dbl_weights)
#
#   # rm(call_kflex_macro)
#   rm(lgc_sample_variance)
#
#   # Scale skewness
#   dbl_skew * dbl_scale_ub -> dbl_skew
#
#   rm(dbl_scale_ub)
#
#   # Center data
#   rbind(dbl_skew)[rep(
#     1, nrow(dbl_var)
#   ), ] -> dbl_skew
#
#   dbl_var - dbl_skew -> dbl_var
#
#   rm(dbl_skew)
#
#   if(lgc_truncate){
#
#     pmax(
#       dbl_var
#       , dbl_scale_lb
#     ) -> dbl_var
#
#   }
#
#   rm(dbl_scale_lb)
#
#   # Normalize data
#   vapply(
#     as.data.frame(dbl_var)
#     , min
#     , FUN.VALUE = numeric(1)
#     , na.rm = T
#   ) -> dbl_min
#
#   vapply(
#     as.data.frame(dbl_var)
#     , max
#     , FUN.VALUE = numeric(1)
#     , na.rm = T
#   ) -> dbl_max
#
#   rbind(dbl_min)[rep(
#     1, nrow(dbl_var)
#   ), ] -> dbl_min
#
#   rbind(dbl_max)[rep(
#     1, nrow(dbl_var)
#   ), ] -> dbl_max
#
#   dbl_var / c(
#     dbl_max -
#       dbl_min
#   ) -
#     dbl_min / c(
#       dbl_max -
#         dbl_min
#     ) -> dbl_var
#
#   rm(dbl_max)
#   rm(dbl_min)
#
#   # Output
#   return(dbl_var)
#
# }

# # [TEST] ------------------------------------------------------------------
# # - Sd-adjusted mode 1 ------------------------------------------------------
# fun_skew_sdmode(
#   dbl_var = pmax(rnorm(1000, 50, sd = 15), 0)
#   , dbl_weights = runif(1000, 25000, 250000)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , dbl_discount = 0.25
# )
#
# # - Sd-adjusted mode 2 ------------------------------------------------------
# fun_skew_sdmode(
#   dbl_var =
#     pmax(
#       cbind(
#         rnorm(1000, 50, sd = 15),
#         rnorm(1000, 50, sd = 15),
#         rnorm(1000, 50, sd = 15),
#         rnorm(1000, 50, sd = 15),
#         rnorm(1000, 50, sd = 15)
#       ), 0
#     )
#   , dbl_weights = runif(1000, 25000, 250000)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , dbl_discount = 0.25
# )
#
# # - Sd-adjusted mode 3 ------------------------------------------------------
# pmax(
#   cbind(
#     rnorm(1000, 50, sd = 15),
#     rnorm(1000, 50, sd = 15),
#     rnorm(1000, 50, sd = 15),
#     rnorm(1000, 50, sd = 15),
#     rnorm(1000, 50, sd = 15)
#   ), 0
# ) -> dsds
#
# colnames(dsds) <- letters[1:ncol(dsds)]
#
# fun_skew_sdmode(
#   dbl_var = dsds
#   , dbl_weights = runif(1000, 25000, 250000)
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , dbl_discount = 0.25
# )
