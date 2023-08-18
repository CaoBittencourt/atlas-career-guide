# # # [SETUP] ----------------------------------------------------------------
# # - Packages ---------------------------------------------------------------
# pkg <- c(
#   'dplyr', 'tidyr' #Data wrangling
#   , 'atlas.skew' #Sd-adjusted mode metric
#   # , 'vctrs'
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
# - Capital flexibility --------------------------------------------
fun_kcoef_kflex_macro <- function(
    dbl_var
    , dbl_weights = NULL
    , dbl_scale_lb
    , dbl_scale_ub
    , lgc_sample_variance = F
){

  # Arguments validation within 'fun_skew_sdmode'

  # Call 'fun_skew_sdmode'
  call_kflex_macro <- match.call()

  call_kflex_macro[[1]] <- as.name('fun_skew_sdmode')

  # Run 'fun_skew_sdmode' with try catch
  tryCatch(
    expr = {return(eval.parent(call_kflex_macro))}
    , error = function(e){return(NA)}
  ) -> dbl_kflex

  # Output
  return(dbl_kflex)

}

# - Capital flexibility data frame --------------------------------------------
fun_kcoef_kflex_macro_df <- function(
    df_data
    , dbl_weights = NULL
    , dbl_scale_lb
    , dbl_scale_ub
    , lgc_sample_variance = F
){

  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric data." =
      all(
        any(
          is.data.frame(df_data)
          , is.matrix(df_data)
        )
        , any(sapply(
          df_occupations
          , is.numeric
        ))
      )
  )

  # Data wrangling
  df_data %>%
    as_tibble() %>%
    select(where(
      is.numeric
    )) %>%
    as.matrix() ->
    df_data

  # Apply 'fun_kcoef_kflex_macro' function
  fun_kcoef_kflex_macro(
    dbl_var =
      df_data
    , dbl_weights =
      dbl_weights
    , dbl_scale_lb =
      dbl_scale_lb
    , dbl_scale_ub =
      dbl_scale_ub
    , lgc_sample_variance =
      lgc_sample_variance
  ) -> dbl_kflex

  rm(df_data)
  rm(dbl_weights)
  rm(dbl_scale_lb)
  rm(dbl_scale_ub)
  rm(lgc_sample_variance)

  # Data wrangling
  dbl_kflex %>%
    as_tibble(
      rownames = 'item'
    ) %>%
    rename(
      kflex_macro = 2
    ) -> df_kflex_items

  rm(dbl_kflex)

  # # Add kflex class to data frame
  # c(
  #   'df_kflex'
  #   , class(
  #     df_kflex_items
  #   )) -> class(df_kflex_items)

  # Output
  return(df_kflex_items)

}

# # - Aggregate Capital Flexibility --------------------------------------------
# fun_kflex.aggregate <- function(
#     df_data
#     , .df_kflex_items
# ){
#
#   # Arguments validation
#   stopifnot(
#     "'df_data' must be a data frame containing item scores." =
#       all(
#         is.data.frame(df_data)
#         , any(
#           .df_kflex_items$
#             item %in%
#             names(df_data)
#         )))
#
#   stopifnot(
#     "'.df_kflex_items' must be the output data frame from the 'fun_kflex.df' function." =
#       'df_kflex' %in%
#       class(.df_kflex_items)
#   )
#
#   # Aggregate capital flexibility
#   df_data %>%
#     pivot_longer(
#       cols = where(is.numeric)
#       , names_to = 'item'
#       , values_to = 'item_score'
#     ) %>%
#     left_join(
#       .df_kflex_items
#     ) %>%
#     drop_na() %>%
#     group_by(across(c(
#       !where(is.numeric)
#       , -item
#     ))) %>%
#     reframe(
#       aggregate.kflex =
#         sum(
#           item_kflex *
#             item_score
#         ) / sum(item_score)
#     ) %>%
#     full_join(
#       df_data
#     ) -> df_data.kflex
#
#   # Output
#   return(df_data.kflex)
#
# }

# # [TEST] ------------------------------------------------------------------
# # - Data ------------------------------------------------------------------
# library(readr)
#
# read_csv(
#   'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
# ) -> df_occupations
#
# # - Capital flexibility ------------------------------------------------------
# fun_kcoef_kflex_macro(
#   dbl_var = df_occupations$active_listening.l
#   , dbl_weights = df_occupations$employment2
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , dbl_discount = 0.25
# )
#
# # - Capital flexibility data frame ------------------------------------------------------
# fun_kcoef_kflex_macro_df(
#   df_data =
#     df_occupations %>%
#     select(
#       occupation
#       , ends_with('.l')
#     )
#   , dbl_weights =
#     df_occupations$
#     employment2
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , dbl_discount = 0.25
#   , lgc_sample_variance = F
# ) -> df_kflex
#
# df_kflex
