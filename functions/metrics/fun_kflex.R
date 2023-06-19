# [SETUP] ----------------------------------------------------------------
# - Packages ---------------------------------------------------------------
pkg <- c(
  'modeest' #Mode
  , 'Hmisc' #Weighted variance
  # 'KbMvtSkew' #Bowley skewness
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTIONS] -------------------------------------------------------------
# - Capital Flexibility --------------------------------------------
fun_kflex <- function(
    .dbl_variable
    , .dbl_weights = NULL
    , .dbl_scale.lb = 0
    , .dbl_scale.ub = 100
    , .dbl_discount = 0.25
    , .lgc_sample.variance = F
){
  
  # Arguments validation
  stopifnot(
    "'.dbl_variable' must be numeric." = 
      is.numeric(.dbl_variable)
  )
  
  stopifnot(
    "'.dbl_weights' must be either NULL or a numeric vector the same length as '.dbl_variable'." = 
      any(
        all(
          is.numeric(.dbl_weights)
          , length(.dbl_weights) ==
            length(.dbl_variable)
        )
        , is.null(.dbl_weights)
      )
  )
  
  stopifnot(
    "'.dbl_scale.lb' must be numeric." = 
      is.numeric(.dbl_scale.lb)
  )
  
  stopifnot(
    "'.dbl_scale.ub' must be numeric." = 
      is.numeric(.dbl_scale.ub)
  )
  
  stopifnot(
    "'.dbl_discount' must be a numeric value between 0 and 1." = 
      all(
        is.numeric(.dbl_discount)
        , .dbl_discount >= 0
        , .dbl_discount <= 1
      )
  )
  
  stopifnot(
    "'.lgc_sample.variance' must be either TRUE or FALSE." = 
      all(
        is.logical(.lgc_sample.variance)
        , !is.na(.lgc_sample.variance)
      )
  )
  
  # Coerce bounds to one element
  .dbl_scale.lb[[1]] -> .dbl_scale.lb
  .dbl_scale.ub[[1]] -> .dbl_scale.ub
  
  # Prevent division by zero by increasing the scale by 1
  if(.dbl_scale.ub == 0){
    
    .dbl_scale.lb + 1 -> .dbl_scale.lb
    .dbl_scale.ub + 1 -> .dbl_scale.ub
    .dbl_variable + 1 -> .dbl_variable
    
  }
  
  # Coerce the data into the bounds
  .dbl_variable[
    .dbl_variable > 
      .dbl_scale.ub
  ] <- .dbl_scale.ub
  
  .dbl_variable[
    .dbl_variable < 
      .dbl_scale.lb
  ] <- .dbl_scale.lb
  
  # Calculate upper limit for standard deviation
  sd(c(
    .dbl_scale.lb
    , .dbl_scale.ub
  )) -> dbl_sd.ub
  
  if(!.lgc_sample.variance){
    
    # Population standard deviation
    dbl_sd.ub / sqrt(2) -> dbl_sd.ub
    
  }
  
  # Calculate standard deviation
  # Sample standard deviation
  sqrt(wtd.var(
    .dbl_variable
    , .dbl_weights
  )) -> dbl_sd
  
  if(is.na(dbl_sd)){
    
    0 -> dbl_sd
    
  }
  
  if(!.lgc_sample.variance){
    
    # Population standard deviation
    dbl_sd * 
      sqrt(
        (length(.dbl_variable) - 1) / 
          length(.dbl_variable)
      ) -> dbl_sd
    
  }
  
  # If weights are provided, repeat each element W times
  if(length(.dbl_weights)){
    
    .dbl_variable[rep(
      .dbl_variable
      , .dbl_weights
    )] -> .dbl_variable
    
  }
  
  # Calculate Mode
  unique(mlv(
    x = .dbl_variable
    , method = 'shorth'
  )) -> dbl_mode
  
  # Calculate capital flexibility score
  .dbl_discount +
    (1 - .dbl_discount) * (dbl_mode / .dbl_scale.ub) +
    -.dbl_discount * (dbl_sd / dbl_sd.ub) +
    -.dbl_discount * (1 - (dbl_mode / .dbl_scale.ub)) *
    (1 - 2 * (dbl_sd / dbl_sd.ub)) -> dbl_kflex
  
  # Output
  return(dbl_kflex)
  
}

# - Capital Flexibility data frame --------------------------------------------
fun_kflex.df <- function(
    .df_data
    , .dbl_weights = NULL
    , .dbl_scale.lb = 0
    , .dbl_scale.ub = 100
    , .dbl_discount = 0.25
    , .lgc_sample.variance = F
){
  
  # Arguments validation
  stopifnot(
    "'.df_data' must be a data frame with numeric data." = 
      all(
        any(
          is.data.frame(.df_data)
          , is.matrix(.df_data)
        )
        , any(sapply(
          df_occupations
          , is.numeric
        ))
      )
  )
  
  # Data wrangling
  .df_data %>% 
    select(where(
      is.numeric
    )) %>% 
    reframe(across(
      .cols = everything()
      ,.fns = ~ 
        fun_kflex(
          .dbl_variable = .x
          , .dbl_weights = 
            .dbl_weights
          , .dbl_scale.lb = 
            .dbl_scale.lb
          , .dbl_scale.ub = 
            .dbl_scale.ub
          , .dbl_discount = 
            .dbl_discount
        )
    )) %>% 
    pivot_longer(
      cols = everything()
      , names_to = 'item'
      , values_to = 'item.kflex'
    ) -> df_kflex.items
  
  # Add kflex class to data frame
  c(
    'df_kflex'
    , class(
      df_kflex.items
    )) -> class(df_kflex.items)
  
  # Output
  return(df_kflex.items)
  
}

# - Aggregate Capital Flexibility --------------------------------------------
fun_kflex.aggregate <- function(
    .df_data
    , .df_kflex.items
){
  
  # Arguments validation
  stopifnot(
    "'.df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(.df_data)
        , any(
          .df_kflex.items$
            item %in%
            names(.df_data)
        )))
  
  stopifnot(
    "'.df_kflex.items' must be the output data frame from the 'fun_kflex.df' function." =
      'df_kflex' %in% 
      class(.df_kflex.items)
  )
  
  # Aggregate capital flexibility
  .df_data %>% 
    pivot_longer(
      cols = where(is.numeric)
      , names_to = 'item'
      , values_to = 'item.score'
    ) %>% 
    left_join(
      .df_kflex.items
    ) %>% 
    drop_na() %>% 
    group_by(across(c(
      !where(is.numeric)
      , -item
    ))) %>% 
    reframe(
      aggregate.kflex = 
        sum(
          item.kflex * 
            item.score
        ) / sum(item.score)
    ) %>% 
    full_join(
      .df_data
    ) -> df_data.kflex
  
  # Output
  return(df_data.kflex)
  
}
