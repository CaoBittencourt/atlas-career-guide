# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'bvls'
  , 'fastglm'
  , 'weights'
  # 'atlas.ftools' #Factor analysis tools
  , 'dplyr', 'tidyr', 'purrr' #Data wrangling
  # , 'vctrs' #Data wrangling
  # , 'modeest' #Mode
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [EQUIVALENCE FUNCTIONS] ------------------------------------------
# - Equivalence function ------------------------------------
fun_match_equivalence <- function(
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
  
  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  dbl_scaling[[1]] -> dbl_scaling
  
  # Normalize data to percentage scale
  if(max(dbl_var, na.rm = T) != 1){
    
    # Normalize by upper bound, if any
    if(length(dbl_scale_ub)){
      
      dbl_var / 
        dbl_scale_ub -> 
        dbl_var
      
    } else { 
      
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

# [MATCHING FUNCTIONS] -------------------------------------------------------------
# # - Regression weights --------------------------------------------
# fun_match_weights <- function(
    #     df_data_cols
#     , dbl_scaling = 0.25
# ){
#   
#   # Get maximum item scores for each column
#   vapply(
#     as_tibble(df_data_cols)
#     , function(x){max(x, na.rm = T)}
#     , FUN.VALUE = numeric(1)
#   ) -> mtx_weights
#   
#   # Prevent division by zero
#   mtx_weights[
#     mtx_weights == 0
#   ] <- 1
#   
#   # Relative importance compared to top item
#   t(
#     t(df_data_cols) / 
#       mtx_weights
#   ) -> mtx_weights
#   
#   # Calculate matching regression weights
#   fun_interchangeability(
#     .mtx_similarity = mtx_weights
#     , .dbl_scaling = dbl_scaling
#   ) -> mtx_weights
#   
#   # Output
#   return(mtx_weights)
#   
# }

# - Regression weights --------------------------------------------
fun_match_weights <- function(
    dbl_var
    , dbl_scale_ub = NULL
    , dbl_scaling = 0.25
){
  
  # Argument validation
  stopifnot(
    "'dbl_var' must be a numeric vector or matrix." =
      is.numeric(dbl_var)
  )
  
  # Get maximum item score
  max(dbl_var, na.rm = T) -> 
    dbl_weights
  
  # Prevent division by zero
  if(dbl_weights == 0){
    
    dbl_weights <- 1
    
  }
  
  # Relative importance compared to top item
  dbl_var / 
    dbl_weights ->
    dbl_weights
  
  # Apply equivalence function to regression weights
  fun_match_equivalence(
    dbl_var = 
      dbl_weights
    , dbl_scale_ub = 
      dbl_scale_ub
    , dbl_scaling = 
      dbl_scaling
  ) -> dbl_weights
  
  # Output
  return(dbl_weights)
  
}

# - Vectorized regression weights -----------------------------------------
fun_match_vweights <- function(
    df_data
    , dbl_scale_ub = NULL
    , dbl_scaling = 0.25 
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame." =
      is.data.frame(df_data)
  )
  
  # Data wrangling
  df_data %>% 
    select(where(
      is.numeric
    )) %>% 
    as_tibble() -> 
    df_data
  
  # Map weights function
  map_df(
    .x = df_data
    , ~ fun_match_weights(
      dbl_var = .x
      , dbl_scale_ub = 
        dbl_scale_ub
      , dbl_scaling = 
        dbl_scaling
    )
  ) -> df_data
  
  # Output
  return(df_data)
  
}

# - Similarity helper function ---------------------------------------------------
fun_match_similarity_helper <- function(
    
){
  
  
  
}

# - Similarity function ---------------------------------------------------
fun_match_similarity <- function(
    
){
  
  
  
}

# [ATLAS PROFESSIONAL TYPE INDICATOR FUNCTIONS] ---------------------------

# - ACTI estimator helper function --------------------------------------------------------

# - ACTI estimator --------------------------------------------------------

# [INTERCHANGEABILITY FUNCTIONS] ------------------------------------------
# - Interchangeability function -------------------------------------------

# [EMPLOYABILITY FUNCTIONS] -----------------------------------------------
# - Employability helper function -----------------------------------------
# - Employability function -----------------------------------------

# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)
library(tictoc)

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

# - Equivalence test 1 ------------------------------------------------------
tic()
fun_match_equivalence(
  dbl_var = 1:10
  , dbl_scale_ub = 19
  , dbl_scaling = 1
)
toc()

# - Equivalence test 2 ------------------------------------------------------
tic()
fun_match_equivalence(
  dbl_var = 
    df_occupations %>% 
    select(ends_with('.l')) %>% 
    as.matrix() %>%
    `rownames<-`(
      df_occupations$
        occupation
    )
  , dbl_scale_ub = NULL
  , dbl_scaling = 1
)
toc()

# - Regression weights 1 ----------------------------------------------------
tic()
fun_match_weights(
  dbl_var = runif(50, 0, 100)
  , dbl_scale_ub = 100
)
toc()

# - Regression weights 2 --------------------------------------------------
tic()
fun_match_vweights(
  df_data = 
    df_occupations %>% 
    select(ends_with('.l')) %>% 
    t() %>% 
    as_tibble()
  , dbl_scale_ub = 100
)
toc()

# - Similarity test ------------------------------------------------------------------

# - Atlas Career Type Indicator test --------------------------------------

# - Interchangeability test -----------------------------------------------

# - Employability test ----------------------------------------------------
