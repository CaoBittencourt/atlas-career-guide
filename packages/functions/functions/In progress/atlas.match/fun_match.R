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

# - BVLS regression matching ----------------------------------------------
fun_match_bvls <- function(
    df_data_cols
    , df_query_cols
    , mtx_weights = NULL
){
  
  # Arguments validation
  stopifnot(
    "'df_data_cols' must be a data frame." = 
      is.data.frame(df_data_cols)
  )
  
  stopifnot(
    "'df_query_cols' must be a data frame." = 
      all(
        is.data.frame(df_query_cols)
        , nrow(df_query_cols) ==
          nrow(df_data_cols)
      )
  )
  
  stopifnot(
    "'mtx_weights' must be either NULL or a numeric matrix." = 
      any(
        all(
          is.numeric(mtx_weights)
          , is.matrix(mtx_weights)
        )
        , is.null(mtx_weights)
      )
  )
  
  # BVLS regression matching without weights
  if(!length(mtx_weights)){
    
    map_dbl(
      .x = as_tibble(df_data_cols)
      , ~ 
        coef(bvls(
          as.matrix(.x)
          , df_query_cols[,]
          , bl = 0
          , bu = 1
        ))
    ) -> dbl_similarity
    
  } else {
    
    # Add weights to regression 
    sqrt(mtx_weights) ->
      mtx_weights
    
    df_data_cols *
      mtx_weights ->
      df_data_cols
    
    # BVLS regression matching with weights
    map2_dbl(
      .x = as_tibble(df_data_cols)
      , .y = as_tibble(mtx_weights)
      , ~
        coef(bvls(
          as.matrix(.x)
          , df_query_cols[,] * .y
          , bl = 0
          , bu = 1
        ))
    ) -> dbl_similarity
    
  }
  
  # Output
  return(dbl_similarity)
  
}

# - Logistic regression matching ------------------------------------------
# - Pearson correlation matching ------------------------------------------

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

# - BVLS regression matching -------------------------------------------------------
tic()
fun_match_bvls(
  df_data_cols = 
    df_occupations %>% 
    select(ends_with('.l')) %>% 
    t() %>% 
    as_tibble()
  , df_query_cols = 
    df_occupations %>% 
    select(ends_with('.l')) %>% 
    t() %>% 
    as_tibble() %>% 
    select(1) * 
    runif(1, 0, 1)
  , mtx_weights = NULL
)
toc()

# - Similarity test ------------------------------------------------------------------

# - Atlas Career Type Indicator test --------------------------------------

# - Interchangeability test -----------------------------------------------

# - Employability test ----------------------------------------------------
