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
    df_data_cols
    , dbl_scale_ub = NULL
    , dbl_scaling = 0.25 
){
  
  # Arguments validation
  stopifnot(
    "'df_data_cols' must be a data frame." =
      is.data.frame(df_data_cols)
  )
  
  # Data wrangling
  df_data_cols %>% 
    select(where(
      is.numeric
    )) %>% 
    as_tibble() -> 
    df_data_cols
  
  # Map weights function
  map_df(
    .x = df_data_cols
    , ~ fun_match_weights(
      dbl_var = .x
      , dbl_scale_ub = 
        dbl_scale_ub
      , dbl_scaling = 
        dbl_scaling
    )
  ) -> df_data_cols
  
  # Output
  return(df_data_cols)
  
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
  
  # BVLS regression
  if(!length(mtx_weights)){
    
    # Run BVLS regression matching without weights
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

# - Pearson correlation matching ----------------------------------------------
fun_match_pearson <- function(
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
  
  # Pearson correlation
  if(!length(mtx_weights)){
    
    # Pearson correlation matching without weights
    map_dbl(
      .x = as_tibble(df_data_cols)
      , ~ (
        1 +
          wtd.cors(
            df_query_cols[,]
            , .x
          )[,]
      ) / 2
    ) -> dbl_similarity
    
  } else {
    
    # Pearson correlation matching with weights
    map2_dbl(
      .x = as_tibble(df_data_cols)
      , .y = as_tibble(mtx_weights)
      , ~ (
        1 +
          wtd.cors(
            df_query_cols[,]
            , .x
            , weight = .y
          )[,]
      ) / 2
    ) -> dbl_similarity
    
  }
  
  # Output
  return(dbl_similarity)
  
}

# - Logistic regression matching ------------------------------------------
fun_match_logit <- function(
    df_data_cols
    , df_query_cols
    , dbl_scale_ub
    , dbl_scale_lb
    , chr_method = c('logit', 'probit')
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
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'chr_method' must be either 'logit' or 'probit'." =
      any(
        chr_method == 'logit',
        chr_method == 'probit'
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
  
  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  as.integer(dbl_scale_ub) -> dbl_scale_ub
  
  as.integer(dbl_scale_lb) -> dbl_scale_lb
  
  chr_method[[1]] -> chr_method
  
  # Convert query to a Bernoulli variable
  list_c(map(
    .x = as.integer(df_query_cols[,])
    , ~ rep(
      c(1,0), times = c(
        .x, (dbl_scale_ub - dbl_scale_lb) - .x
      )
    )
  )) -> int_query_bernoulli
  
  rm(df_query_cols)
  
  # Convert data to a Bernoulli variable
  map(
    .x = as_tibble(df_data_cols)
    , ~
      as.matrix(list_c(map(
        .x = as.integer(.x)
        , ~ rep(
          c(1,0), times = c(
            .x, (dbl_scale_ub - dbl_scale_lb) - .x
          )
        )
      )))
  ) -> list_data_bernoulli
  
  rm(df_data_cols)
  
  # Logistic regression
  if(!length(mtx_weights)){
    
    # Run logistic regression matching without weights
    map_dbl(
      .x = list_data_bernoulli
      , ~
        coef(fastglmPure(
          x = .x
          , y = int_query_bernoulli
          , family = binomial(
            link = 'logit'
          )
        ))
    ) -> dbl_similarity
    
    exp(dbl_similarity) /
      (1 + exp(dbl_similarity)) ->
      dbl_similarity
    
  } else {
    
    # Repeat mtx_weight's rows
    as_tibble(mtx_weights)[rep(
      1:nrow(mtx_weights)
      , each = 
        dbl_scale_ub - 
        dbl_scale_lb
    ), ] -> df_weights
    
    rm(mtx_weights)
    
    # Run logistic regression matching with weights
    map2_dbl(
      .x = list_data_bernoulli
      , .y = df_weights
      , ~
        coef(fastglmPure(
          x = .x
          , y = int_query_bernoulli
          , family = binomial(
            link = 'logit'
          ), weights = .y
        ))
    ) -> dbl_similarity
    
    exp(dbl_similarity) /
      (1 + exp(dbl_similarity)) ->
      dbl_similarity
    
  }
  
  # Output
  return(dbl_similarity)
  
}

# - Similarity function (col vectors) ---------------------------------------------------
fun_match_similarity_cols <- function(
    df_data_cols
    , df_query_cols
    , chr_method = c('bvls', 'logit', 'probit', 'pearson')
    , dbl_scale_ub = 100
    , dbl_scale_lb = 0
    , mtx_weights = NULL
    , dbl_scaling = 0.25
    , lgc_sort = F
){
  
  # Argument validation
  stopifnot(
    "'df_data_cols' must be a data frame." =
      is.data.frame(df_data_cols)
  )
  
  stopifnot(
    "'df_query_cols' must be a data frame." =
      is.data.frame(df_query_cols)
  )
  
  stopifnot(
    "'chr_method' must be one of the following methods: 'bvls', 'logit', 'probit', or 'pearson'." =
      any(
        chr_method == 'bvls',
        chr_method == 'logit',
        chr_method == 'probit',
        chr_method == 'pearson'
      )
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'mtx_weights' must be either NULL or numeric." =
      any(
        is.numeric(mtx_weights)
        , is.null(mtx_weights)
      )
  )
  
  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  chr_method[[1]] -> chr_method
  
  # Weights
  if(!length(mtx_weights)){
    
    match.call()
    formalArgs(fun_match_vweights)
    
    fun_match_vweights(
      df_data_cols = 
        df_data_cols
      , dbl_scale_ub = 
        dbl_scale_ub
      , dbl_scaling = 
        dbl_scaling
    ) -> mtx_weights
    
  }
  
  # Apply matching method
  if(chr_method == 'bvls'){
    
    formalArgs(fun_match_bvls)
    
    # Apply BVLS regression matching
    fun_match_bvls(
      df_data_cols =
        df_data_cols
      , df_query_cols = 
        df_query_cols
      , mtx_weights = 
        mtx_weights
    ) -> dbl_similarity
    
  } else if(chr_method == 'pearson') {
    
    formalArgs(fun_match_pearson)
    
    # Apply Pearson correlation matching
    fun_match_pearson(
      df_data_cols = 
        df_data_cols
      , df_query_cols = 
        df_query_cols
      , mtx_weights = 
        mtx_weights
    ) -> dbl_similarity
    
  } else {
    
    formalArgs(fun_match_logit)
    
    # Apply logistic regression matching
    fun_match_logit(
      df_data_cols = 
        df_data_cols
      , df_query_cols = 
        df_query_cols
      , dbl_scale_ub = 
        dbl_scale_ub
      , dbl_scale_lb = 
        dbl_scale_lb
      , chr_method = 
        chr_method
      , mtx_weights = 
        mtx_weights
    ) -> dbl_similarity
    
  }
  
  # Output
  return(dbl_similarity)
  
}

# - Similarity function (row vectors) ---------------------------------------------------
fun_match_similarity <- function(
    df_data_rows
    , df_query_rows
    , chr_method = c('bvls', 'logit', 'probit', 'pearson')
    , dbl_scale_ub = 100
    , dbl_scale_lb = 0
    , mtx_weights = NULL
    , dbl_scaling = 0.25
    , lgc_sort = F
){
  
  # Argument validation
  stopifnot(
    "'df_data_rows' must be a data frame." =
      is.data.frame(df_data_rows)
  )
  
  stopifnot(
    "'df_query_rows' must be a data frame." =
      is.data.frame(df_query_rows)
  )
  
  stopifnot(
    "'chr_method' must be one of the following methods: 'bvls', 'logit', 'probit', or 'pearson'." =
      any(
        chr_method == 'bvls',
        chr_method == 'logit',
        chr_method == 'probit',
        chr_method == 'pearson'
      )
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'mtx_weights' must be either NULL or numeric." =
      any(
        is.numeric(mtx_weights)
        , is.null(mtx_weights)
      )
  )
  
  stopifnot(
    "'lgc_sort' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_sort)
        , !is.na(lgc_sort)
      )
  )
  
  # Data wrangling
  Filter(
    function(x){all(is.numeric(x))}
    , df_query_rows
  ) -> df_query_cols
  
  rm(df_query_rows)
  
  df_data_rows[names(
    df_query_cols
  )] -> df_data_cols
  
  # Pivot data
  t(df_query_cols) -> df_query_cols
  
  t(df_data_cols) -> df_data_cols
  
  # Match call
  sym_call <- match.call()
  
  sym_call[[1]] <- as.name('fun_match_similarity_cols')
  
  gsub(
    '_rows'
    , '_cols'
    , names(sym_call)
  ) -> names(sym_call)
  
  # Apply similarity function
  if(ncol(df_query_cols) == 1){
    
    eval.parent(sym_call) ->
      df_data_rows$
      similarity
    
    return(df_data_rows)
    stop()
    
    # fun_match_similarity_cols(
    #   df_data_cols = df_data_cols
    #   , df_query_cols = df_query_cols
    #   , chr_method = chr_method
    #   , dbl_scale_ub = dbl_scale_ub
    #   , dbl_scale_lb = dbl_scale_lb
    #   , mtx_weights = mtx_weights
    # ) -> df_data_rows$similarity
    
    # Sort data frame
    if(lgc_sort){
      
      df_data_rows %>%
        arrange(desc(
          similarity
        )) -> df_data_rows
      
    }
    
    list_similarity <- NULL
    mtx_similarity <- NULL
    
  } else {
    
    df_query_cols %>%
      as_tibble() %>% 
      map(
        ~ fun_s(
          df_data_cols = df_data_cols
          , df_query_cols = as.matrix(.x)
          , chr_method = chr_method
          , dbl_scale_ub = dbl_scale_ub
          , dbl_scale_lb = dbl_scale_lb
          , mtx_weights = mtx_weights
        )
      ) -> list_similarity
    
    # Similarity matrix
    if(
      all(
        df_data_cols ==
        df_query_cols
      )
    ){
      
      list_similarity %>%
        bind_cols() %>%
        as.matrix() ->
        mtx_similarity
      
      if(length(id_col)){
        
        id_col[[1]] -> id_col
        
        df_data_rows %>%
          pull(!!sym(id_col)) ->
          colnames(
            mtx_similarity
          )
        
        colnames(
          mtx_similarity
        ) ->
          rownames(
            mtx_similarity
          )
        
        colnames(
          mtx_similarity
        ) ->
          names(
            list_similarity
          )
        
      }
      
    }
    
    df_data_rows <- NULL
    
  }
  
  # Output
  return(compact(list(
    'df_similarity' = df_data_rows
    , 'list_similarity' = list_similarity
    , 'mtx_similarity' = mtx_similarity
  )))
  
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

read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
) -> df_input

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

# - Logistic regression matching -------------------------------------------------------
tic()
fun_match_logit(
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
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
)
toc()

# - Pearson correlation matching -------------------------------------------------------
tic()
fun_match_pearson(
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
tic()
fun_match_similarity(
  df_data_rows = 
    df_occupations %>% 
    select(
      occupation
      , ends_with('.l')
    )
  , df_query_rows = 
    df_input
  # , chr_method = 
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
) -> dsds
toc()

dsds$query %>% View
dsds$data %>% View

# - Atlas Career Type Indicator test --------------------------------------

# - Interchangeability test -----------------------------------------------

# - Employability test ----------------------------------------------------
