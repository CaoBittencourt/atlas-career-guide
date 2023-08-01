# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
pkg <- c(
  # 'tidyverse', 'glue' #Data wrangling
  'dplyr', 'tidyr', 'readr', 'stringr', 'purrr', 'glue' #Data wrangling
  , 'modeest' #Mode
  , 'psych', 'GPArotation' #EFA
  , 'weights' #Weighted correlations
  , 'Hmisc' #Weighted variance
  , 'matrixcalc'
  , 'plm'
  # , 'ctv' #Most relevant psychometrics packages
  # , 'paletteer' #Palettes for visualization
  , 'stats'
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})


# [BASIC FUNCTIONS] Perform EFA for a given number of factors -------------------------------------------------------
# - Item correlations -----------------------------------------------------
fun_efa_correlations <- function(df_data, dbl_weights = NULL){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." = 
      all(
        is.data.frame(df_data),
        df_data %>% 
          map_lgl(is.numeric) %>% 
          any()
      )
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." = 
      any(
        is.null(dbl_weights),
        all(
          is.numeric(dbl_weights),
          length(dbl_weights) ==
            nrow(df_data)
        )
      )
  )
  
  # Data wrangling
  df_data %>% 
    select(where(
      is.numeric
    )) -> df_data
  
  # Correlation matrix
  df_data %>%
    wtd.cors(
      weight =
        dbl_weights
    ) -> mtx_correlations
  
  # EFA correlation matrix class
  structure(
    mtx_correlations,
    class = c(
      class(mtx_correlations),
      'mtx_correlations'
    )
  ) -> mtx_correlations
  
  # Output
  return(mtx_correlations)
  
}

# - Factor adequacy test --------------------------------------------------
fun_efa_adequacy <- function(mtx_correlations, int_nrow = 100){
  
  # Arguments validation
  stopifnot(
    "'mtx_correlations' must be a correlation matrix obtained via the 'fun_efa_correlations' function." =
      any(class(mtx_correlations) == 'mtx_correlations')
  )
  
  stopifnot(
    "'int_nrow' must be an integer." = 
      is.numeric(int_nrow)
  )
  
  # Data wrangling
  int_nrow[[1]] -> int_nrow
  
  ceiling(int_nrow) -> int_nrow
  
  # Adequacy tests
  # K-M-O factor adequacy test
  if(
    tryCatch(
      invisible(capture.output(
        {
          
          KMO(mtx_correlations)
          
        }
      ))
      , message = function(i){T}
    )
    
  ){
    
    tryCatch(
      invisible(capture.output(
        {
          
          mtx_correlations %>%
            round(10) %>%
            KMO() -> list_kmo
          
        }
      ))
      , message = function(i){
        
        NA ->> list_kmo
        
      }
    )
    
  } else {
    
    KMO(mtx_correlations) -> list_kmo
    
  }
  
  # Default values
  df_adequacy_tests <- NULL
  
  df_items_problematic <- NULL
  
  if(is.list(list_kmo)){
    
    # Test statistic for the K-M-O adequacy test
    c(kmo = list_kmo$MSA) %>% 
      as_tibble(rownames = 'test') %>% 
      rename(statistic = 2) %>% 
      mutate(
        evaluation = 
          findInterval(
            statistic
            , seq(0.4, 0.9, 0.1)
            , rightmost.closed = F
          )
        , evaluation = 
          case_match(
            evaluation
            , c(0, 1) ~ 'unacceptable'
            , 2 ~ 'miserable'
            , 3 ~ 'mediocre'
            , 4 ~ 'middling'
            , 5 ~ 'meritorious'
            , 6 ~ 'marvelous'
          )
      ) -> df_adequacy_tests
    
    # Bartlett's correlation test
    c(
      bartlett = 
        cortest.bartlett(
          mtx_correlations
          , n = int_nrow
        )$p.value
    ) %>% 
      as_tibble(rownames = 'test') %>% 
      rename(statistic = 2) %>% 
      mutate(
        evaluation = 
          if_else(
            statistic <= 0.1
            , true = 'factorable'
            , false = 'uncertain'
          )
      ) %>% 
      bind_rows(
        df_adequacy_tests
      ) -> df_adequacy_tests
    
    # Problematic items (MSAi < .5, i.e. unacceptable)
    list_kmo$MSAi[
      round(
        list_kmo$MSAi, 2
      ) < 0.5
    ] %>% 
      as_tibble(
        rownames = 'item'
      ) %>% 
      rename(item_msai = 2) ->
      df_items_problematic
    
  }
  
  # Output
  return(list(
    'adequacy_tests' = df_adequacy_tests,
    'problematic_items' = df_items_problematic
  ))
  
}

# - Optimal number of factors ---------------------------------------------
fun_efa_nfactors <- function(mtx_correlations){
  
  # Arguments validation
  stopifnot(
    "'mtx_correlations' must be a correlation matrix obtained via the 'fun_efa_correlations' function." =
      any(class(mtx_correlations) == 'mtx_correlations')
  )
  
  # Kaiser criterion
  mtx_correlations %>% 
    eigen() %>% 
    pluck(1) %>% 
    map_lgl(~ .x >= 1) %>% 
    sum() -> int_kaiser
  
  # Parallel analysis
  mtx_correlations %>%
    round(7) %>%
    fa.parallel(
      fa = 'fa'
      , plot = F
    ) -> pa_analysis
  
  pa_analysis$nfact -> int_pa
  
  # Other metrics
  mtx_correlations %>%
    vss(
      n = 2 * int_pa
      , plot = F
    ) -> psy_vss
  
  # Very simple structure criterion (VSS)
  which.max(psy_vss$vss.stats$cfit.1) -> int_vss1
  
  which.max(psy_vss$vss.stats$cfit.2) -> int_vss2
  
  # Velicer Map
  which.min(psy_vss$map) -> int_map
  
  # BIC
  which.min(psy_vss$vss.stats$BIC) -> int_bic
  
  # Empirical BIC
  which.min(psy_vss$vss.stats$eBIC) -> int_ebic
  
  # Sample Size Adjusted BIC
  which.min(psy_vss$vss.stats$SABIC) -> int_sabic
  
  # Average of previous criteria
  round(mlv(c(
    int_kaiser
    , int_pa
    , int_vss1
    , int_vss2
    , int_map
    , int_bic
    , int_ebic
    , int_sabic
  ), method = 'shorth'
  )) -> int_mode
  
  # Criteria data frame
  c(
    'kaiser' = int_kaiser
    , 'parallel analysis' = int_pa
    , 'vss1' = int_vss1
    , 'vss2' = int_vss2
    , 'velicer map' = int_map
    , 'bic' = int_bic
    , 'empirical bic' = int_ebic
    , 'adjusted bic' = int_sabic
    , 'mode' = int_mode
  ) %>% 
    as_tibble(
      rownames = 'criterion'
    ) %>% 
    rename(nfactors = 2) -> 
    df_nfactors
  
  # Output
  return(df_nfactors)
  
}

# - Factor loadings -------------------------------------------------------
fun_efa_loadings <- function(efa_model){
  
  # Arguments validation
  stopifnot(
    "'efa_model' must be a factor analysis object." =
      any(
        class(efa_model) == 'factanal'
        , class(efa_model) == 'fa'
        , class(efa_model) == 'principal'
      )
  )
  
  # Get factor loadings
  loadings(efa_model)[,] %>%
    as_tibble(
      rownames = 'item'
    ) -> df_loadings
  
  # Data wrangling
  if(ncol(df_loadings) == 2){
    
    df_loadings %>%
      rename(
        factor1 = 2
      ) -> df_loadings
    
  }
  
  df_loadings %>%
    rename_with(
      .cols = -item
      , .fn = ~ .x %>% 
        str_extract(
          '[[:digit:]]+'
        ) %>%
        paste0('factor', .)
    ) %>% 
    relocate(
      item
      , str_sort(
        names(.)
        , numeric = T
      )
    ) -> df_loadings
  
  # Add loadings class
  df_loadings %>% 
    structure(
      class = c(
        class(.)
        , 'list'
        , 'df_loadings'
      )
    ) -> df_loadings
  
  # Output
  return(df_loadings)
  
}

# - Factor loadings match ----------------------------------------------------------
fun_efa_factor_match <- function(df_loadings){
  
  # Arguments validation
  stopifnot(
    "'df_loadings' must be a data frame obtained via the 'fun_efa_loadings' function." =
      any(class(df_loadings) == 'df_loadings')
  )
  
  # Match items to factors by max loading
  df_loadings %>% 
    pivot_longer(
      cols = -item
      , names_to = 'factor'
      , values_to = 'loading'
    ) %>% 
    relocate(factor) %>% 
    group_by(item) %>%
    mutate(
      crossloadings =
        sum(loading) - 
        max(loading)
    ) %>% 
    filter(
      loading ==
        max(loading)
    ) %>% 
    ungroup() %>%
    # Manually add factors that don't have any items
    bind_rows(
      tibble(
        factor = 
          setdiff(
            names(df_loadings[-1])
            , unique(.$factor)
          )
      )
    ) %>%
    mutate(
      factor =
        factor(
          factor
          , levels = names(
            df_loadings[-1]
          )
        )
    ) %>%
    arrange(factor) %>% 
    structure(
      class = c(
        class(.)
        , 'df_loadings_long'
        , 'list'
      )
    ) -> df_loadings_long
  
  # Output
  return(df_loadings_long)
  
}

# - Factor reliability ----------------------------------------------------
fun_efa_reliability <- function(
    mtx_correlations
    , list_factors
    , int_min_items_factor = 3
    , chr_rotation = 'oblimin'
){
  
  # Arguments validation
  stopifnot(
    "'mtx_correlations' must be a correlation matrix obtained via the 'fun_efa_correlations' function." =
      any(class(mtx_correlations) == 'mtx_correlations')
  )
  
  stopifnot(
    "'list_factors' must be a named list of character vectors." = 
      all(
        is.list(list_factors)
        , map_lgl(
          list_factors
          , is.character
        ))
  )
  
  stopifnot(
    "'int_min_items_factor' must be an integer." = 
      is.numeric(int_min_items_factor)
  )
  
  stopifnot(
    "'chr_rotation' must be a character." = 
      is.character(chr_rotation)
  )
  
  # Data wrangling
  int_min_items_factor[[1]] %>% 
    abs() %>% 
    ceiling() -> 
    int_min_items_factor
  
  # Reliability helper function
  fun_efa_reliability_helper <- function(
    mtx_correlation
    , int_min_items_factor = 3
    , chr_rotation = 'oblimin'
  ){
    
    # Single item factors => reliability = NA
    if(nrow(mtx_correlation) == 1){
      
      rep(NA, 9) %>%
        as.list() %>%
        set_names(
          c(
            'omega.tot'
            , 'maxrb'
            , 'minrb'
            , 'meanr'
            , 'av.r'
            , 'med.r'
            , 'alpha'
            , 'lambda2'
            , 'lambda6'
          )
        ) -> list_reliability
      
    } else {
      
      # Estimate reliability metrics
      splitHalf(
        mtx_correlation
      ) -> list_reliability
      
      list_reliability[
        map_lgl(
          list_reliability
          , ~ all(
            is.numeric(.x)
            , length(.x) == 1
          ))
      ] -> list_reliability
      
      c(
        omega.tot =
          omega(
            mtx_correlation
            , rotate =
              chr_rotation
            , nfactors = 1
          )$omega.tot
        , list_reliability
      ) -> list_reliability
      
    }
    
    # Data wrangling
    as_tibble(
      list_reliability
    ) %>%
      mutate(
        .before = 1
        , nitems = nrow(
          mtx_correlation
        ) - any(is.na(
          mtx_correlation
        ))
        , sufficient_items = 
          nitems >= 
          int_min_items_factor
      ) -> df_reliability
    
    # Output
    return(df_reliability)
    
  }
  
  # Subset correlation matrix
  map_if(
    .x = list_factors
    , .p = ~ !any(is.na(.x))
    , .f = ~ 
      mtx_correlations[
        all_of(.x), 
        all_of(.x)
      ]
    , .else = ~ NA
  ) %>% 
    map(as.matrix) -> 
    list_factors
  
  # Estimate factors' internal consistency
  map(
    list_factors
    , ~  
      fun_efa_reliability_helper(
        mtx_correlation = .x
        , int_min_items_factor = 
          int_min_items_factor
        , chr_rotation = 
          chr_rotation
      )
  ) %>%
    bind_rows(
      .id = 'factor'
    ) %>%
    structure(
      class = c(
        class(.)
        , 'list'
        , 'df_reliability'
      )
    ) -> df_reliability
  
  # Output
  return(df_reliability)
  
}

# - Reliability evaluation helper -----------------------------------------
fun_efa_eval <- function(dbl_coef){
  
  # Arguments validation
  stopifnot(
    "'dbl_coef' must be either numeric or NA." = 
      any(
        is.numeric(dbl_coef)
        , is.na(dbl_coef)
      )
  )
  
  # Find interval
  findInterval(
    round(dbl_coef, 2)
    , seq(0, 1, 0.1)
    , rightmost.closed = T
  ) %>% 
    case_match(
      seq(0,5) ~ 'unacceptable'
      , 6 ~ 'poor'
      , 7 ~ 'questionable'
      , 8 ~ 'acceptable'
      , 9 ~ 'good'
      , 10 ~ 'excelent'
    ) -> chr_eval
  
  # Output
  return(chr_eval)
  
}

# - Reliability evaluation interitem helper -----------------------------------------
fun_efa_eval_interitem <- function(dbl_coef){
  
  # Arguments validation
  stopifnot(
    "'dbl_coef' must be either numeric or NA." = 
      any(
        is.numeric(dbl_coef)
        , is.na(dbl_coef)
      )
  )
  
  # Find interval
  findInterval(
    round(dbl_coef, 2)
    , c(0.15, 0.5, 1)
    , left.open = F
    , rightmost.closed = T
  ) %>%
    case_match(
      0 ~ 'incoherent'
      , 1 ~ 'ideal'
      , 2 ~ 'too similar'
    ) -> chr_eval
  
  # Output
  return(chr_eval)
  
}

# - Factor evaluation -----------------------------------------------------
fun_efa_evaluation <- function(df_reliability){
  
  # Arguments validation
  stopifnot(
    "'df_reliability' must be a data frame obtained via the 'fun_efa_reliability' function." = 
      any(class(df_reliability) == 'df_reliability')
  )
  
  # Evaluate reliability
  df_reliability %>% 
    mutate(
      across(
        .cols = ends_with('.r')
        ,.fns = fun_efa_eval_interitem
      )
      , across(
        .cols = 
          c(
            where(is.numeric)
            , -ends_with('.r')
            , -nitems
          )
        ,.fns = fun_efa_eval
      )
    ) -> df_evaluation
  
  # Add df_evaluation class
  df_evaluation %>% 
    structure(
      class = c(
        class(.)
        , 'list'
        , 'df_evaluation'
      )
    ) -> df_evaluation
  
  # Output
  return(df_evaluation)
  
}

# - Factor consistency -----------------------------------------
fun_efa_consistency <- function(
    mtx_correlations
    , df_loadings_long
    , int_min_items_factor = 3
    , chr_rotation = 'oblimin'
){
  
  # Arguments validation
  stopifnot(
    "'mtx_correlations' must be a correlation matrix obtained via the 'fun_efa_correlations' function." =
      any(class(mtx_correlations) == 'mtx_correlations')
  )
  
  stopifnot(
    "'df_loadings_long' must be a data frame obtained via the 'fun_efa_factor_match' function." =
      any(class(df_loadings_long) == 'df_loadings_long')
  )
  
  stopifnot(
    "'int_min_items_factor' must be an integer." = 
      is.numeric(int_min_items_factor)
  )
  
  stopifnot(
    "'chr_rotation' must be a character." = 
      is.character(chr_rotation)
  )
  
  # Get items that belong to each factor
  df_loadings_long %>%
    split(.$factor) %>% 
    map(~ pull(.x, item)) ->
    list_factors
  
  rm(df_loadings_long)
  
  # Estimate factors' internal consistency
  fun_efa_reliability(
    mtx_correlations = 
      mtx_correlations
    , list_factors = 
      list_factors
    , int_min_items_factor = 
      int_min_items_factor
    , chr_rotation = 
      chr_rotation
  ) -> df_reliability
  
  # Evaluate consistency
  fun_efa_evaluation(
    df_reliability
  ) -> df_evaluation
  
  # Output
  return(list(
    'reliability_metrics' = df_reliability,
    'reliability_evaluation' = df_evaluation
  ))
  
}

# - Factor correlations ---------------------------------------------------
fun_efa_factor_correlations <- function(efa_model){
  
  # Factor correlation matrix
  if(efa_model$factors > 1){
    
    efa_model$
      rot.mat %>%
      solve() -> 
      mtx_tmat
    
    mtx_tmat %*% t(mtx_tmat) -> 
      mtx_factor_correlations
    
  } else {
    
    matrix(1) -> mtx_factor_correlations
    
  }
  
  paste0('factor', 1:ncol(mtx_factor_correlations)) ->   
    colnames(mtx_factor_correlations)
  
  colnames(mtx_factor_correlations) ->
    rownames(mtx_factor_correlations)
  
  round(mtx_factor_correlations, 4) -> 
    mtx_factor_correlations
  
  # Redundant factors helper function
  fun_efa_redundancy <- function(mtx_factor_correlations){
    
    # Redundant factors
    mtx_factor_correlations <- (
      mtx_factor_correlations >= 0.8
    ) * mtx_factor_correlations
    
    diag(mtx_factor_correlations) <- 0
    
    # Data wrangling
    mtx_factor_correlations %>% 
      as_tibble(rownames = 'factor') %>% 
      pivot_longer(
        cols = -1
        , names_to = 'redundant_with'
        , values_to = 'correlation'
      ) %>% 
      filter(correlation != 0) ->
      mtx_factor_correlations
    
    # Warning
    if(length(mtx_factor_correlations)){
      
      warning('There are redundant factors in the model! Check the factor correlations matrix.')
      
    }
    
    # Output
    return(mtx_factor_correlations)
    
  }
  
  # Redundant factors
  fun_efa_redundancy(
    mtx_factor_correlations
  ) -> df_factor_redundancy
  
  # Suggested rotation
  if_else(
    mtx_factor_correlations[lower.tri(
      mtx_factor_correlations
    )] %>%
      abs() %>% 
      mean() %>% 
      round(1) >= 0.3
    , 'oblique'
    , 'orthogonal'
  ) -> chr_suggested_rotation
  
  return(list(
    'factor_correlations' = mtx_factor_correlations,
    'redundant_factors' = df_factor_redundancy,
    'suggested_rotation' = chr_suggested_rotation
  ))
  
}

# - Model performance --------------------------------------------------------
fun_efa_model_performance <- function(
    df_reliability
    , int_optimal_nfactors = NULL
){
  
  # Arguments validation
  stopifnot(
    "'df_reliability' must be a 'df_reliability' objected obtained via the 'fun_efa_reliability' function." =
      any(class(df_reliability) == 'df_reliability')
  )
  
  stopifnot(
    "'int_optimal_nfactors' must be either NULL or an integer vector of optimal number of factors." = 
      any(
        is.numeric(int_optimal_nfactors)
        , is.null(int_optimal_nfactors)
      )
  )
  
  # Data wrangling
  if(length(int_optimal_nfactors)){
    
    int_optimal_nfactors %>% 
      abs() %>%
      ceiling() %>%
      pmax(1) -> 
      int_optimal_nfactors
    
  } else {
    
    nrow(df_reliability) ->
      int_optimal_nfactors
    
  }
  
  # Model performance coefficient
  if(# Sufficient items per factor
    !all(
      df_reliability$
      sufficient_items
    ) 
  ){
    
    # Evaluate model performance
    list(
      'performance' = 0,
      'evaluation' = fun_efa_eval(0)
    ) -> list_model_performance
    
  } else {
    
    # Mean factor reliability
    df_reliability %>% 
      select(
        where(is.numeric)
        , -ends_with('.r')
        , -nitems
      ) %>% 
      as.matrix() %>% 
      pmax(0) %>% 
      pmin(1) %>%
      mean(na.rm = T) * 
      # Sufficient factors
      mean(pmin(
        nrow(df_reliability) /
          int_optimal_nfactors
        , 1), na.rm = T
      ) -> coef_model_performance
    
    rm(int_optimal_nfactors)
    
    # Evaluate model performance
    list(
      'performance' = coef_model_performance,
      'evaluation' = fun_efa_eval(
        coef_model_performance
      )
    ) -> list_model_performance
    
  }
  
  # Output
  return(list_model_performance)
  
}

# - Automated EFA ---------------------------------------------------------
fun_efa_fa <- function(
    df_data
    , int_factors = 1
    , chr_rotation = 'oblimin'
    , dbl_weights = NULL
    , int_min_items_factor = 3
    , lgc_remove_low_msai_items = T
    , lgc_adequacy_testing = F
    , lgc_optimal_nfactors = F
    , lgc_show_diagrams = T
    , lgc_show_results = F
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." = 
      all(
        is.data.frame(df_data),
        df_data %>% 
          map_lgl(is.numeric) %>% 
          any()
      )
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." = 
      any(
        is.null(dbl_weights),
        all(
          is.numeric(dbl_weights),
          length(dbl_weights) ==
            nrow(df_data)
        )
      )
  )
  
  stopifnot(
    "'int_min_items_factor' must be an integer." = 
      is.numeric(int_min_items_factor)
  )
  
  stopifnot(
    "'int_factors' must be an integer." = 
      is.numeric(int_factors)
  )
  
  stopifnot(
    "'chr_rotation' must be a character." = 
      is.character(chr_rotation)
  )
  
  stopifnot(
    "'lgc_remove_low_msai_items' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_remove_low_msai_items),
        !is.na(lgc_remove_low_msai_items)
      )
  )
  
  stopifnot(
    "'lgc_optimal_nfactors' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_optimal_nfactors),
        !is.na(lgc_optimal_nfactors)
      )
  )
  
  stopifnot(
    "'lgc_show_diagrams' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_show_diagrams),
        !is.na(lgc_show_diagrams)
      )
  )
  
  stopifnot(
    "'lgc_show_results' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_show_results),
        !is.na(lgc_show_results)
      )
  )
  
  # Data wrangling
  df_data %>%
    select(where(
      is.numeric
    )) -> df_data
  
  chr_rotation[[1]] -> chr_rotation
  
  int_factors[[1]] -> int_factors
  
  ceiling(int_factors) -> int_factors
  
  int_min_items_factor[[1]] -> int_min_items_factor
  
  ceiling(int_min_items_factor) -> int_min_items_factor
  
  # Correlation matrix
  fun_efa_correlations(
    df_data = df_data
    , dbl_weights = 
      dbl_weights
  ) -> mtx_correlations
  
  # Adequacy tests
  df_adequacy_tests <- NULL
  
  if(lgc_adequacy_testing){
    
    fun_efa_adequacy(
      mtx_correlations = 
        mtx_correlations
      , int_nrow = 
        if_else(
          is.null(dbl_weights)
          , nrow(df_data)
          , sum(dbl_weights)
        )
    ) -> df_adequacy_tests
    
    # Remove problematic items
    if(lgc_remove_low_msai_items){
      
      df_data %>% 
        select(!any_of(
          df_adequacy_tests$
            problematic_items$
            item
        )) -> df_data
      
    }
    
  }
  
  # Run EFA
  fa(
    r = df_data
    , nfactors = int_factors
    , rotate = chr_rotation
    , weight = dbl_weights
  ) -> efa_model
  
  rm(df_data)
  rm(dbl_weights)
  
  # Show diagram
  if(lgc_show_diagrams){
    
    fa.diagram(efa_model)
    
  }
  
  # Show results
  if(lgc_show_results){
    
    print(
      efa_model
      , digits = 2
      , cutoff = 0.3
      , sort = T
    )
    
  }
  
  # Loadings data frames
  efa_model %>% 
    fun_efa_loadings() %>%
    fun_efa_factor_match() -> 
    df_loadings_long
  
  # Internal consistency
  fun_efa_consistency(
    mtx_correlations = 
      mtx_correlations
    , df_loadings_long = 
      df_loadings_long
    , int_min_items_factor = 
      int_min_items_factor
    , chr_rotation = 
      chr_rotation
  ) -> list_reliability
  
  # Factor correlations
  fun_efa_factor_correlations(
    efa_model 
  ) -> list_factor_correlations
  
  # Recommended number of factors
  df_nfactors <- NULL
  
  if(lgc_optimal_nfactors){
    
    fun_efa_nfactors(
      mtx_correlations 
    ) -> df_nfactors
    
    df_nfactors %>% 
      slice(-n()) %>%
      pull(nfactors) ->
      int_factors
    
  }
  
  # Overall model performance
  fun_efa_model_performance(
    df_reliability = 
      list_reliability$
      reliability_metrics
    , int_optimal_nfactors = 
      int_factors
  ) -> list_model_performance
  
  # Output
  return(list(
    'model_performance' = list_model_performance
    , 'reliability_metrics' = list_reliability$reliability_metrics
    , 'reliability_evaluation' = list_reliability$reliability_evaluation
    , 'factor_correlations' = list_factor_correlations
    , 'loadings_long' = df_loadings_long
    , 'adequacy_tests' = df_adequacy_tests
    , 'nfactors' = df_nfactors
    , 'model' = efa_model
  ))
  
}

# - Top items function ----------------------------------------------------
fun_efa_top_items <- function(
    df_data
    , dbl_weights = NULL
    , efa_model
    , int_items_total = 50
    , lgc_uneven_factors = F
    , int_min_factor_size = 3
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." = 
      all(
        is.data.frame(df_data),
        df_data %>% 
          map_lgl(is.numeric) %>% 
          any()
      )
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." = 
      any(
        is.null(dbl_weights),
        all(
          is.numeric(dbl_weights),
          length(dbl_weights) ==
            nrow(df_data)
        )
      )
  )
  
  stopifnot(
    "'efa_model' must be a factor analysis object." =
      any(
        class(efa_model) == 'factanal'
        , class(efa_model) == 'fa'
        , class(efa_model) == 'principal'
      )
  )
  
  stopifnot(
    "'int_items_total' must be an integer." = 
      is.numeric(int_items_total)
  )
  
  stopifnot(
    "'int_min_factor_size' must be an integer." = 
      is.numeric(int_min_factor_size)
  )
  
  stopifnot(
    "'lgc_uneven_factors' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_uneven_factors),
        !is.na(lgc_uneven_factors)
      )
  )
  
  # Data wrangling
  int_items_total[[1]] -> int_items_total
  ceiling(int_items_total) -> int_items_total
  
  int_min_factor_size[[1]] -> int_min_factor_size
  ceiling(int_min_factor_size) -> int_min_factor_size
  
  df_data %>% 
    select(where(
      is.numeric
    )) -> df_data
  
  # Item variance
  df_data %>%
    reframe(across(
      .cols = everything()
      ,.fns = 
        ~ wtd.var(
          x = .x
          , weights = 
            dbl_weights
        )
    )) %>% 
    pivot_longer(
      cols = everything()
      , names_to = 'item'
      , values_to = 'variance'
    ) -> df_items_var
  
  rm(df_data)
  
  # Get factor loadings
  # Match items to factors
  efa_model %>%
    fun_efa_loadings() %>%
    fun_efa_factor_match() ->
    df_loadings_long
  
  rm(efa_model)
  
  # Assess item relevance as a function of 
  # item purity and captured variance
  df_loadings_long %>% 
    full_join(
      df_items_var
    ) %>% 
    as_tibble() -> 
    df_top_items
  
  rm(df_loadings_long)
  rm(df_items_var)
  
  df_top_items %>% 
    mutate(
      purity_norm =
        loading -
        crossloadings
      , purity_norm =
        purity_norm -
        min(purity_norm)
      , purity_norm =
        purity_norm /
        max(purity_norm)
      , relevance_norm =
        (purity_norm ^ 2) *
        sqrt(
          variance /
            sum(variance)
        )
      , relevance_norm = 
        relevance_norm / 
        max(relevance_norm)
    ) -> df_top_items
  
  if(lgc_uneven_factors){
    
    # Get min items from each factor
    df_top_items %>% 
      group_by(factor) %>% 
      arrange(desc(
        relevance_norm
      )) %>% 
      slice_head(
        n = int_min_factor_size
      ) %>%
      pull(item) -> chr_items
    
    # Get remaining items
    df_top_items %>% 
      filter(!(
        item %in%
          chr_items
      )) %>% 
      arrange(desc(
        relevance_norm
      )) %>% 
      slice_head(
        n = max(
          int_items_total -
            length(chr_items)
          , 0
        )) %>%
      pull(item) %>% 
      c(chr_items) -> chr_items
    
    df_top_items %>% 
      filter(
        item %in% 
          chr_items
      ) %>% 
      group_by(factor) %>% 
      mutate(
        nitems = n()
      ) %>% 
      ungroup() %>% 
      relocate(
        factor
        , nitems
        , item
      ) %>% 
      slice(str_order(
        factor
        , numeric = T
      )) -> df_top_items
    
  } else {
    
    df_top_items %>% 
      mutate(
        nitems = 
          int_items_total / 
          length(unique(
            factor
          ))
        , nitems = 
          floor(nitems)
        , nitems = 
          pmax(
            nitems
            , int_min_factor_size
          )
      ) %>% 
      group_by(factor) %>%
      arrange(desc(
        relevance_norm
      )) %>%
      slice_head(
        n = first(.$nitems)
      ) %>% 
      relocate(
        factor
        , nitems
        , item
      ) -> df_top_items
    
  }
  
  # Output
  return(df_top_items)
  
}

# [MULTI-FACTOR FUNCTIONS] Perform EFA for a range of factors ------------------------------------------------
# - Vectorized automated EFA ----------------------------------------------
fun_efa_vfa <- function(
    df_data
    , int_factors = NULL
    , chr_rotation = 'oblimin'
    , dbl_weights = NULL
    , int_min_items_factor = 3
    , lgc_remove_low_msai_items = T
    , lgc_adequacy_testing = F
    , lgc_optimal_nfactors = F
    , lgc_show_diagrams = T
    , lgc_show_results = F  
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." = 
      all(
        is.data.frame(df_data),
        df_data %>% 
          map_lgl(is.numeric) %>% 
          any()
      )
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." = 
      any(
        is.null(dbl_weights),
        all(
          is.numeric(dbl_weights),
          length(dbl_weights) ==
            nrow(df_data)
        )
      )
  )
  
  stopifnot(
    "'int_min_items_factor' must be an integer." = 
      is.numeric(int_min_items_factor)
  )
  
  stopifnot(
    "'int_factors' must be either NULL or an integer vector." = 
      any(
        is.null(int_factors)
        , is.numeric(int_factors)
      )
  )
  
  stopifnot(
    "'chr_rotation' must be a character vector." = 
      is.character(chr_rotation)
  )
  
  stopifnot(
    "'lgc_remove_low_msai_items' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_remove_low_msai_items),
        !is.na(lgc_remove_low_msai_items)
      )
  )
  
  stopifnot(
    "'lgc_optimal_nfactors' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_optimal_nfactors),
        !is.na(lgc_optimal_nfactors)
      )
  )
  
  stopifnot(
    "'lgc_show_diagrams' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_show_diagrams),
        !is.na(lgc_show_diagrams)
      )
  )
  
  stopifnot(
    "'lgc_show_results' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_show_results),
        !is.na(lgc_show_results)
      )
  )
  
  # Data wrangling
  df_data %>%
    select(where(
      is.numeric
    )) -> df_data
  
  int_min_items_factor[[1]] -> int_min_items_factor
  
  ceiling(int_min_items_factor) -> int_min_items_factor
  
  # Correlation matrix
  fun_efa_correlations(
    df_data = df_data
    , dbl_weights = 
      dbl_weights
  ) -> mtx_correlations
  
  # Adequacy tests
  df_adequacy_tests <- NULL
  
  if(lgc_adequacy_testing){
    
    fun_efa_adequacy(
      mtx_correlations = 
        mtx_correlations
      , int_nrow = 
        if_else(
          is.null(dbl_weights)
          , nrow(df_data)
          , sum(dbl_weights)
        )
    ) -> df_adequacy_tests
    
    # Remove problematic items
    if(lgc_remove_low_msai_items){
      
      df_data %>% 
        select(!any_of(
          df_adequacy_tests$
            problematic_items$
            item
        )) -> df_data
      
    }
    
  }
  
  # Auto select number of factors
  df_nfactors <- NULL
  
  if(!length(int_factors)){
    
    fun_efa_correlations(
      df_data = df_data
      , dbl_weights = 
        dbl_weights
    ) %>% 
      fun_efa_nfactors() ->
      df_nfactors
    
    df_nfactors %>% 
      slice(
        which.max(nfactors)
        , n()
      ) %>%
      pull(nfactors) %>% 
      pmin(floor(
        ncol(mtx_correlations) / 
          int_min_items_factor
      )) -> int_factors
    
    seq(
      min(int_factors)
      , max(int_factors)
    ) -> int_factors
    
  }
  
  # Name the number of factors vector
  expand_grid(
    nfactors = unique(
      int_factors
    )
    , rotation = unique(
      chr_rotation
    )
  ) %>% 
    mutate(
      model = 
        paste0(
          'EFA_'
          , rotation
          , '_'
          , nfactors
          , 'factors'
        )
      , model = 
        str_replace(
          model
          , '1factors'
          , '1factor'
        )
    ) -> df_models
  
  set_names(
    df_models$nfactors
    , df_models$model
  ) -> int_factors
  
  set_names(
    df_models$rotation
    , df_models$model
  ) -> chr_rotation
  
  rm(df_models)
  
  # Run EFA
  map2(
    .x = int_factors
    , .y = chr_rotation
    , ~ fa(
      r = df_data
      , nfactors = .x
      , rotate = .y
      , weight = dbl_weights
    )
  ) -> list_efa_models
  
  rm(df_data)
  rm(dbl_weights)
  
  # # Show diagram
  # if(lgc_show_diagrams){
  #   
  #   list_efa_models %>% 
  #     map(fa.diagram)
  #   
  # }
  # 
  # # Show results
  # if(lgc_show_results){
  #   
  #   map(
  #     list_efa_models
  #     , ~ print(
  #       .x
  #       , digits = 2
  #       , cutoff = 0.3
  #       , sort = T
  #     )
  #   )
  #   
  # }
  
  # Loadings data frames
  list_efa_models %>% 
    map(fun_efa_loadings) %>% 
    map(fun_efa_factor_match) ->
    list_loadings_long
  
  # Internal consistency
  map2(
    .x = list_loadings_long
    , .y = chr_rotation
    , ~ fun_efa_consistency(
      mtx_correlations = 
        mtx_correlations
      , df_loadings_long = .x
      , int_min_items_factor =
        int_min_items_factor
      , chr_rotation = .y
    )
  ) -> list_reliability
  
  list_reliability %>% 
    list_flatten() -> 
    list_reliability
  
  list(
    'reliability' = 
      list_reliability[
        str_detect(
          names(
            list_reliability
          ), 'metrics'
        )] %>% 
      set_names(names(
        list_loadings_long
      ))
    , 'evaluation' = 
      list_reliability[
        str_detect(
          names(
            list_reliability
          ), 'evaluation'
        )] %>% 
      set_names(names(
        list_loadings_long
      ))
  ) -> list_reliability
  
  # Factor correlations
  list_efa_models %>% 
    map(fun_efa_factor_correlations) ->
    list_factor_correlations
  
  # Recommended number of factors
  if(all(
    lgc_optimal_nfactors
    , !length(df_nfactors)
  )){
    
    fun_efa_nfactors(
      mtx_correlations
    ) -> df_nfactors
    
    df_nfactors %>% 
      slice(-n()) %>%
      pull(nfactors) %>% 
      list() %>% 
      rep(length(
        int_factors
      )) -> int_factors
    
  }
  
  # Overall model performance
  map2(
    .x = 
      list_reliability$
      reliability
    , .y = int_factors
    , ~ fun_efa_model_performance(
      df_reliability = .x
      , int_optimal_nfactors = .y
    )
  ) %>% 
    bind_rows(.id = 'model') -> 
    df_model_performance
  
  # Output
  return(list(
    'model_performance' = df_model_performance
    , 'reliability_metrics' = list_reliability$reliability
    , 'reliability_evaluation' = list_reliability$evaluation
    , 'factor_correlations' = list_factor_correlations
    , 'loadings_long' = list_loadings_long
    , 'adequacy_tests' = df_adequacy_tests
    , 'nfactors' = df_nfactors
    , 'models' = list_efa_models
  ))
  
}

# [WORKFLOW FUNCTIONS] Perform EFA and top items selection ----------------------------------------------------
# [BEST MODELS] Perform EFA within a range of factors, pick most consistent model -----------------------------------------------------------
# [x] MULTI FUNCTIONS: PERFORM EFA WITHIN A RANGE OF FACTOR NUMBERS -------
#  [x] MULTI AUTOMATED EFA FUNCTION -----------------------------------------------------------
fun_efa.mfa <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_nfactors.vector = seq(1,5)
  , .chr_rotation = 'promax'
  , .dbl_weights = NULL
  # problematic items (unacceptable MSAi)
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = F
){
  
  # Make sure there is only numeric data
  .df_data.numeric %>% 
    select(where(is.numeric)) -> .df_data.numeric
  
  # If auto select number of factors,
  # the range of min and max number of factors in the models 
  # will start from the minimum in the selection criteria data frame
  # and end with the maximum
  if(.auto_select.nfactors){
    
    fun_efa.nfactors(.df_data.numeric) -> df_auto_select
    
    seq(
      min(df_auto_select$factors.suggested)
      , max(df_auto_select$factors.suggested)
    ) -> .int_nfactors.vector
    
  }
  
  # If repetitions in vector, keep only unique values
  .int_nfactors.vector <- unique(.int_nfactors.vector)
  
  # List names = number of factors
  names(.int_nfactors.vector) <- paste0('EFA.', .int_nfactors.vector, 'factors')
  
  names(.int_nfactors.vector) <- str_replace(names(.int_nfactors.vector), 'EFA.1factors', 'EFA.1factor')
  
  # Apply automated factor analysis for each number of factors
  lapply(
    .int_nfactors.vector
    , function(nfacts){
      
      fun_efa.fa(
        # Basic
        .df_data.numeric = .df_data.numeric
        , .int_nfactors = nfacts
        , .chr_rotation = .chr_rotation
        , .dbl_weights = .dbl_weights
        # problematic items (unacceptable MSAi)
        , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
        # Underloadings and crossloadings
        , .remove_under_loading.items = .remove_under_loading.items
        , .remove_cross_loading.items = .remove_cross_loading.items
        , .dbl_under_loading.threshold = .dbl_under_loading.threshold
        , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
        # Diagrams and tests
        , .show_diagrams = .show_diagrams
        , .show_results = .show_results
      ) %>%
        return(.)
      
    }) -> list_EFA.multi
  
  # Remove NA's returned when optimization fails
  list_EFA.multi[!is.na(list_EFA.multi)] -> list_EFA.multi
  
  .int_nfactors.vector[names(list_EFA.multi)] -> .int_nfactors.vector
  
  # Data frames comparing reliability metrics across models
  Map(
    function(facts.int, facts.name){
      
      list_EFA.multi[[facts.name]]$reliability.metrics %>%
        reframe(
          factors = facts.int
          , Useful_factors = nrow(.)
          # , Useful_factors = n()
          , Unused_factors = factors - Useful_factors
          , items.Min = min(items)
          , items.Avg = mean(items)
          , items.max = max(items)
          , items.Total = round(items.Avg * Useful_factors)
          , across(
            .cols = -contains(c('factor', 'items'))
            , .fns = function(x){mean(x, na.rm = T)}
            , .names = '{col}.Avg'
          )
        ) %>%
        return(.)
    }
    , facts.int = .int_nfactors.vector
    , facts.name = names(.int_nfactors.vector)
    
  ) %>%
    bind_rows(.id = 'Model') -> df_summary
  
  df_summary %>%
    mutate(
      across(
        # The minimum required consistency score
        # may be higher or lower, depending on the context.
        .cols = -contains(c('Model','factors', 'items', 'interitem'))
        , .fns = function(x){
          case_when(
            x < 0.5 ~ 'Unacceptable'
            , x >= 0.5 & x < 0.6 ~ 'Poor'
            , x >= 0.6 & x < 0.7 ~ 'Questionable'
            , x >= 0.7 & x < 0.8 ~ 'Acceptable'
            , x >= 0.8 & x < 0.9 ~ 'Good'
            , x >= 0.9 ~ 'Excellent'
          )
        }
      )
    ) %>%
    mutate(
      across(
        .cols = starts_with('interitem')
        , .fns = function(x){
          case_when(
            x < 0.15 ~ 'Incoherent'
            , x >= 0.15 & x <= 0.5 ~ 'Ideal'
            , x > 0.5 ~ 'Too similar'
          )
        }
      )
    ) -> df_summary.evaluation
  
  # Output
  list(
    'EFA' = list_EFA.multi
    , 'reliability.metrics' = df_summary
    , 'reliability.evaluation' = df_summary.evaluation
    , 'data' = .df_data.numeric
    # , 'plot' = plot_loadings.heatmap
  ) %>% return(.)
  
}

#  [x] MULTI TOP ITEMS FUNCTION ------------------------------------------------------
fun_efa.mtopitems <-function(
    .list_EFA
    , .int_n.items.total = 15
){
  
  # Apply top items function to each EFA in the list returned by fun_EFA.multi
  lapply(
    .list_EFA
    , function(EFA){
      
      fun_efa.topitems(
        .df_loadings.long = EFA$loadings.long
        , .int_n.items.total = .int_n.items.total
      )
      
    }
  ) %>% return(.)
  
}

# [x] WORKFLOW FUNCTIONS: PERFORM EFA AND TOP ITEMS SELECTION FROM BEGINNING TO END --------
#  [x] TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
fun_efa.fa.topitems <- function(
    # Basic
  .df_data.numeric
  , .int_nfactors = 1
  , .int_n.items.total = 15
  , .chr_rotation = 'promax'
  , .dbl_weights = NULL
  # problematic items (unacceptable MSAi)
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = F
){
  
  # Make sure there is only numeric data
  .df_data.numeric %>% 
    select(where(is.numeric)) -> .df_data.numeric
  
  # EFA
  fun_efa.fa(
    # Basic
    .df_data.numeric = .df_data.numeric
    , .int_nfactors = .int_nfactors
    , .chr_rotation = .chr_rotation
    , .dbl_weights = .dbl_weights
    # problematic items (unacceptable MSAi)
    , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
    # Underloadings and crossloadings
    , .remove_under_loading.items = .remove_under_loading.items
    , .remove_cross_loading.items = .remove_cross_loading.items
    , .dbl_under_loading.threshold = .dbl_under_loading.threshold
    , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
    # Diagrams and tests
    , .show_diagrams = .show_diagrams
    , .show_results = .show_results
  ) -> list_EFA
  
  
  # Top items
  tryCatch(
    
    expr = {
      
      fun_efa.topitems(
        .df_loadings.long = list_EFA$loadings.long
        , .int_n.items.total= .int_n.items.total
      ) %>% 
        return(.)
      
    }
    , error = function(e){return(NA)}
    
  ) -> df_top.items
  
  
  # Repeat EFA with top items only
  tryCatch(
    
    expr = {
      
      list_EFA$data %>% 
        select(df_top.items$item) -> df_data.top.items
      
      fun_efa.fa(
        .df_data.numeric = df_data.top.items
        , .int_nfactors = .int_nfactors
        , .chr_rotation = .chr_rotation
        , .dbl_weights = .dbl_weights
        # problematic items (unacceptable MSAi)
        , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
        # Underloadings and crossloadings
        , .remove_under_loading.items = .remove_under_loading.items
        , .remove_cross_loading.items = .remove_cross_loading.items
        , .dbl_under_loading.threshold = .dbl_under_loading.threshold
        , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
        # Diagrams and tests
        , .show_diagrams = .show_diagrams
        , .show_results = .show_results
      ) %>%
        return(.)
      
    }
    , error = function(e){return(NA)}
    
  ) -> list_EFA.top.items
  
  # Top items
  tryCatch(
    expr = {
      
      list_EFA.top.items$loadings.long.factors %>% 
        select(
          item
          , factor
          , loading
          , loading_Crossloadings.Diff
        ) -> df_top.items
      
    }
    , error = function(e){return(NA)}
  )
  
  # Output
  list(
    'EFA' = list_EFA
    , 'top.items' = df_top.items
    , 'EFA.top.items' = list_EFA.top.items
  ) %>% 
    return(.)
  
}

#  [x] MULTI TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
fun_efa.mfa.topitems <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_nfactors.vector = seq(1,5)
  , .int_n.items.total= 15
  , .chr_rotation = 'promax'
  , .dbl_weights = NULL
  # problematic items (unacceptable MSAi)
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = F
){
  
  # Make sure there is only numeric data
  .df_data.numeric %>% 
    select(where(is.numeric)) -> .df_data.numeric
  
  # If repetitions in vector, keep only unique values
  .int_nfactors.vector <- unique(.int_nfactors.vector)
  
  # Multi EFA
  fun_efa.mfa(
    # Basic
    .df_data.numeric = .df_data.numeric
    , .auto_select.nfactors = .auto_select.nfactors
    , .int_nfactors.vector = .int_nfactors.vector
    , .chr_rotation = .chr_rotation
    , .dbl_weights = .dbl_weights
    # problematic items (unacceptable MSAi)
    , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
    # Underloadings and crossloadings
    , .remove_under_loading.items = .remove_under_loading.items
    , .remove_cross_loading.items = .remove_cross_loading.items
    , .dbl_under_loading.threshold = .dbl_under_loading.threshold
    , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
    # Diagrams and tests
    , .show_diagrams = .show_diagrams
    , .show_results = .show_results
  ) -> list_EFA
  
  # Multi top items
  fun_efa.mtopitems(
    .list_EFA = list_EFA$EFA
    , .int_n.items.total = .int_n.items.total
  ) -> list_df_top.items
  
  
  # Repeat EFA with top items only
  # Data for each EFA (top items only)
  Map(
    function(EFA, top.items){
      
      EFA$data %>%
        select(all_of(top.items$item)) %>%
        return(.)
      
    }
    , EFA = list_EFA$EFA
    , top.items = list_df_top.items
    
  ) -> list_data.numeric.top_items
  
  # Retrieve number of factors in each EFA in list
  list_EFA$reliability.metrics$factors -> .int_nfactors.vector
  list_EFA$reliability.metrics$Model -> names(.int_nfactors.vector)
  
  # Map automated factor analysis for each number of factors
  # with subset of data (top items)
  Map(
    function(data, nfacts){
      
      fun_efa.fa(
        # Basic
        .df_data.numeric = data
        , .chr_rotation = .chr_rotation
        , .int_nfactors = nfacts
        , .dbl_weights = .dbl_weights
        # problematic items (unacceptable MSAi)
        , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
        # Underloadings and crossloadings
        , .remove_under_loading.items = .remove_under_loading.items
        , .remove_cross_loading.items = .remove_cross_loading.items
        , .dbl_under_loading.threshold = .dbl_under_loading.threshold
        , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
        # Diagrams and tests
        , .show_diagrams = .show_diagrams
        , .show_results = .show_results
      ) %>%
        return(.)
      
    }
    , data = list_data.numeric.top_items
    , nfacts = .int_nfactors.vector
    
  ) -> list_EFA.top.items
  
  # Remove NA's returned when optimization fails
  list_EFA.top.items[!is.na(list_EFA.top.items)] -> list_EFA.top.items
  
  # Update number of factors
  .int_nfactors.vector[names(list_EFA.top.items)] -> .int_nfactors.vector
  
  # Run top items function again, in case top items have changed
  lapply(
    list_EFA.top.items
    , function(EFA){
      
      fun_efa.topitems(
        .df_loadings.long = EFA$loadings.long
        , .int_n.items.total = .int_n.items.total
      ) %>% 
        return(.)
      
    }) -> list_df_top.items
  
  # Data frames comparing reliability metrics across models
  Map(
    function(facts.int, facts.name){
      
      list_EFA.top.items[[facts.name]]$reliability.metrics %>%
        reframe(
          factors = facts.int
          , Useful_factors = nrow(.)
          # , Useful_factors = n()
          , Unused_factors = factors - Useful_factors
          , items.Min = min(items)
          , items.Avg = mean(items)
          , items.max = max(items)
          , items.Total = round(items.Avg * Useful_factors)
          , across(
            .cols = -contains(c('factor', 'items'))
            , .fns = function(x){mean(x, na.rm = T)}
            , .names = '{col}.Avg'
          )
        ) %>%
        return(.)
    }
    , facts.int = .int_nfactors.vector
    , facts.name = names(.int_nfactors.vector)
    
  ) %>%
    bind_rows(.id = 'Model') -> df_summary
  
  df_summary %>%
    mutate(
      across(
        # The minimum required consistency score
        # may be higher or lower, depending on the context.
        .cols = -contains(c('Model','factors', 'items', 'interitem'))
        , .fns = function(x){
          case_when(
            x < 0.5 ~ 'Unacceptable'
            , x >= 0.5 & x < 0.6 ~ 'Poor'
            , x >= 0.6 & x < 0.7 ~ 'Questionable'
            , x >= 0.7 & x < 0.8 ~ 'Acceptable'
            , x >= 0.8 & x < 0.9 ~ 'Good'
            , x >= 0.9 ~ 'Excellent'
          )
        }
      )
    ) %>%
    mutate(
      across(
        .cols = starts_with('interitem')
        , .fns = function(x){
          case_when(
            x < 0.15 ~ 'Incoherent'
            , x >= 0.15 & x <= 0.5 ~ 'Ideal'
            , x > 0.5 ~ 'Too similar'
          )
        }
      )
    ) -> df_summary.evaluation
  
  # Output
  list(
    'EFA' = list_EFA
    , 'top.items' = list_df_top.items
    , 'EFA.top.items' = list_EFA.top.items
    , 'reliability.metrics' = df_summary
    , 'reliability.evaluation' = df_summary.evaluation
    , 'data' = list_data.numeric.top_items
    # , 'plot' = plot_loadings.heatmap
  ) %>% return(.)
  
}

# [x] BEST MODELS: PERFORM EFA WITHIN A RANGE OF FACTOR NUMBERS AND PICK THE BEST MODEL --------
#  [x] FULLY AUTOMATED EFA TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
fun_efa.bestmodel.topitems <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_min.factor_size = 3
  , .int_nfactors.vector = seq(1,5)
  , .int_n.items.total = 15
  , .chr_rotation = 'promax'
  , .dbl_weights = NULL
  # problematic items (unacceptable MSAi)
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = F
){
  
  # Make sure there is only numeric data
  .df_data.numeric %>% 
    select(where(is.numeric)) -> .df_data.numeric
  
  # If repetitions in vector, keep only unique values
  .int_nfactors.vector <- unique(.int_nfactors.vector)
  
  # Run multi EFA top items workflow
  fun_efa.mfa.topitems(
    # Basic
    .df_data.numeric = .df_data.numeric
    , .auto_select.nfactors = .auto_select.nfactors
    , .int_nfactors.vector = .int_nfactors.vector
    , .int_n.items.total= .int_n.items.total
    , .chr_rotation = .chr_rotation
    , .dbl_weights = .dbl_weights
    # problematic items (unacceptable MSAi)
    , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
    # Underloadings and crossloadings
    , .remove_under_loading.items = .remove_under_loading.items
    , .remove_cross_loading.items = .remove_cross_loading.items
    , .dbl_under_loading.threshold = .dbl_under_loading.threshold
    , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
    # Diagrams and tests
    , .show_diagrams = .show_diagrams
    , .show_results = .show_results
  ) -> list_EFA.multi.top_items
  
  # Exclusion criteria
  list_EFA.multi.top_items$reliability.metrics %>% 
    # 1. Unnecessary factors: if unused factors > 0, exclude model
    filter(Unused_factors == 0) %>%
    # 2. Minimum items per factor: if min items per factor < .int_min.factor_size, exclude model
    filter(items.Min >= .int_min.factor_size) -> df_reliability
  
  # 4. Reliability comparison
  df_reliability %>%
    group_by(
      across(
        contains(c('Model', 'factors', 'items', 'interitem'))
      )
    ) %>%
    transmute(
      Reliability.Avg = mean(
        c_across(-contains(c('Model', 'factors', 'items', 'interitem')))
        , na.rm = T)) %>% 
    ungroup() %>%
    top_n(1, Reliability.Avg) -> df_reliability.best
  
  
  # Best models
  list_EFA.multi.top_items$reliability.evaluation %>%
    filter(Model %in% df_reliability$Model) -> df_reliability.eval
  
  # Most internally consistent model ("Best model")
  list(
    'EFA' = list_EFA.multi.top_items$EFA$EFA[df_reliability.best$Model] %>% purrr::flatten()
    , 'top.items' = list_EFA.multi.top_items$top.items[df_reliability.best$Model] %>% purrr::flatten_df()
    , 'EFA.top.items' = list_EFA.multi.top_items$EFA.top.items[df_reliability.best$Model] %>% purrr::flatten()
  ) -> list_EFA.Best
  
  # Overall reliability comparison
  list_EFA.multi.top_items$reliability.metrics -> df_reliability.all
  list_EFA.multi.top_items$reliability.evaluation -> df_reliability.eval.all
  
  
  # Output
  list(
    'EFA.workflow' = list_EFA.multi.top_items
    , 'best.model' = list_EFA.Best
    , 'all.models.reliability' = df_reliability.all
    , 'all.models.evaluation' = df_reliability.eval.all
    , 'best.models.reliability' = df_reliability
    , 'best.models.evaluation' = df_reliability.eval
    
  ) %>%
    return(.)
  
}

#  [x] FULLY AUTOMATED EFA WORKFLOW FUNCTION (WITHOUT TOP ITEMS SELECTION) ------------------------------------------------------
fun_efa.bestmodel <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_min.factor_size = 3
  , .int_nfactors.vector = seq(1,5)
  , .chr_rotation = 'promax'
  , .dbl_weights = NULL
  # problematic items (unacceptable MSAi)
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = F
){
  
  # Make sure there is only numeric data
  .df_data.numeric %>% 
    select(where(is.numeric)) -> .df_data.numeric
  
  # If repetitions in vector, keep only unique values
  .int_nfactors.vector <- unique(.int_nfactors.vector)
  
  # Run multi EFA workflow (without top items)
  fun_efa.mfa(
    # Basic
    .df_data.numeric
    , .auto_select.nfactors = .auto_select.nfactors
    , .int_nfactors.vector = .int_nfactors.vector
    , .chr_rotation = .chr_rotation
    , .dbl_weights = .dbl_weights
    # problematic items (unacceptable MSAi)
    , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
    # Underloadings and crossloadings
    , .remove_under_loading.items = .remove_under_loading.items
    , .remove_cross_loading.items = .remove_cross_loading.items
    , .dbl_under_loading.threshold = .dbl_under_loading.threshold
    , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
    # Diagrams and tests
    , .show_diagrams = .show_diagrams
    , .show_results = .show_results
  ) -> list_EFA.multi
  
  # Exclusion criteria
  list_EFA.multi$reliability.metrics %>% 
    # 1. Unnecessary factors: if unused factors > 0, exclude model
    filter(Unused_factors == 0) %>%
    # 2. Minimum items per factor: if min items per factor < .int_min.factor_size, exclude model
    filter(items.Min >= .int_min.factor_size) -> df_reliability
  
  # 4. Reliability comparison
  df_reliability %>%
    group_by(
      across(
        contains(c('Model', 'factors', 'items', 'interitem'))
      )
    ) %>%
    transmute(
      Reliability.Avg = mean(
        c_across(-contains(c('Model', 'factors', 'items', 'interitem')))
        , na.rm = T)) %>% 
    ungroup() %>%
    top_n(1, Reliability.Avg) -> df_reliability.best
  
  
  # Best models
  list_EFA.multi$reliability.evaluation %>%
    filter(Model %in% df_reliability$Model) -> df_reliability.eval
  
  # Most internally consistent model ("Best model")
  list_EFA.multi$EFA[df_reliability.best$Model] %>% 
    purrr::flatten() -> list_EFA.Best
  
  # Overall reliability comparison
  list_EFA.multi$reliability.metrics -> df_reliability.all
  list_EFA.multi$reliability.evaluation -> df_reliability.eval.all
  
  
  # Output
  list(
    'EFA.workflow' = list_EFA.multi
    , 'best.model' = list_EFA.Best
    , 'all.models.reliability' = df_reliability.all
    , 'all.models.evaluation' = df_reliability.eval.all
    , 'best.models.reliability' = df_reliability
    , 'best.models.evaluation' = df_reliability.eval
    
  ) %>%
    return(.)
  
}

# [TEST] ------------------------------------------------------------------
# - vtest -----------------------------------------------------------------
rm(dsdsdsds)

fun_efa_vfa(
  df_data =
    df_occupations %>%
    select(ends_with('.l'))
  , int_factors = NULL
  , int_min_items_factor = 3
  , chr_rotation = 'equamax'
  , dbl_weights =
    df_occupations$
    employment2
  , lgc_adequacy_testing = F
  , lgc_optimal_nfactors = T
  , lgc_remove_low_msai_items = T
  , lgc_show_diagrams = F
  , lgc_show_results = F
) -> dsdsdsds

# - Test ------------------------------------------------------------------
# Data
read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

# Correlations
df_occupations %>% 
  select(ends_with('.l')) %>%
  fun_efa_correlations(
    dbl_weights = 
      df_occupations$
      employment2
  ) -> dsdsds

# Adequacy
fun_efa_adequacy(
  mtx_correlations = dsdsds
  , int_nrow = 873
)

# Optimal number of factors
fun_efa_nfactors(mtx_correlations = dsdsds)

# fa model
fa(
  r = dsdsds
  , nfactors = 10
  , weight = 
    df_occupations$
    employment2
  , rotate = 'varimax'
) -> dsds

# factor loadings
fun_efa_loadings(dsds) -> lalala

lalala

class(lalala)

# factor loadings match
fun_efa_factor_match(lalala) -> lala

lala

class(lala)

# reliability
fun_efa_reliability(
  mtx_correlation = dsdsds
  , list_factors = 
    lala %>% 
    split(.$factor) %>% 
    map( ~ pull(.x, item))
  , chr_rotation = 'oblimin'
)

# evaluation
fun_efa_reliability(
  mtx_correlation = dsdsds
  , list_factors = 
    lala %>% 
    split(.$factor) %>% 
    map( ~ pull(.x, item))
  , chr_rotation = 'oblimin'
) %>%
  fun_efa_evaluation()

# consistency
fun_efa_consistency(
  mtx_correlations = dsdsds
  , df_loadings_long = lala
  , chr_rotation = 'oblimin'
)

# efa
fun_efa_fa(
  df_data = 
    df_occupations %>% 
    select(ends_with('.l'))
  , int_factors = 15
  , int_min_items_factor = 3
  # , chr_rotation = 'oblimin'
  , chr_rotation = 'equamax'
  # , chr_rotation = 'promax'
  , dbl_weights = 
    df_occupations$
    employment2
  , lgc_adequacy_testing = F
  , lgc_optimal_nfactors = T
  , lgc_remove_low_msai_items = T
  , lgc_show_diagrams = T
  , lgc_show_results = F
) -> dsdsdsds

fun_efa_vfa(
  df_data =
    df_occupations %>%
    select(ends_with('.l'))
  # , int_factors = c(5, 15, 19)
  , int_factors = rep(19, 2)
  # , int_factors = NULL
  , int_min_items_factor = 3
  # , chr_rotation = 'oblimin'
  , chr_rotation = c('equamax', 'oblimin')
  # , chr_rotation = 'promax'
  , dbl_weights =
    df_occupations$
    employment2
  , lgc_adequacy_testing = F
  , lgc_optimal_nfactors = F
  , lgc_remove_low_msai_items = T
  , lgc_show_diagrams = F
  , lgc_show_results = F
) -> dsdsdsds

dsdsdsds$EFA_equamax_19factors

map(
  dsdsdsds
  , fun_efa_loadings
) %>% 
  map(
    fun_efa_factor_match
  )


# dsdsdsds[[1]]$reliability_metrics
# dsdsdsds[[2]]$reliability_metrics
# dsdsdsds[[3]]$reliability_metrics
# 
# dsdsdsds[[1]]$model_performance
# dsdsdsds[[2]]$model_performance
# dsdsdsds[[3]]$model_performance

dsdsdsds$model_performance
dsdsdsds$reliability_metrics %>% View
dsdsdsds$reliability_evaluation
dsdsdsds$factor_correlations
