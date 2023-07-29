# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
pkg <- c(
  # 'tidyverse', 'glue' #Data wrangling
  'dplyr', 'tidyr', 'readr', 'stringr', 'purrr', 'glue' #Data wrangling
  , 'psych', 'GPArotation' #EFA
  , 'weights' #Weighted correlations
  , 'Hmisc'
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
  round(
    mean(c(
      int_kaiser
      , int_pa
      , int_vss1
      , int_vss2
      , int_map
      , int_bic
      , int_ebic
      , int_sabic
    ))) -> int_avg
  
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
    , 'average' = int_avg
  ) %>% 
    as_tibble(
      rownames = 'criterion'
    ) -> df_nfactors
  
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
    "'chr_rotation' must be a character." = 
      is.character(chr_rotation)
  )
  
  # Reliability helpter function
  fun_efa_reliability_helper <- function(
    mtx_correlation
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
          nitems >= 3
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
        ,.fns = ~ .x %>% 
          findInterval(
            c(0.15, 0.5, 1)
            , left.open = F
            , rightmost.closed = T
          ) %>%
          case_match(
            0 ~ 'incoherent'
            , 1 ~ 'ideal'
            , 2 ~ 'too similar'
          )
      )
      , across(
        .cols = 
          c(
            where(is.numeric)
            , -ends_with('.r')
            , -nitems
          )
        ,.fns = ~ .x %>% 
          findInterval(
            seq(0, 1, 0.1)
            , rightmost.closed = T
          ) %>% 
          case_match(
            seq(0,5) ~ 'unacceptable'
            , 6 ~ 'poor'
            , 7 ~ 'questionable'
            , 8 ~ 'acceptable'
            , 9 ~ 'good'
            , 10 ~ 'excelent'
          )
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
fun_efa_performance <- function(
    df_reliability
    , int_optimal_nfactors = NULL
    , int_min_items_factor = 3
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
  
  stopifnot(
    "'int_min_items_factor' must be either NULL or an integer." = 
      any(
        is.numeric(int_min_items_factor)
        , is.null(int_min_items_factor)
      )
  )
  
  # Data wrangling
  ceiling(int_optimal_nfactors) ->
    int_optimal_nfactors
  
  ceiling(int_min_items_factor)[[1]] -> 
    int_min_items_factor
  
  # 
  
  
}

# - Automated EFA ---------------------------------------------------------
fun_efa_fa <- function(
    df_data
    , int_factors = 1
    , chr_rotation = 'oblimin'
    , dbl_weights = NULL
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
  fun_efa_loadings(
    efa_model = efa_model
  ) -> df_loadings
  
  fun_efa_factor_match(
    df_loadings 
  ) -> df_loadings_long
  
  rm(df_loadings)
  
  # Internal consistency
  fun_efa_consistency(
    mtx_correlations = 
      mtx_correlations
    , df_loadings_long = 
      df_loadings_long
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
    
  }
  
  # Output
  return(list(
    'reliability_metrics' = list_reliability$reliability_metrics
    , 'reliability_evaluation' = list_reliability$reliability_evaluation
    , 'factor_correlations' = list_factor_correlations
    , 'loadings_long' = df_loadings_long
    , 'adequacy_tests' = df_adequacy_tests
    , 'nfactors' = df_nfactors
    , 'model' = efa_model
  ))
  
}

# - Top items function ----------------------------------------------------

# [MULTI-FACTOR FUNCTIONS] Perform EFA for a range of factors ------------------------------------------------
# [WORKFLOW FUNCTIONS] Perform EFA and top items selection ----------------------------------------------------
# [BEST MODELS] Perform EFA within a range of factors, pick most consistent model -----------------------------------------------------------
# [x] BASIC FUNCTIONS: PERFORM EFA FOR A GIVEN FACTOR NUMBER -------------
#  [x] AUTOMATED EFA FUNCTION (psych' fa) -----------------------------------------------------------
fun_efa.fa <- function(
    # Basic
  .df_data.numeric
  , .int_nfactors = 1
  , .chr_rotation = 'promax'
  , .dbl_weights = NULL
  # problematic items (unacceptable MSAi)
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
  # factor redundancy
  # , .dbl_factor_redundancy.threshold = 0.9 #Higher than 90% correlation = redundant factors
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = F
){
  
  # Make sure there is only numeric data
  .df_data.numeric %>% 
    select(where(
      is.numeric
    )) -> .df_data.numeric
  
  # Return NA if error (i.e. unable to optimize EFA)
  # safely(
  tryCatch(
    
    expr = {
      
      # For output, log of removed, cross loading, under loading and low MSAi items
      chr_removed.items <- character()
      chr_cross.items <- character()
      chr_under.items <- character()
      chr_low_MSAi.items <- character()
      
      # Fit factor model
      # If remove items with unacceptable MSAi
      list_MSA <- fun_efa.adequacy(.df_numeric = .df_data.numeric)
      
      chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$problematic_items$items.problematic)
      
      if(.remove_unacceptable_MSAi.items){
        
        chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
        
        .df_data.numeric %>%
          select(-all_of(list_MSA$problematic_items$items.problematic)) -> .df_data.numeric
        
      }
      
      fa(
        r = .df_data.numeric
        , nfactors = .int_nfactors
        , rotate = .chr_rotation
        , weight = .dbl_weights
      ) -> fit
      
      # Evaluation
      # loadings DF
      loadings(fit)[,] %>% 
        as_tibble(
          rownames = 'item'
        ) %>% 
        set_names(
          c(
            'item'
            , loadings(fit) %>%
              colnames() %>% 
              str_extract(
                '[[:digit:]]+'
              ) %>%
              paste0('factor',.)
          )
        ) %>% 
        relocate(
          item
          , str_sort(
            names(.)
            , numeric = T
          )
        ) -> df_loadings
      
      # Do variables load to the factors sufficiently?
      # |factor loading| >= under loading threshold (generally, 0.4)
      df_loadings %>%
        mutate(
          across(
            .cols = -starts_with('item')
            , .fns = function(x){abs(round(x,2)) >= .dbl_under_loading.threshold}
          )
        ) %>%
        mutate(
          Load.Sufficient = rowSums(select(.,-starts_with('item')))
          , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
        ) -> df_loadings.sufficient
      
      # Percentage of variables that load significantly to at least one factor
      prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
      
      # Variables that do not load significantly to any factor
      df_loadings.sufficient %>% 
        filter(Load.Sufficient.Bin == 0) %>% 
        pull(item) %>% 
        as.character() -> chr_under_loading.items
      
      # Remove under loading items
      if(.remove_under_loading.items){
        
        while(prc_loadings.sufficient != 1){
          
          # Clear data
          rm(
            fit
            , df_loadings
            , df_loadings.sufficient
            , prc_loadings.sufficient
          )
          
          # Remove underloading items
          .df_data.numeric %>%
            select(-all_of(chr_under_loading.items)) -> .df_data.numeric
          
          # Removed items log
          chr_removed.items <- c(chr_removed.items, chr_under_loading.items)  
          chr_under.items <- c(chr_under.items, chr_under_loading.items)
          
          # Rerun model
          # Fit factor model
          # If remove items with unacceptable MSAi
          list_MSA <- fun_efa.adequacy(.df_numeric = .df_data.numeric)
          
          chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$problematic_items$items.problematic)
          
          if(.remove_unacceptable_MSAi.items){
            
            chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
            
            .df_data.numeric %>%
              select(-all_of(list_MSA$problematic_items$items.problematic)) -> .df_data.numeric
            
          }
          
          fa(
            r = .df_data.numeric
            , nfactors = .int_nfactors
            , rotate = .chr_rotation
            , weight = .dbl_weights
          ) -> fit
          
          # Evaluation
          # loadings DF
          fit$loadings[,] %>%
            as.matrix() %>%
            as_tibble(rownames = 'item') %>%
            mutate(item = factor(item)) -> df_loadings
          
          # Do variables load to the factors sufficiently?
          # |factor loading| >= under loading threshold (generally, 0.4)
          df_loadings %>%
            mutate(
              across(
                .cols = -starts_with('item')
                , .fns = function(x){abs(round(x,2)) >= .dbl_under_loading.threshold}
              )
            ) %>%
            mutate(
              Load.Sufficient = rowSums(select(.,-starts_with('item')))
              , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
            ) -> df_loadings.sufficient
          
          # Percentage of variables that load significantly to at least one factor
          prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
          
          # Variables that do not load significantly to any factor
          df_loadings.sufficient %>% 
            filter(Load.Sufficient.Bin == 0) %>% 
            pull(item) %>%
            as.character() -> chr_under_loading.items
          
        }
        
      }  
      
      # Do all factors have at least three - or, better, four - or more variables loading onto them?
      df_loadings.sufficient %>% 
        reframe(
          across(
            .cols = -starts_with(c('item','Load.Sufficient'))
            , .fns = function(x){sum(x)}
          )
        ) %>% 
        pivot_longer(
          cols = everything()
          , names_to = 'factor'
          , values_to = 'loading.items'
        ) %>%
        mutate(
          Greater_3 = loading.items >= 3
          , Greater_4 = loading.items >= 4
        ) -> df_loadings.sufficient.sum
      
      # Crossloadings: variables that load to more than one factor with loading values (generally) within 0.05 of one another
      # Max loading vs other loadings
      df_loadings %>%
        pivot_longer(#Convert to long data format
          cols = -starts_with('item')
          , names_to = 'factor'
          , values_to = 'loading'
        ) %>%
        group_by(item) %>%
        mutate(
          loading.max = max(loading) #Max loading per variable
          , loading.Diff.Abs = abs(loading.max - loading) #Absolute difference
          , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
            loading.Diff.Abs == 0
            , yes = F #loading.Diff.Abs == 0 <=> Max loading (i.e. difference between max value and itself)
            , no = round(loading.Diff.Abs,2) <= .dbl_cross_loading.threshold
          )
        ) -> df_loadings.long
      
      # Crossloading items
      df_loadings.long %>% 
        filter(Diff.Significant) %>% 
        pull(item) %>%
        as.character() -> chr_cross_loading.items
      
      # Remove cross loading items
      if(.remove_cross_loading.items){
        
        while(length(chr_cross_loading.items) != 0){
          
          # Clear data
          rm(
            fit
            , df_loadings
            , df_loadings.sufficient
            , prc_loadings.sufficient
            , df_loadings.sufficient.sum
            , df_loadings.long
          )
          
          # Remove crossloading items
          .df_data.numeric %>%
            select(-all_of(chr_cross_loading.items)) -> .df_data.numeric
          
          # Removed items log
          chr_removed.items <- c(chr_removed.items, chr_cross_loading.items)  
          chr_cross.items <- c(chr_cross.items, chr_cross_loading.items)  
          
          # Rerun model
          # Fit factor model
          # If remove items with unacceptable MSAi
          list_MSA <- fun_efa.adequacy(.df_numeric = .df_data.numeric)
          
          chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$problematic_items$items.problematic)
          
          if(.remove_unacceptable_MSAi.items){
            
            chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
            
            .df_data.numeric %>%
              select(-all_of(list_MSA$problematic_items$items.problematic)) -> .df_data.numeric
            
          }
          
          fa(
            r = .df_data.numeric
            , nfactors = .int_nfactors
            , rotate = .chr_rotation
            , weight = .dbl_weights
          ) -> fit
          
          # Evaluation
          # loadings DF
          fit$loadings[,] %>%
            as.matrix() %>%
            as_tibble(rownames = 'item') %>%
            mutate(item = factor(item)) -> df_loadings
          
          # Max loading vs other loadings
          df_loadings %>%
            pivot_longer(#Convert to long data format
              cols = -starts_with('item')
              , names_to = 'factor'
              , values_to = 'loading'
            ) %>%
            group_by(item) %>%
            mutate(
              loading.max = max(loading) #Max loading per variable
              , loading.Diff.Abs = abs(loading.max - loading) #Absolute difference
              , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
                loading.Diff.Abs == 0
                , yes = F #loading.Diff.Abs == 0 <=> Max loading (i.e. difference between max value and itself)
                , no = round(loading.Diff.Abs,2) <= .dbl_cross_loading.threshold
              )
            ) -> df_loadings.long
          
          # Do variables load to the factors sufficiently?
          # |factor loading| >= cross loading threshold (generally, 0.4)
          df_loadings %>%
            mutate(
              across(
                .cols = -starts_with('item')
                , .fns = function(x){abs(round(x,2)) >= .dbl_cross_loading.threshold}
              )
            ) %>%
            mutate(
              Load.Sufficient = rowSums(select(.,-starts_with('item')))
              , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
            ) -> df_loadings.sufficient
          
          # Percentage of variables that load significantly to at least one factor
          prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
          
          # Variables that do not load significantly to any factor
          df_loadings.sufficient %>% 
            filter(Load.Sufficient.Bin == 0) %>% 
            pull(item) %>%
            as.character() -> chr_under_loading.items
          
          # If there are new under loading items, remove them
          if(.remove_under_loading.items){
            
            while(prc_loadings.sufficient != 1){
              
              # Clear data
              rm(
                fit
                , df_loadings
                , df_loadings.sufficient
                , prc_loadings.sufficient
              )
              
              # Remove underloading items
              .df_data.numeric %>%
                select(-all_of(chr_under_loading.items)) -> .df_data.numeric
              
              # Removed items log
              chr_removed.items <- c(chr_removed.items, chr_under_loading.items)  
              chr_under.items <- c(chr_under.items, chr_under_loading.items)  
              
              # Rerun model
              # Fit factor model
              # If remove items with unacceptable MSAi
              list_MSA <- fun_efa.adequacy(.df_numeric = .df_data.numeric)
              
              chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$problematic_items$items.problematic)
              
              if(.remove_unacceptable_MSAi.items){
                
                chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
                
                .df_data.numeric %>%
                  select(-all_of(list_MSA$problematic_items$items.problematic)) -> .df_data.numeric
                
              }
              
              fa(
                r = .df_data.numeric
                , nfactors = .int_nfactors
                , rotate = .chr_rotation
                , weight = .dbl_weights
              ) -> fit
              
              # Evaluation
              # loadings DF
              fit$loadings[,] %>%
                as.matrix() %>%
                as_tibble(rownames = 'item') %>%
                mutate(item = factor(item)) -> df_loadings
              
              # Do variables load to the factors sufficiently?
              # |factor loading| >= under loading threshold (generally, 0.4)
              df_loadings %>%
                mutate(
                  across(
                    .cols = -starts_with('item')
                    , .fns = function(x){abs(round(x,2)) >= .dbl_under_loading.threshold}
                  )
                ) %>%
                mutate(
                  Load.Sufficient = rowSums(select(.,-starts_with('item')))
                  , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
                ) -> df_loadings.sufficient
              
              # Percentage of variables that load significantly to at least one factor
              prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
              
              # Variables that do not load significantly to any factor
              df_loadings.sufficient %>% 
                filter(Load.Sufficient.Bin == 0) %>% 
                pull(item) %>%
                as.character() -> chr_under_loading.items
              
            }
            
          }
          
          # Check if there are still cross loading items
          df_loadings %>%
            pivot_longer(#Convert to long data format
              cols = -starts_with('item')
              , names_to = 'factor'
              , values_to = 'loading'
            ) %>%
            group_by(item) %>%
            mutate(
              loading.max = max(loading) #Max loading per variable
              , loading.Diff.Abs = abs(loading.max - loading) #Absolute difference
              , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
                loading.Diff.Abs == 0
                , yes = F #loading.Diff.Abs == 0 <=> Max loading (i.e. difference between max value and itself)
                , no = round(loading.Diff.Abs,2) <= .dbl_cross_loading.threshold
              )
            ) -> df_loadings.long
          
          # Crossloading items
          df_loadings.long %>% 
            filter(Diff.Significant) %>% 
            pull(item) %>%
            as.character() -> chr_cross_loading.items
          
        }
        
      }
      
      # Do all factors have at least three - or, better, four - or more variables loading onto them?
      df_loadings.sufficient %>% 
        reframe(
          across(
            .cols = -starts_with(c('item','Load.Sufficient'))
            , .fns = function(x){sum(x)}
          )
        ) %>% 
        pivot_longer(
          cols = everything()
          , names_to = 'factor'
          , values_to = 'loading.items'
        ) %>%
        mutate(
          Greater_3 = loading.items >= 3
          , Greater_4 = loading.items >= 4
        ) -> df_loadings.sufficient.sum
      
      # Reliability
      # Do the factors form a coherent group in and of themselves?
      
      # Matching items to factors by maximum loading
      df_loadings.long %>%
        filter(
          loading == loading.max
        ) -> df_loadings.long.factors
      
      # Separate factors into individual data frames
      factors.names <- str_sort(unique(df_loadings.long.factors$factor), numeric = T)
      names(factors.names) <- factors.names
      
      # Arrange data frames for output
      df_loadings.long %>% 
        mutate(
          factor = factor(factor, levels = factors.names)
        ) %>% 
        group_by(item) %>%
        arrange(item, desc(loading), .by_group = T) -> df_loadings.long
      
      df_loadings.long.factors %>% 
        mutate(
          factor = factor(factor, levels = factors.names)
        ) %>% 
        arrange(factor, desc(loading)) -> df_loadings.long.factors
      
      lapply(
        factors.names
        , function(factors){
          
          df_loadings.long.factors %>%
            filter(
              factor == factors
            ) %>%
            pull(item) %>%
            factor(.) %>%
            return(.)
          
        }
      ) -> list_chr_loadings.long.factors
      
      # Calculate reliability measures for each subset of variables
      # Temporarily disable warnings
      options(warn = -1) 
      
      .df_data.numeric %>% 
        weights::wtd.cors(
          weight = 
            .dbl_weights
        ) -> mtx_correlationsrelation
      
      lapply(
        list_chr_loadings.long.factors
        , function(factors){
          
          mtx_correlationsrelation[
            all_of(factors)
            , all_of(factors)
          ] -> df.temp
          
          # .df_data.numeric %>%
          #   select(all_of(factors)) -> df.temp #Select only the variables that match to each factor
          # 
          if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
            
            df.temp %>% 
              splitHalf() -> metrics.other
            
            df.temp %>%
              omega(
                nfactors = 1
                , rotate = .chr_rotation
              ) -> metrics.omega
            
            tibble(
              'items' = length(factors)
              , 'lambda6' = metrics.other$lambda6
              , 'omega.t' = metrics.omega$omega.tot
              , 'lambda2' = metrics.other$lambda2
              , 'alpha' = metrics.other$alpha
              , 'split.max' = metrics.other$maxrb
              , 'split.avg' = metrics.other$meanr
              , 'split.min' = metrics.other$minrb
              , 'interitem.r' = metrics.other$av.r
            ) -> df_metrics
            
            return(df_metrics)
            
          }
          else{
            
            tibble(
              'items' = length(factors)
              , 'lambda6' = NA
              , 'omega.t' = NA
              , 'lambda2' = NA
              , 'alpha' = NA
              , 'split.max' = NA
              , 'split.avg' = NA
              , 'split.min' = NA
              , 'interitem.r' = NA
            ) -> df_metrics
            
            return(df_metrics)
            
          }
          
        }
      ) %>%
        bind_rows(.id = 'factor') %>%
        select(factor, everything()) -> df_reliability
      
      options(warn = getOption('warn'))
      
      df_reliability %>%
        mutate(
          across(
            # The minimum required consistency score
            # may be higher or lower, depending on the context.
            .cols = -starts_with(c('factor', 'items', 'interitem'))
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
        ) -> df_reliability.evaluation
      
      
      # Adequacy tests
      fun_efa.adequacy(
        .df_numeric = .df_data.numeric
        , .dbl_weights = .dbl_weights
      ) -> df_adequacy
      
      # Recommended number of factors
      fun_efa.nfactors(
        .df_numeric = .df_data.numeric
        , .dbl_weights = .dbl_weights
      ) -> df_nfactors
      
      # factor correlations table
      if(.int_nfactors != 1){
        
        fit$rot.mat %>%
          solve() -> mtx_tmat
        
        mtx_tmat %*%
          t(mtx_tmat) -> mtx_correlationsr
        
        mtx_correlationsr %>%
          round(6) -> mtx_correlationsr
        
      } else {
        
        matrix(1) -> mtx_correlationsr
        
      }
      
      colnames(mtx_correlationsr) <- paste0('factor', 1:ncol(mtx_correlationsr))
      rownames(mtx_correlationsr) <- paste0('factor', 1:nrow(mtx_correlationsr))
      
      # Redundant factors (factors are considered redundant if correlation > ...)
      
      # Suggested rotation matrix
      ifelse(
        mtx_correlationsr[lower.tri(mtx_correlationsr)] %>%
          abs() %>% 
          mean() %>% 
          round(1) >= 0.3
        
        , yes = 'oblique'
        , no = 'orthogonal'
        
      ) -> chr_suggested.rotation
      
      # Visualizations and results
      # factor Analysis Diagram and fit results
      if(.show_diagrams){fa.diagram(fit$loadings)}
      if(.show_results){print(fit, digits = 2, cutoff = 0.3, sort = T)}
      # Heatmaps
      
      # Output 
      list(
        'model' = fit
        , 'adequacy.tests' = df_adequacy
        , 'n.factors' = df_nfactors
        , 'sufficient.loadings' = df_loadings.sufficient.sum
        , 'reliability.metrics' = df_reliability
        , 'reliability.evaluation' = df_reliability.evaluation
        , 'factor.correlation' = mtx_correlationsr
        , 'suggested.rotation' = chr_suggested.rotation
        , 'removed.items' = unique(chr_removed.items)
        , 'under_loading.items' = unique(chr_under.items)
        , 'cross_loading.items' = unique(chr_cross.items)
        , 'unacceptable_MSAi.items' = unique(chr_low_MSAi.items)
        , 'data' = .df_data.numeric
        , 'loadings' = df_loadings
        , 'loadings.long' = df_loadings.long
        , 'loadings.long.factors' = df_loadings.long.factors
        # , 'plot' = plot_loadings.heatmap
      ) %>% return(.)
      
    }
    , error = function(e){return(NA)}
  )
  
}


# #  [x] TOP ITEMS FUNCTION ------------------------------------------------------
# fun_efa.topitems <- function(
    #     .df_loadings.long
#     , .int_n.items.total = 15
# ){
#   
#   # items per factor
#   .df_loadings.long$factor %>% 
#     unique() %>% 
#     length() -> nfacts
#   
#   # Round up
#   int_n.items <- ceiling(.int_n.items.total / nfacts)
#   
#   .df_loadings.long %>% 
#     group_by(item) %>%
#     mutate(
#       Crossloadings.Abs.Sum = sum(abs(loading)) - loading.max #Sum of absolute value of crossloadings
#     ) %>%
#     filter(
#       loading == loading.max
#     ) %>%
#     # Pick factors with max loadings and min crossloadings
#     mutate(
#       loading_Crossloadings.Diff = loading - Crossloadings.Abs.Sum
#     ) %>% 
#     ungroup() %>%
#     group_by(factor) %>%
#     arrange(
#       desc(loading_Crossloadings.Diff)
#       , .by_group = T
#     ) %>% 
#     # top_n(int_n.items, loading_Crossloadings.Diff) %>%
#     slice(1:int_n.items) %>%
#     select(
#       item
#       , factor
#       , loading
#       , loading_Crossloadings.Diff
#     ) %>% return(.)
#   
# }

# #  [x] TOP ITEMS FUNCTION ------------------------------------------------------
# fun_efa.topitems <- function(
    #     .df_data.numeric
#     , .dbl_weights = NULL
#     , .efa_model
#     , .int_n.items.total = 15
#     , .lgc_uneven.factors = F
#     , .int_min.factor_size = 3
# ){
#   
#   # items per factor
#   .df_data.numeric %>%
#     reframe(across(
#       .cols = where(is.numeric)
#       ,.fns = 
#         ~ Hmisc::wtd.var(
#           x = .x
#           , weights = 
#             .dbl_weights
#         )
#     )) %>% 
#     pivot_longer(
#       cols = everything()
#       , names_to = 'item'
#       , values_to = 'item.var'
#     ) -> df_items.var
#   
#   
#   loadings(.efa_model)[,] %>% 
#     as_tibble(
#       rownames = 'item'
#     ) %>%
#     set_names(
#       c(
#         'item'
#         , loadings(.efa_model) %>%
#           colnames() %>% 
#           str_extract(
#             '[[:digit:]]+'
#           ) %>%
#           paste0('factor',.)
#       )
#     ) %>%
#     pivot_longer(
#       cols = where(is.numeric)
#       , names_to = 'factor'
#       , values_to = 'factor.loading'
#     ) %>% 
#     group_by(item) %>% 
#     mutate(
#       loading.max = 
#         max(factor.loading)
#       , crossloading = 
#         # sum(abs(factor.loading)) - loading.max
#         sum(factor.loading) - loading.max
#     ) %>%
#     ungroup() %>%
#     filter(
#       factor.loading == 
#         loading.max
#     ) %>%
#     full_join(
#       df_items.var
#     ) %>% 
#     ungroup() %>% 
#     mutate(
#       item.purity =
#         factor.loading -
#         crossloading
#       , item.purity.norm =
#         item.purity -
#         min(item.purity)
#       , item.purity.norm =
#         item.purity.norm /
#         max(item.purity.norm)
#       , item.relevance =
#         (item.purity.norm ^ 2) *
#         sqrt(
#           item.var /
#             sum(item.var)
#         )
#     ) -> df_loadings.long
#   
#   df_loadings.long %>% 
#     group_by(factor) %>% 
#     arrange(desc(
#       item.relevance
#     )) %>% 
#     slice(1:.int_min.factor_size) %>%
#     pull(item) -> chr_items
#   
#   df_loadings.long %>% 
#     filter(!(
#       item %in%
#         chr_items
#     )) %>% 
#     arrange(desc(
#       item.relevance
#     )) %>% 
#     slice(1:max(
#       .int_n.items.total -
#         length(chr_items)
#       , 0
#     )) %>% 
#     pull(item) %>% 
#     c(chr_items) -> chr_items
#   
#   df_loadings.long %>% 
#     filter(
#       item %in% 
#         chr_items
#     ) %>% 
#     group_by(factor) %>% 
#     mutate(
#       factor.items = n()
#     ) %>% 
#     ungroup() %>% 
#     relocate(
#       factor
#       , factor.items
#       , item
#       , everything()
#     ) %>% 
#     slice(str_order(
#       factor, numeric = T
#     )) -> df_loadings.long
#   
#   # df_loadings.long %>% 
#   #   ungroup() %>% 
#   #   mutate(
#   #     top.item = 
#   #       item %in% 
#   #       chr_items 
#   #     , nitems = 
#   #       if_else(
#   #         top.item
#   #         , length(chr_items)
#   #         , .int_n.items.total - 
#   #           length(chr_items)
#   #       )
#   #     , nitems = 
#   #       max(nitems, 0)
#   #   ) %>% 
#   #   group_by(top.item) %>% 
#   #   arrange(desc(
#   #     item.relevance
#   #   )) %>% 
#   #   slice(1:nitems) %>% 
#   #   return()
#   # filter(!(
#   #   item %in% 
#   #     chr_items
#   # )) %>% 
#   # arrange(desc(
#   #   item.relevance
#   # )) %>% 
#   # slice(
#   #   1:(.int_n.items.total - length(chr_items))
#   # ) %>% 
#   # pull(item) %>%
#   # c(chr_items) %>% 
#   # return()
#   
#   
#   # reframe(
#   #   factor.var =
#   #     sum(item.var)
#   # ) %>%
#   #   mutate(
#   #     total.items = 
#   #       .int_n.items.total
#   #     , factor.items = 
#   #       total.items * 
#   #       factor.var / 
#   #       sum(factor.var)
#   #     , item.sum = 
#   #       sum(factor.items)
#   #   ) %>% 
#   #   return()
#   
#   # if(.lgc_uneven.factors){
#   #   
#   #   df_loadings.long %>% 
#   #     group_by(factor) %>%
#   #     reframe(
#   #       factor.var =
#   #         sum(item.var)
#   #     ) %>% 
#   #     mutate(
#   #       total.items = 
#   #         .int_n.items.total
#   #       , factor.items = 
#   #         total.items * 
#   #         factor.var / 
#   #         sum(factor.var)
#   #       , factor.items = 
#   #         round(factor.items)
#   #       , item.sum = 
#   #         sum(factor.items)
#   #       , factor.items = 
#   #         if_else(
#   #           item.sum ==
#   #             .int_n.items.total
#   #           , factor.items
#   #           , ceiling(
#   #             total.items * 
#   #               factor.var / 
#   #               sum(factor.var) 
#   #           ))
#   #       , factor.items = 
#   #         pmax(
#   #           factor.items
#   #           , .int_min.factor_size
#   #         )
#   #     ) %>% 
#   #     select(
#   #       factor
#   #       , factor.var
#   #       , factor.items
#   #     ) -> df_factor.items
#   #   
#   # } else { 
#   #   
#   #   df_loadings.long %>% 
#   #     mutate(
#   #       factor.items =
#   #         .int_n.items.total / 
#   #         length(unique(factor))
#   #     ) %>% 
#   #     group_by(factor) %>% 
#   #     reframe(
#   #       factor.items = 
#   #         factor.items %>% 
#   #         unique() %>% 
#   #         ceiling()
#   #     ) %>% 
#   #     mutate(
#   #       factor.items = 
#   #         pmax(
#   #           factor.items
#   #           , .int_min.factor_size
#   #         )
#   #     ) -> df_factor.items
#   # }
#   
#   # df_loadings.long %>%
#   #   full_join(
#   #     df_factor.items
#   #   ) %>%
#   #   group_by(factor) %>%
#   #   arrange(desc(
#   #     item.relevance
#   #   )) %>%
#   #   slice(1:unique(
#   #     factor.items
#   #   )) %>%
#   #   ungroup() %>%
#   #   slice(str_order(
#   #     factor, numeric = T
#   #   )) -> df_loadings.long
#   
#   return(df_loadings.long)
#   
# }

#  [x] TOP ITEMS FUNCTION ------------------------------------------------------
fun_efa.topitems <- function(
    .df_data.numeric
    , .dbl_weights = NULL
    , .efa_model
    , .int_n.items.total = 15
    , .lgc_uneven.factors = F
    , .int_min.factor_size = 3
){
  
  # items per factor
  .df_data.numeric %>%
    reframe(across(
      .cols = where(is.numeric)
      ,.fns = 
        ~ Hmisc::wtd.var(
          x = .x
          , weights = 
            .dbl_weights
        )
    )) %>% 
    pivot_longer(
      cols = everything()
      , names_to = 'item'
      , values_to = 'item.var'
    ) -> df_items.var
  
  
  loadings(.efa_model)[,] %>% 
    as_tibble(
      rownames = 'item'
    ) %>%
    set_names(
      c(
        'item'
        , loadings(.efa_model) %>%
          colnames() %>% 
          str_extract(
            '[[:digit:]]+'
          ) %>%
          paste0('factor',.)
      )
    ) %>%
    pivot_longer(
      cols = where(is.numeric)
      , names_to = 'factor'
      , values_to = 'factor.loading'
    ) %>% 
    group_by(item) %>% 
    mutate(
      loading.max = 
        max(factor.loading)
      , crossloading = 
        # sum(abs(factor.loading)) - loading.max
        sum(factor.loading) - loading.max
    ) %>%
    ungroup() %>%
    filter(
      factor.loading == 
        loading.max
    ) %>%
    full_join(
      df_items.var
    ) %>% 
    ungroup() %>% 
    mutate(
      item.purity =
        factor.loading -
        crossloading
      , item.purity.norm =
        item.purity -
        min(item.purity)
      , item.purity.norm =
        item.purity.norm /
        max(item.purity.norm)
      , item.relevance =
        (item.purity.norm ^ 2) *
        sqrt(
          item.var /
            sum(item.var)
        )
    ) -> df_loadings.long
  
  df_loadings.long %>% 
    group_by(factor) %>% 
    arrange(desc(
      item.relevance
    )) %>% 
    slice_head(
      n = .int_min.factor_size
    ) %>%
    pull(item) -> chr_items
  
  df_loadings.long %>% 
    filter(!(
      item %in%
        chr_items
    )) %>% 
    arrange(desc(
      item.relevance
    )) %>% 
    slice_head(
      n = max(
        .int_n.items.total -
          length(chr_items)
        , 0
      )) %>%
    pull(item) %>% 
    c(chr_items) -> chr_items
  
  df_loadings.long %>% 
    filter(
      item %in% 
        chr_items
    ) %>% 
    mutate(
      factor = factor(factor)
    ) %>% 
    group_by(factor) %>% 
    mutate(
      factor.items = n()
    ) %>% 
    ungroup() %>% 
    relocate(
      factor
      , factor.items
      , item
      , everything()
    ) %>% 
    slice(str_order(
      factor, numeric = T
    )) -> df_loadings.long
  
  
  return(df_loadings.long)
  
}

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
  , int_factors = 4
  # , chr_rotation = 'equamax'
  , chr_rotation = 'promax'
  , dbl_weights = 
    df_occupations$
    employment2
  , lgc_adequacy_testing = F
  , lgc_optimal_nfactors = F
  , lgc_remove_low_msai_items = T
  , lgc_show_diagrams = T
  , lgc_show_results = F
) -> dsdsdsds

dsdsdsds$reliability_metrics
dsdsdsds$reliability_evaluation
dsdsdsds$factor_correlations
