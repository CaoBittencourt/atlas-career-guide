# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'tidyverse', 'glue' #Data wrangling
  , 'psych', 'GPArotation' #factor analysis
  , 'matrixcalc'
  , 'plm'
  , 'Hmisc'
  # , 'ctv' #Most relevant psychometrics packages
  , 'paletteer' #Palettes for visualization
  , 'stats'
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [x] BASIC FUNCTIONS: PERFORM EFA FOR A GIVEN FACTOR NUMBER -------------
#  [x] ADEQUACY TESTING FUNCTION -----------------------------------------------
fun_efa.adequacy <- function(
    .df_numeric
    , .dbl_weights = NULL
){
  
  # Make sure there is only numeric data
  .df_numeric %>%
    select(where(
      is.numeric
    )) -> .df_numeric
  
  # Adequacy tests
  # K-M-O factor adequacy test
  .df_numeric %>%
    weights::wtd.cors(
      weight =
        .dbl_weights
    ) -> mtx_cor
  
  if(
    tryCatch(
      invisible(capture.output(
        {
          
          KMO(mtx_cor)
          
        }
      ))
      , message = function(i){T}
    )
    
  ){
    
    tryCatch(
      invisible(capture.output(
        {
          
          mtx_cor %>%
            round(10) %>%
            KMO() -> list_KMO
          
        }
      ))
      , message = function(i){
        
        NA ->> list_KMO
        
      }
    )
    
  } else {
    
    KMO(mtx_cor) -> list_KMO
    
  }
  
  tibble(
    'Adequacy_Index' = 'KMO'
    , 'Summary' = 'Measures sampling adequacy to factor analysis.'
    , 'Statistic' = round(list_KMO$MSA,2)
  ) %>%
    mutate(
      Evaluation = case_when(
        Statistic < 0.5 ~ 'Unacceptable'
        , Statistic >= 0.5 & Statistic < 0.6 ~ 'Miserable'
        , Statistic >= 0.6 & Statistic < 0.7 ~ 'Mediocre'
        , Statistic >= 0.7 & Statistic < 0.8 ~ 'Middling'
        , Statistic >= 0.8 & Statistic < 0.9 ~ 'Meritorious'
        , Statistic >= 0.9 ~ 'Marvelous'
      )
    ) -> df_KMO
  
  # Barlett's correlation test
  tibble(
    'Adequacy_Index' = 'Barlett'
    , 'Summary' = 'P-value to reject the hypothesis that the data is uncorrelated and cannot be grouped into factors.'
    , 'Statistic' = 
      cortest.bartlett(
        mtx_cor
        , n = if_else(
          !is.null(.dbl_weights)
          , sum(.dbl_weights)
          , nrow(.df_numeric)
        ))$p.value
  ) %>%
    mutate(
      Evaluation = ifelse(
        Statistic <= 0.1
        , yes = 'factorable'
        , no = 'Possibly factorable'
      )
    ) -> df_Barlett
  
  # Adequacy criteria data frame
  df_KMO %>%
    bind_rows(df_Barlett) -> df_KMO
  
  # problematic items (MSAi < .5, i.e. unacceptable)
  num_items.problem <- list_KMO$MSAi[round(list_KMO$MSAi,2) < 0.5]
  
  tibble(
    items.problematic = names(num_items.problem)
    , items.MSAi = num_items.problem
  ) -> df_items.problem
  
  # Output
  list(
    'Adequacy_Tests' = df_KMO
    ,'problematic_items' = df_items.problem
  )
  
}

#  [x] OPTIMAL NUMBER OF FACTORS FUNCTION --------------------------------
fun_efa.nfactors <- function(
    .df_numeric
    , .dbl_weights = NULL
){
  
  # Make sure there is only numeric data
  .df_numeric %>% 
    select(where(
      is.numeric
    )) -> .df_numeric
  
  # Correlation matrix
  .df_numeric %>%
    weights::wtd.cors(
      weight =
        .dbl_weights
    ) -> mtx_cor
  
  # Kaiser criterion
  mtx_cor %>% 
    eigen() %>% 
    map(~ .x >= 1) %>% 
    flatten_lgl() %>% 
    sum() -> int_kaiser
  
  # Parallel analysis
  mtx_cor %>%
    round(7) %>%
    fa.parallel(
      fa = 'fa'
      , plot = F
    ) -> pa_analysis
  
  pa_analysis$nfact -> int_pa
  
  # Other metrics
  mtx_cor %>%
    VSS(
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
  
  tibble(
    'Criterion' = c(
      'Kaiser'
      , 'Parallel_Analysis'
      , 'VSS1'
      , 'VSS2'
      , 'Velicer_Map'
      , 'BIC'
      , 'Empirical_BIC'
      , 'Adjusted_BIC'
      , 'Average')
    , 'factors.suggested' = c(
      int_kaiser
      , int_pa
      , int_vss1
      , int_vss2
      , int_map
      , int_bic
      , int_ebic
      , int_sabic
      , int_avg
    )) %>% return(.)
  
}

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
        ) -> mtx_correlation
      
      lapply(
        list_chr_loadings.long.factors
        , function(factors){
          
          mtx_correlation[
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
          t(mtx_tmat) -> mtx_corr
        
        mtx_corr %>%
          round(6) -> mtx_corr
        
      } else {
        
        matrix(1) -> mtx_corr
        
      }
      
      colnames(mtx_corr) <- paste0('factor', 1:ncol(mtx_corr))
      rownames(mtx_corr) <- paste0('factor', 1:nrow(mtx_corr))
      
      # Redundant factors (factors are considered redundant if correlation > ...)
      
      # Suggested rotation matrix
      ifelse(
        mtx_corr[lower.tri(mtx_corr)] %>%
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
        , 'factor.correlation' = mtx_corr
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
