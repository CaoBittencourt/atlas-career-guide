# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'tidyverse', 'glue' #Data wrangling
  , 'psych', 'GPArotation' #Factor analysis
  # , 'ctv' #Most relevant psychometrics packages
  , 'paletteer' #Palettes for visualization
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
fun_adequacy.tests <- function(.df_numeric){
  
  # Make sure there is only numeric data
  .df_numeric %>% 
    select(where(is.numeric)) -> .df_numeric
  
  # Adequacy tests
  # K-M-O factor adequacy test
  list_KMO <- KMO(.df_numeric)
  
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
    , 'Statistic' = cortest.bartlett(
      cor(.df_numeric)
      , n = nrow(.df_numeric))$p.value
  ) %>% 
    mutate(
      Evaluation = ifelse(
        Statistic <= 0.1
        , yes = 'Factorable'
        , no = 'Possibly factorable'
      )
    ) -> df_Barlett
  
  # Adequacy criteria data frame
  df_KMO %>%
    bind_rows(df_Barlett) -> df_KMO
  
  # Problematic items (MSAi < .5, i.e. unacceptable)
  num_items.problem <- list_KMO$MSAi[round(list_KMO$MSAi,2) < 0.5]
  
  tibble(
    Items.Problematic = names(num_items.problem)
    , Items.MSAi = num_items.problem
  ) -> df_items.problem
  
  # Output
  list(
    'Adequacy_Tests' = df_KMO
    ,'Problematic_Items' = df_items.problem
  )
  
}

#  [x] OPTIMAL NUMBER OF FACTORS FUNCTION --------------------------------
fun_nfactors.selection <- function(.df_numeric){
  
  # Make sure there is only numeric data
  .df_numeric %>% 
    select(where(is.numeric)) -> .df_numeric
  
  # Kaiser criterion
  sum(
    (.df_numeric %>%
       cor() %>%
       eigen()
    )$values >= 1) -> int_kaiser
  
  # Parallel analysis
  .df_numeric %>%
    fa.parallel(fa = 'fa', plot = F) -> pa_analysis
  
  pa_analysis$nfact -> int_pa
  
  # Other metrics
  .df_numeric %>%
    VSS(n = 2 * int_pa, plot = F) -> psy_vss
  
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
    , 'Factors.Suggested' = c(
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

# #  [x] AUTOMATED EFA FUNCTION (psych' fa) -----------------------------------------------------------
# fun_EFA <- function(
    #     # Basic
#   .df_data.numeric
#   , .int_nfactors = 1
#   , .chr_rotation = 'promax'
#   # Problematic items (unacceptable MSAi)
#   , .remove_unacceptable_MSAi.items = T
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = T
#   , .remove_cross_loading.items = T
#   , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
#   , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
#   # Factor redundancy
#   # , .dbl_factor_redundancy.threshold = 0.9 #Higher than 90% correlation = redundant factors
#   # Diagrams and tests
#   , .show_diagrams = T
#   , .show_results = F
# ){
#   
#   # Make sure there is only numeric data
#   .df_data.numeric %>% 
#     select(where(is.numeric)) -> .df_data.numeric
#   
#   # Return NA if error (i.e. unable to optimize EFA)
#   tryCatch(
#     
#     expr = {
#       
#       # For output, log of removed, cross loading, under loading and low MSAi items
#       chr_removed.items <- character()
#       chr_cross.items <- character()
#       chr_under.items <- character()
#       chr_low_MSAi.items <- character()
#       
#       # Fit factor model
#       # If remove items with unacceptable MSAi
#       list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
#       
#       chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
#       
#       if(.remove_unacceptable_MSAi.items){
#         
#         chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
#         
#         .df_data.numeric %>%
#           select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
#         
#       }
#       
#       fa(
#         r = .df_data.numeric
#         , nfactors = .int_nfactors
#         , rotate = .chr_rotation
#         , scores = 'none'
#         , fm = 'ml'
#       ) -> fit
#       
#       # Evaluation
#       # Loadings DF
#       fit$loadings[,] %>%
#         as.matrix() %>%
#         as_tibble(rownames = 'Item') %>%
#         mutate(Item = factor(Item)) -> df_loadings
#       
#       # Do variables load to the factors sufficiently?
#       # |factor loading| >= under loading threshold (generally, 0.4)
#       df_loadings %>%
#         mutate(
#           across(
#             .cols = -starts_with('Item')
#             , .fns = function(x){abs(x) >= .dbl_under_loading.threshold}
#           )
#         ) %>%
#         mutate(
#           Load.Sufficient = rowSums(select(.,-starts_with('Item')))
#           , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#         ) -> df_loadings.sufficient
#       
#       # Percentage of variables that load significantly to at least one factor
#       prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
#       
#       # Variables that do not load significantly to any factor
#       df_loadings.sufficient %>% 
#         filter(Load.Sufficient.Bin == 0) %>% 
#         pull(Item) %>% 
#         as.character() -> chr_under_loading.items
#       
#       # Remove under loading items
#       if(.remove_under_loading.items){
#         
#         while(prc_loadings.sufficient != 1){
#           
#           # Clear data
#           rm(
#             fit
#             , df_loadings
#             , df_loadings.sufficient
#             , prc_loadings.sufficient
#           )
#           
#           # Remove underloading items
#           .df_data.numeric %>%
#             select(-all_of(chr_under_loading.items)) -> .df_data.numeric
#           
#           # Removed items log
#           chr_removed.items <- c(chr_removed.items, chr_under_loading.items)  
#           chr_under.items <- c(chr_under.items, chr_under_loading.items)
#           
#           # Rerun model
#           # Fit factor model
#           # If remove items with unacceptable MSAi
#           list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
#           
#           chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
#           
#           if(.remove_unacceptable_MSAi.items){
#             
#             chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
#             
#             .df_data.numeric %>%
#               select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
#             
#           }
#           
#           fa(
#             r = .df_data.numeric
#             , nfactors = .int_nfactors
#             , rotate = .chr_rotation
#             , scores = 'none'
#             , fm = 'ml'
#           ) -> fit
#           
#           # Evaluation
#           # Loadings DF
#           fit$loadings[,] %>%
#             as.matrix() %>%
#             as_tibble(rownames = 'Item') %>%
#             mutate(Item = factor(Item)) -> df_loadings
#           
#           # Do variables load to the factors sufficiently?
#           # |factor loading| >= under loading threshold (generally, 0.4)
#           df_loadings %>%
#             mutate(
#               across(
#                 .cols = -starts_with('Item')
#                 , .fns = function(x){abs(x) >= .dbl_under_loading.threshold}
#               )
#             ) %>%
#             mutate(
#               Load.Sufficient = rowSums(select(.,-starts_with('Item')))
#               , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#             ) -> df_loadings.sufficient
#           
#           # Percentage of variables that load significantly to at least one factor
#           prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
#           
#           # Variables that do not load significantly to any factor
#           df_loadings.sufficient %>% 
#             filter(Load.Sufficient.Bin == 0) %>% 
#             pull(Item) %>%
#             as.character() -> chr_under_loading.items
#           
#         }
#         
#       }  
#       
#       # Do all factors have at least three - or, better, four - or more variables loading onto them?
#       df_loadings.sufficient %>% 
#         reframe(
#           across(
#             .cols = -starts_with(c('Item','Load.Sufficient'))
#             , .fns = function(x){sum(x)}
#           )
#         ) %>% 
#         pivot_longer(
#           cols = everything()
#           , names_to = 'Factor'
#           , values_to = 'Loading.Items'
#         ) %>%
#         mutate(
#           Greater_3 = Loading.Items >= 3
#           , Greater_4 = Loading.Items >= 4
#         ) -> df_loadings.sufficient.sum
#       
#       # Crossloadings: variables that load to more than one factor with loading values (generally) within 0.05 of one another
#       # Max loading vs other loadings
#       df_loadings %>%
#         pivot_longer(#Convert to long data format
#           cols = -starts_with('Item')
#           , names_to = 'Factor'
#           , values_to = 'Loading'
#         ) %>%
#         group_by(Item) %>%
#         mutate(
#           Loading.Max = max(Loading) #Max loading per variable
#           , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
#           , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
#             Loading.Diff.Abs == 0
#             , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
#             , no = Loading.Diff.Abs <= .dbl_cross_loading.threshold
#           )
#         ) -> df_loadings.long
#       
#       # Crossloading items
#       df_loadings.long %>% 
#         filter(Diff.Significant) %>% 
#         pull(Item) %>%
#         as.character() -> chr_cross_loading.items
#       
#       # Remove cross loading items
#       if(.remove_cross_loading.items){
#         
#         while(length(chr_cross_loading.items) != 0){
#           
#           # Clear data
#           rm(
#             fit
#             , df_loadings
#             , df_loadings.sufficient
#             , prc_loadings.sufficient
#             , df_loadings.sufficient.sum
#             , df_loadings.long
#           )
#           
#           # Remove crossloading items
#           .df_data.numeric %>%
#             select(-all_of(chr_cross_loading.items)) -> .df_data.numeric
#           
#           # Removed items log
#           chr_removed.items <- c(chr_removed.items, chr_cross_loading.items)  
#           chr_cross.items <- c(chr_cross.items, chr_cross_loading.items)  
#           
#           # Rerun model
#           # Fit factor model
#           # If remove items with unacceptable MSAi
#           list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
#           
#           chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
#           
#           if(.remove_unacceptable_MSAi.items){
#             
#             chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
#             
#             .df_data.numeric %>%
#               select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
#             
#           }
#           
#           fa(
#             r = .df_data.numeric
#             , nfactors = .int_nfactors
#             , rotate = .chr_rotation
#             , scores = 'none'
#             , fm = 'ml'
#           ) -> fit
#           
#           # Evaluation
#           # Loadings DF
#           fit$loadings[,] %>%
#             as.matrix() %>%
#             as_tibble(rownames = 'Item') %>%
#             mutate(Item = factor(Item)) -> df_loadings
#           
#           # Max loading vs other loadings
#           df_loadings %>%
#             pivot_longer(#Convert to long data format
#               cols = -starts_with('Item')
#               , names_to = 'Factor'
#               , values_to = 'Loading'
#             ) %>%
#             group_by(Item) %>%
#             mutate(
#               Loading.Max = max(Loading) #Max loading per variable
#               , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
#               , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
#                 Loading.Diff.Abs == 0
#                 , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
#                 , no = Loading.Diff.Abs <= .dbl_cross_loading.threshold
#               )
#             ) -> df_loadings.long
#           
#           # Do variables load to the factors sufficiently?
#           # |factor loading| >= cross loading threshold (generally, 0.4)
#           df_loadings %>%
#             mutate(
#               across(
#                 .cols = -starts_with('Item')
#                 , .fns = function(x){abs(x) >= .dbl_cross_loading.threshold}
#               )
#             ) %>%
#             mutate(
#               Load.Sufficient = rowSums(select(.,-starts_with('Item')))
#               , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#             ) -> df_loadings.sufficient
#           
#           # Percentage of variables that load significantly to at least one factor
#           prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
#           
#           # Variables that do not load significantly to any factor
#           df_loadings.sufficient %>% 
#             filter(Load.Sufficient.Bin == 0) %>% 
#             pull(Item) %>%
#             as.character() -> chr_under_loading.items
#           
#           # If there are new under loading items, remove them
#           if(.remove_under_loading.items){
#             
#             while(prc_loadings.sufficient != 1){
#               
#               # Clear data
#               rm(
#                 fit
#                 , df_loadings
#                 , df_loadings.sufficient
#                 , prc_loadings.sufficient
#               )
#               
#               # Remove underloading items
#               .df_data.numeric %>%
#                 select(-all_of(chr_under_loading.items)) -> .df_data.numeric
#               
#               # Removed items log
#               chr_removed.items <- c(chr_removed.items, chr_under_loading.items)  
#               chr_under.items <- c(chr_under.items, chr_under_loading.items)  
#               
#               # Rerun model
#               # Fit factor model
#               # If remove items with unacceptable MSAi
#               list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
#               
#               chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
#               
#               if(.remove_unacceptable_MSAi.items){
#                 
#                 chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
#                 
#                 .df_data.numeric %>%
#                   select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
#                 
#               }
#               
#               fa(
#                 r = .df_data.numeric
#                 , nfactors = .int_nfactors
#                 , rotate = .chr_rotation
#                 , scores = 'none'
#                 , fm = 'ml'
#               ) -> fit
#               
#               # Evaluation
#               # Loadings DF
#               fit$loadings[,] %>%
#                 as.matrix() %>%
#                 as_tibble(rownames = 'Item') %>%
#                 mutate(Item = factor(Item)) -> df_loadings
#               
#               # Do variables load to the factors sufficiently?
#               # |factor loading| >= under loading threshold (generally, 0.4)
#               df_loadings %>%
#                 mutate(
#                   across(
#                     .cols = -starts_with('Item')
#                     , .fns = function(x){abs(x) >= .dbl_under_loading.threshold}
#                   )
#                 ) %>%
#                 mutate(
#                   Load.Sufficient = rowSums(select(.,-starts_with('Item')))
#                   , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#                 ) -> df_loadings.sufficient
#               
#               # Percentage of variables that load significantly to at least one factor
#               prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
#               
#               # Variables that do not load significantly to any factor
#               df_loadings.sufficient %>% 
#                 filter(Load.Sufficient.Bin == 0) %>% 
#                 pull(Item) %>%
#                 as.character() -> chr_under_loading.items
#               
#             }
#             
#           }
#           
#           # Check if there are still cross loading items
#           df_loadings %>%
#             pivot_longer(#Convert to long data format
#               cols = -starts_with('Item')
#               , names_to = 'Factor'
#               , values_to = 'Loading'
#             ) %>%
#             group_by(Item) %>%
#             mutate(
#               Loading.Max = max(Loading) #Max loading per variable
#               , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
#               , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
#                 Loading.Diff.Abs == 0
#                 , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
#                 , no = Loading.Diff.Abs <= .dbl_cross_loading.threshold
#               )
#             ) -> df_loadings.long
#           
#           # Crossloading items
#           df_loadings.long %>% 
#             filter(Diff.Significant) %>% 
#             pull(Item) %>%
#             as.character() -> chr_cross_loading.items
#           
#         }
#         
#       }
#       
#       # Do all factors have at least three - or, better, four - or more variables loading onto them?
#       df_loadings.sufficient %>% 
#         reframe(
#           across(
#             .cols = -starts_with(c('Item','Load.Sufficient'))
#             , .fns = function(x){sum(x)}
#           )
#         ) %>% 
#         pivot_longer(
#           cols = everything()
#           , names_to = 'Factor'
#           , values_to = 'Loading.Items'
#         ) %>%
#         mutate(
#           Greater_3 = Loading.Items >= 3
#           , Greater_4 = Loading.Items >= 4
#         ) -> df_loadings.sufficient.sum
#       
#       # Reliability
#       # Do the factors form a coherent group in and of themselves?
#       
#       # Matching items to factors by maximum loading
#       df_loadings.long %>%
#         filter(
#           Loading == Loading.Max
#         ) -> df_loadings.long.factors
#       
#       # Separate factors into individual data frames
#       factors.names <- str_sort(unique(df_loadings.long.factors$Factor), numeric = T)
#       names(factors.names) <- factors.names
#       
#       # Arrange data frames for output
#       df_loadings.long %>% 
#         mutate(
#           Factor = factor(Factor, levels = factors.names)
#         ) %>% 
#         group_by(Item) %>%
#         arrange(Item, desc(Loading), .by_group = T) -> df_loadings.long
#       
#       df_loadings.long.factors %>% 
#         mutate(
#           Factor = factor(Factor, levels = factors.names)
#         ) %>% 
#         arrange(Factor, desc(Loading)) -> df_loadings.long.factors
#       
#       lapply(
#         factors.names
#         , function(factors){
#           
#           df_loadings.long.factors %>%
#             filter(
#               Factor == factors
#             ) %>%
#             pull(Item) %>%
#             factor(.) %>%
#             return(.)
#           
#         }
#       ) -> list_chr_loadings.long.factors
#       
#       # Calculate reliability measures for each subset of variables
#       # Temporarily disable warnings
#       options(warn = -1) 
#       
#       lapply(
#         list_chr_loadings.long.factors
#         , function(factors){
#           
#           .df_data.numeric %>%
#             select(all_of(factors)) -> df.temp #Select only the variables that match to each factor
#           
#           if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
#             
#             df.temp %>% 
#               splitHalf() -> metrics.other
#             
#             df.temp %>%
#               omega(nfactors = 1) -> metrics.omega
#             
#             tibble(
#               'Items' = length(factors)
#               , 'Lambda6' = metrics.other$lambda6
#               , 'OmegaT' = metrics.omega$omega.tot
#               , 'Lambda2' = metrics.other$lambda2
#               , 'Alpha' = metrics.other$alpha
#               , 'Split.Max' = metrics.other$maxrb
#               , 'Split.Avg' = metrics.other$meanr
#               , 'Split.Min' = metrics.other$minrb
#               , 'Interitem.r' = metrics.other$av.r
#             ) -> df_metrics
#             
#             return(df_metrics)
#             
#           }
#           else{
#             
#             tibble(
#               'Items' = length(factors)
#               , 'Lambda6' = NA
#               , 'OmegaT' = NA
#               , 'Lambda2' = NA
#               , 'Alpha' = NA
#               , 'Split.Max' = NA
#               , 'Split.Avg' = NA
#               , 'Split.Min' = NA
#               , 'Interitem.r' = NA
#             ) -> df_metrics
#             
#             return(df_metrics)
#             
#           }
#           
#         }
#       ) %>%
#         bind_rows(.id = 'Factor') %>%
#         select(Factor, everything()) -> df_reliability
#       
#       options(warn = getOption('warn'))
#       
#       df_reliability %>%
#         mutate(
#           across(
#             # The minimum required consistency score
#             # may be higher or lower, depending on the context.
#             .cols = -starts_with(c('Factor', 'Items', 'Interitem'))
#             , .fns = function(x){
#               case_when(
#                 x < 0.5 ~ 'Unacceptable'
#                 , x >= 0.5 & x < 0.6 ~ 'Poor'
#                 , x >= 0.6 & x < 0.7 ~ 'Questionable'
#                 , x >= 0.7 & x < 0.8 ~ 'Acceptable'
#                 , x >= 0.8 & x < 0.9 ~ 'Good'
#                 , x >= 0.9 ~ 'Excellent'
#               )
#             }
#           )
#         ) %>%
#         mutate(
#           across(
#             .cols = starts_with('Interitem')
#             , .fns = function(x){
#               case_when(
#                 x < 0.15 ~ 'Incoherent'
#                 , x >= 0.15 & x <= 0.5 ~ 'Ideal'
#                 , x > 0.5 ~ 'Too similar'
#               )
#             }
#           )
#         ) -> df_reliability.evaluation
#       
#       
#       # Adequacy tests
#       df_adequacy <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
#       
#       # Recommended number of factors
#       df_nfactors <- fun_nfactors.selection(.df_numeric = .df_data.numeric)
#       
#       # Factor correlations table
#       if(.int_nfactors != 1){
#         fit$rot.mat %>%
#           solve() -> mtx_tmat
#         
#         mtx_tmat %*%
#           t(mtx_tmat) -> mtx_corr
#         
#         mtx_corr %>%
#           round(6) -> mtx_corr
#       } else {
#         
#         matrix(1) -> mtx_corr
#         
#       }
#       
#       colnames(mtx_corr) <- factors.names
#       rownames(mtx_corr) <- factors.names
#       
#       # Redundant factors (factors are considered redundant if correlation > ...)
#       
#       # Suggested rotation matrix
#       ifelse(
#           mtx_corr[lower.tri(mtx_corr)] %>%
#             abs() %>% 
#             mean() >= 0.3
#         
#         , yes = 'promax'
#         , no = 'varimax'
#         
#       ) -> chr_suggested.rotation
#       
#       # Visualizations and results
#       # Factor Analysis Diagram and fit results
#       if(.show_diagrams){fa.diagram(fit$loadings)}
#       if(.show_results){print(fit, digits = 2, cutoff = 0.3, sort = T)}
#       # Heatmaps
#       
#       # Output 
#       list(
#         'model' = fit
#         , 'adequacy.tests' = df_adequacy
#         , 'n.factors' = df_nfactors
#         , 'sufficient.loadings' = df_loadings.sufficient.sum
#         , 'reliability.metrics' = df_reliability
#         , 'reliability.evaluation' = df_reliability.evaluation
#         , 'factor.correlation' = mtx_corr
#         , 'suggested.rotation' = chr_suggested.rotation
#         , 'removed.items' = unique(chr_removed.items)
#         , 'under_loading.items' = unique(chr_under.items)
#         , 'cross_loading.items' = unique(chr_cross.items)
#         , 'unacceptable_MSAi.items' = unique(chr_low_MSAi.items)
#         , 'data' = .df_data.numeric
#         , 'loadings' = df_loadings
#         , 'loadings.long' = df_loadings.long
#         , 'loadings.long.factors' = df_loadings.long.factors
#         # , 'plot' = plot_loadings.heatmap
#       ) %>% return(.)
#       
#     }
#     , error = function(e){return(NA)}
#     
#   )
#   
# }
# 

#  [x] AUTOMATED EFA FUNCTION (base R' factanal) -----------------------------------------------------------
fun_EFA <- function(
    # Basic
  .df_data.numeric
  , .int_nfactors = 1
  , .chr_rotation = 'promax'
  # Problematic items (unacceptable MSAi)
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
  # Factor redundancy
  # , .dbl_factor_redundancy.threshold = 0.9 #Higher than 90% correlation = redundant factors
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = F
){
  
  # Make sure there is only numeric data
  .df_data.numeric %>% 
    select(where(is.numeric)) -> .df_data.numeric
  
  # Return NA if error (i.e. unable to optimize EFA)
  tryCatch(
    
    expr = {
      
      # For output, log of removed, cross loading, under loading and low MSAi items
      chr_removed.items <- character()
      chr_cross.items <- character()
      chr_under.items <- character()
      chr_low_MSAi.items <- character()
      
      # Fit factor model
      # If remove items with unacceptable MSAi
      list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
      
      chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
      
      if(.remove_unacceptable_MSAi.items){
        
        chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
        
        .df_data.numeric %>%
          select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
        
      }
      
      factanal(
        x = .df_data.numeric
        , factors = .int_nfactors
        , rotation = .chr_rotation
        , control = list(
          nstart = 4
          , lower = 0.1
        )
      ) -> fit
      
      # Evaluation
      # Loadings DF
      fit$loadings[,] %>%
        as.matrix() %>%
        as_tibble(rownames = 'Item') %>%
        mutate(Item = factor(Item)) -> df_loadings
      
      # Do variables load to the factors sufficiently?
      # |factor loading| >= under loading threshold (generally, 0.4)
      df_loadings %>%
        mutate(
          across(
            .cols = -starts_with('Item')
            , .fns = function(x){abs(round(x,2)) >= .dbl_under_loading.threshold}
          )
        ) %>%
        mutate(
          Load.Sufficient = rowSums(select(.,-starts_with('Item')))
          , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
        ) -> df_loadings.sufficient
      
      # Percentage of variables that load significantly to at least one factor
      prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
      
      # Variables that do not load significantly to any factor
      df_loadings.sufficient %>% 
        filter(Load.Sufficient.Bin == 0) %>% 
        pull(Item) %>% 
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
          list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
          
          chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
          
          if(.remove_unacceptable_MSAi.items){
            
            chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
            
            .df_data.numeric %>%
              select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
            
          }
          
          factanal(
            x = .df_data.numeric
            , factors = .int_nfactors
            , rotation = .chr_rotation
            , control = list(
              nstart = 4
              , lower = 0.1
            )
          ) -> fit
          
          # Evaluation
          # Loadings DF
          fit$loadings[,] %>%
            as.matrix() %>%
            as_tibble(rownames = 'Item') %>%
            mutate(Item = factor(Item)) -> df_loadings
          
          # Do variables load to the factors sufficiently?
          # |factor loading| >= under loading threshold (generally, 0.4)
          df_loadings %>%
            mutate(
              across(
                .cols = -starts_with('Item')
                , .fns = function(x){abs(round(x,2)) >= .dbl_under_loading.threshold}
              )
            ) %>%
            mutate(
              Load.Sufficient = rowSums(select(.,-starts_with('Item')))
              , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
            ) -> df_loadings.sufficient
          
          # Percentage of variables that load significantly to at least one factor
          prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
          
          # Variables that do not load significantly to any factor
          df_loadings.sufficient %>% 
            filter(Load.Sufficient.Bin == 0) %>% 
            pull(Item) %>%
            as.character() -> chr_under_loading.items
          
        }
        
      }  
      
      # Do all factors have at least three - or, better, four - or more variables loading onto them?
      df_loadings.sufficient %>% 
        reframe(
          across(
            .cols = -starts_with(c('Item','Load.Sufficient'))
            , .fns = function(x){sum(x)}
          )
        ) %>% 
        pivot_longer(
          cols = everything()
          , names_to = 'Factor'
          , values_to = 'Loading.Items'
        ) %>%
        mutate(
          Greater_3 = Loading.Items >= 3
          , Greater_4 = Loading.Items >= 4
        ) -> df_loadings.sufficient.sum
      
      # Crossloadings: variables that load to more than one factor with loading values (generally) within 0.05 of one another
      # Max loading vs other loadings
      df_loadings %>%
        pivot_longer(#Convert to long data format
          cols = -starts_with('Item')
          , names_to = 'Factor'
          , values_to = 'Loading'
        ) %>%
        group_by(Item) %>%
        mutate(
          Loading.Max = max(Loading) #Max loading per variable
          , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
          , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
            Loading.Diff.Abs == 0
            , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
            , no = round(Loading.Diff.Abs,2) <= .dbl_cross_loading.threshold
          )
        ) -> df_loadings.long
      
      # Crossloading items
      df_loadings.long %>% 
        filter(Diff.Significant) %>% 
        pull(Item) %>%
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
          list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
          
          chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
          
          if(.remove_unacceptable_MSAi.items){
            
            chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
            
            .df_data.numeric %>%
              select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
            
          }
          
          factanal(
            x = .df_data.numeric
            , factors = .int_nfactors
            , rotation = .chr_rotation
            , control = list(
              nstart = 4
              , lower = 0.1
            )
          ) -> fit
          
          # Evaluation
          # Loadings DF
          fit$loadings[,] %>%
            as.matrix() %>%
            as_tibble(rownames = 'Item') %>%
            mutate(Item = factor(Item)) -> df_loadings
          
          # Max loading vs other loadings
          df_loadings %>%
            pivot_longer(#Convert to long data format
              cols = -starts_with('Item')
              , names_to = 'Factor'
              , values_to = 'Loading'
            ) %>%
            group_by(Item) %>%
            mutate(
              Loading.Max = max(Loading) #Max loading per variable
              , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
              , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
                Loading.Diff.Abs == 0
                , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
                , no = round(Loading.Diff.Abs,2) <= .dbl_cross_loading.threshold
              )
            ) -> df_loadings.long
          
          # Do variables load to the factors sufficiently?
          # |factor loading| >= cross loading threshold (generally, 0.4)
          df_loadings %>%
            mutate(
              across(
                .cols = -starts_with('Item')
                , .fns = function(x){abs(round(x,2)) >= .dbl_cross_loading.threshold}
              )
            ) %>%
            mutate(
              Load.Sufficient = rowSums(select(.,-starts_with('Item')))
              , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
            ) -> df_loadings.sufficient
          
          # Percentage of variables that load significantly to at least one factor
          prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
          
          # Variables that do not load significantly to any factor
          df_loadings.sufficient %>% 
            filter(Load.Sufficient.Bin == 0) %>% 
            pull(Item) %>%
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
              list_MSA <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
              
              chr_low_MSAi.items <- c(chr_low_MSAi.items, list_MSA$Problematic_Items$Items.Problematic)
              
              if(.remove_unacceptable_MSAi.items){
                
                chr_removed.items <- c(chr_removed.items, chr_low_MSAi.items)
                
                .df_data.numeric %>%
                  select(-all_of(list_MSA$Problematic_Items$Items.Problematic)) -> .df_data.numeric
                
              }
              
              factanal(
                x = .df_data.numeric
                , factors = .int_nfactors
                , rotation = .chr_rotation
                , control = list(
                  nstart = 4
                  , lower = 0.1
                )
              ) -> fit
              
              # Evaluation
              # Loadings DF
              fit$loadings[,] %>%
                as.matrix() %>%
                as_tibble(rownames = 'Item') %>%
                mutate(Item = factor(Item)) -> df_loadings
              
              # Do variables load to the factors sufficiently?
              # |factor loading| >= under loading threshold (generally, 0.4)
              df_loadings %>%
                mutate(
                  across(
                    .cols = -starts_with('Item')
                    , .fns = function(x){abs(round(x,2)) >= .dbl_under_loading.threshold}
                  )
                ) %>%
                mutate(
                  Load.Sufficient = rowSums(select(.,-starts_with('Item')))
                  , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
                ) -> df_loadings.sufficient
              
              # Percentage of variables that load significantly to at least one factor
              prc_loadings.sufficient <- sum(df_loadings.sufficient$Load.Sufficient.Bin)/nrow(df_loadings.sufficient)
              
              # Variables that do not load significantly to any factor
              df_loadings.sufficient %>% 
                filter(Load.Sufficient.Bin == 0) %>% 
                pull(Item) %>%
                as.character() -> chr_under_loading.items
              
            }
            
          }
          
          # Check if there are still cross loading items
          df_loadings %>%
            pivot_longer(#Convert to long data format
              cols = -starts_with('Item')
              , names_to = 'Factor'
              , values_to = 'Loading'
            ) %>%
            group_by(Item) %>%
            mutate(
              Loading.Max = max(Loading) #Max loading per variable
              , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
              , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
                Loading.Diff.Abs == 0
                , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
                , no = round(Loading.Diff.Abs,2) <= .dbl_cross_loading.threshold
              )
            ) -> df_loadings.long
          
          # Crossloading items
          df_loadings.long %>% 
            filter(Diff.Significant) %>% 
            pull(Item) %>%
            as.character() -> chr_cross_loading.items
          
        }
        
      }
      
      # Do all factors have at least three - or, better, four - or more variables loading onto them?
      df_loadings.sufficient %>% 
        reframe(
          across(
            .cols = -starts_with(c('Item','Load.Sufficient'))
            , .fns = function(x){sum(x)}
          )
        ) %>% 
        pivot_longer(
          cols = everything()
          , names_to = 'Factor'
          , values_to = 'Loading.Items'
        ) %>%
        mutate(
          Greater_3 = Loading.Items >= 3
          , Greater_4 = Loading.Items >= 4
        ) -> df_loadings.sufficient.sum
      
      # Reliability
      # Do the factors form a coherent group in and of themselves?
      
      # Matching items to factors by maximum loading
      df_loadings.long %>%
        filter(
          Loading == Loading.Max
        ) -> df_loadings.long.factors
      
      # Separate factors into individual data frames
      factors.names <- str_sort(unique(df_loadings.long.factors$Factor), numeric = T)
      names(factors.names) <- factors.names
      
      # Arrange data frames for output
      df_loadings.long %>% 
        mutate(
          Factor = factor(Factor, levels = factors.names)
        ) %>% 
        group_by(Item) %>%
        arrange(Item, desc(Loading), .by_group = T) -> df_loadings.long
      
      df_loadings.long.factors %>% 
        mutate(
          Factor = factor(Factor, levels = factors.names)
        ) %>% 
        arrange(Factor, desc(Loading)) -> df_loadings.long.factors
      
      lapply(
        factors.names
        , function(factors){
          
          df_loadings.long.factors %>%
            filter(
              Factor == factors
            ) %>%
            pull(Item) %>%
            factor(.) %>%
            return(.)
          
        }
      ) -> list_chr_loadings.long.factors
      
      # Calculate reliability measures for each subset of variables
      # Temporarily disable warnings
      options(warn = -1) 
      
      lapply(
        list_chr_loadings.long.factors
        , function(factors){
          
          .df_data.numeric %>%
            select(all_of(factors)) -> df.temp #Select only the variables that match to each factor
          
          if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
            
            df.temp %>% 
              splitHalf() -> metrics.other
            
            df.temp %>%
              omega(nfactors = 1) -> metrics.omega
            
            tibble(
              'Items' = length(factors)
              , 'Lambda6' = metrics.other$lambda6
              , 'OmegaT' = metrics.omega$omega.tot
              , 'Lambda2' = metrics.other$lambda2
              , 'Alpha' = metrics.other$alpha
              , 'Split.Max' = metrics.other$maxrb
              , 'Split.Avg' = metrics.other$meanr
              , 'Split.Min' = metrics.other$minrb
              , 'Interitem.r' = metrics.other$av.r
            ) -> df_metrics
            
            return(df_metrics)
            
          }
          else{
            
            tibble(
              'Items' = length(factors)
              , 'Lambda6' = NA
              , 'OmegaT' = NA
              , 'Lambda2' = NA
              , 'Alpha' = NA
              , 'Split.Max' = NA
              , 'Split.Avg' = NA
              , 'Split.Min' = NA
              , 'Interitem.r' = NA
            ) -> df_metrics
            
            return(df_metrics)
            
          }
          
        }
      ) %>%
        bind_rows(.id = 'Factor') %>%
        select(Factor, everything()) -> df_reliability
      
      options(warn = getOption('warn'))
      
      df_reliability %>%
        mutate(
          across(
            # The minimum required consistency score
            # may be higher or lower, depending on the context.
            .cols = -starts_with(c('Factor', 'Items', 'Interitem'))
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
            .cols = starts_with('Interitem')
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
      df_adequacy <- fun_adequacy.tests(.df_numeric = .df_data.numeric)
      
      # Recommended number of factors
      df_nfactors <- fun_nfactors.selection(.df_numeric = .df_data.numeric)
      
      # Factor correlations table
      if(.int_nfactors != 1){
        fit$rotmat %>%
          solve() -> mtx_tmat
        
        mtx_tmat %*%
          t(mtx_tmat) -> mtx_corr
        
        mtx_corr %>%
          round(6) -> mtx_corr
      } else {
        
        matrix(1) -> mtx_corr
        
      }
      
      colnames(mtx_corr) <- factors.names
      rownames(mtx_corr) <- factors.names
      
      # Redundant factors (factors are considered redundant if correlation > ...)
      
      # Suggested rotation matrix
      ifelse(
        mtx_corr[lower.tri(mtx_corr)] %>%
          abs() %>% 
          mean() %>% 
          round(1) >= 0.3
        
        , yes = 'promax'
        , no = 'varimax'
        
      ) -> chr_suggested.rotation
      
      # Visualizations and results
      # Factor Analysis Diagram and fit results
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


#  [x] TOP ITEMS FUNCTION ------------------------------------------------------
fun_top.items <- function(
    .df_loadings.long
    , .int_n.items.total = 15
){
  
  # Items per factor
  .df_loadings.long$Factor %>% 
    unique() %>% 
    length() -> nfacts
  
  # Round up
  int_n.items <- ceiling(.int_n.items.total / nfacts)
  
  .df_loadings.long %>% 
    group_by(Item) %>%
    mutate(
      Crossloadings.Abs.Sum = sum(abs(Loading)) - Loading.Max #Sum of absolute value of crossloadings
    ) %>%
    filter(
      Loading == Loading.Max
    ) %>%
    # Pick factors with max loadings and min crossloadings
    mutate(
      Loading_Crossloadings.Diff = Loading - Crossloadings.Abs.Sum
    ) %>% 
    ungroup() %>%
    group_by(Factor) %>%
    arrange(
      desc(Loading_Crossloadings.Diff)
      , .by_group = T
    ) %>% 
    top_n(int_n.items, Loading_Crossloadings.Diff) %>%
    select(
      Item
      , Factor
      , Loading
      , Loading_Crossloadings.Diff
    ) %>% return(.)
  
}


# [x] MULTI FUNCTIONS: PERFORM EFA WITHIN A RANGE OF FACTOR NUMBERS -------
#  [x] MULTI AUTOMATED EFA FUNCTION -----------------------------------------------------------
fun_EFA.multi <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_nfactors.vector = seq(1,5)
  , .chr_rotation = 'promax'
  # Problematic items (unacceptable MSAi)
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
    
    fun_nfactors.selection(.df_data.numeric) -> df_auto_select
    
    seq(
      min(df_auto_select$Factors.Suggested)
      , max(df_auto_select$Factors.Suggested)
    ) -> .int_nfactors.vector
    
  }
  
  # If repetitions in vector, keep only unique values
  .int_nfactors.vector <- unique(.int_nfactors.vector)
  
  # List names = number of factors
  names(.int_nfactors.vector) <- paste0('EFA.', .int_nfactors.vector, 'Factors')
  
  names(.int_nfactors.vector) <- str_replace(names(.int_nfactors.vector), 'EFA.1Factors', 'EFA.1Factor')
  
  # Apply automated factor analysis for each number of factors
  lapply(
    .int_nfactors.vector
    , function(nfacts){
      
      fun_EFA(
        # Basic
        .df_data.numeric = .df_data.numeric
        , .int_nfactors = nfacts
        , .chr_rotation = .chr_rotation
        # Problematic items (unacceptable MSAi)
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
          Factors = facts.int
          , Useful_Factors = nrow(.)
          # , Useful_Factors = n()
          , Unused_Factors = Factors - Useful_Factors
          , Items.Min = min(Items)
          , Items.Avg = mean(Items)
          , Items.Max = max(Items)
          , Items.Total = round(Items.Avg * Useful_Factors)
          , across(
            .cols = -contains(c('Factor', 'Items'))
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
        .cols = -contains(c('Model','Factors', 'Items', 'Interitem'))
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
        .cols = starts_with('Interitem')
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
fun_top.items.multi <- function(
    .list_EFA
    , .int_n.items.total = 15
){
  
  # Apply top items function to each EFA in the list returned by fun_EFA.multi
  lapply(
    .list_EFA
    , function(EFA){
      
      fun_top.items(
        .df_loadings.long = EFA$loadings.long
        , .int_n.items.total = .int_n.items.total
      )
      
    }
  ) %>% return(.)
  
}



# [x] WORKFLOW FUNCTIONS: PERFORM EFA AND TOP ITEMS SELECTION FROM BEGINNING TO END --------
# #  [x] TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
# fun_top.items.workflow <- function(
    #     # Basic
#   .df_data.numeric
#   , .int_nfactors = 1
#   , .int_n.items.total = 15
#   , .chr_rotation = 'promax'
#   # Problematic items (unacceptable MSAi)
#   , .remove_unacceptable_MSAi.items = T
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = T
#   , .remove_cross_loading.items = T
#   , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
#   , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
#   # Diagrams and tests
#   , .show_diagrams = T
#   , .show_results = F
# ){
#   
#   # Make sure there is only numeric data
#   .df_data.numeric %>% 
#     select(where(is.numeric)) -> .df_data.numeric
#   
#   # EFA
#   fun_EFA(
#     # Basic
#     .df_data.numeric = .df_data.numeric
#     , .int_nfactors = .int_nfactors
#     , .chr_rotation = .chr_rotation
#     # Problematic items (unacceptable MSAi)
#     , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#     # Underloadings and crossloadings
#     , .remove_under_loading.items = .remove_under_loading.items
#     , .remove_cross_loading.items = .remove_cross_loading.items
#     , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#     , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#     # Diagrams and tests
#     , .show_diagrams = .show_diagrams
#     , .show_results = .show_results
#   ) -> list_EFA
#   
#   
#   # Top items
#   tryCatch(
#     
#     expr = {
#       
#       fun_top.items(
#         .df_loadings.long = list_EFA$loadings.long
#         , .int_n.items.total= .int_n.items.total
#       ) %>% 
#         return(.)
#       
#     }
#     , error = function(e){return(NA)}
#     
#   ) -> df_top.items
#   
#   
#   # Repeat EFA with top items only
#   tryCatch(
#     
#     expr = {
#       
#       list_EFA$data %>% 
#         select(df_top.items$Item) -> df_data.top.items
#       
#       fun_EFA(
#         .df_data.numeric = df_data.top.items
#         , .int_nfactors = .int_nfactors
#         , .chr_rotation = .chr_rotation
#         # Problematic items (unacceptable MSAi)
#         , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#         # Underloadings and crossloadings
#         , .remove_under_loading.items = .remove_under_loading.items
#         , .remove_cross_loading.items = .remove_cross_loading.items
#         , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#         , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#         # Diagrams and tests
#         , .show_diagrams = .show_diagrams
#         , .show_results = .show_results
#       ) %>%
#         return(.)
#       
#     }
#     , error = function(e){return(NA)}
#     
#   ) -> list_EFA.top.items
#   
#   
#   # Output
#   list(
#     'EFA' = list_EFA
#     , 'top.items' = df_top.items
#     , 'EFA.top.items' = list_EFA.top.items
#   ) %>% 
#     return(.)
#   
# }

#  [x] TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
fun_top.items.workflow <- function(
    # Basic
  .df_data.numeric
  , .int_nfactors = 1
  , .int_n.items.total = 15
  , .chr_rotation = 'promax'
  # Problematic items (unacceptable MSAi)
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
  fun_EFA(
    # Basic
    .df_data.numeric = .df_data.numeric
    , .int_nfactors = .int_nfactors
    , .chr_rotation = .chr_rotation
    # Problematic items (unacceptable MSAi)
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
      
      fun_top.items(
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
        select(df_top.items$Item) -> df_data.top.items
      
      fun_EFA(
        .df_data.numeric = df_data.top.items
        , .int_nfactors = .int_nfactors
        , .chr_rotation = .chr_rotation
        # Problematic items (unacceptable MSAi)
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
          Item
          , Factor
          , Loading
          , Loading_Crossloadings.Diff
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

# #  [x] MULTI TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
# fun_top.items.multi.workflow <- function(
    #     # Basic
#   .df_data.numeric
#   , .auto_select.nfactors = F
#   , .int_nfactors.vector = seq(1,5)
#   , .int_n.items.total= 15
#   , .chr_rotation = 'promax'
#   # Problematic items (unacceptable MSAi)
#   , .remove_unacceptable_MSAi.items = T
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = T
#   , .remove_cross_loading.items = T
#   , .dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
#   , .dbl_cross_loading.threshold = 0.2 #Lesser than 0.2 loading difference = cross loading
#   # Diagrams and tests
#   , .show_diagrams = T
#   , .show_results = F
# ){
#   
#   # Make sure there is only numeric data
#   .df_data.numeric %>% 
#     select(where(is.numeric)) -> .df_data.numeric
#   
#   # If repetitions in vector, keep only unique values
#   .int_nfactors.vector <- unique(.int_nfactors.vector)
#   
#   # Multi EFA
#   fun_EFA.multi(
#     # Basic
#     .df_data.numeric = .df_data.numeric
#     , .auto_select.nfactors = .auto_select.nfactors
#     , .int_nfactors.vector = .int_nfactors.vector
#     , .chr_rotation = .chr_rotation
#     # Problematic items (unacceptable MSAi)
#     , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#     # Underloadings and crossloadings
#     , .remove_under_loading.items = .remove_under_loading.items
#     , .remove_cross_loading.items = .remove_cross_loading.items
#     , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#     , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#     # Diagrams and tests
#     , .show_diagrams = .show_diagrams
#     , .show_results = .show_results
#   ) -> list_EFA
#   
#   # Multi top items
#   fun_top.items.multi(
#     .list_EFA = list_EFA$EFA
#     , .int_n.items.total = .int_n.items.total
#   ) -> list_df_top.items
#   
#   
#   # Repeat EFA with top items only
#   # Data for each EFA (top items only)
#   Map(
#     function(EFA, top.items){
#       
#       EFA$data %>%
#         select(all_of(top.items$Item)) %>%
#         return(.)
#       
#     }
#     , EFA = list_EFA$EFA
#     , top.items = list_df_top.items
#     
#   ) -> list_data.numeric.top_items
#   
#   # Retrieve number of factors in each EFA in list
#   list_EFA$reliability.metrics$Factors -> .int_nfactors.vector
#   list_EFA$reliability.metrics$Model -> names(.int_nfactors.vector)
#   
#   # Map automated factor analysis for each number of factors
#   # with subset of data (top items)
#   Map(
#     function(data, nfacts){
#       
#       fun_EFA(
#         # Basic
#         .df_data.numeric = data
#         , .chr_rotation = .chr_rotation
#         , .int_nfactors = nfacts
#         # Problematic items (unacceptable MSAi)
#         , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#         # Underloadings and crossloadings
#         , .remove_under_loading.items = .remove_under_loading.items
#         , .remove_cross_loading.items = .remove_cross_loading.items
#         , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#         , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#         # Diagrams and tests
#         , .show_diagrams = .show_diagrams
#         , .show_results = .show_results
#       ) %>%
#         return(.)
#       
#     }
#     , data = list_data.numeric.top_items
#     , nfacts = .int_nfactors.vector
#     
#   ) -> list_EFA.top.items
#   
#   # Remove NA's returned when optimization fails
#   list_EFA.top.items[!is.na(list_EFA.top.items)] -> list_EFA.top.items
#   
#   # Update number of factors
#   .int_nfactors.vector[names(list_EFA.top.items)] -> .int_nfactors.vector
#   
#   # Data frames comparing reliability metrics across models
#   Map(
#     function(facts.int, facts.name){
#       
#       list_EFA.top.items[[facts.name]]$reliability.metrics %>%
#         reframe(
#           Factors = facts.int
#           , Useful_Factors = nrow(.)
#           , Unused_Factors = Factors - Useful_Factors
#           , Items.Min = min(Items)
#           , Items.Avg = mean(Items)
#           , Items.Max = max(Items)
#           , Items.Total = round(Items.Avg * Useful_Factors)
#           , across(
#             .cols = -contains(c('Factor', 'Items'))
#             , .fns = function(x){mean(x, na.rm = T)}
#             , .names = '{col}.Avg'
#           )
#         ) %>%
#         return(.)
#     }
#     , facts.int = .int_nfactors.vector
#     , facts.name = names(.int_nfactors.vector)
#     
#   ) %>%
#     bind_rows(.id = 'Model') -> df_summary
#   
#   df_summary %>%
#     mutate(
#       across(
#         # The minimum required consistency score
#         # may be higher or lower, depending on the context.
#         .cols = -contains(c('Model','Factors', 'Items', 'Interitem'))
#         , .fns = function(x){
#           case_when(
#             x < 0.5 ~ 'Unacceptable'
#             , x >= 0.5 & x < 0.6 ~ 'Poor'
#             , x >= 0.6 & x < 0.7 ~ 'Questionable'
#             , x >= 0.7 & x < 0.8 ~ 'Acceptable'
#             , x >= 0.8 & x < 0.9 ~ 'Good'
#             , x >= 0.9 ~ 'Excellent'
#           )
#         }
#       )
#     ) %>%
#     mutate(
#       across(
#         .cols = starts_with('Interitem')
#         , .fns = function(x){
#           case_when(
#             x < 0.15 ~ 'Incoherent'
#             , x >= 0.15 & x <= 0.5 ~ 'Ideal'
#             , x > 0.5 ~ 'Too similar'
#           )
#         }
#       )
#     ) -> df_summary.evaluation
#   
#   # Output
#   list(
#     'EFA' = list_EFA
#     , 'top.items' = list_df_top.items
#     , 'EFA.top.items' = list_EFA.top.items
#     , 'reliability.metrics' = df_summary
#     , 'reliability.evaluation' = df_summary.evaluation
#     , 'data' = list_data.numeric.top_items
#     # , 'plot' = plot_loadings.heatmap
#   ) %>% return(.)
#   
# }
# 
# 
# 
# 
# 
# 
# 
# 

#  [x] MULTI TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
fun_top.items.multi.workflow <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_nfactors.vector = seq(1,5)
  , .int_n.items.total= 15
  , .chr_rotation = 'promax'
  # Problematic items (unacceptable MSAi)
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
  fun_EFA.multi(
    # Basic
    .df_data.numeric = .df_data.numeric
    , .auto_select.nfactors = .auto_select.nfactors
    , .int_nfactors.vector = .int_nfactors.vector
    , .chr_rotation = .chr_rotation
    # Problematic items (unacceptable MSAi)
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
  fun_top.items.multi(
    .list_EFA = list_EFA$EFA
    , .int_n.items.total = .int_n.items.total
  ) -> list_df_top.items
  
  
  # Repeat EFA with top items only
  # Data for each EFA (top items only)
  Map(
    function(EFA, top.items){
      
      EFA$data %>%
        select(all_of(top.items$Item)) %>%
        return(.)
      
    }
    , EFA = list_EFA$EFA
    , top.items = list_df_top.items
    
  ) -> list_data.numeric.top_items
  
  # Retrieve number of factors in each EFA in list
  list_EFA$reliability.metrics$Factors -> .int_nfactors.vector
  list_EFA$reliability.metrics$Model -> names(.int_nfactors.vector)
  
  # Map automated factor analysis for each number of factors
  # with subset of data (top items)
  Map(
    function(data, nfacts){
      
      fun_EFA(
        # Basic
        .df_data.numeric = data
        , .chr_rotation = .chr_rotation
        , .int_nfactors = nfacts
        # Problematic items (unacceptable MSAi)
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

        fun_top.items(
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
          Factors = facts.int
          , Useful_Factors = nrow(.)
          # , Useful_Factors = n()
          , Unused_Factors = Factors - Useful_Factors
          , Items.Min = min(Items)
          , Items.Avg = mean(Items)
          , Items.Max = max(Items)
          , Items.Total = round(Items.Avg * Useful_Factors)
          , across(
            .cols = -contains(c('Factor', 'Items'))
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
        .cols = -contains(c('Model','Factors', 'Items', 'Interitem'))
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
        .cols = starts_with('Interitem')
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









# [x] BEST MODELS: PERFORM EFA WITHIN A RANGE OF FACTOR NUMBERS AND PICK THE BEST MODEL" --------
#  [x] FULLY AUTOMATED EFA TOP ITEMS WORKFLOW FUNCTION ------------------------------------------------------
fun_best.model.top.items.workflow <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_min.factor_size = 3
  , .int_nfactors.vector = seq(1,5)
  , .int_n.items.total = 15
  , .chr_rotation = 'promax'
  # Problematic items (unacceptable MSAi)
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
  fun_top.items.multi.workflow(
    # Basic
    .df_data.numeric = .df_data.numeric
    , .auto_select.nfactors = .auto_select.nfactors
    , .int_nfactors.vector = .int_nfactors.vector
    , .int_n.items.total= .int_n.items.total
    , .chr_rotation = .chr_rotation
    # Problematic items (unacceptable MSAi)
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
    filter(Unused_Factors == 0) %>%
    # 2. Minimum items per factor: if min items per factor < .int_min.factor_size, exclude model
    filter(Items.Min >= .int_min.factor_size) -> df_reliability
  
  # 4. Reliability comparison
  df_reliability %>%
    group_by(
      across(
        contains(c('Model', 'Factors', 'Items', 'Interitem'))
      )
    ) %>%
    transmute(
      Reliability.Avg = mean(
        c_across(-contains(c('Model', 'Factors', 'Items', 'Interitem')))
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
fun_best.model.workflow <- function(
    # Basic
  .df_data.numeric
  , .auto_select.nfactors = F
  , .int_min.factor_size = 3
  , .int_nfactors.vector = seq(1,5)
  , .chr_rotation = 'promax'
  # Problematic items (unacceptable MSAi)
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
  fun_EFA.multi(
    # Basic
    .df_data.numeric
    , .auto_select.nfactors = .auto_select.nfactors
    , .int_nfactors.vector = .int_nfactors.vector
    , .chr_rotation = .chr_rotation
    # Problematic items (unacceptable MSAi)
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
    filter(Unused_Factors == 0) %>%
    # 2. Minimum items per factor: if min items per factor < .int_min.factor_size, exclude model
    filter(Items.Min >= .int_min.factor_size) -> df_reliability
  
  # 4. Reliability comparison
  df_reliability %>%
    group_by(
      across(
        contains(c('Model', 'Factors', 'Items', 'Interitem'))
      )
    ) %>%
    transmute(
      Reliability.Avg = mean(
        c_across(-contains(c('Model', 'Factors', 'Items', 'Interitem')))
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






