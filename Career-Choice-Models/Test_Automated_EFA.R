# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'tidyverse', 'glue' #Data wrangling
  , 'psych' #Factor analysis
  , 'paletteer' #Palettes for visualization
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# AUTOMATED EFA FUNCTION -----------------------------------------------------------
fun_factor.analysis <- function(
    # Basic
  df_data.numeric
  , int_nfactors = 1
  , chr_rotation = 'promax'
  # Underloadings and crossloadings
  , remove_under_loading.items = T
  , remove_cross_loading.items = F
  , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
  , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
  # Diagrams and tests
  , show_diagrams = T
  , show_results = T
  , stop_at_warnings = F
){
  
  # For output, if any items are removed
  chr_removed.items <- character()
  
  # Adequacy tests
  # 
  
  # Criteria to determine number of factors
  # 
  
  # Fit factor model
  factanal(
    x = df_data.numeric %>% select(where(is.numeric))
    , factors = int_nfactors
    , rotation = chr_rotation
  ) -> fit
  
  # Diagram and fit results
  if(show_diagrams){fa.diagram(fit$loadings)}
  if(show_results){print(fit, digits = 2, cutoff = 0.3, sort = T)}
  
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
        ,.fns = function(x){abs(x) >= dbl_under_loading.threshold}
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
  
  
  if(show_results){
    
    cat(glue(
      '\n{100*round(prc_loadings.sufficient,4)}% of items load to at least one factor.'
    ))
    
  }
  
  
  if(show_results & prc_loadings.sufficient != 1){
    
    cat(glue(
      '\nThe following items do not sufficiently load to at least one factor: {paste0(chr_under_loading.items, collapse = ", ")}.'
    ))
    
  }
  
  
  if(remove_under_loading.items){
    
    while(prc_loadings.sufficient != 1){
      
      # Clear data
      rm(
        fit
        , df_loadings
        , df_loadings.sufficient
        , prc_loadings.sufficient
      )
      
      # Remove underloading items
      df_data.numeric %>%
        select(-all_of(chr_under_loading.items)) -> df_data.numeric
      
      # Removed items log
      chr_removed.items <- c(chr_removed.items, chr_under_loading.items)  
      
      # Rerun model
      # Adequacy tests
      # 
      
      # Criteria to determine number of factors
      # 
      
      # Fit factor model
      factanal(
        x = df_data.numeric %>% select(where(is.numeric))
        , factors = int_nfactors
        , rotation = chr_rotation
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
            ,.fns = function(x){abs(x) >= dbl_under_loading.threshold}
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
        # as.character() -> chr_temp
        as.character() -> chr_under_loading.items
      
      # chr_under_loading.items <<- unique(c(chr_under_loading.items, chr_temp))
      
      
      if(show_results){
        
        print(glue(
          '{100*round(prc_loadings.sufficient,4)}% of items load to at least one factor.'
        ))
        
      }
      
      
      if(show_results & prc_loadings.sufficient != 1){
        
        print(glue(
          '\nThe following items do not sufficiently load to at least one factor: {paste0(chr_under_loading.items, collapse = ", ")}.'
        ))
        
      }
    }
  }  
  
  # Do all factors have at least three - or, better, four - or more variables loading onto them?
  df_loadings.sufficient %>% 
    summarise(
      across(
        .cols = -starts_with(c('Item','Load.Sufficient'))
        ,.fns = function(x){sum(x)}
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
  
  # Rewrite later
  if(show_results){print(df_loadings.sufficient.sum)}
  
  
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
        , no = Loading.Diff.Abs <= dbl_cross_loading.threshold
      )
    ) -> df_loadings.long
  
  # Crossloading items
  df_loadings.long %>% 
    filter(Diff.Significant) %>% 
    pull(Item) %>%
    as.character() -> chr_cross_loading.items
  
  if(remove_cross_loading.items){
    
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
      df_data.numeric %>%
        select(-all_of(c(
          chr_under_loading.items
          , chr_cross_loading.items
        ))) -> df_data.numeric
      
      # Removed items log
      chr_removed.items <- c(chr_removed.items, chr_under_loading.items)  
      
      # Rerun model
      # Adequacy tests
      # 
      
      # Criteria to determine number of factors
      # 
      
      # Fit factor model
      factanal(
        x = df_data.numeric %>% select(where(is.numeric))
        , factors = int_nfactors
        , rotation = chr_rotation
      ) -> fit
      
      # Evaluation
      # Loadings DF
      fit$loadings[,] %>%
        as.matrix() %>%
        as_tibble(rownames = 'Item') %>%
        mutate(Item = factor(Item)) -> df_loadings
      
      # Do variables load to the factors sufficiently?
      # |factor loading| >= cross loading threshold (generally, 0.4)
      df_loadings %>%
        mutate(
          across(
            .cols = -starts_with('Item')
            ,.fns = function(x){abs(x) >= dbl_cross_loading.threshold}
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
        # as.character() -> chr_temp
        as.character() -> chr_cross_loading.items
      
      if(remove_under_loading.items){
        
        while(prc_loadings.sufficient != 1){
          
          # Clear data
          rm(
            fit
            , df_loadings
            , df_loadings.sufficient
            , prc_loadings.sufficient
          )
          
          # Remove underloading items
          df_data.numeric %>%
            select(-all_of(chr_under_loading.items)) -> df_data.numeric
          
          # Removed items log
          chr_removed.items <- c(chr_removed.items, chr_under_loading.items)  
          
          # Rerun model
          # Adequacy tests
          # 
          
          # Criteria to determine number of factors
          # 
          
          # Fit factor model
          factanal(
            x = df_data.numeric %>% select(where(is.numeric))
            , factors = int_nfactors
            , rotation = chr_rotation
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
                ,.fns = function(x){abs(x) >= dbl_under_loading.threshold}
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
            # as.character() -> chr_temp
            as.character() -> chr_under_loading.items
          
          # chr_under_loading.items <<- unique(c(chr_under_loading.items, chr_temp))
          
          
          if(show_results){
            
            print(glue(
              '{100*round(prc_loadings.sufficient,4)}% of items load to at least one factor.'
            ))
            
          }
          
          
          if(show_results & prc_loadings.sufficient != 1){
            
            print(glue(
              '\nThe following items do not sufficiently load to at least one factor: {paste0(chr_under_loading.items, collapse = ", ")}.'
            ))
            
          }
        }
      }
      
      # Do all factors have at least three - or, better, four - or more variables loading onto them?
      df_loadings.sufficient %>% 
        summarise(
          across(
            .cols = -starts_with(c('Item','Load.Sufficient'))
            ,.fns = function(x){sum(x)}
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
      
      # Rewrite later
      if(show_results){print(df_loadings.sufficient.sum)}
    }
  }  
  
  
  # Reliability
  # Do the factors form a coherent group in and of themselves?
  
  # Matching items to factors by maximum loading
  df_loadings.long %>%
    filter(
      Loading == Loading.Max
    ) -> df_loadings.long.factors
  
  # Separate factors into individual data frames
  lapply(
    str_sort(unique(df_loadings.long.factors$Factor))
    , function(factors){
      
      df_loadings.long.factors %>%
        filter(
          Factor == factors
        ) %>%
        pull(Item) %>%
        factor(.) %>%
        return(.)
      
    }) -> list_chr_loadings.long.factors
  
  # Calculate reliability measures for each subset of variables
  lapply(
    list_chr_loadings.long.factors
    , function(factors){
      
      df_occupations.numeric %>%
        select(factors) -> df.temp #Select only the variables that match to each factor
      
      if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
        
        df.temp %>% 
          splitHalf() -> metrics.other
        
        df.temp %>%
          omega(nfactors = 1) -> metrics.omega
        
        tibble(
          'Lambda4' = metrics.other$maxrb
          , 'Lambda6' = metrics.other$lambda6
          , 'Split.Avg' = metrics.other$meanr
          , 'Alpha' = metrics.other$alpha
          , 'Lambda2' = metrics.other$lambda2
          , 'Beta' = metrics.other$minrb
          , 'Interitem.r' = metrics.other$av.r
          , 'Omega' = metrics.omega$omega.tot
        ) -> df_metrics
        
        # df_metrics %>% 
        #   mutate(
        #     across(
        #       .cols = 
        #     )
        #   )
        
        return(df_metrics)
        
      }
      else{
        
        return(NA)
        
      }
      
    }) -> list_reliability
  
  
  list(
    'model' = fit
    , 'reliability' = list_reliability
    , 'loadings' = df_loadings
    , 'loadings.long' = df_loadings.long
    , 'sufficient' = df_loadings.sufficient.sum
    , 'removed.items' = chr_removed.items
    , 'data' = df_data.numeric
  ) %>% return(.)
  
  # Loadings DF (long)
  
  
  # Visualizations
  
  
  # Remove cross loading items
  
  # Assess reliability
  
  # Return (list)
  # Loadings
  # Loadings (Long)
  # Loadings (Long, Factors)
  # Reliability Items
  
  
}


# TOP ITEMS FUNCTION ------------------------------------------------------
# EFA II: Max loadings, 5 factors --------------------------------------------------------
# # Exploratory Factor Analysis (EFA)
# 'Not in' operator
# `%!in%` <- Negate(`%in%`)

# K-M-O factor adequacy test
# * 0.00 to 0.49 unacceptable
# * 0.50 to 0.59 miserable
# * 0.60 to 0.69 mediocre
# * 0.70 to 0.79 middling
# * 0.80 to 0.89 meritorious
# => 0.90 to 1.00 marvelous
round(KMO(df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50])$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

sum(ev$values >= 1)

# Kaiser criterion suggests 6 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 6 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 1 factors are sufficient
# VSS criterion 2 suggests 2 factors are sufficient
# Velicer MAP criterion suggests 7 factors are sufficient

# Factor Analysis
n.facts <- 5

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are fairly correlated to one another.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# On first sight, Factors 5 and 6 seem disposable.
# This is confirmed by further analyses.

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
(abs(fit$loadings) > 0.4) %>% 
  as_tibble(rownames = 'Item') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,-starts_with('Item')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Item)

# Food production does not load sufficiently to any factor
# Since it is a very negatively asymItemal variable,
# so much so that most careers don't require any level of this field of knowledge,
# it seems thus appropriate to drop it from the data set.
df_occupations.numeric %>% 
  select(-starts_with('Food_Production')) -> df_occupations.numeric

# Re-running the model

# K-M-O factor adequacy test
# * 0.00 to 0.49 unacceptable
# * 0.50 to 0.59 miserable
# * 0.60 to 0.69 mediocre
# * 0.70 to 0.79 middling
# * 0.80 to 0.89 meritorious
# => 0.90 to 1.00 marvelous
round(KMO(df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50])$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

sum(ev$values >= 1)

# Kaiser criterion suggests 6 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 6 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 1 factors are sufficient
# VSS criterion 2 suggests 2 factors are sufficient
# Velicer MAP criterion suggests 7 factors are sufficient

# Factor Analysis
n.facts <- 5

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are fairly correlated to one another.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# On first sight, Factors 5 and 6 seem disposable.
# This is confirmed by further analyses.

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
(abs(fit$loadings) > 0.4) %>% 
  as_tibble(rownames = 'Item') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,-starts_with('Item')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Item)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(-starts_with('Item')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(-starts_with('Item')) %>%
  colSums() >= 4


# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another

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
      , no = Loading.Diff.Abs <= 0.05
    )
  ) -> df_loadings.long

# Loadings Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Item))
    , fill = Loading
  )) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#FF0000"
    , mid = "#FFFFCC"
    , high = "#075AFF")

# Max Loadings Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Item))
    , fill = Loading == Loading.Max
  )) +
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Item))
    , fill = Loading.Diff.Abs
  )) +
  geom_tile() +
  scale_fill_gradient2(
    low = "#FF0000"
    , mid = "#FFFFCC"
    , high = "#075AFF")

# Significant Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Item))
    , fill = Diff.Significant
  )) +
  geom_tile()

# Almost no significant difference between factor loadings for each item.
# Conclusion: Very few crossloadings. Each item maps considerably to only one factor.


# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?

# Matching items to factors
# Since there are relatively few crossloadings, it is adequate to
# use maximum loading as the criteria for factor matching.
df_loadings.long %>%
  filter(
    Loading == Loading.Max
  ) -> df_loadings.long.factors


# Separate factors into individual data frames
lapply(
  str_sort(unique(df_loadings.long.factors$Factor))
  , function(factors){
    
    df_loadings.long.factors %>%
      filter(
        Factor == factors
      ) %>%
      pull(Item) %>%
      factor(.) %>%
      return(.)
    
  }) -> list_chr_loadings.long.factors

# Apply Alpha test to each subset of variables
lapply(
  list_chr_loadings.long.factors
  , function(factors){
    
    df_occupations.numeric %>%
      select(factors) -> df.temp #Select only the variables that match to each factor
    
    if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
      
      df.temp %>% 
        splitHalf() -> metrics.other
      
      df.temp %>%
        omega(nfactors = 1) -> metrics.omega
      
      tibble(
        'Lambda4' = metrics.other$maxrb
        , 'Lambda6' = metrics.other$lambda6
        , 'Split.Avg' = metrics.other$meanr
        , 'Alpha' = metrics.other$alpha
        , 'Lambda2' = metrics.other$lambda2
        , 'Beta' = metrics.other$minrb
        , 'Interitem.r' = metrics.other$av.r
        , 'Omega' = metrics.omega$omega.tot
      ) -> df_metrics
      
      # df_metrics %>% 
      #   mutate(
      #     across(
      #       .cols = 
      #     )
      #   )
      
      return(df_metrics)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

df_occupations.numeric %>% omega(nfactors = 1) -> dsdsds
dsdsds$omega.group
dsdsds$omega.tot
lapply(
  1:length(list_alpha)
  , 
  )
list_alpha[[]]

# Raw alpha score
lapply(
  seq_along(list_alpha)
  , function(index){
    
    if(!is.na(list_alpha[index])){
      
      list_alpha[[index]]$total$raw_alpha %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_raw_alpha

# The general rule of thumb is that a Cronbach's alpha of 0.70 and above is good
lapply(
  list_raw_alpha
  , function(cronbach){
    
    cronbach %>%
      as_tibble(.) -> cronbach
    
    colnames(cronbach) <- 'Cronbach_Alpha'
    
    cronbach %>%
      mutate(
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>%
      return(.)
    
  }) %>%
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha5

df_raw_alpha5

# Conclusion: There are 5 internally consistent factors, all of them with high or very high Alpha values.

# Factor 1 has 19 abilities loading to it.
# Factor 2 has 14 abilities loading to it.
# Factor 3 has 10 abilities loading to it.
# Factor 4 has 7 abilities loading to it.
# Factor 5 has 2 abilities loading to it.
# Factor 5 is indeed loaded.
df_loadings.long.factors %>%
  group_by(Factor) %>%
  tally(.) %>%
  mutate(prct = round(n/sum(n),2)) %>% 
  left_join(df_raw_alpha5) -> EFA5

EFA5




# TESTING --------------------------------------------------------------------
fun_factor.analysis(
  # Basic
  df_data.numeric = df_occupations.numeric
  , int_nfactors = 4
  , chr_rotation = 'promax'
  # Underloadings and crossloadings
  , remove_under_loading.items = T
  , remove_cross_loading.items = F
  , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = underloading
  , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = crossloading
  # Diagrams and tests
  , show_diagrams = T
  , show_results = T
  , stop_at_warnings = F
) -> test 

