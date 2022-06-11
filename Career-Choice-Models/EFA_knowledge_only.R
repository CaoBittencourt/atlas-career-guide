# PACKAGES -----------------------------------------------------------------
pkg <- c(
  # 'ggthemes', 'scales' #Visualization
  'readr', 'readxl','openxlsx' #Read and write utilities
  , 'tidyverse', 'labelled' #Data wrangling
  , 'psych' #Factor analysis
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models')

# DATA --------------------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Labels character vector
chr_labels <- scan(
  url('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=1223197022&single=true&output=csv')
  , sep=','
  , what = ''
  , quiet = T
)

# Exploratory analyses
df_occupations %>% glimpse()
df_occupations %>% class()
df_occupations %>% head()

chr_labels %>% glimpse()
chr_labels %>% class()
chr_labels %>% head()

ncol(df_occupations) == length(chr_labels) 

# Apply labels
df_occupations %>%
  labelled::set_variable_labels(
    .labels = chr_labels
  ) -> df_occupations

# Only numeric variables
df_occupations %>%
  select(
    where(is.numeric)
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance values
  ) %>%
  select(
    starts_with('English_Language'):starts_with('Transportation') #Abilities only
  ) -> df_occupations.numeric

# Simpler names, if needed
# colnames(df_occupations.numeric) <- paste0('V',seq_along(df_occupations.numeric))

# Testing without problematic variables (viz. Food Production)

df_occupations.numeric %>%
  select(
    -starts_with('Food_Production') #Remove Food Production
  ) -> df_occupations.numeric

# EFA I: 8 factors => 7 factors? --------------------------------------------------------
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
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 8 factors are sufficient

# Factor Analysis
n.facts <- 8

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')
# fit <- factanal(df_occupations.numeric, n.facts, rotation = 'varimax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat uncorrelated to one another.
# => Orthogonal rotation could be appropriate.
# => Try Oblique rotation factors first

# Plot factors
fa.diagram(fit$loadings)

# On first sight, all factors seem relevant.

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# Factors 6 and 8 don't have at least 3 variables loading to them.
# Factors 6, 7, and 8 don't have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()


# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
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

# Interestingly, although all factors are statistically relevant,
# Factor 3, 6, 7 and 8 have very few items maximally loading to them.
# This means they are likely secondary factors, if useful at all.
# The exclusion of these factors should be tested later on.


# Separate factors into individual data frames
lapply(
  str_sort(str_sort(unique(df_loadings.long.factors$Factor))) 
  , function(factors){
    
    df_loadings.long.factors %>%
      filter(
        Factor == factors
      ) %>%
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, 3 of them with high Alpha values.
# 2 other factors are not internally consistent. And the last factor is composed of a single variable.
# It is, thefore, necessary to reevaluate the number of factors.

# Factor 1 has 7 fields of knowledge loading to it.
# Factor 2 has 4 fields of knowledge loading to it.
# Factor 3 has 6 fields of knowledge loading to it.
# Factor 4 has 6 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
# Factor 6 has 3 fields of knowledge loading to it.
# Factor 7 has 3 fields of knowledge loading to it.
# Factor 8 has 1 field of knowledge loading to it.
# Factor 8 is indeed underloaded.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)

# EFA II: 8 factors => 7 factors! --------------------------------------------------------
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
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 8 factors are sufficient
# Testing whether 7 or less factors are sufficient

# Factor Analysis
n.facts <- 7

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat uncorrelated.
# => Orthogonal rotation could be appropriate.
# => Test Oblique rotation of factors first.

# Plot factors
fa.diagram(fit$loadings)

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# Factors 6 and 7 do not have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
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
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 6 internally consistent factors, all of them with high Alpha values.
# The last factor does not have a sufficiently high Alpha value.
# It should be reconsidered whether this is the appropriate number of factors to use.

# Factor 1 has 7 fields of knowledge loading to it.
# Factor 2 has 6 fields of knowledge loading to it.
# Factor 3 has 6 fields of knowledge loading to it.
# Factor 4 has 5 fields of knowledge loading to it.
# Factor 5 has 4 fields of knowledge loading to it.
# Factor 6 has 2 fields of knowledge loading to it.
# Factor 7 has 3 fields of knowledge loading to it.
# Factor 6 is somewhat underloaded.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)


# EFA III: 7 factors => 6 factors? --------------------------------------------------------
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
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 8 factors are sufficient
# Testing whether 6 or less factors are sufficient

# Factor Analysis
n.facts <- 6

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat correlated.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# Factors 5 and 6 do not have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Diff.Significant
  )) + 
  geom_tile()

# No significant difference between factor loadings for each item.
# Conclusion: No significant crossloadings. Each item maps considerably to only one factor.


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
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, most of them with high Alpha values.
# Factor 5 is not internally consistent.

# Factor 1 has 8 fields of knowledge loading to it.
# Factor 2 has 8 fields of knowledge loading to it.
# Factor 3 has 5 fields of knowledge loading to it.
# Factor 4 has 6 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)


# EFA III: 6 factors => 5 factors? --------------------------------------------------------
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
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 8 factors are sufficient
# Testing whether 6 or less factors are sufficient

# Factor Analysis
n.facts <- 5

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat correlated.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# Factors 5 and 6 do not have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Diff.Significant
  )) + 
  geom_tile()

# No significant difference between factor loadings for each item.
# Conclusion: No significant crossloadings. Each item maps considerably to only one factor.


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
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, most of them with high Alpha values.
# Factor 5 is not internally consistent.

# Factor 1 has 8 fields of knowledge loading to it.
# Factor 2 has 8 fields of knowledge loading to it.
# Factor 3 has 5 fields of knowledge loading to it.
# Factor 4 has 6 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)


# EFA IV: Manually assign 5 factors --------------------------------------------------------
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
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 8 factors are sufficient
# Testing whether 6 or less factors are sufficient

# Factor Analysis
n.facts <- 5

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat correlated.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# Factors 5 and 6 do not have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Diff.Significant
  )) + 
  geom_tile()

# No significant difference between factor loadings for each item.
# Conclusion: No significant crossloadings. Each item maps considerably to only one factor.


# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
# Since there are relatively few crossloadings, it is adequate to  
# use maximum loading as the criteria for factor matching.
df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=777169973&single=true&output=csv')

# df_loadings.long %>% 
#   filter(
#     Loading == Loading.Max
#   ) -> df_loadings.long.factors

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Manually_Assigned_Factor
  ) -> df_loadings.long.factors


# Separate factors into individual data frames
lapply(
  str_sort(unique(df_loadings.long.factors$Factor)) 
  , function(factors){
    
    df_loadings.long.factors %>%
      filter(
        Factor == factors
      ) %>%
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, all of them with high Alpha values.

# Factor 1 has 6 fields of knowledge loading to it.
# Factor 2 has 10 fields of knowledge loading to it.
# Factor 3 has 6 fields of knowledge loading to it.
# Factor 4 has 4 fields of knowledge loading to it.
# Factor 5 has 7 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)



# EFA V: 10 factors? No. --------------------------------------------------------
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
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 8 factors are sufficient
# Testing whether 6 or less factors are sufficient

# Factor Analysis
n.facts <- 10

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat correlated.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# Factors 5 and 6 do not have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Diff.Significant
  )) + 
  geom_tile()

# No significant difference between factor loadings for each item.
# Conclusion: No significant crossloadings. Each item maps considerably to only one factor.


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
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, most of them with high Alpha values.
# Factor 5 is not internally consistent.

# Factor 1 has 8 fields of knowledge loading to it.
# Factor 2 has 8 fields of knowledge loading to it.
# Factor 3 has 5 fields of knowledge loading to it.
# Factor 4 has 6 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)


# EFA VI: 9 factors? No. --------------------------------------------------------
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
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 8 factors are sufficient
# Testing whether 6 or less factors are sufficient

# Factor Analysis
n.facts <- 9

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat correlated.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# Factors 5 and 6 do not have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Diff.Significant
  )) + 
  geom_tile()

# No significant difference between factor loadings for each item.
# Conclusion: No significant crossloadings. Each item maps considerably to only one factor.


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
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, most of them with high Alpha values.
# Factor 5 is not internally consistent.

# Factor 1 has 8 fields of knowledge loading to it.
# Factor 2 has 8 fields of knowledge loading to it.
# Factor 3 has 5 fields of knowledge loading to it.
# Factor 4 has 6 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
# Factor 5 has 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)


# CONCLUSION --------------------------------------------------------------
# There are 4 internally consistent factors, all of them with very high Alpha values.
# This means that all abilities can be consistently reduced to these 4 factors.
# Alpha values are only slightly lower with 4 factors rather than 5.
# However, internal consistency is still basically the same (very high).

# Factor 1 has 22 fields of knowledge loading to it.
# Factor 2 has 14 fields of knowledge loading to it.
# Factor 3 has 10 fields of knowledge loading to it.
# Factor 4 has 6 fields of knowledge loading to it.

# These numbers seem to be satisfactory.
# PICKING BEST ITEMS (NOT USING IRT) ---------------------------------------------
# Number of items to pick
n.items <- 3
# n.items <- 2

# min crossloadings
df_loadings.long %>% 
  group_by(Metric) %>%
  mutate(
    Crossloadings.Abs.Sum = sum(abs(Loading)) - Loading.Max #Sum of crossloadings
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
  top_n(n.items,Loading_Crossloadings.Diff) %>%
  select(
    Metric
    , Factor
    , Loading
    , Loading_Crossloadings.Diff
  ) -> df_loadings.items


df_loadings.items


# TESTING BEST ITEMS: EFA III STILL CONSISTENT? NO! --------------------------------------------------------
# # Exploratory Factor Analysis (EFA)

# 'Not in' operator
# `%!in%` <- Negate(`%in%`)

df_occupations.numeric %>%
  select(
    df_loadings.items$Metric
  ) -> df_occupations.numeric

# K-M-O factor adequacy test
# * 0.00 to 0.49 unacceptable
# * 0.50 to 0.59 miserable
# * 0.60 to 0.69 mediocre
# * 0.70 to 0.79 middling
# * 0.80 to 0.89 meritorious
# => 0.90 to 1.00 marvelous
df_occupations.numeric <- df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50]

round(KMO(df_occupations.numeric)$MSA, 2)

# Null hypothesis: the variables in the data set are essentially uncorrelated.
# Rejected the null hypothesis. Therefore, the data may be grouped into factors.
cortest.bartlett(df_occupations.numeric)

# Determine Number of Factors to Extract
df_occupations.numeric %>%
  cor() %>%
  eigen() -> ev

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 5 factors are sufficient
# Testing whether 4 or less factors are sufficient

# Factor Analysis
n.facts <- 5

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat (but not entirely) uncorrelated.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# Limiting the analysis to only 4 factors, Factor 4 now does seem to be irrelevant.
# This is confirmed by further analyses.

# Evaluation
# Do variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficient <- abs(fit$loadings) > 0.4

# Variables sufficiently load to each factor
colSums(fct_load.sufficient)
nrow(fct_load.sufficient)
# Variables that sufficiently load to each factor (%)
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2)
# All factors have variables that sufficiently load to them
round(colSums(fct_load.sufficient)/nrow(fct_load.sufficient), 2) > 0

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficient) >= 3
colSums(fct_load.sufficient) >= 4

# All factors have at least 4 variables loading to them.

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings[,] %>% 
  as.matrix() %>% 
  as_tibble(rownames = 'Metric') %>% 
  mutate(Metric = factor(Metric)) -> df_loadings

# Max loading vs other loadings
df_loadings %>%
  pivot_longer(#Convert to long data format
    cols = starts_with('Factor')
    , names_to = 'Factor'
    , values_to = 'Loading'
  ) %>% 
  group_by(Metric) %>%
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
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
    , fill = Loading == Loading.Max
  )) + 
  geom_tile()

# Loadings Difference Heatmap
df_loadings.long %>%
  ggplot(aes(
    x = fct_inorder(Factor)
    , y = fct_rev(fct_inorder(Metric))
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
    , y = fct_rev(fct_inorder(Metric))
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


# Interestingly, although all factors are statistically relevant,
# Factor 4 has no item maximally loading to it.
# This means this is likely a secondary factors, if useful at all.
# Therefore, unless we manually assign items to Factor 4,
# the inclusion criteria (i.e. max loadings) leads us to disconsider it.
# The proposed test to exclude Factor 4 confirmed the previous hypothesis.
# We conclude this factor is not necessary for the present purposes.

# Separate factors into individual data frames
lapply(
  str_sort(unique(df_loadings.long.factors$Factor)) 
  , function(factors){
    
    df_loadings.long.factors %>%
      filter(
        Factor == factors
      ) %>%
      pull(Metric) %>%
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
        alpha(.) %>%
        return(.)
      
    }
    else{
      
      return(NA)
      
    }
    
  }) -> list_alpha

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
        Good.Cronbach_Alpha = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 3 internally consistent factors, all of them with very high Alpha values.
# This means that all skills can be consistently reduced to these 3 factors.

# Factor 1 has 17 skills loading to it.
# Factor 2 has 10 skills loading to it.
# Factor 3 has 8 skills loading to it.
# Therefore, no factor is underloaded.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)

