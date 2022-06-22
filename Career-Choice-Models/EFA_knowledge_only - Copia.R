# PACKAGES -----------------------------------------------------------------
pkg <- c(
  # 'ggthemes', 'scales' #Visualization
  'readr', 'readxl','openxlsx' #Read and write utilities
  , 'tidyverse', 'labelled' #Data wrangling
  # , 'caret' #Variance filter
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
  # select(
  #   where(is.numeric)
  #   # , -ends_with('.I') #Using recommended levels
  #   , -ends_with('.L') #Using importance values
  # ) %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Knowledge only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
    # starts_with('English_Language'):starts_with('Transportation') #Knowledge only
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric

# Simpler names, if needed
# colnames(df_occupations.numeric) <- paste0('V',seq_along(df_occupations.numeric))

# Testing without problematic variables (viz. Food Production)
# df_occupations.numeric %>%
#   select(
#     -starts_with('Food_Production') #Remove Food Production
#   ) -> df_occupations.numeric

# Manual EFA I: 3 factors (Exact++, Humanities+++, [Biological]) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

# Factor Analysis
n.facts <- 3

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification1_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# Exact sciences and Humanities (Factors 1 and 2) are internally consistent
# Biological sciences are not.

# Factor 1 has 15 fields of knowledge loading to it.
# Factor 2 has 15 fields of knowledge loading to it.
# Factor 3 has only 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA II: 5 factors (Exact++, Humanities+++, Biological++, [Arts], Technical+) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
# df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification2_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# Exact, Humanities, Biological, and Technical (Factors 1, 2, 3, and 5) are internally consistent
# Arts are not.

# Factor 1 has 9 fields of knowledge loading to it.
# Factor 2 has 13 fields of knowledge loading to it.
# Factor 3 has only 2 fields of knowledge loading to it.
# Factor 4 has only 3 fields of knowledge loading to it.
# Factor 5 has 6 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA III (O.P.S): 4 factors (NT++, NF+++, [ST], [SF]) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

# Factor Analysis
n.facts <- 4

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
# df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification3_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# NT and NF (Factors 1 and 2) are internally consistent
# ST and SF (Factors 3 and 4) are not

# Factor 1 has 11 fields of knowledge loading to it.
# Factor 2 has 8 fields of knowledge loading to it.
# Factor 3 has 9 fields of knowledge loading to it.
# Factor 4 has 5 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA IV (Domain): 3 factors (Intellectual+, Social++, [Physical]) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

# Factor Analysis
n.facts <- 3

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
# df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification4_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# Intellectual and Social fields (Factors 1 and 2) are internally consistent
# Physical fields (Factors 3) are not

# Factor 1 has 14 fields of knowledge loading to it.
# Factor 2 has 10 fields of knowledge loading to it.
# Factor 3 has 9 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA V (Keirsey): 4 factors (NT++, [SP], SJ+, NF+++) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

# Factor Analysis
n.facts <- 4

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
# df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification5_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# Rationals, Guardians, and Idealists (Factors 1, 3 and 4) are internally consistent
# Guardians (Factor 3) is only slightly above the minimum threshold for consistency
# Artisans (Factor 2) are not consistent

# Factor 1 has 9 fields of knowledge loading to it.
# Factor 2 has 9 fields of knowledge loading to it.
# Factor 3 has 5 fields of knowledge loading to it.
# Factor 3 has 10 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA VI (Holland): 6 factors (R++, I+, [A], S+++, E+, C+) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
# df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification6_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# R, I, S, E, and C (Factors 1, 3, 4, 5, and 6) are internally consistent
# A (Factor 3) is not

# Factor 1 has 8 fields of knowledge loading to it.
# Factor 2 has 7 fields of knowledge loading to it.
# Factor 3 has 4 fields of knowledge loading to it.
# Factor 4 has 7 fields of knowledge loading to it.
# Factor 5 has 4 fields of knowledge loading to it.
# Factor 6 has 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA VII (Wikipedia): 5 factors (Humanities, Social Sciences, Natural Sciences, Formal Sciences, Applied Sciences) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
# df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification7_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# All sciences (Factors 2, 3, 4, and 5) are internally consistent
# Humanities (Factor 1) are not consistent

# Factor 1 has 5 fields of knowledge loading to it.
# Factor 2 has 8 fields of knowledge loading to it.
# Factor 3 has only 3 fields of knowledge loading to it.
# Factor 4 has only 2 fields of knowledge loading to it.
# Factor 5 has 15 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA VIII (VII modified): 6 factors (Arts, Humanities, Social Sciences, Natural Sciences, Formal Sciences, Applied Sciences) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification8_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# All sciences (Factors 2, 3, 4, and 5) are internally consistent
# Humanities (Factor 1) are now consistent
# But the arts (Factor 6) are not

# Factor 1 has only 3 fields of knowledge loading to it.
# Factor 2 has 8 fields of knowledge loading to it.
# Factor 3 has only 3 fields of knowledge loading to it.
# Factor 4 has only 2 fields of knowledge loading to it.
# Factor 5 has 15 fields of knowledge loading to it.
# Factor 6 has only 2 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA IX: 4 factors (Make Stuff, Know Stuff, Help People, Manage People) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

# Factor Analysis
n.facts <- 4

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification9_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# All factors are internally consistent

# Factor 1 has 10 fields of knowledge loading to it.
# Factor 2 has 12 fields of knowledge loading to it.
# Factor 3 has 8 fields of knowledge loading to it.
# Factor 4 has only 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA X (IX modified): 4 factors (Doing, Knowing, Helping, Administering) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

# Factor Analysis
n.facts <- 4

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification10_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# More consistent than the original version (EFA IX)

# Factor 1 has 10 fields of knowledge loading to it.
# Factor 2 has 11 fields of knowledge loading to it.
# Factor 3 has 5 fields of knowledge loading to it.
# Factor 4 has 7 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA XI (Holland modified): 5 factors (RA, IA, S, E, C) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification11_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# R, I, S, E, and C (Factors 1, 3, 4, 5, and 6) are internally consistent
# A (Factor 3) is not

# Factor 1 has 8 fields of knowledge loading to it.
# Factor 2 has 7 fields of knowledge loading to it.
# Factor 3 has 4 fields of knowledge loading to it.
# Factor 4 has 7 fields of knowledge loading to it.
# Factor 5 has 4 fields of knowledge loading to it.
# Factor 6 has 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

# Manual EFA XII (Holland modified): 4 factors (RA, IA, S, EC) --------------------------------------------------------
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

# Kaiser criterion suggests 7 factors are sufficient

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa') -> pa_analysis

# Parallel analysis suggests 8 factors are sufficient

# Very simple structure criterion (VSS)
df_occupations.numeric %>%
  VSS(n = 2 * pa_analysis$nfact) %>%
  summary()

# VSS criterion 1 suggests 2 factors are sufficient
# VSS criterion 2 suggests 3 factors are sufficient
# Velicer MAP criterion suggests 9 factors are sufficient

# Factor Analysis
n.facts <- 4

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

# Internal consistency test: Cronbach's Alpha
# Do the factors form a coherent group in and of themselves?  

# Matching items to factors
df_manually_assign <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=26093558&single=true&output=csv')

df_loadings.long %>% 
  right_join(
    df_manually_assign
    , by = c('Metric' = 'Subspecies')
  ) -> df_loadings.long

df_loadings.long %>% 
  filter(
    Factor == Classification12_Factor
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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>% 
      return(.)
    
  }) %>% 
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion:

# R, I, S, E, and C (Factors 1, 3, 4, 5, and 6) are internally consistent
# A (Factor 3) is not

# Factor 1 has 8 fields of knowledge loading to it.
# Factor 2 has 7 fields of knowledge loading to it.
# Factor 3 has 4 fields of knowledge loading to it.
# Factor 4 has 7 fields of knowledge loading to it.
# Factor 5 has 4 fields of knowledge loading to it.
# Factor 6 has 3 fields of knowledge loading to it.
df_loadings.long.factors %>% 
  group_by(Factor) %>% 
  tally(.) %>% 
  mutate(prct = round(n/sum(n),2))

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

# min crossloadings
df_loadings.long.factors %>% 
  group_by(Metric) %>%
  mutate(
    Crossloadings.Abs.Sum = sum(abs(Loading)) - Loading.Max #Sum of crossloadings
  ) %>% 
  # Pick factors with max loadings and min crossloadings
  mutate(
    Category = 'Knowledge'
    , Loading_Crossloadings.Diff = Loading - Crossloadings.Abs.Sum
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
    , Classification9_Factor
    , Classification9_Name
  ) -> df_loadings.items.know

df_loadings.items.know


# TESTING BEST ITEMS: EFA III STILL CONSISTENT? NO! --------------------------------------------------------
# # Exploratory Factor Analysis (EFA)

# 'Not in' operator
# `%!in%` <- Negate(`%in%`)

df_occupations.numeric %>%
  select(
    df_loadings.items.know$Metric
  ) -> df_occupations.numeric

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

sum(ev$values > 1) 

# Scree plot
df_occupations.numeric %>%
  scree(pc = F) #Factor analysis => pc = False 

# Parallel analysis
df_occupations.numeric %>%
  fa.parallel(fa = 'fa')

# Parallel analysis suggests 5 factors are sufficient
# Testing whether 4 or less factors are sufficient

# Factor Analysis
n.facts <- 4

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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
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
















