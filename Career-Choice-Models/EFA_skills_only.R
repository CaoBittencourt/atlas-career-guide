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
  # select(
  #   where(is.numeric)
  #   , -ends_with('.I') #Using recommended levels
  #   # , -ends_with('.L') #Using importance levels
  # ) %>%
  select(
    where(function(x){str_detect(attributes(x)$label, '_Skills.')}) #Skills only
      , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
    # starts_with('Active_Listening'):starts_with('Operations_Analysis') #Skills only
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric

# Simpler names, if needed
# colnames(df_occupations.numeric) <- paste0('V',seq_along(df_occupations.numeric))


# # EFA I: 5 factors => 4 factors --------------------------------------------------------
# # # Exploratory Factor Analysis (EFA)
# 
# # 'Not in' operator
# # `%!in%` <- Negate(`%in%`)
# 
# # K-M-O factor adequacy test
# # * 0.00 to 0.49 unacceptable
# # * 0.50 to 0.59 miserable
# # * 0.60 to 0.69 mediocre
# # * 0.70 to 0.79 middling
# # * 0.80 to 0.89 meritorious
# # => 0.90 to 1.00 marvelous
# round(KMO(df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50])$MSA, 2)
# 
# # Null hypothesis: the variables in the data set are essentially uncorrelated.
# # Rejected the null hypothesis. Therefore, the data may be grouped into factors.
# cortest.bartlett(df_occupations.numeric)
# 
# # Determine Number of Factors to Extract
# df_occupations.numeric %>%
#   cor() %>%
#   eigen() -> ev
# 
# sum(ev$values >= 1)
# 
# # Kaiser criterion suggests 4 factors are sufficient
# 
# # Scree plot
# df_occupations.numeric %>%
#   scree(pc = F) #Factor analysis => pc = False
# 
# # Parallel analysis
# df_occupations.numeric %>%
#   fa.parallel(fa = 'fa') -> pa_analysis
# 
# # Parallel analysis suggests 5 factors are sufficient
# 
# # Very simple structure criterion (VSS)
# df_occupations.numeric %>%
#   VSS(n = 2 * pa_analysis$nfact) %>%
#   summary()
# 
# # VSS criterion 1 suggests 2 factors are sufficient
# # VSS criterion 2 suggests 2 factors are sufficient
# # Velicer MAP criterion suggests 9 factors are sufficient
# 
# # Factor Analysis
# n.facts <- 5
# 
# # Initially, we suppose that factors could be correlated to one another.
# # Therefore, we apply an oblique rotation to the factors.
# fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')
# 
# print(fit, digits = 2, cutoff = 0.3, sort = T)
# 
# # Factors are somewhat (but not entirely) uncorrelated.
# # => Orthogonal rotation does not seem appropriate.
# # => Keep Oblique rotation factors
# 
# # Plot factors
# fa.diagram(fit$loadings)
# 
# # On first sight, Factors 4 and 5 seem disposable.
# # This is confirmed by further analyses.
# 
# # Evaluation
# # Do variables load to the factors sufficiently?
# # |factor loading| > 0.4
# (abs(fit$loadings) > 0.4) %>% 
#   as_tibble(rownames = 'Metric') %>% 
#   mutate(
#     Load.Sufficient = rowSums(select(.,starts_with('Factor')))
#     , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#   ) -> df_loadings.sufficient
# 
# # Percentage of variables that load significantly to at least one factor
# df_loadings.sufficient %>%
#   summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))
# 
# # Variables that do not load significantly to any factor
# df_loadings.sufficient %>% 
#   filter(Load.Sufficient.Bin == 0) %>% 
#   pull(Metric)
# 
# # Do all factors have at least three - or, better, four - or more variables loading onto them?
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 3
# 
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 4
# 
# # Factors 4 and 5 don't have at least 3 variables loading to them.
# 
# # Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
# fit$loadings[,] %>%
#   as.matrix() %>%
#   as_tibble(rownames = 'Metric') %>%
#   mutate(Metric = factor(Metric)) -> df_loadings
# 
# # Max loading vs other loadings
# df_loadings %>%
#   pivot_longer(#Convert to long data format
#     cols = starts_with('Factor')
#     , names_to = 'Factor'
#     , values_to = 'Loading'
#   ) %>%
#   group_by(Metric) %>%
#   mutate(
#     Loading.Max = max(Loading) #Max loading per variable
#     , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
#     , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
#       Loading.Diff.Abs == 0
#       , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
#       , no = Loading.Diff.Abs <= 0.05
#     )
#   ) -> df_loadings.long
# 
# # Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Max Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading == Loading.Max
#   )) +
#   geom_tile()
# 
# # Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading.Diff.Abs
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Significant Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Diff.Significant
#   )) +
#   geom_tile()
# 
# # Almost no significant difference between factor loadings for each item.
# # Conclusion: Very few crossloadings. Each item maps considerably to only one factor.
# # P.S.: The only two items that crossload are very generic in nature (viz. Critical Thinking, Judgment and Decision)
# 
# 
# # Internal consistency test: Cronbach's Alpha
# # Do the factors form a coherent group in and of themselves?
# 
# # Matching items to factors
# # Since there are relatively few crossloadings, it is adequate to
# # use maximum loading as the criteria for factor matching.
# df_loadings.long %>%
#   filter(
#     Loading == Loading.Max
#   ) -> df_loadings.long.factors
# 
# # Interestingly, although all factors are statistically relevant,
# # Factor 5 has no item maximally loading to it, and Factor 4 has only two.
# # This means they are likely secondary factors, if useful at all.
# # Therefore, unless we manually assign items to Factor 5,
# # the inclusion criteria (i.e. max loadings) leads us to disconsider it.
# # The exclusion of Factor 4 should be tested later on.
# 
# # Separate factors into individual data frames
# lapply(
#   str_sort(unique(df_loadings.long.factors$Factor))
#   , function(factors){
# 
#     df_loadings.long.factors %>%
#       filter(
#         Factor == factors
#       ) %>%
#       pull(Metric) %>%
#       factor(.) %>%
#       return(.)
# 
#   }) -> list_chr_loadings.long.factors
# 
# # Apply Alpha test to each subset of variables
# lapply(
#   list_chr_loadings.long.factors
#   , function(factors){
# 
#     df_occupations.numeric %>%
#       select(factors) -> df.temp #Select only the variables that match to each factor
# 
#     if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
# 
#       df.temp %>%
#         alpha(.) %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_alpha
# 
# # Raw alpha score
# lapply(
#   seq_along(list_alpha)
#   , function(index){
# 
#     if(!is.na(list_alpha[index])){
# 
#       list_alpha[[index]]$total$raw_alpha %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_raw_alpha
# 
# # The general rule of thumb is that a Cronbach's alpha of 0.70 and above is good
# lapply(
#   list_raw_alpha
#   , function(cronbach){
# 
#     cronbach %>%
#       as_tibble(.) -> cronbach
# 
#     colnames(cronbach) <- 'Cronbach_Alpha'
# 
#     cronbach %>%
#       mutate(
#         Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
#       ) %>%
#       return(.)
# 
#   }) %>%
#   bind_rows(.) %>%
#   mutate(
#     Factor = paste0('Factor', row_number())
#   ) -> df_raw_alpha
# 
# df_raw_alpha
# 
# # Conclusion: There are 4 internally consistent factors, all of them with very high Alpha values.
# # This means that all skills can be consistently reduced to these 4 factors.
# # P.S.: Factor 4 is composed of only 2 items. It should be considered whether or not this factor is truly necessary.
# 
# # Factor 1 has 13 skills loading to it.
# # Factor 2 has 8 skills loading to it.
# # Factor 3 has 12 skills loading to it.
# # Factor 4 has 2 skills loading to it.
# # Factor 4 is indeed underloaded.
# df_loadings.long.factors %>%
#   group_by(Factor) %>%
#   tally(.) %>%
#   mutate(prct = round(n/sum(n),2))
# 
# # Other visualizations (test later)
# # install.packages('FactoMineR')
# # library(FactoMineR)
# # result <- PCA(df_occupations.numeric)

# # EFA II: 4 factors => 3 factors? --------------------------------------------------------
# # # Exploratory Factor Analysis (EFA)
# 
# # 'Not in' operator
# # `%!in%` <- Negate(`%in%`)
# 
# # K-M-O factor adequacy test
# # * 0.00 to 0.49 unacceptable
# # * 0.50 to 0.59 miserable
# # * 0.60 to 0.69 mediocre
# # * 0.70 to 0.79 middling
# # * 0.80 to 0.89 meritorious
# # => 0.90 to 1.00 marvelous
# round(KMO(df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50])$MSA, 2)
# 
# # Null hypothesis: the variables in the data set are essentially uncorrelated.
# # Rejected the null hypothesis. Therefore, the data may be grouped into factors.
# cortest.bartlett(df_occupations.numeric)
# 
# # Determine Number of Factors to Extract
# df_occupations.numeric %>%
#   cor() %>%
#   eigen() -> ev
# 
# sum(ev$values >= 1)
# 
# # Kaiser criterion suggests 4 factors are sufficient
# 
# # Scree plot
# df_occupations.numeric %>%
#   scree(pc = F) #Factor analysis => pc = False
# 
# # Parallel analysis
# df_occupations.numeric %>%
#   fa.parallel(fa = 'fa') -> pa_analysis
# 
# # Parallel analysis suggests 5 factors are sufficient
# 
# # Very simple structure criterion (VSS)
# df_occupations.numeric %>%
#   VSS(n = 2 * pa_analysis$nfact) %>%
#   summary()
# 
# # VSS criterion 1 suggests 2 factors are sufficient
# # VSS criterion 2 suggests 2 factors are sufficient
# # Velicer MAP criterion suggests 9 factors are sufficient
# 
# # Testing whether 4 or less factors are sufficient
# 
# # Factor Analysis
# n.facts <- 4
# 
# # Initially, we suppose that factors could be correlated to one another.
# # Therefore, we apply an oblique rotation to the factors.
# fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')
# 
# print(fit, digits = 2, cutoff = 0.3, sort = T)
# 
# # Factors are somewhat (but not entirely) uncorrelated.
# # => Orthogonal rotation does not seem appropriate.
# # => Keep Oblique rotation factors
# 
# # Plot factors
# fa.diagram(fit$loadings)
# 
# # Limiting the analysis to only 4 factors, Factor 4 now does seem to be irrelevant.
# # This is confirmed by further analyses.
# 
# # Evaluation
# # Do variables load to the factors sufficiently?
# # |factor loading| > 0.4
# (abs(fit$loadings) > 0.4) %>% 
#   as_tibble(rownames = 'Metric') %>% 
#   mutate(
#     Load.Sufficient = rowSums(select(.,starts_with('Factor')))
#     , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#   ) -> df_loadings.sufficient
# 
# # Percentage of variables that load significantly to at least one factor
# df_loadings.sufficient %>%
#   summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))
# 
# # Variables that do not load significantly to any factor
# df_loadings.sufficient %>% 
#   filter(Load.Sufficient.Bin == 0) %>% 
#   pull(Metric)
# 
# # Do all factors have at least three - or, better, four - or more variables loading onto them?
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 3
# 
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 4
# 
# # All factors have at least 4 variables loading to them.
# 
# # Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
# fit$loadings[,] %>%
#   as.matrix() %>%
#   as_tibble(rownames = 'Metric') %>%
#   mutate(Metric = factor(Metric)) -> df_loadings
# 
# # Max loading vs other loadings
# df_loadings %>%
#   pivot_longer(#Convert to long data format
#     cols = starts_with('Factor')
#     , names_to = 'Factor'
#     , values_to = 'Loading'
#   ) %>%
#   group_by(Metric) %>%
#   mutate(
#     Loading.Max = max(Loading) #Max loading per variable
#     , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
#     , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
#       Loading.Diff.Abs == 0
#       , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
#       , no = Loading.Diff.Abs <= 0.05
#     )
#   ) -> df_loadings.long
# 
# # Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Max Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading == Loading.Max
#   )) +
#   geom_tile()
# 
# # Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading.Diff.Abs
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Significant Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Diff.Significant
#   )) +
#   geom_tile()
# 
# # Almost no significant difference between factor loadings for each item.
# # Conclusion: Very few crossloadings. Each item maps considerably to only one factor.
# 
# 
# # Internal consistency test: Cronbach's Alpha
# # Do the factors form a coherent group in and of themselves?
# 
# # Matching items to factors
# # Since there are relatively few crossloadings, it is adequate to
# # use maximum loading as the criteria for factor matching.
# df_loadings.long %>%
#   filter(
#     Loading == Loading.Max
#   ) -> df_loadings.long.factors
# 
# 
# # Interestingly, although all factors are statistically relevant,
# # Factor 4 has no item maximally loading to it.
# # This means this is likely a secondary factors, if useful at all.
# # Therefore, unless we manually assign items to Factor 4,
# # the inclusion criteria (i.e. max loadings) leads us to disconsider it.
# # The proposed test to exclude Factor 4 confirmed the previous hypothesis.
# # We conclude this factor is not necessary for the present purposes.
# 
# # Separate factors into individual data frames
# lapply(
#   str_sort(unique(df_loadings.long.factors$Factor))
#   , function(factors){
# 
#     df_loadings.long.factors %>%
#       filter(
#         Factor == factors
#       ) %>%
#       pull(Metric) %>%
#       factor(.) %>%
#       return(.)
# 
#   }) -> list_chr_loadings.long.factors
# 
# # Apply Alpha test to each subset of variables
# lapply(
#   list_chr_loadings.long.factors
#   , function(factors){
# 
#     df_occupations.numeric %>%
#       select(factors) -> df.temp #Select only the variables that match to each factor
# 
#     if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
# 
#       df.temp %>%
#         alpha(.) %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_alpha
# 
# # Raw alpha score
# lapply(
#   seq_along(list_alpha)
#   , function(index){
# 
#     if(!is.na(list_alpha[index])){
# 
#       list_alpha[[index]]$total$raw_alpha %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_raw_alpha
# 
# # The general rule of thumb is that a Cronbach's alpha of 0.70 and above is good
# lapply(
#   list_raw_alpha
#   , function(cronbach){
# 
#     cronbach %>%
#       as_tibble(.) -> cronbach
# 
#     colnames(cronbach) <- 'Cronbach_Alpha'
# 
#     cronbach %>%
#       mutate(
#         Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
#       ) %>%
#       return(.)
# 
#   }) %>%
#   bind_rows(.) %>%
#   mutate(
#     Factor = paste0('Factor', row_number())
#   ) -> df_raw_alpha
# 
# df_raw_alpha
# 
# # Conclusion: There are 3 internally consistent factors, all of them with very high Alpha values.
# # This means that all skills can be consistently reduced to these 3 factors.
# 
# # Factor 1 has 17 skills loading to it.
# # Factor 2 has 10 skills loading to it.
# # Factor 3 has 8 skills loading to it.
# # Therefore, no factor is underloaded.
# df_loadings.long.factors %>%
#   group_by(Factor) %>%
#   tally(.) %>%
#   mutate(prct = round(n/sum(n),2))
# 
# # Other visualizations (test later)
# # install.packages('FactoMineR')
# # library(FactoMineR)
# # result <- PCA(df_occupations.numeric)
# 

# # EFA III: 4 factors => 3 factors! --------------------------------------------------------
# # # Exploratory Factor Analysis (EFA)
# 
# # 'Not in' operator
# # `%!in%` <- Negate(`%in%`)
# 
# # K-M-O factor adequacy test
# # * 0.00 to 0.49 unacceptable
# # * 0.50 to 0.59 miserable
# # * 0.60 to 0.69 mediocre
# # * 0.70 to 0.79 middling
# # * 0.80 to 0.89 meritorious
# # => 0.90 to 1.00 marvelous
# round(KMO(df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50])$MSA, 2)
# 
# # Null hypothesis: the variables in the data set are essentially uncorrelated.
# # Rejected the null hypothesis. Therefore, the data may be grouped into factors.
# cortest.bartlett(df_occupations.numeric)
# 
# # Determine Number of Factors to Extract
# df_occupations.numeric %>%
#   cor() %>%
#   eigen() -> ev
# 
# sum(ev$values >= 1)
# 
# # Kaiser criterion suggests 4 factors are sufficient
# 
# # Scree plot
# df_occupations.numeric %>%
#   scree(pc = F) #Factor analysis => pc = False
# 
# # Parallel analysis
# df_occupations.numeric %>%
#   fa.parallel(fa = 'fa') -> pa_analysis
# 
# # Parallel analysis suggests 5 factors are sufficient
# 
# # Very simple structure criterion (VSS)
# df_occupations.numeric %>%
#   VSS(n = 2 * pa_analysis$nfact) %>%
#   summary()
# 
# # VSS criterion 1 suggests 2 factors are sufficient
# # VSS criterion 2 suggests 2 factors are sufficient
# # Velice MAP criterion suggests 9 factors are sufficient
# 
# # Testing whether 4 or less factors are sufficient
# 
# # Factor Analysis
# n.facts <- 3
# 
# # Initially, we suppose that factors could be correlated to one another.
# # Therefore, we apply an oblique rotation to the factors.
# fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')
# 
# print(fit, digits = 2, cutoff = 0.3, sort = T)
# 
# # Factors are somewhat (but not entirely) uncorrelated.
# # => Orthogonal rotation does not seem appropriate.
# # => Keep Oblique rotation factors
# 
# # Plot factors
# fa.diagram(fit$loadings)
# 
# # Limiting the analysis to only 4 factors, Factor 4 now does seem to be irrelevant.
# # This is confirmed by further analyses.
# 
# # Evaluation
# # Do variables load to the factors sufficiently?
# # |factor loading| > 0.4
# (abs(fit$loadings) > 0.4) %>% 
#   as_tibble(rownames = 'Metric') %>% 
#   mutate(
#     Load.Sufficient = rowSums(select(.,starts_with('Factor')))
#     , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#   ) -> df_loadings.sufficient
# 
# # Percentage of variables that load significantly to at least one factor
# df_loadings.sufficient %>%
#   summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))
# 
# # Variables that do not load significantly to any factor
# df_loadings.sufficient %>% 
#   filter(Load.Sufficient.Bin == 0) %>% 
#   pull(Metric)
# 
# # Do all factors have at least three - or, better, four - or more variables loading onto them?
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 3
# 
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 4
# 
# # All factors have at least 4 variables loading to them.
# 
# # Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
# fit$loadings[,] %>%
#   as.matrix() %>%
#   as_tibble(rownames = 'Metric') %>%
#   mutate(Metric = factor(Metric)) -> df_loadings
# 
# # Max loading vs other loadings
# df_loadings %>%
#   pivot_longer(#Convert to long data format
#     cols = starts_with('Factor')
#     , names_to = 'Factor'
#     , values_to = 'Loading'
#   ) %>%
#   group_by(Metric) %>%
#   mutate(
#     Loading.Max = max(Loading) #Max loading per variable
#     , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
#     , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
#       Loading.Diff.Abs == 0
#       , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
#       , no = Loading.Diff.Abs <= 0.05
#     )
#   ) -> df_loadings.long
# 
# # Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Max Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading == Loading.Max
#   )) +
#   geom_tile()
# 
# # Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading.Diff.Abs
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Significant Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Diff.Significant
#   )) +
#   geom_tile()
# 
# # Almost no significant difference between factor loadings for each item.
# # Conclusion: Very few crossloadings. Each item maps considerably to only one factor.
# 
# 
# # Internal consistency test: Cronbach's Alpha
# # Do the factors form a coherent group in and of themselves?
# 
# # Matching items to factors
# # Since there are relatively few crossloadings, it is adequate to
# # use maximum loading as the criteria for factor matching.
# df_loadings.long %>%
#   filter(
#     Loading == Loading.Max
#   ) -> df_loadings.long.factors
# 
# 
# # Interestingly, although all factors are statistically relevant,
# # Factor 4 has no item maximally loading to it.
# # This means this is likely a secondary factors, if useful at all.
# # Therefore, unless we manually assign items to Factor 4,
# # the inclusion criteria (i.e. max loadings) leads us to disconsider it.
# # The proposed test to exclude Factor 4 confirmed the previous hypothesis.
# # We conclude this factor is not necessary for the present purposes.
# 
# # Separate factors into individual data frames
# lapply(
#   str_sort(unique(df_loadings.long.factors$Factor))
#   , function(factors){
# 
#     df_loadings.long.factors %>%
#       filter(
#         Factor == factors
#       ) %>%
#       pull(Metric) %>%
#       factor(.) %>%
#       return(.)
# 
#   }) -> list_chr_loadings.long.factors
# 
# # Apply Alpha test to each subset of variables
# lapply(
#   list_chr_loadings.long.factors
#   , function(factors){
# 
#     df_occupations.numeric %>%
#       select(factors) -> df.temp #Select only the variables that match to each factor
# 
#     if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
# 
#       df.temp %>%
#         alpha(.) %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_alpha
# 
# # Raw alpha score
# lapply(
#   seq_along(list_alpha)
#   , function(index){
# 
#     if(!is.na(list_alpha[index])){
# 
#       list_alpha[[index]]$total$raw_alpha %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_raw_alpha
# 
# # The general rule of thumb is that a Cronbach's alpha of 0.70 and above is good
# lapply(
#   list_raw_alpha
#   , function(cronbach){
# 
#     cronbach %>%
#       as_tibble(.) -> cronbach
# 
#     colnames(cronbach) <- 'Cronbach_Alpha'
# 
#     cronbach %>%
#       mutate(
#         Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
#       ) %>%
#       return(.)
# 
#   }) %>%
#   bind_rows(.) %>%
#   mutate(
#     Factor = paste0('Factor', row_number())
#   ) -> df_raw_alpha
# 
# df_raw_alpha
# 
# # Conclusion: There are 3 internally consistent factors, all of them with very high Alpha values.
# # This means that all skills can be consistently reduced to these 3 factors.
# 
# # Factor 1 has 17 skills loading to it.
# # Factor 2 has 10 skills loading to it.
# # Factor 3 has 8 skills loading to it.
# # Therefore, no factor is underloaded.
# df_loadings.long.factors %>%
#   group_by(Factor) %>%
#   tally(.) %>%
#   mutate(prct = round(n/sum(n),2))
# 
# # Other visualizations (test later)
# # install.packages('FactoMineR')
# # library(FactoMineR)
# # result <- PCA(df_occupations.numeric)
# 

# # CONCLUSION --------------------------------------------------------------
# # There are 3 internally consistent factors, all of them with very high Alpha values.
# # This means that all skills can be consistently reduced to these 3 factors.
# 
# # Factor 1 has 18 skills loading to it.
# # Factor 2 has 8 skills loading to it.
# # Factor 3 has 9 skills loading to it.
# # Therefore, no factor is underloaded.
# 
# # These numbers seem to be satisfactory.
# 
# 
# 
# # PICKING BEST ITEMS (NOT USING IRT) ---------------------------------------------
# # Number of items to pick
# n.items <- 3
# 
# # min crossloadings
# df_loadings.long %>%
#   group_by(Metric) %>%
#   mutate(
#     Crossloadings.Abs.Sum = sum(abs(Loading)) - Loading.Max #Sum of absolute value of crossloadings
#     # Crossloadings.Abs.Sum = sum(Loading) - Loading.Max #Sum of crossloadings
#   ) %>%
#   filter(
#     Loading == Loading.Max
#   ) %>%
#   # Pick factors with max loadings and min crossloadings
#   mutate(
#     Category = 'Skills'
#     , Loading_Crossloadings.Diff = Loading - Crossloadings.Abs.Sum
#   ) %>%
#   ungroup() %>%
#   group_by(Factor) %>%
#   arrange(
#     desc(Loading_Crossloadings.Diff)
#     , .by_group = T
#   ) %>%
#   top_n(n.items, Loading_Crossloadings.Diff) %>%
#   select(
#     Category
#     , Metric
#     , Factor
#     , Loading
#     , Loading_Crossloadings.Diff
#   ) -> df_loadings.items.skills
# # ) -> df_loadings.items.skills2
# 
# df_loadings.items.skills
# # df_loadings.items.skills2

# # TESTING BEST ITEMS: EFA II STILL CONSISTENT? YES! --------------------------------------------------------
# # # Exploratory Factor Analysis (EFA)
# 
# # 'Not in' operator
# # `%!in%` <- Negate(`%in%`)
# 
# df_occupations.numeric %>%
#   select(
#     df_loadings.items.skills$Metric
#     # df_loadings.items.skills2$Metric
#   ) -> df_occupations.numeric
# 
# # K-M-O factor adequacy test
# # * 0.00 to 0.49 unacceptable
# # * 0.50 to 0.59 miserable
# # * 0.60 to 0.69 mediocre
# # * 0.70 to 0.79 middling
# # * 0.80 to 0.89 meritorious
# # => 0.90 to 1.00 marvelous
# round(KMO(df_occupations.numeric[, KMO(df_occupations.numeric)$MSAi > 0.50])$MSA, 2)
# 
# # Null hypothesis: the variables in the data set are essentially uncorrelated.
# # Rejected the null hypothesis. Therefore, the data may be grouped into factors.
# cortest.bartlett(df_occupations.numeric)
# 
# # Determine Number of Factors to Extract
# df_occupations.numeric %>%
#   cor() %>%
#   eigen() -> ev
# 
# sum(ev$values >= 1)
# 
# # Kaiser criterion suggests 2 factors are sufficient
# 
# # Scree plot
# df_occupations.numeric %>%
#   scree(pc = F) #Factor analysis => pc = False
# 
# # Parallel analysis
# df_occupations.numeric %>%
#   fa.parallel(fa = 'fa') -> pa_analysis
# 
# # Parallel analysis suggests 3 factors are sufficient
# 
# # Very simple structure criterion (VSS)
# df_occupations.numeric %>%
#   VSS(n = 2 * pa_analysis$nfact) %>%
#   summary()
# 
# # VSS criterion 1 suggests 2 factors are sufficient
# # VSS criterion 2 suggests 2 factors are sufficient
# # Velicer MAP criterion suggests 3 factors are sufficient
# 
# # Testing whether 4 or less factors are sufficient
# 
# # Factor Analysis
# n.facts <- 3
# 
# # Initially, we suppose that factors could be correlated to one another.
# # Therefore, we apply an oblique rotation to the factors.
# fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')
# 
# print(fit, digits = 2, cutoff = 0.3, sort = T)
# 
# # Factors are somewhat (but not entirely) uncorrelated.
# # => Orthogonal rotation does not seem appropriate.
# # => Keep Oblique rotation factors
# 
# # Plot factors
# fa.diagram(fit$loadings)
# 
# # Limiting the analysis to only 4 factors, Factor 4 now does seem to be irrelevant.
# # This is confirmed by further analyses.
# 
# # Evaluation
# # Do variables load to the factors sufficiently?
# # |factor loading| > 0.4
# (abs(fit$loadings) > 0.4) %>% 
#   as_tibble(rownames = 'Metric') %>% 
#   mutate(
#     Load.Sufficient = rowSums(select(.,starts_with('Factor')))
#     , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
#   ) -> df_loadings.sufficient
# 
# # Percentage of variables that load significantly to at least one factor
# df_loadings.sufficient %>%
#   summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))
# 
# # Variables that do not load significantly to any factor
# df_loadings.sufficient %>% 
#   filter(Load.Sufficient.Bin == 0) %>% 
#   pull(Metric)
# 
# # Do all factors have at least three - or, better, four - or more variables loading onto them?
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 3
# 
# df_loadings.sufficient %>% 
#   select(starts_with('Factor')) %>%
#   colSums() >= 4
# 
# # All factors have at least 4 variables loading to them.
# 
# # Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
# fit$loadings[,] %>%
#   as.matrix() %>%
#   as_tibble(rownames = 'Metric') %>%
#   mutate(Metric = factor(Metric)) -> df_loadings
# 
# # Max loading vs other loadings
# df_loadings %>%
#   pivot_longer(#Convert to long data format
#     cols = starts_with('Factor')
#     , names_to = 'Factor'
#     , values_to = 'Loading'
#   ) %>%
#   group_by(Metric) %>%
#   mutate(
#     Loading.Max = max(Loading) #Max loading per variable
#     , Loading.Diff.Abs = abs(Loading.Max - Loading) #Absolute difference
#     , Diff.Significant = ifelse(#Whether the difference is significant or not (i.e. <= 0.05)
#       Loading.Diff.Abs == 0
#       , yes = F #Loading.Diff.Abs == 0 <=> Max Loading (i.e. difference between max value and itself)
#       , no = Loading.Diff.Abs <= 0.05
#     )
#   ) -> df_loadings.long
# 
# # Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Max Loadings Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading == Loading.Max
#   )) +
#   geom_tile()
# 
# # Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Loading.Diff.Abs
#   )) +
#   geom_tile() +
#   scale_fill_gradient2(
#     low = "#FF0000"
#     , mid = "#FFFFCC"
#     , high = "#075AFF")
# 
# # Significant Loadings Difference Heatmap
# df_loadings.long %>%
#   ggplot(aes(
#     x = fct_inorder(Factor)
#     , y = fct_rev(fct_inorder(Metric))
#     , fill = Diff.Significant
#   )) +
#   geom_tile()
# 
# # Almost no significant difference between factor loadings for each item.
# # Conclusion: Very few crossloadings. Each item maps considerably to only one factor.
# 
# 
# # Internal consistency test: Cronbach's Alpha
# # Do the factors form a coherent group in and of themselves?
# 
# # Matching items to factors
# # Since there are relatively few crossloadings, it is adequate to
# # use maximum loading as the criteria for factor matching.
# df_loadings.long %>%
#   filter(
#     Loading == Loading.Max
#   ) -> df_loadings.long.factors
# 
# 
# # Interestingly, although all factors are statistically relevant,
# # Factor 4 has no item maximally loading to it.
# # This means this is likely a secondary factors, if useful at all.
# # Therefore, unless we manually assign items to Factor 4,
# # the inclusion criteria (i.e. max loadings) leads us to disconsider it.
# # The proposed test to exclude Factor 4 confirmed the previous hypothesis.
# # We conclude this factor is not necessary for the present purposes.
# 
# # Separate factors into individual data frames
# lapply(
#   str_sort(unique(df_loadings.long.factors$Factor))
#   , function(factors){
# 
#     df_loadings.long.factors %>%
#       filter(
#         Factor == factors
#       ) %>%
#       pull(Metric) %>%
#       factor(.) %>%
#       return(.)
# 
#   }) -> list_chr_loadings.long.factors
# 
# # Apply Alpha test to each subset of variables
# lapply(
#   list_chr_loadings.long.factors
#   , function(factors){
# 
#     df_occupations.numeric %>%
#       select(factors) -> df.temp #Select only the variables that match to each factor
# 
#     if(length(factors) > 1){#By definition, internal consistency tests only apply to groups of more than one variable
# 
#       df.temp %>%
#         alpha(.) %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_alpha
# 
# # Raw alpha score
# lapply(
#   seq_along(list_alpha)
#   , function(index){
# 
#     if(!is.na(list_alpha[index])){
# 
#       list_alpha[[index]]$total$raw_alpha %>%
#         return(.)
# 
#     }
#     else{
# 
#       return(NA)
# 
#     }
# 
#   }) -> list_raw_alpha
# 
# # The general rule of thumb is that a Cronbach's alpha of 0.70 and above is good
# lapply(
#   list_raw_alpha
#   , function(cronbach){
# 
#     cronbach %>%
#       as_tibble(.) -> cronbach
# 
#     colnames(cronbach) <- 'Cronbach_Alpha'
# 
#     cronbach %>%
#       mutate(
#         Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
#       ) %>%
#       return(.)
# 
#   }) %>%
#   bind_rows(.) %>%
#   mutate(
#     Factor = paste0('Factor', row_number())
#   ) -> df_raw_alpha
# # ) -> df_raw_alpha2
# 
# df_raw_alpha
# # df_raw_alpha2
# 
# # Conclusion: There are 3 internally consistent factors, all of them with very high Alpha values.
# # This means that all skills can be consistently reduced to these 3 factors.
# 
# # Factor 1 has 17 skills loading to it.
# # Factor 2 has 10 skills loading to it.
# # Factor 3 has 8 skills loading to it.
# # Therefore, no factor is underloaded.
# df_loadings.long.factors %>%
#   group_by(Factor) %>%
#   tally(.) %>%
#   mutate(prct = round(n/sum(n),2))
# 
# # Other visualizations (test later)
# # install.packages('FactoMineR')
# # library(FactoMineR)
# # result <- PCA(df_occupations.numeric)
# 

# # # # -------------------------------------------------------------------------
# # # 
# # # df_occupations %>% 
# # #   select(
# # #     Code
# # #     , Occupation
# # #     # , Industry
# # #     , Career_Cluster
# # #     , Career_Pathway
# # #     , Entry_level_Education
# # #     , Typical_On_the_Job_Training
# # #     , Annual_Wage_2021
# # #     , Projected_Growth_2020.2030
# # #     , df_loadings.items$Metric
# # #   ) %>% 
# # #   pivot_longer(#Convert to long data format
# # #     cols = df_loadings.items$Metric
# # #     , names_to = 'Metric'
# # #     , values_to = 'Recommended_level'
# # #   ) %>% 
# # #   left_join(df_loadings.items) %>% 
# # #   group_by(Code, Factor) %>%
# # #   mutate(
# # #     Factor.Level.Mean = mean(Recommended_level)
# # #   ) %>% ungroup() -> df_occupations.items.long
# # # 
# # # # Loadings Heatmap
# # # df_occupations.items.long %>%
# # #   filter(
# # #     Career_Cluster == 'Finance'
# # #   ) %>%
# # #   mutate(
# # #     Occupation = fct_reorder(
# # #       Occupation, Annual_Wage_2021, .desc = F
# # #     )
# # #   ) %>% 
# # #   # group_by(Occupation, Factor) %>%
# # #   # summarize(
# # #   # Factor.Level.Mean = mean(Recommended_level)
# # #   # ) %>%
# # #   ggplot(aes(
# # #     x = fct_rev(fct_inorder(Metric))
# # #     # x = str_sort(Factor)
# # #     , y = Occupation
# # #     , fill = Factor.Level.Mean
# # #   )) + 
# # #   geom_tile() + 
# # #   viridis::scale_fill_viridis(option = 'viridis')
# # # 
# # # 
# # # 
# # 
# # # # SUM(CROSSLOADINGS) VS SUM(ABS(CROSSLOADINGS))  -------------------------------------------------------------------
# # # df_loadings.items.skills
# # # df_loadings.items.skills2
# # # 
# # # df_loadings.items.skills %>% 
# # #   group_by(Factor) %>%
# # #   summarize(
# # #     Loading.Sum = sum(Loading)
# # #     , Loading_Crossloadings.Diff.Sum = sum(Loading_Crossloadings.Diff)
# # #   ) -> loadings.sum
# # # 
# # # df_loadings.items.skills2 %>% 
# # #   group_by(Factor) %>%
# # #   summarize(
# # #     Loading.Sum = sum(Loading)
# # #     , Loading_Crossloadings.Diff.Sum = sum(Loading_Crossloadings.Diff)
# # #   ) -> loadings.sum2
# # # 
# # # sum(loadings.sum$Loading.Sum) > sum(loadings.sum2$Loading.Sum)
# # # sum(loadings.sum$Loading.Sum) - sum(loadings.sum2$Loading.Sum)
# # # 
# # # sum(loadings.sum$Loading_Crossloadings.Diff.Sum) > sum(loadings.sum2$Loading_Crossloadings.Diff.Sum)
# # # sum(loadings.sum$Loading_Crossloadings.Diff.Sum) - sum(loadings.sum2$Loading_Crossloadings.Diff.Sum)
# # # 
# # # df_raw_alpha
# # # df_raw_alpha2
# # # 
# # # sum(df_raw_alpha$Cronbach_Alpha) > sum(df_raw_alpha2$Cronbach_Alpha)
# # # sum(df_raw_alpha$Cronbach_Alpha) == sum(df_raw_alpha2$Cronbach_Alpha)
# # # 
# # 
# # 
# # 
# EXPORT RESULTS ONLY (SOURCE) --------------------------------------------------------
n.facts <- 3

fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

fit$loadings[,] %>%
  as.matrix() %>%
  as_tibble(rownames = 'Metric') %>%
  mutate(Metric = factor(Metric)) -> df_loadings

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

df_loadings.long %>%
  filter(
    Loading == Loading.Max
  ) -> df_loadings.long.factors

n.items <- 3

df_loadings.long %>%
  group_by(Metric) %>%
  mutate(
    Crossloadings.Abs.Sum = sum(abs(Loading)) - Loading.Max #Sum of absolute value of crossloadings
  ) %>%
  filter(
    Loading == Loading.Max
  ) %>%
  # Pick factors with max loadings and min crossloadings
  mutate(
    Category = 'Skills'
    , Loading_Crossloadings.Diff = Loading - Crossloadings.Abs.Sum
  ) %>%
  ungroup() %>%
  group_by(Factor) %>%
  arrange(
    desc(Loading_Crossloadings.Diff)
    , .by_group = T
  ) %>%
  top_n(n.items, Loading_Crossloadings.Diff) %>%
  select(
    Category
    , Metric
    , Factor
    , Loading
    , Loading_Crossloadings.Diff
  ) -> df_loadings.items.skills
