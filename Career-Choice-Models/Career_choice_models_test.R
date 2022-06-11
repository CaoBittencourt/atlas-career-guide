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


# RELEVANT COLUMNS --------------------------------------------------------
# Pearson across columns

# # Exploratory Factor Analysis (EFA)
# P.S.: try breaking down the data frame by skills and abilities categories (i.e. perform separate factor analyses to reduce each category individually)
# df_occupations %>%
#   select(
#     where(is.numeric)
#     , -ends_with('.I') #Using recommended levels
#     # , -ends_with('.L') #Using importance values
#   ) %>%
#   select(
#     starts_with('Active_Listening'):last_col()
#   ) -> df_occupations.numeric

# 'Not in' operator
# `%!in%` <- Negate(`%in%`)

df_occupations %>%
  select(
    where(is.numeric)
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance values
  ) %>%
  select(
    starts_with('Active_Listening'):starts_with('Transportation') #Skills, abilities, and knowledge only
  ) -> df_occupations.numeric

# Simpler names, if needed
# colnames(df_occupations.numeric) <- paste0('V',seq_along(df_occupations.numeric))


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

# Parallel analysis suggests 11 factors are sufficient
# P.S.: If using importance values rather than level, Parallel analysis suggests 17 factors are sufficient
# P.S.: Using only skills, abilities and knowledge, Parallel analysis suggests 11 factors are sufficient
# P.S.: If using importance values rather than level, with only skills, abilities and knowledge, Parallel analysis suggests 12 factors are sufficient

# Factor Analysis
n.facts <- 11
# n.facts <- 12
# n.facts <- 15
# n.facts <- 17

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are somewhat (but not entirely) uncorrelated.
# => Orthogonal rotation does not seem appropriate.
# => Keep Oblique rotation factors

# Plot factors
fa.diagram(fit$loadings)

# On first sight, Factors 9 and 10 seem disposable.
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
# All factors have enough variables loading to themselves
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
# factors 9 and 10 have no item maximally loading to them.
# This means they are likely secondary factors, if useful at all.
# Therefore, unless we manually assign items to these two factors,
# the inclusion criteria (i.e. max loadings) leads us to disconsider them.  

# Separate factors into individual data frames
lapply(
  unique(df_loadings.long.factors$Factor)
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


# Conclusion: There are 9 internally consistent factors, most with very high Alpha values.
# This means that all items can be consistently reduced to these 9 factors.
# P.S.: Factor 4 is composed of only 1 item. Therefore, it is internally consistent by definition.

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)


# EUCLIDEAN DISTANCE ------------------------------------------------------
# MULTINOMIAL PROBIT MODEL ------------------------------------------------
# SIMPLER ALGORITHMS ------------------------------------------------------
