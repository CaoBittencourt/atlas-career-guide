# PACKAGES -----------------------------------------------------------------
pkg <- c(
  # 'paletteer', 'ggthemes', 'scales' #Visualization
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

# # Testing without problematic variables (viz. Food Production)
df_occupations.numeric %>%
  select(
    -starts_with(c(
    'Food_Production' #Food production doesn't load significantly to any factor and is not needed for the vast majority of professions
    , 'Education_and_Training' #Too many crossloadings and normally distributed (not polarizing)
    # , 'Design' #Does not fit well in the factor (theoretically)
    ))
  ) -> df_occupations.numeric

# HISTOGRAMS AND DENSITIES ------------------------------------------------
lapply(df_occupations.numeric, qplot) -> dsdsds
lapply(df_occupations.numeric, function(know){qplot(know, geom = 'density')}) -> dsds

# Really Positively Asymmetric
# None
# Somewhat Positively Asymmetric
dsdsds$Customer_and_Personal_Service.L
# Normal
dsdsds$Administrative.L
dsdsds$Administration_and_Management.L
dsdsds$Computers_and_Electronics.L
dsdsds$Public_Safety_and_Security.L
dsdsds$Education_and_Training.L
dsdsds$English_Language.L
dsdsds$Mathematics.L
# Somewhat Negatively Asymmetric
dsdsds$Law_and_Government.L
dsdsds$Transportation.L
dsdsds$Psychology.L
dsdsds$Communications_and_Media.L
dsdsds$Personnel_and_Human_Resources.L
dsdsds$Telecommunications.L
dsdsds$Economics_and_Accounting.L
dsdsds$Sales_and_Marketing.L
dsdsds$Production_and_Processing.L
# Negatively Asymmetric
dsdsds$Foreign_Language.L
dsdsds$Engineering_and_Technology.L
dsdsds$Mechanical.L
dsdsds$Design.L
dsdsds$Geography.L
dsdsds$Chemistry.L
dsdsds$Physics.L
dsdsds$Sociology_and_Anthropology.L
# Really Negatively Asymmetric
dsdsds$History_and_Archeology.L
dsdsds$Philosophy_and_Theology.L
dsdsds$Fine_Arts.L
dsdsds$Building_and_Construction.L
dsdsds$Therapy_and_Counseling.L
dsdsds$Medicine_and_Dentistry.L
dsdsds$Food_Production.L
dsdsds$Biology.L

# Really Positively Asymmetric
# None
# Somewhat Positively Asymmetric
dsds$Customer_and_Personal_Service.L
# Normal
dsds$Administrative.L
dsds$English_Language.L
dsds$Administration_and_Management.L
dsds$Computers_and_Electronics.L
dsds$Education_and_Training.L
dsds$Public_Safety_and_Security.L
dsds$Mathematics.L
# Somewhat Negatively Asymmetric
dsds$Law_and_Government.L
dsds$Transportation.L
dsds$Psychology.L
dsds$Communications_and_Media.L
dsds$Personnel_and_Human_Resources.L
dsds$Telecommunications.L
dsds$Economics_and_Accounting.L
dsds$Sales_and_Marketing.L
dsds$Production_and_Processing.L
# Negatively Asymmetric
dsds$Foreign_Language.L
dsds$Engineering_and_Technology.L
dsds$Mechanical.L
dsds$Design.L
dsds$Geography.L
dsds$Chemistry.L
dsds$Physics.L
dsds$Sociology_and_Anthropology.L
# Really Negatively Asymmetric
dsds$History_and_Archeology.L
dsds$Philosophy_and_Theology.L
dsds$Fine_Arts.L
dsds$Building_and_Construction.L
dsds$Therapy_and_Counseling.L
dsds$Medicine_and_Dentistry.L
dsds$Food_Production.L
dsds$Biology.L









# [NO] EFA I: Max loadings, 6 factors --------------------------------------------------------
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
n.facts <- 6

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Food production does not load sufficiently to any factor
# Since it is a very negatively asymmetrical variable,
# so much so that most careers don't require any level of this field of knowledge,
# it seems thus appropriate to drop it from the data set.
# English language also does not sufficiently load to any factor
# It is normally distributed. Thus, most professions use this field of knowledge to some degree.
# Therefore, it is adequate to drop it.
df_occupations.numeric %>% 
  select(-starts_with(c(
    'Food_Production'
    , 'English_Language'
  ))) -> df_occupations.numeric

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
n.facts <- 6

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4


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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>%
      return(.)
    
  }) %>%
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, all of them with high or very high Alpha values.

# Factor 1 has 19 abilities loading to it.
# Factor 2 has 14 abilities loading to it.
# Factor 3 has 10 abilities loading to it.
# Factor 4 has 7 abilities loading to it.
# Factor 5 has 2 abilities loading to it.
# Factor 5 is indeed underloaded.
df_loadings.long.factors %>%
  group_by(Factor) %>%
  tally(.) %>%
  mutate(prct = round(n/sum(n),2))

# Other visualizations (test later)
# install.packages('FactoMineR')
# library(FactoMineR)
# result <- PCA(df_occupations.numeric)

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  # pull(Metric) -> chr_remove2
  pull(Metric) -> chr_remove4

# Food production does not load sufficiently to any factor
# Since it is a very negatively asymmetrical variable,
# so much so that most careers don't require any level of this field of knowledge,
# it seems thus appropriate to drop it from the data set.
# df_occupations.numeric %>% 
#   select(-starts_with('Food_Production')) -> df_occupations.numeric
df_occupations.numeric %>% 
  # select(-all_of(chr_remove2)) -> df_occupations.numeric
  select(-all_of(chr_remove4)) -> df_occupations.numeric

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4


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

df_loadings.long %>% 
  filter(Diff.Significant) %>%
  pull(Metric) %>%
  as.character() -> chr_remove
  # as.character() -> chr_remove3

df_occupations.numeric %>%
  select(-all_of(chr_remove)) -> df_occupations.numeric

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
        # alpha(.) %>%
        splitHalf(.) %>%
        # omega(nfactors = 1) %>%
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
  ) -> df_raw_alpha5

df_raw_alpha5

# Conclusion: There are 5 internally consistent factors, all of them with high or very high Alpha values.

# Factor 1 has 19 abilities loading to it.
# Factor 2 has 14 abilities loading to it.
# Factor 3 has 10 abilities loading to it.
# Factor 4 has 7 abilities loading to it.
# Factor 5 has 2 abilities loading to it.
# Factor 5 is indeed underloaded.
df_loadings.long.factors %>%
  group_by(Factor) %>%
  tally(.) %>%
  mutate(prct = round(n/sum(n),2)) %>% 
  left_join(df_raw_alpha5) -> EFA5

EFA5

# EFA III: Max loadings, 4 factors --------------------------------------------------------
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
n.facts <- 4

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4

# Food production does not load sufficiently to any factor
# Since it is a very negatively asymmetrical variable,
# so much so that most careers don't require any level of this field of knowledge,
# it seems thus appropriate to drop it from the data set.
# Transportation does not sufficiently load to any factor as well.
# Since transportation is normally distributed, it is not as polarizing as other variables.
# Therefore, it could be better to drop it.
# Public Safety and Security also does not sufficiently load to any factor.
df_occupations.numeric %>% 
  select(-starts_with(c(
    # 'Food_Production'
    'Transportation' #Doesn't load sufficiently and most professions don't need it too much
    , 'Public_Safety_' #Doesn't load sufficiently and is normally distributed
    # , 'English_' #Too many crossloadings (confirmed later on)
    # , 'Mathematics' #Too many crossloadings (confirmed later on)
    # , 'Education_' #Too many crossloadings (confirmed later on)
  ))) -> df_occupations.numeric

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
n.facts <- 4

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4

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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>%
      return(.)
    
  }) %>%
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha4

df_raw_alpha4

# Conclusion: There are 5 internally consistent factors, all of them with very high Alpha values.
# This means that all abilities can be consistently reduced to these 5 factors.
# P.S.: Factor 5 is composed of only 2 items. It should be considered whether or not this factor is truly necessary.

# Factor 1 has 19 abilities loading to it.
# Factor 2 has 14 abilities loading to it.
# Factor 3 has 10 abilities loading to it.
# Factor 4 has 7 abilities loading to it.
# Factor 5 has 2 abilities loading to it.
# Factor 5 is indeed underloaded.
df_loadings.long.factors %>%
  group_by(Factor) %>%
  tally(.) %>%
  mutate(prct = round(n/sum(n),2)) %>% 
  left_join(df_raw_alpha4) -> EFA4

EFA4

# [NO] EFA IV: Max loadings, 3 factors --------------------------------------------------------
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
n.facts <- 3

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4

# Food production does not load sufficiently to any factor
# Since it is a very negatively asymmetrical variable,
# so much so that most careers don't require any level of this field of knowledge,
# it seems thus appropriate to drop it from the data set.
# Transportation does not sufficiently load to any factor as well.
# Since transportation is normally distributed, it is not as polarizing as other variables.
# Therefore, it could be better to drop it.
# Fine arts and foreign language also do not load sufficiently to any factor.
df_occupations.numeric %>% 
  select(-starts_with(c(
    'Food_Production'
    , 'Transportation'
    , 'Fine_Arts'
    , 'Foreign_Language'
    , 'Customer_and_Personal_Service' #Later verified
  ))) -> df_occupations.numeric

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
n.facts <- 3

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4

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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>%
      return(.)
    
  }) %>%
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha

df_raw_alpha

# Conclusion: There are 5 internally consistent factors, all of them with very high Alpha values.
# This means that all abilities can be consistently reduced to these 5 factors.
# P.S.: Factor 5 is composed of only 2 items. It should be considered whether or not this factor is truly necessary.

# Factor 1 has 19 abilities loading to it.
# Factor 2 has 14 abilities loading to it.
# Factor 3 has 10 abilities loading to it.
# Factor 4 has 7 abilities loading to it.
# Factor 5 has 2 abilities loading to it.
# Factor 5 is indeed underloaded.
df_loadings.long.factors %>%
  group_by(Factor) %>%
  tally(.) %>%
  mutate(prct = round(n/sum(n),2))


# PICKING BEST ITEMS (NOT USING IRT) -------------------------------------------------------------------------
n.items <- 4

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
  # top_n(n.items,Loading) %>%
  select(
    Category
    , Metric
    , Factor
    , Loading
    , Loading_Crossloadings.Diff
    ) -> df_loadings.items.know5
    # ) -> df_loadings.items.know4

df_loadings.items.know5
# df_loadings.items.know4

view(df_loadings.items.know5)
# view(df_loadings.items.know4)

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
# TESTING BEST ITEMS: EFA II STILL CONSISTENT? NO! --------------------------------------------------------
# # Exploratory Factor Analysis (EFA)

# 'Not in' operator
# `%!in%` <- Negate(`%in%`)

df_occupations.numeric %>%
  select(
    df_loadings.items.know5$Metric
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

# TESTING BEST ITEMS: EFA III STILL CONSISTENT? --------------------------------------------------------
# # Exploratory Factor Analysis (EFA)

# 'Not in' operator
# `%!in%` <- Negate(`%in%`)

df_occupations.numeric %>%
  select(
    df_loadings.items.know4$Metric
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
n.facts <- 4

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4

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
n.facts <- 4

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
  as_tibble(rownames = 'Metric') %>% 
  mutate(
    Load.Sufficient = rowSums(select(.,starts_with('Factor')))
    , Load.Sufficient.Bin = ifelse(Load.Sufficient > 1, 1, Load.Sufficient)
  ) -> df_loadings.sufficient

# Percentage of variables that load significantly to at least one factor
df_loadings.sufficient %>%
  summarise(Load.Sufficient.Prct = sum(Load.Sufficient.Bin)/nrow(.))

# Variables that do not load significantly to any factor
df_loadings.sufficient %>% 
  filter(Load.Sufficient.Bin == 0) %>% 
  pull(Metric)

# Do all factors have at least three - or, better, four - or more variables loading onto them?
df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 3

df_loadings.sufficient %>% 
  select(starts_with('Factor')) %>%
  colSums() >= 4

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
        Cronbach_Alpha.Good = Cronbach_Alpha >= 0.7
      ) %>%
      return(.)
    
  }) %>%
  bind_rows(.) %>%
  mutate(
    Factor = paste0('Factor', row_number())
  ) -> df_raw_alpha4

df_raw_alpha4

# Conclusion: There are 5 internally consistent factors, all of them with very high Alpha values.
# This means that all abilities can be consistently reduced to these 5 factors.
# P.S.: Factor 5 is composed of only 2 items. It should be considered whether or not this factor is truly necessary.

# Factor 1 has 19 abilities loading to it.
# Factor 2 has 14 abilities loading to it.
# Factor 3 has 10 abilities loading to it.
# Factor 4 has 7 abilities loading to it.
# Factor 5 has 2 abilities loading to it.
# Factor 5 is indeed underloaded.
df_loadings.long.factors %>%
  group_by(Factor) %>%
  tally(.) %>%
  mutate(prct = round(n/sum(n),2)) %>% 
  left_join(df_raw_alpha4) -> EFA4

EFA4
