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
setwd('C:/Users/Cao/Documents/Github/Atlas-Research')

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
df_occupations %>%
  select(
    where(is.numeric)
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance values
  ) %>%
  select(
    starts_with('Active_Listening'):last_col()
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

# Parallel analysis suggests 15 factors are sufficient
# P.S.: If using importance values rather than level, Parallel analysis suggests 17 factors are sufficient

# Factor Analysis
n.facts <- 15
# n.facts <- 17

# Initially, we suppose that factors could be correlated to one another.
# Therefore, we apply an oblique rotation to the factors.
fit <- factanal(df_occupations.numeric, n.facts, rotation = 'promax')

print(fit, digits = 2, cutoff = 0.3, sort = T)

# Factors are mostly (but not entirely) uncorrelated.
# => Try orthogonal rotation as well.
fit2 <- factanal(df_occupations.numeric, n.facts, rotation = 'varimax')

print(fit2, digits = 2, cutoff = 0.3, sort = T)

# Plot factors
# Oblique
fa.diagram(fit$loadings)
# Orthogonal
fa.diagram(fit2$loadings)

# Oblique rotation seems to be cleaner.

# fa.diagram(fit$loadings[,1:2])
# fa.diagram(fit2$loadings[,1:2])

# Further evaluation
# Do all the variables load to the factors sufficiently?
# |factor loading| > 0.4
fct_load.sufficent <- abs(fit$loadings) > 0.4 

fct_load.sufficent2 <- abs(fit2$loadings) > 0.4 

round(colSums(fct_load.sufficent)/nrow(fct_load.sufficent), 2) 
round(colSums(fct_load.sufficent2)/nrow(fct_load.sufficent2), 2)

# Factor 13, 14, and 15 are irrelevant if supposing orthogonal rotation.

# Do all factors have at least three - or, better, four - or more variables loading onto them?
colSums(fct_load.sufficent) >= 3
colSums(fct_load.sufficent) >= 4

colSums(fct_load.sufficent2) >= 3
colSums(fct_load.sufficent2) >= 4

# Crossloadings: variables that load to more than one factor with loading values within 0.05 of one another
fit$loadings %>% view()
# sapply(list, function)


# EUCLIDEAN DISTANCE ------------------------------------------------------
# MULTINOMIAL PROBIT MODEL ------------------------------------------------
# SIMPLER ALGORITHMS ------------------------------------------------------
