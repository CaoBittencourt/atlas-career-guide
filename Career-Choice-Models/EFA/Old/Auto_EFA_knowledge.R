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

# AUTO-EFA FUNCTIONS ----------------------------------------------------------------
source('./Auto_EFA.R')

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
    where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Knowledge only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.know


# EFA 1: Average N Factors ---------------------------------------------------------------------
# Use average suggested number of factors as starting point
df_occupations.numeric.know %>% 
  fun_nfactors.selection() %>% 
  filter(Criterion == 'Average') %>%
  pull(Factors.Suggested) -> n.facts

# AUTO-EFA
df_occupations.numeric.know %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
    ) -> EFA_know.avg

# Output
EFA_know.avg$adequacy.tests
EFA_know.avg$n.factors
EFA_know.avg$sufficient.loadings

# Factor 9 doesn't have any variables maximally loading to it
# Factor 8 is a single item factor
# Factor 4, 6, and 7 have only two items maximally loading to them
EFA_know.avg$reliability.metrics
EFA_know.avg$reliability.evaluation
# Although Factors 1, and 3 have good to excellent internal consistency,
# the other factors do not. 

# Nine fields of knowledge were removed
EFA_know.avg$removed.items

# We should rerun the analysis with fewer than 8 factors.

# EFA 2: 7 factors ---------------------------------------------------------------------
n.facts <- 7

# AUTO-EFA
df_occupations.numeric.know %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
    ) -> EFA_know.7

# Output
EFA_know.7$adequacy.tests
EFA_know.7$n.factors
EFA_know.7$sufficient.loadings
# All factors have items maximally loading to them
EFA_know.7$reliability.metrics
EFA_know.7$reliability.evaluation
# Factors 1, 2, and 3 have good to excelent internal consistency
# Factors 4 and 5 have acceptable internal consistency
# Factors 6 and 7 have questionable reliability
# Also, Factor 7 has only 2 items maximally loading to it

# Five fields of knowledge were removed
EFA_know.7$removed.items

# Interpretability of the factors could be improved as well.

# Thus, we try the 6 factor model next.

# EFA 3: 6 factors ---------------------------------------------------------------------
n.facts <- 6

# AUTO-EFA
df_occupations.numeric.know %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
    ) -> EFA_know.6

# Output
EFA_know.6$adequacy.tests
EFA_know.6$n.factors
EFA_know.6$sufficient.loadings

# All factors have items maximally loading to them
EFA_know.6$reliability.metrics
EFA_know.6$reliability.evaluation
# Factor 6 is not sufficiently consistent
# Factor 5' internal consistency is questionable / acceptable
# The other factors have good / excellent internal consistency
# Also, Factors 5 and 6 has only two items loading to it.

# Six fields of knowledge were removed 
EFA_know.6$removed.items

# Aiming to obtain more robust factors, the five factor model is tried next.

# EFA 4: 5 factors ---------------------------------------------------------------------
n.facts <- 5

# AUTO-EFA
df_occupations.numeric.know %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
    ) -> EFA_know.5

# Output
EFA_know.5$adequacy.tests
EFA_know.5$n.factors
EFA_know.5$sufficient.loadings

# All factors have items maximally loading to them
EFA_know.5$reliability.metrics
EFA_know.5$reliability.evaluation
# Factor 1 to 4 have good to excellent internal consistency measures
# Factor 5, however, is poor to questionable in this regard
# It also has only to items maximally loading to it.

# Seven fields of knowledge were removed 
EFA_know.5$removed.items

# EFA 5: 4 factors ---------------------------------------------------------------------
n.facts <- 4

# AUTO-EFA
df_occupations.numeric.know %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
    ) -> EFA_know.4

# Output
EFA_know.4$adequacy.tests
EFA_know.4$n.factors
EFA_know.4$sufficient.loadings

# All factors have items maximally loading to them
EFA_know.4$reliability.metrics
EFA_know.4$reliability.evaluation
# The four factor model is significantly more consistent than the previous models
# All factors have at least 4 items maximally loading to them as well

# Six fields of knowledge were removed 
EFA_know.4$removed.items

# Interpretating the factors
# Factor 1 is composed of fields of knowledge oriented towards physical things (Build/Product)
# Factor 2 is composed of "conventional" and "enterprising" fields of knowledge (FGV)
# Factor 3 is composed of Arts & Humanities (CPDOC) 
# Factor 4 is composed of health, psychology and biology related fields of knowledge (Health & Well-being)

# These factors seem satisfactorily interpretable and consistent.

# TOP N ITEMS 1: EFA4 Model ---------------------------------------------------------------------
# TOP 3 ITEMS
top.n <- 3

EFA_know.4$loadings.long %>%
  fun_top.items(n.items = top.n) -> df_know.items

df_know.items

# EFA 6: RERUN EFA WITH TOP ITEMS ONLY ------------------------------------
n.facts <- 4

df_occupations.numeric.know %>% 
  select(df_know.items$Item) -> df_occupations.numeric.know.items

# AUTO-EFA
df_occupations.numeric.know.items %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
  ) -> EFA_know.4.items

# Output
EFA_know.4.items$adequacy.tests
# Adequacy is now mediocre
# Maybe the number of top items should be increased
EFA_know.4.items$n.factors
EFA_know.4.items$sufficient.loadings

# All factors have items maximally loading to them
EFA_know.4.items$reliability.metrics
EFA_know.4.items$reliability.evaluation
# Reliability has decreased with this subset of items

# No field of knowledge was removed
EFA_know.4.items$removed.items

# Repeat EFA with more items



# TOP N ITEMS 2: EFA4 Model ---------------------------------------------------------------------
# TOP 3 ITEMS
top.n <- 4

EFA_know.4$loadings.long %>%
  fun_top.items(n.items = top.n) -> df_know.items

df_know.items

# EFA 7: RERUN EFA WITH TOP ITEMS ONLY ------------------------------------
n.facts <- 4

df_occupations.numeric.know %>% 
  select(df_know.items$Item) -> df_occupations.numeric.know.items

# AUTO-EFA
df_occupations.numeric.know.items %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
  ) -> EFA_know.4.items

# Output
EFA_know.4.items$adequacy.tests
# Adequacy is meritorious again
EFA_know.4.items$n.factors
EFA_know.4.items$sufficient.loadings

# All factors have items maximally loading to them
EFA_know.4.items$reliability.metrics
EFA_know.4.items$reliability.evaluation
# Reliability has increased to acceptable levels
# Most factors have good internal consistency

# No field of knowledge was removed
EFA_know.4.items$removed.items

# This seems to be an appropriate result for now.
