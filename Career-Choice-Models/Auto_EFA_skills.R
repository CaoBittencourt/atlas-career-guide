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
    where(function(x){str_detect(attributes(x)$label, '_Skills.')}) #Skills only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.skills


# EFA 1: Average N Factors ---------------------------------------------------------------------
# Use average suggested number of factors as starting point
df_occupations.numeric.skills %>% 
  fun_nfactors.selection() %>% 
  filter(Criterion == 'Average') %>%
  pull(Factors.Suggested) -> n.facts

# AUTO-EFA
df_occupations.numeric.skills %>%
  fun_EFA(
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
    ) -> EFA_skills.avg

# Output
EFA_skills.avg$adequacy.tests
EFA_skills.avg$n.factors
EFA_skills.avg$sufficient.loadings
# Factors 5 and 6 don't have any variables maximally loading to them
EFA_skills.avg$reliability.metrics
EFA_skills.avg$reliability.evaluation
# Three very generic skills were removed
EFA_skills.avg$removed.items

# Since Factors 5 and 6 were not needed in practice,
# we should rerun the analysis with 4 factors.
# EFA 2: 4 factors ---------------------------------------------------------------------
n.facts <- 4

# AUTO-EFA
df_occupations.numeric.skills %>%
  fun_EFA(
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
    ) -> EFA_skills.4

# Output
EFA_skills.4$adequacy.tests
EFA_skills.4$n.factors
EFA_skills.4$sufficient.loadings
# Factor 4 doesn't have any variables maximally loading to it
EFA_skills.4$reliability.metrics
EFA_skills.4$reliability.evaluation
# Four skills were removed
EFA_skills.4$removed.items

# Since Factor 4 was not needed in practice,
# we should rerun the analysis with 3 factors.
# EFA 3: 3 factors ---------------------------------------------------------------------
n.facts <- 3

# AUTO-EFA
df_occupations.numeric.skills %>%
  fun_EFA(
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
    ) -> EFA_skills.3

# Output
EFA_skills.3$adequacy.tests
EFA_skills.3$n.factors
EFA_skills.3$sufficient.loadings
# Finally, Factor 3 was not removed
# All factors have items maximally loading to them
EFA_skills.3$reliability.metrics
EFA_skills.3$reliability.evaluation
# All factors have excellent internal consistency
# However, interitem correlation is too high
# This means some items are redundant and could be removed

# Only one skill was removed 
EFA_skills.3$removed.items

# Interpreting factors
# Factor 1 is composed of cognitive/intellectual skills (intelligence)
# Factor 2 is composed of technical/mechanical skills (hands-on)
# Factor 3 is composed of social/managerial skills (extroversion and agreeableness)

# Since all factors are consistent and meaningful, 
# it is safe to conclude EFA here. Now, we must select the top N items for each factor.

# TOP N ITEMS: EFA3 Model ---------------------------------------------------------------------
# TOP 3 ITEMS
top.n <- 3

EFA_skills.3$loadings.long %>%
  fun_top.items(n.items = top.n) -> df_skills.items

df_skills.items

# EFA 4: RERUN EFA WITH TOP ITEMS ONLY ------------------------------------
n.facts <- 3

df_occupations.numeric.skills %>% 
  select(df_skills.items$Item) -> df_occupations.numeric.skills.items

# AUTO-EFA
df_occupations.numeric.skills.items %>%
  fun_EFA(
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
  ) -> EFA_skills.3.items

# Output
EFA_skills.3.items$adequacy.tests
EFA_skills.3.items$n.factors
EFA_skills.3.items$sufficient.loadings

# All factors have items maximally loading to them
EFA_skills.3.items$reliability.metrics
EFA_skills.3.items$reliability.evaluation
# All factors still remain excellent/good with respect to internal consistency
# However, interitem correlation has increased

# No skill was removed
EFA_skills.3.items$removed.items

# Interpreting factors
# Factor 1 is composed of cognitive/intellectual skills (intelligence)
# Factor 2 is composed of technical/mechanical skills (hands-on)
# Factor 3 is composed of social/managerial skills (extroversion and agreeableness)

# Since all factors are consistent and meaningful, 
# it is safe to conclude EFA here.


