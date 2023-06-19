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
    where(function(x){str_detect(attributes(x)$label, '_Skills.')}) #Skills
    , where(function(x){str_detect(attributes(x)$label, 'Abilities.')}) #Abilities
    , where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #And Knowledge
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric


# EFA 1: Average N Factors ---------------------------------------------------------------------
# Use average suggested number of factors as starting point
df_occupations.numeric %>% 
  fun_nfactors.selection() %>% 
  filter(Criterion == 'Average') %>%
  pull(Factors.Suggested) -> n.facts

# AUTO-EFA
df_occupations.numeric %>%
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
    ) -> EFA.avg

# Output
EFA.avg$adequacy.tests
EFA.avg$n.factors
EFA.avg$sufficient.loadings

# Factors 11, 12 and 13 don't have any variables maximally loading onto them
# Factor 10 has only one
EFA.avg$reliability.metrics
EFA.avg$reliability.evaluation

# 22 items were removed
EFA.avg$removed.items

# We should rerun the EFA with 10 factors.

# EFA 2: 10 factors ---------------------------------------------------------------------
n.facts <- 10

# AUTO-EFA
df_occupations.numeric %>%
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
    ) -> EFA.10

# Output
EFA.10$adequacy.tests
EFA.10$n.factors
EFA.10$sufficient.loadings

# Factor 10 doesn't have any items maximally loading onto it
# Factor 9 has only one
EFA.10$reliability.metrics
EFA.10$reliability.evaluation

# 19 items were removed
EFA.10$removed.items

# Rerun the analysis with 9 factors

# EFA 3: 9 factors ---------------------------------------------------------------------
n.facts <- 9

# AUTO-EFA
df_occupations.numeric %>%
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
    ) -> EFA.9

# Output
EFA.9$adequacy.tests
EFA.9$n.factors
EFA.9$sufficient.loadings

# All factors have items maximally loading to them
# However, Factors 8 and 9 have only one item
EFA.9$reliability.metrics
EFA.9$reliability.evaluation

# 22 items were removed 
EFA.9$removed.items

# # EFA 4: 7 factors ---------------------------------------------------------------------
# n.facts <- 7
# 
# # AUTO-EFA
# df_occupations.numeric %>%
#   fun_factor.analysis(
#     # Basic
#     int_nfactors = 8
#     , chr_rotation = 'promax'
#     # Underloadings and crossloadings
#     , remove_under_loading.items = T
#     , remove_cross_loading.items = T
#     , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
#     , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
#     # Diagrams and tests
#     , show_diagrams = T
#     , show_results = F
#     ) -> EFA.7
# 
# # Output
# EFA.7$adequacy.tests
# EFA.7$n.factors
# EFA.7$sufficient.loadings
# 
# # All factors have items maximally loading to them
# EFA.7$reliability.metrics
# EFA.7$reliability.evaluation
# # Internal consistency remains the same in the four factor model
# # However, now the smallest factor has 6 items
# # The factors are more interpretable as well.
# 
# # No abilities were removed 
# EFA.7$removed.items
# 
# # Interpreting factors
# # Factor 1 is composed of cognitive/intellectual abilities (intelligence)
# # Factor 2 is composed of coordination/crafts-related abilities (dexterity)
# # Factor 3 is composed of bodily abilities (body robustness and equilibrium)
# # Factor 4 is composed of perceptual abilities (perception)
# 
# # These factors are consistent and interpretable.
# # Furthermore, it seems grouping them together even further would be 
# # artificial/counter-productive. We, thus, stop EFA at the four factor model.

# EFA 3: 9 factors ---------------------------------------------------------------------
n.facts <- 5

# AUTO-EFA
df_occupations.numeric %>%
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
  ) -> EFA.9

# Output
EFA.9$adequacy.tests
EFA.9$n.factors
EFA.9$sufficient.loadings

# All factors have items maximally loading to them
# However, Factors 8 and 9 have only one item
EFA.9$reliability.metrics
EFA.9$reliability.evaluation

# 22 items were removed 
EFA.9$removed.items

# TOP N ITEMS: EFA4 Model ---------------------------------------------------------------------
# TOP 3 ITEMS
top.n <- 3

EFA.7$loadings.long %>%
  fun_top.items(n.items = top.n) -> df_ablt.items

df_ablt.items

# EFA 5: RERUN EFA WITH TOP ITEMS ONLY ------------------------------------
n.facts <- 4

df_occupations.numeric %>% 
  select(df_ablt.items$Item) -> df_occupations.numeric.items

# AUTO-EFA
df_occupations.numeric.items %>%
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
  ) -> EFA.7.items

# Output
EFA.7.items$adequacy.tests
EFA.7.items$n.factors
EFA.7.items$sufficient.loadings

# All factors have items maximally loading to them
EFA.7.items$reliability.metrics
EFA.7.items$reliability.evaluation
# All factors still remain excellent/good with respect to internal consistency
# However, interitem correlation has increased

# No ability was removed
EFA.7.items$removed.items

# Interpreting factors
# Factor 1 is composed of cognitive/intellectual abilities (intelligence)
# Factor 2 is composed of technical/mechanical abilities (hands-on)
# Factor 3 is composed of social/managerial abilities (extroversion and agreeableness)

# Since all factors are consistent and meaningful, 
# it is safe to conclude EFA here.


