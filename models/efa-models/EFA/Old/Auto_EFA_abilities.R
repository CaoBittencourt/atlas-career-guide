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
    where(function(x){str_detect(attributes(x)$label, 'Abilities.')}) #Abilities only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.ablt


# EFA 1: Average N Factors ---------------------------------------------------------------------
# Use average suggested number of factors as starting point
df_occupations.numeric.ablt %>% 
  fun_nfactors.selection() %>% 
  filter(Criterion == 'Average') %>%
  pull(Factors.Suggested) -> n.facts

# AUTO-EFA
df_occupations.numeric.ablt %>%
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
    ) -> EFA_ablt.avg

# Output
EFA_ablt.avg$adequacy.tests
EFA_ablt.avg$n.factors
EFA_ablt.avg$sufficient.loadings

# Factor 7 doesn't have any variables maximally loading to it
EFA_ablt.avg$reliability.metrics
EFA_ablt.avg$reliability.evaluation

# Three abilities were removed
EFA_ablt.avg$removed.items

# Since Factor 7 was not needed in practice,
# we should rerun the analysis with 6 factors.
# EFA 2: 6 factors ---------------------------------------------------------------------
n.facts <- 6

# AUTO-EFA
df_occupations.numeric.ablt %>%
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
    ) -> EFA_ablt.6

# Output
EFA_ablt.6$adequacy.tests
EFA_ablt.6$n.factors
EFA_ablt.6$sufficient.loadings

EFA_ablt.6$reliability.metrics
EFA_ablt.6$reliability.evaluation

# Seven abilities were removed
EFA_ablt.6$removed.items

# Factor 6 has only two items in it.
# Factor 5 has lower consistency compared to other factors.
# In terms of interpretability, both factors could be
# reduced to the other factors

# Thus, we try the five factor model next.

# EFA 3: 5 factors ---------------------------------------------------------------------
n.facts <- 5

# AUTO-EFA
df_occupations.numeric.ablt %>%
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
    ) -> EFA_ablt.5

# Output
EFA_ablt.5$adequacy.tests
EFA_ablt.5$n.factors
EFA_ablt.5$sufficient.loadings

# All factors have items maximally loading to them
EFA_ablt.5$reliability.metrics
EFA_ablt.5$reliability.evaluation
# Internal consistency was substantially increased in the five factor model
# However, interitem correlation is too high
# This means some items are redundant and could be removed
# Also, Factor 5 has only two items loading to it.

# Four abilities were removed 
EFA_ablt.5$removed.items

# Aiming to obtain more robust factors, the four factor model is tried next.

# EFA 4: 4 factors ---------------------------------------------------------------------
n.facts <- 4

# AUTO-EFA
df_occupations.numeric.ablt %>%
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
    ) -> EFA_ablt.4

# Output
EFA_ablt.4$adequacy.tests
EFA_ablt.4$n.factors
EFA_ablt.4$sufficient.loadings

# All factors have items maximally loading to them
EFA_ablt.4$reliability.metrics
EFA_ablt.4$reliability.evaluation
# Internal consistency remains the same in the four factor model
# However, now the smallest factor has 6 items
# The factors are more interpretable as well.

# No abilities were removed 
EFA_ablt.4$removed.items

# Interpreting factors
# Factor 1 is composed of cognitive/intellectual abilities (intelligence)
# Factor 2 is composed of coordination/crafts-related abilities (dexterity)
# Factor 3 is composed of bodily abilities (body robustness and equilibrium)
# Factor 4 is composed of perceptual abilities (perception)

# These factors are consistent and interpretable.
# Furthermore, it seems grouping them together even further would be 
# artificial/counter-productive. We, thus, stop EFA at the four factor model.

# TOP N ITEMS: EFA4 Model ---------------------------------------------------------------------
# TOP 3 ITEMS
top.n <- 3

EFA_ablt.4$loadings.long %>%
  fun_top.items(n.items = top.n) -> df_ablt.items

df_ablt.items

# EFA 5: RERUN EFA WITH TOP ITEMS ONLY ------------------------------------
n.facts <- 4

df_occupations.numeric.ablt %>% 
  select(df_ablt.items$Item) -> df_occupations.numeric.ablt.items

# AUTO-EFA
df_occupations.numeric.ablt.items %>%
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
  ) -> EFA_ablt.4.items

# Output
EFA_ablt.4.items$adequacy.tests
EFA_ablt.4.items$n.factors
EFA_ablt.4.items$sufficient.loadings

# All factors have items maximally loading to them
EFA_ablt.4.items$reliability.metrics
EFA_ablt.4.items$reliability.evaluation
# All factors still remain excellent/good with respect to internal consistency
# However, interitem correlation has increased

# No ability was removed
EFA_ablt.4.items$removed.items

# Interpreting factors
# Factor 1 is composed of cognitive/intellectual abilities (intelligence)
# Factor 2 is composed of technical/mechanical abilities (hands-on)
# Factor 3 is composed of social/managerial abilities (extroversion and agreeableness)

# Since all factors are consistent and meaningful, 
# it is safe to conclude EFA here.


