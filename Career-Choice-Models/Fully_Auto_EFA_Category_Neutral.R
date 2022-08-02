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
    where(function(x){str_detect(attributes(x)$label, '_Skill.')})
    , where(function(x){str_detect(attributes(x)$label, 'Abilities.')})
    , where(function(x){str_detect(attributes(x)$label, 'Knowledge.')})
    , where(function(x){str_detect(attributes(x)$label, 'Work_Context')})
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric

# GLOBAL EFA PARAMETERS FIRST TEST ---------------------------------------------------
# Number of factors
.auto_select.nfactors <- T
# int_nfactors.vector <- seq(1,5)

# Minimum factor size
# .int_min.factor_size.basic <- 2
.int_min.factor_size <- 5

# Top items
.int_n.items.total <- 50

# Rotation (Oblique)
.chr_rotation <- 'promax'
# .chr_rotation <- 'oblimin'
# Rotation (Orthogonal)
# .chr_rotation <- 'varimax'
.remove_unacceptable_MSAi.items <- T
# Underloadings and crossloadings
.remove_under_loading.items <- T
.remove_cross_loading.items <- T
.dbl_under_loading.threshold <- 0.4 #Lesser than 0.4 loading <- under loading
# .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
.dbl_cross_loading.threshold <- 0.35

# Diagrams and tests
.show_diagrams <- T
.show_results <- F

# PREPARATION TESTS -----------------------------------------------
fun_nfactors.selection(
  .df_numeric = df_occupations.numeric
)

fun_adequacy.tests(
  .df_numeric = df_occupations.numeric
)

# FULLY AUTOMATED EFA WORKFLOW (WITH TOP ITEMS) --------------------------------------------
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total
  , .chr_rotation = .chr_rotation
  , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
  # Underloadings and crossloadings
  , .remove_under_loading.items = .remove_under_loading.items
  , .remove_cross_loading.items = .remove_cross_loading.items
  , .dbl_under_loading.threshold = .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
  # Diagrams and tests
  , .show_diagrams = .show_diagrams
  , .show_results = .show_results
) -> EFA_All

# # FULLY AUTOMATED EFA WORKFLOW (ONLY STAGE ONE) --------------------------------------------
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size
#   , .chr_rotation = .chr_rotation
#   , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_All2
# 

EFA_All$best.models.evaluation %>% view
EFA_All$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation
EFA_All$EFA.workflow$top.items$EFA.3Factors %>% view()

  