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
    where(function(x){str_detect(attributes(x)$label, 'Work_Context.')}) #Work contexts only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.context

# GLOBAL EFA PARAMETERS FIRST TEST ---------------------------------------------------
# Number of factors
.auto_select.nfactors <- T
# int_nfactors.vector <- seq(1,5)

# Minimum factor size
# .int_min.factor_size.basic <- 2
.int_min.factor_size <- 3

# Top items
.int_n.items.total.context <- 15

# Rotation (Oblique)
.chr_rotation <- 'promax'
# .chr_rotation <- 'oblimin'
# Rotation (Orthogonal)
# .chr_rotation <- 'varimax'
.remove_unacceptable_MSAi.items <- T
# Underloadings and crossloadings
.remove_under_loading.items <- T
.remove_cross_loading.items <- F
.dbl_under_loading.threshold <- 0.4 #Lesser than 0.4 loading <- under loading
# .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
.dbl_cross_loading.threshold <- 0.35

# Diagrams and tests
.show_diagrams <- T
.show_results <- F

# FULLY AUTOMATED EFA WORKFLOW (WITH TOP ITEMS) --------------------------------------------
# Work Context
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.context
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.context
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
) -> EFA_Context

# FULLY AUTOMATED EFA WORKFLOW (ONLY STAGE ONE) --------------------------------------------
# Work Context
fun_best.model.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.context
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
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
) -> EFA_Context.1

# SUGGESTED NUMBER OF FACTORS ---------------------------------------------
fun_nfactors.selection(
  .df_numeric = df_occupations.numeric.context
)

# On average, selection criteria suggest 10 factors.
# Very Simple Structure criteria suggest 2 to 3 factors.

# RESULTS EVALUATION (BEST MODELS) ----------------------------------------
EFA_Context$best.models.evaluation
EFA_Context.1$best.models.evaluation

# All models are fairly consistent. 
# Two factors model has excellent reliability.

EFA_Context$all.models.evaluation

# The 4 and 6 factors models did not have sufficient loadings across all factors after top items selection.
EFA_Context$EFA.workflow$EFA.top.items$EFA.4Factors$sufficient.loadings
EFA_Context$EFA.workflow$EFA.top.items$EFA.6Factors$sufficient.loadings

# RESULTS EVALUATION (BEST MODEL = 2 FACTORS) ------------------------------------------------------
# Best model at each stage
EFA_Context.1$best.model$sufficient.loadings
EFA_Context$best.model$EFA.top.items$sufficient.loadings

# Two factors model is best both before and after selecting top items.

# Removed items at each stage
EFA_Context$best.model$EFA$removed.items
EFA_Context$best.model$EFA.top.items$removed.items

# 13 items were removed at stage 1 (none at stage 2) due to underloading.

# Removed items as % of original data set
length(EFA_Context$best.model$EFA$removed.items) / ncol(df_occupations.numeric.context)

# Internal consistency of factors
EFA_Context$best.model$EFA.top.items$reliability.evaluation

# Factors have very high internal consistency.
# Reliability averages at good to excellent.

# Factors interpretation
EFA_Context$best.model$top.items

# Factor 1 includes items related to physically hazardous work contexts (Physical Danger).
# Factor 2 includes items related to daily office / social activities (Social Adaptability).

# RESULTS EVALUATION (3 FACTORS MODEL) ------------------------------------------------------
EFA_Context$EFA.workflow$EFA$EFA$EFA.3Factors$sufficient.loadings
EFA_Context$EFA.workflow$EFA.top.items$EFA.3Factors$sufficient.loadings

# Sufficient loadings at both stages.

# Removed items at each stage
EFA_Context$EFA.workflow$EFA$EFA$EFA.3Factors$removed.items
EFA_Context$EFA.workflow$EFA.top.items$EFA.3Factors$removed.items

# 8 items were removed at stage 1 (none at stage 2) due to underloading.

# Removed items as % of original data set
length(EFA_Context$EFA.workflow$EFA$EFA$EFA.3Factors$removed.items) / ncol(df_occupations.numeric.context)

# Internal consistency of factors
EFA_Context$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation

# Factors have very high internal consistency.
# Reliability averages at good to excellent.

# Factors interpretation
EFA_Context$EFA.workflow$top.items$EFA.3Factors

# Factor 1 includes items related to physically hazardous work contexts (Physical Danger).
# Factor 2 includes items related to physically demanding work contexts (Physical Exertion).
# Factor 3 includes items related to daily office / social activities (Social Adaptability).





# RESULTS EVALUATION (5 FACTORS MODEL) ------------------------------------------------------
EFA_Context$EFA.workflow$EFA$EFA$EFA.5Factors$sufficient.loadings
EFA_Context$EFA.workflow$EFA.top.items$EFA.5Factors$sufficient.loadings

# Sufficient loadings at both stages.

# Removed items at each stage
EFA_Context$EFA.workflow$EFA$EFA$EFA.5Factors$removed.items
EFA_Context$EFA.workflow$EFA.top.items$EFA.5Factors$removed.items

# 3 items were removed at stage 1 (none at stage 2) due to underloading.

# Removed items as % of original data set
length(EFA_Context$EFA.workflow$EFA$EFA$EFA.5Factors$removed.items) / ncol(df_occupations.numeric.context)

# Internal consistency of factors
EFA_Context$EFA.workflow$EFA.top.items$EFA.5Factors$reliability.evaluation

# Factors 4 and 5 have somewhat questionable internal consistency.
# Other factors have internal consistency.
# Reliability averages at good to acceptable.

# Factors interpretation
EFA_Context$EFA.workflow$top.items$EFA.5Factors %>% view()

# Factor interpretation is somewhat "fuzzy".
# Interpret later.


