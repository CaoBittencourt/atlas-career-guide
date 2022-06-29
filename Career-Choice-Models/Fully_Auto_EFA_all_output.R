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
    where(function(x){str_detect(attributes(x)$label, '_Skill')}) #All Skills only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.skill

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'Basic')}) #Basic Skills only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.basic

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'Cross')}) #Cross functional Skills only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.cross

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


# GLOBAL EFA PAREMETERS ---------------------------------------------------
# Number of factors
auto_select.nfactors <- T
# int_nfactors.min <- 1
# int_nfactors.max <- 5

# Minimum factor size
int_min.factor_size.basic <- 2
int_min.factor_size <- 3

# Top items
int_n.items.total.basic <- 5
int_n.items.total.cross <- 10

int_n.items.total.skill <- 15
int_n.items.total.ablt <- 20
int_n.items.total.know <- 15

# Rotation (Oblique)
chr_rotation <- 'promax'
# chr_rotation <- 'oblimin'
# chr_rotation <- 'varimax'
# Underloadings and crossloadings
remove_under_loading.items <- F
remove_cross_loading.items <- T
dbl_under_loading.threshold <- 0.4 #Lesser than 0.4 loading <- under loading
# dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
dbl_cross_loading.threshold <- 0.25
# Diagrams and tests
show_diagrams <- T
show_results <- F

# FULLY AUTOMATED EFA WORKFLOW --------------------------------------------
# # Basic Skills
# fun_best.model.multi.workflow(
#   # Basic
#   df_data.numeric = df_occupations.numeric.basic
#   , auto_select.nfactors = auto_select.nfactors
#   , int_nfactors.min = int_nfactors.min
#   , int_nfactors.max = int_nfactors.max
#   , int_min.factor_size = int_min.factor_size.basic
#   , int_n.items.total = int_n.items.total.basic
#   , chr_rotation = chr_rotation
#   # Underloadings and crossloadings
#   , remove_under_loading.items = remove_under_loading.items
#   , remove_cross_loading.items = remove_cross_loading.items
#   , dbl_under_loading.threshold = dbl_under_loading.threshold
#   , dbl_cross_loading.threshold = dbl_cross_loading.threshold
#   # Diagrams and tests
#   , show_diagrams = show_diagrams
#   , show_results = show_results
# ) -> EFA_Basic
# 
# # Cross Functional Skills
# fun_best.model.multi.workflow(
#   # Basic
#   df_data.numeric = df_occupations.numeric.cross
#   , auto_select.nfactors = auto_select.nfactors
#   , int_nfactors.min = int_nfactors.min
#   , int_nfactors.max = int_nfactors.max
#   , int_min.factor_size = int_min.factor_size.basic
#   , int_n.items.total = int_n.items.total.cross
#   , chr_rotation = chr_rotation
#   # Underloadings and crossloadings
#   , remove_under_loading.items = remove_under_loading.items
#   , remove_cross_loading.items = remove_cross_loading.items
#   , dbl_under_loading.threshold = dbl_under_loading.threshold
#   , dbl_cross_loading.threshold = dbl_cross_loading.threshold
#   # Diagrams and tests
#   , show_diagrams = show_diagrams
#   , show_results = show_results
# ) -> EFA_Cross


# All Skills
fun_best.model.multi.workflow(
  # Basic
  df_data.numeric = df_occupations.numeric.skill
  , auto_select.nfactors = auto_select.nfactors
  , int_nfactors.min = int_nfactors.min
  , int_nfactors.max = int_nfactors.max
  , int_min.factor_size = int_min.factor_size
  , int_n.items.total = int_n.items.total.skill
  , chr_rotation = chr_rotation
  # Underloadings and crossloadings
  , remove_under_loading.items = remove_under_loading.items
  , remove_cross_loading.items = remove_cross_loading.items
  , dbl_under_loading.threshold = dbl_under_loading.threshold
  , dbl_cross_loading.threshold = dbl_cross_loading.threshold
  # Diagrams and tests
  , show_diagrams = show_diagrams
  , show_results = show_results
) -> EFA_Skill


# Abilities
fun_best.model.multi.workflow(
  # Basic
  df_data.numeric = df_occupations.numeric.ablt
  , auto_select.nfactors = auto_select.nfactors
  , int_min.factor_size = int_min.factor_size
  , int_n.items.total = int_n.items.total.ablt
  , chr_rotation = chr_rotation
  # Underloadings and crossloadings
  , remove_under_loading.items = remove_under_loading.items
  , remove_cross_loading.items = remove_cross_loading.items
  , dbl_under_loading.threshold = dbl_under_loading.threshold
  , dbl_cross_loading.threshold = dbl_cross_loading.threshold
  # Diagrams and tests
  , show_diagrams = show_diagrams
  , show_results = show_results
) -> EFA_Ablt

# Knowledge
fun_best.model.multi.workflow(
  # Basic
  df_data.numeric = df_occupations.numeric.know
  , auto_select.nfactors = auto_select.nfactors
  , int_min.factor_size = int_min.factor_size
  , int_n.items.total = int_n.items.total.know
  , chr_rotation = chr_rotation
  # Underloadings and crossloadings
  , remove_under_loading.items = remove_under_loading.items
  , remove_cross_loading.items = remove_cross_loading.items
  , dbl_under_loading.threshold = dbl_under_loading.threshold
  , dbl_cross_loading.threshold = dbl_cross_loading.threshold
  # Diagrams and tests
  , show_diagrams = show_diagrams
  , show_results = show_results
) -> EFA_Know


# OUTPUT ------------------------------------------------------------------
# Top items in the "best" (most internally consistent) model 

# EFA_Basic$best.model$top.items$Item -> chr_Basic.Items
# EFA_Cross$best.model$top.items$Item -> chr_Cross.Items

EFA_Ablt$best.model$top.items$Item -> chr_Ablt.Items
EFA_Skill$best.model$top.items$Item -> chr_Skill.Items
EFA_Know$best.model$top.items$Item -> chr_Know.Items

# # MOST INTERPRETABLE CONSISTENT MODELS ------------------------------------
# EFA_Skill$best.models.evaluation %>% view()
# EFA_Ablt$best.models.evaluation %>% view()
# EFA_Know$best.models.evaluation %>% view()
# 
# 
# EFA_Skill$EFA.workflow$top.items$EFA.2Factors$Item -> chr_Skill.Items #Only one model meets exclusion criteria
# 
# EFA_Ablt$EFA.workflow$top.items$EFA.1Factor #Left out cognitive abilities
# EFA_Ablt$EFA.workflow$top.items$EFA.2Factor 
# EFA_Ablt$EFA.workflow$top.items$EFA.3Factor
# EFA_Ablt$EFA.workflow$top.items$EFA.4Factor$Item -> chr_Ablt.Items #Most nuanced and yet consistent model
# 
# EFA_Know$EFA.workflow$top.items$EFA.2Factors$Item -> chr_Know.Items #Only one model meets exclusion criteria
