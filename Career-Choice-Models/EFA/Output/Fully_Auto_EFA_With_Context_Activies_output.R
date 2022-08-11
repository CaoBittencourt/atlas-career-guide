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

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'Work_Activities.')}) #Work activities only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.activities

# VARIANCE PROPORTIONALITY ------------------------------------------------
df_occupations %>%
  select(
    colnames(
      df_occupations.numeric.skill
    )
    , colnames(
      df_occupations.numeric.ablt
    )
    , colnames(
      df_occupations.numeric.know
    )
    , colnames(
      df_occupations.numeric.context
    )
    , colnames(
      df_occupations.numeric.activities
    )
  ) %>%
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric

df_occupations.numeric %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_occupations.var.total

df_occupations.numeric.skill %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_skills.var.total

df_occupations.numeric.ablt %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_ablt.var.total

df_occupations.numeric.know %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_know.var.total

df_occupations.numeric.context %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_context.var.total

df_occupations.numeric.activities %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_activities.var.total

# Variance proportionality (how much each category contributes to total variance)
sum(
  dbl_skills.var.total
  , dbl_ablt.var.total
  , dbl_know.var.total
  , dbl_context.var.total
  , dbl_activities.var.total
) == dbl_occupations.var.total

dbl_skills.var.pct <- dbl_skills.var.total / dbl_occupations.var.total
dbl_ablt.var.pct <- dbl_ablt.var.total / dbl_occupations.var.total
dbl_know.var.pct <- dbl_know.var.total / dbl_occupations.var.total
dbl_context.var.pct <- dbl_context.var.total / dbl_occupations.var.total
dbl_activities.var.pct <- dbl_activities.var.total / dbl_occupations.var.total

# Define number of items in the questionnaire
# dbl_items.total <- 50
# dbl_items.total <- 30
# dbl_items.total <- 32
# dbl_items.total <- 40
dbl_items.total <- 60

# Pick N items from each category in proportion to total variability
dbl_skills.items <- dbl_skills.var.pct * dbl_items.total
dbl_ablt.items <- dbl_ablt.var.pct * dbl_items.total
dbl_know.items <- dbl_know.var.pct * dbl_items.total
dbl_context.items <- dbl_context.var.pct * dbl_items.total
dbl_activities.items <- dbl_activities.var.pct * dbl_items.total

dbl_skills.items <- round(dbl_skills.items)
dbl_ablt.items <- round(dbl_ablt.items)
dbl_know.items <- round(dbl_know.items)
dbl_context.items <- round(dbl_context.items)
dbl_activities.items <- round(dbl_activities.items)

dbl_skills.items
dbl_ablt.items
dbl_know.items
dbl_context.items
dbl_activities.items

sum(
  dbl_skills.items
  , dbl_ablt.items
  , dbl_know.items
  , dbl_context.items
  , dbl_activities.items
)

# ITEMS PER CATEGORY PARAMETERS 1 -------------------------------------------
# Manually define number of items
.int_n.items.total.skill <- 6
.int_n.items.total.ablt <- 12
.int_n.items.total.know <- 12
.int_n.items.total.context <- 15
.int_n.items.total.activities <- 15

# ITEMS PER CATEGORY PARAMETERS 2 -------------------------------------------
# Manually define number of items
.int_n.items.total.skill <- 6
.int_n.items.total.ablt <- 9
.int_n.items.total.know <- 12
.int_n.items.total.context <- 21
.int_n.items.total.activities <- 12

# # GLOBAL EFA PARAMETERS 1 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# # int_nfactors.vector <- seq(1,5)
# 
# # Minimum factor size
# .int_min.factor_size.basic <- 2
# .int_min.factor_size <- 3
# 
# # Top items
# .int_n.items.total.basic <- 5
# .int_n.items.total.cross <- 10
# 
# .int_n.items.total.skill <- 15
# .int_n.items.total.ablt <- 20
# .int_n.items.total.know <- 15
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T

# # Underloadings and crossloadings
# .remove_under_loading.items <- F
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.25
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # GLOBAL EFA PARAMETERS 2 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# # int_nfactors.vector <- seq(1,5)
# 
# # Minimum factor size
# .int_min.factor_size.basic <- 2
# .int_min.factor_size <- 3
# 
# # Top items
# .int_n.items.total.basic <- 5
# .int_n.items.total.cross <- 10
# 
# .int_n.items.total.skill <- 15
# .int_n.items.total.ablt <- 20
# .int_n.items.total.know <- 15
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.3 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.3
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # GLOBAL EFA PARAMETERS 3 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# # int_nfactors.vector <- seq(1,5)
# 
# # Minimum factor size
# # .int_min.factor_size.basic <- 2
# .int_min.factor_size <- 3
# 
# # Top items
# # .int_n.items.total.basic <- 5
# # .int_n.items.total.cross <- 10
# 
# .int_n.items.total.skill <- 14
# .int_n.items.total.ablt <- 16
# .int_n.items.total.know <- 12
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.3 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # *GLOBAL EFA PARAMETERS 4* ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# # int_nfactors.vector <- seq(1,5)
# 
# # Minimum factor size
# # .int_min.factor_size.basic <- 2
# .int_min.factor_size <- 3
# 
# # Top items
# # .int_n.items.total.basic <- 5
# # .int_n.items.total.cross <- 10
# 
# .int_n.items.total.skill <- 6
# .int_n.items.total.ablt <- 12
# .int_n.items.total.know <- 12
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.3 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # GLOBAL EFA PARAMETERS 5 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# # int_nfactors.vector <- seq(1,5)
# 
# # Minimum factor size
# # .int_min.factor_size.basic <- 2
# .int_min.factor_size <- 3
# 
# # Top items
# # .int_n.items.total.basic <- 5
# # .int_n.items.total.cross <- 10
# 
# .int_n.items.total.skill <- 8
# .int_n.items.total.ablt <- 12 #don't edit
# .int_n.items.total.know <- 12
# .int_n.items.total.context <- 12
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # GLOBAL EFA PARAMETERS 6 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# # int_nfactors.vector <- seq(1,5)
# 
# # Minimum factor size
# # .int_min.factor_size.basic <- 2
# .int_min.factor_size <- 3
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T
 
# # GLOBAL EFA PARAMETERS 7 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# 
# # Minimum factor size
# .int_min.factor_size <- 3
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4
# .dbl_cross_loading.threshold <- 0.2
# 
# # Diagrams and tests
# .show_diagrams <- F
# .show_results <- T

# GLOBAL EFA PARAMETERS 8 ---------------------------------------------------
# Number of factors
.auto_select.nfactors <- T

# Minimum factor size
.int_min.factor_size <- 3

# Rotation (Oblique)
.chr_rotation <- 'promax'
# .chr_rotation <- 'oblimin'
# Rotation (Orthogonal)
# .chr_rotation <- 'varim ax'
.remove_unacceptable_MSAi.items <- T
# Underloadings and crossloadings
.remove_under_loading.items <- T
.remove_cross_loading.items <- T
.dbl_under_loading.threshold <- 0.4
.dbl_cross_loading.threshold <- 0.3

# Diagrams and tests
.show_diagrams <- F
.show_results <- T

# # GLOBAL EFA PARAMETERS 9 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# 
# # Minimum factor size
# .int_min.factor_size <- 3
# 
# # Rotation (Oblique)
# .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# # .chr_rotation <- 'varim ax'
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- F
# .show_results <- T

# FULLY AUTOMATED EFA WORKFLOW (WITH TOP ITEMS) --------------------------------------------
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.skill
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.skill
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
) -> EFA_Skill

# Abilities
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.ablt
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.ablt
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
) -> EFA_Ablt

# Knowledge
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.know
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.know
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
) -> EFA_Know

# Work context
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

# Work activities
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.activities
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.activities
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
) -> EFA_Activities

# FULLY AUTOMATED EFA WORKFLOW (ONLY STAGE ONE) --------------------------------------------
# All Skills
fun_best.model.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.skill
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
) -> EFA_Skill.1

# Abilities
fun_best.model.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.ablt
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
) -> EFA_Ablt.1

# Knowledge
fun_best.model.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.know
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
) -> EFA_Know.1

# Work context
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

# Work activities
fun_best.model.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.activities
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
) -> EFA_Activities.1

# COMPARING ONE STAGE WITH TWO STAGE EFA --------------------------------
# Skills
EFA_Skill$best.model$EFA.top.items$reliability.evaluation
EFA_Skill.1$best.model$reliability.evaluation

EFA_Skill$best.models.evaluation %>% view()
EFA_Skill.1$best.models.evaluation %>% view()

EFA_Skill$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_Skill$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()

# Abilities
EFA_Ablt$best.model$EFA.top.items$reliability.evaluation
EFA_Ablt.1$best.model$reliability.evaluation

EFA_Ablt$best.models.evaluation %>% view()
EFA_Ablt.1$best.models.evaluation %>% view()

EFA_Ablt$best.model$EFA.top.items$reliability.evaluation %>% view()
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation %>% view()

# Fields of Knowledge
EFA_Know$best.model$EFA.top.items$reliability.evaluation
EFA_Know.1$best.model$reliability.evaluation

EFA_Know$best.models.evaluation %>% view()
EFA_Know$all.models.evaluation %>% view()
EFA_Know.1$best.models.evaluation %>% view()

EFA_Know$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_Know$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()
EFA_Know$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation %>% view()

# Work context
EFA_Context$best.model$EFA.top.items$reliability.evaluation
EFA_Context.1$best.model$reliability.evaluation

EFA_Context$best.models.evaluation %>% view()
EFA_Context.1$best.models.evaluation %>% view()

EFA_Context$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_Context$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()

# Work activities
EFA_Activities$best.model$EFA.top.items$reliability.evaluation
EFA_Activities.1$best.model$reliability.evaluation

EFA_Activities$best.models.evaluation %>% view()
EFA_Activities.1$best.models.evaluation %>% view()

EFA_Activities$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_Activities$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()

# FACTOR CORRELATION AND REDUNDANCY --------------------------------
# Skills
EFA_Skill$best.model$EFA.top.items$factor.correlation
EFA_Skill.1$best.model$factor.correlation

EFA_Skill$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_Skill$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation

# Abilities
EFA_Ablt$best.model$EFA.top.items$factor.correlation
EFA_Ablt.1$best.model$factor.correlation

EFA_Ablt$best.model$EFA.top.items$factor.correlation
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.4Factors$factor.correlation

# Fields of Knowledge
EFA_Know$best.model$EFA.top.items$factor.correlation
EFA_Know.1$best.model$factor.correlation

EFA_Know$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_Know$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation
EFA_Know$EFA.workflow$EFA.top.items$EFA.4Factors$factor.correlation

# Work context
EFA_Context$best.model$EFA.top.items$factor.correlation
EFA_Context.1$best.model$factor.correlation

EFA_Context$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_Context$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation

# Work activities
EFA_Activities$best.model$EFA.top.items$factor.correlation
EFA_Activities.1$best.model$factor.correlation

EFA_Activities$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_Activities$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation

# SUGGESTED ROTATION --------------------------------
# Skills
EFA_Skill$best.model$EFA.top.items$suggested.rotation
EFA_Skill.1$best.model$suggested.rotation

EFA_Skill$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_Skill$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation

# Abilities
EFA_Ablt$best.model$EFA.top.items$suggested.rotation
EFA_Ablt.1$best.model$suggested.rotation

EFA_Ablt$best.model$EFA.top.items$suggested.rotation
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation
EFA_Ablt$EFA.workflow$EFA.top.items$EFA.4Factors$suggested.rotation

# Fields of Knowledge
EFA_Know$best.model$EFA.top.items$suggested.rotation
EFA_Know.1$best.model$suggested.rotation

EFA_Know$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_Know$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation
EFA_Know$EFA.workflow$EFA.top.items$EFA.4Factors$suggested.rotation

# Work context
EFA_Context$best.model$EFA.top.items$suggested.rotation
EFA_Context.1$best.model$suggested.rotation

EFA_Context$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_Context$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation

# Work activities
EFA_Activities$best.model$EFA.top.items$suggested.rotation
EFA_Activities.1$best.model$suggested.rotation

EFA_Activities$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_Activities$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation

# TOP ITEMS ---------------------------------------------------------------
# Skills
# EFA_Skill$best.model$top.items %>% view()
EFA_Skill$EFA.workflow$top.items$EFA.2Factors %>% view()
# EFA_Skill$EFA.workflow$top.items$EFA.3Factors %>% view()

# The two factors model is most reliable and most interpretable.

# Abilities
# EFA_Ablt$best.model$top.items %>% view()
# EFA_Ablt$EFA.workflow$top.items$EFA.2Factors %>% view()
EFA_Ablt$EFA.workflow$top.items$EFA.3Factors %>% view()
EFA_Ablt$EFA.workflow$top.items$EFA.4Factors %>% view()

# All models are highly reliable. However, the four factors is most interpretable.

# Knowledge
# EFA_Know$best.model$top.items %>% view()
# EFA_Know$EFA.workflow$top.items$EFA.3Factors %>% view()
EFA_Know$EFA.workflow$top.items$EFA.4Factors %>% view()

# Work context
# EFA_Context$best.model$top.items %>% view()
EFA_Context$EFA.workflow$top.items$EFA.3Factors %>% view()

# Both the two and three factors models are internally consistent.
# However, the three factors model is more interpretable and has the correct amount of items (15).

# Work activities
# EFA_Activities$EFA.workflow$top.items$EFA.2Factors %>% view()
EFA_Activities$EFA.workflow$top.items$EFA.3Factors %>% view()

# Both the two and three factors models are internally consistent.
# However, the three factors model is more interpretable and has the correct amount of items (15).

# OUTPUT ------------------------------------------------------------------
# Revised and selected models
# Skills => 2 factors
chr_Skill.Items <- EFA_Skill$EFA.workflow$top.items$EFA.2Factors$Item
# Abilities => 4 factors
chr_Ablt.Items <- EFA_Ablt$EFA.workflow$top.items$EFA.4Factors$Item 
# Fields of Knowledge => 4 factors
chr_Know.Items <- EFA_Know$EFA.workflow$top.items$EFA.4Factors$Item
# Work Contexts => 3 factors
chr_Context.Items <- EFA_Context$EFA.workflow$top.items$EFA.3Factors$Item
# Work Activities => 3 factors
chr_Activities.Items <- EFA_Activities$EFA.workflow$top.items$EFA.3Factors$Item
