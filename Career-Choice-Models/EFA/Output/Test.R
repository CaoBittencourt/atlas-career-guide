# ----- SETUP -----------------------------------------------------------
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

# ORIGINAL DATA SET --------------------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# POPULATION-WEIGHTED DATA SET -------------------------------------------------------
# Random number of people employed in each occupation
runif(nrow(df_occupations), 1, 100) %>% 
  round() -> int_people

# Population-weighted data set
# start <- proc.time()

df_occupations %>% 
  mutate(
    row.repeat = int_people
  ) %>% 
  group_by(occupation) %>% 
  slice(rep(1:n(), first(row.repeat))) %>% 
  select(-row.repeat) %>% 
  ungroup() -> df_occupations

# end <- proc.time()
# (end - start)[3]

# CATEGORICAL DATA SETS --------------------------------------------------------------------
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
    where(function(x){str_detect(attributes(x)$label, '_skill')}) #All Skills only
    , -ends_with('.i') #Using recommended levels
    # , -ends_with('.l') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.skill

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'abilities.')}) #Abilities only
    , -ends_with('.i') #Using recommended levels
    # , -ends_with('.l') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.ablt

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'knowledge.')}) #Knowledge only
    , -ends_with('.i') #Using recommended levels
    # , -ends_with('.l') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.know

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'work_context.')}) #Work contexts only
    , -ends_with('.i') #Using recommended levels
    # , -ends_with('.l') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.context

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'work_activities.')}) #Work activities only
    , -ends_with('.i') #Using recommended levels
    # , -ends_with('.l') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.activities

# ----- PARAMETERS -----------------------------------------------------------
# VARIANCE PROPORTIONALITY  ------------------------------------------------
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
dbl_occupations.var.total

sum(
  dbl_skills.var.total
  , dbl_ablt.var.total
  , dbl_know.var.total
  , dbl_context.var.total
  , dbl_activities.var.total
)

dbl_skills.var.pct <- dbl_skills.var.total / dbl_occupations.var.total
dbl_ablt.var.pct <- dbl_ablt.var.total / dbl_occupations.var.total
dbl_know.var.pct <- dbl_know.var.total / dbl_occupations.var.total
dbl_context.var.pct <- dbl_context.var.total / dbl_occupations.var.total
dbl_activities.var.pct <- dbl_activities.var.total / dbl_occupations.var.total

dbl_skills.var.pct %>% round(4)
dbl_ablt.var.pct %>% round(4)
dbl_know.var.pct %>% round(4)
dbl_context.var.pct %>% round(4)
dbl_activities.var.pct %>% round(4)

# Define number of items in the questionnaire
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

# VARIANCE PROPORTIONALITY PLOTS  ------------------------------------------------
df_occupations.numeric %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  )  %>% 
  pivot_longer(
    cols = everything()
  )  %>% 
  ggplot(
    aes(
      y = fct_reorder(name, value) 
      , x = value
    )
  ) +
  geom_col() + 
  scale_x_continuous(
    limits = c(0,0.15)
    )

df_occupations.numeric.skill %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  pivot_longer(
    cols = everything()
  ) %>% 
  ggplot(
    aes(
      y = fct_reorder(name, value) 
      , x = value
    )
  ) + 
  geom_col() + 
  scale_x_continuous(
    limits = c(0,0.15)
  )

df_occupations.numeric.ablt %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  ggplot(
    aes(
      y = fct_reorder(name, value) 
      , x = value
    )
  ) + 
  geom_col() + 
  scale_x_continuous(
    limits = c(0,0.15)
  )

df_occupations.numeric.know %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  ggplot(
    aes(
      y = fct_reorder(name, value) 
      , x = value
    )
  ) + 
  geom_col() + 
  scale_x_continuous(
    limits = c(0,0.15)
  )

df_occupations.numeric.context %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  ggplot(
    aes(
      y = fct_reorder(name, value) 
      , x = value
    )
  ) + 
  geom_col() + 
  scale_x_continuous(
    limits = c(0,0.15)
  )

df_occupations.numeric.activities %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  ggplot(
    aes(
      y = fct_reorder(name, value) 
      , x = value
    )
  ) + 
  geom_col() + 
  scale_x_continuous(
    limits = c(0,0.15)
  )

# # ITEMS PER CATEGORY PARAMETERS 1 -------------------------------------------
# # Manually define number of items
# .int_n.items.total.skill <- 6
# .int_n.items.total.ablt <- 12
# .int_n.items.total.know <- 12
# .int_n.items.total.context <- 15
# .int_n.items.total.activities <- 15

# # ITEMS PER CATEGORY PARAMETERS 2 -------------------------------------------
# # Manually define number of items
# .int_n.items.total.skill <- 6
# .int_n.items.total.ablt <- 9
# .int_n.items.total.know <- 12
# .int_n.items.total.context <- 21
# .int_n.items.total.activities <- 12

# ITEMS PER CATEGORY PARAMETERS 3 -------------------------------------------
# Manually define number of items
.int_n.items.total.skill <- 8
# .int_n.items.total.skill <- 6
.int_n.items.total.ablt <- 9
.int_n.items.total.know <- 16
# .int_n.items.total.context <- 18
# .int_n.items.total.context <- 15
.int_n.items.total.context <- 12
# .int_n.items.total.activities <- 9 #no
# .int_n.items.total.activities <- 12 #no
.int_n.items.total.activities <- 15 #good

# # FACTOR ROTATIONS 1 ---------------------------------------------------------
# .chr_rotation.skill <- 'promax'
# .chr_rotation.ablt <- 'promax'
# .chr_rotation.know <- 'promax'
# .chr_rotation.context <- 'promax'
# .chr_rotation.activities <- 'promax'

# FACTOR ROTATIONS 2 ---------------------------------------------------------
.chr_rotation.skill <- 'promax'
.chr_rotation.ablt <- 'promax'
.chr_rotation.know <- 'promax'
.chr_rotation.context <- 'varimax'
.chr_rotation.activities <- 'promax'

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
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4
# .dbl_cross_loading.threshold <- 0.2
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # GLOBAL EFA PARAMETERS 8 * ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# 
# # Minimum factor size
# .int_min.factor_size <- 3
# 
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4
# .dbl_cross_loading.threshold <- 0.3
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # GLOBAL EFA PARAMETERS 9 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# 
# # Minimum factor size
# .int_min.factor_size <- 3
# 
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T
# 
# # GLOBAL EFA PARAMETERS 10 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# 
# # Minimum factor size
# .int_min.factor_size <- 3
# 
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.4
# .dbl_cross_loading.threshold <- 0.4
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- F
# 
# # GLOBAL EFA PARAMETERS 11 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# 
# # Minimum factor size
# .int_min.factor_size <- 3
# 
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- F
# .dbl_under_loading.threshold <- 0.4
# .dbl_cross_loading.threshold <- 0.3
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# # GLOBAL EFA PARAMETERS 12 ---------------------------------------------------
# # Number of factors
# .auto_select.nfactors <- T
# 
# # Minimum factor size
# .int_min.factor_size <- 3
# 
# .remove_unacceptable_MSAi.items <- T
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- F
# .dbl_under_loading.threshold <- 0.5
# .dbl_cross_loading.threshold <- 0.3
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- T

# * GLOBAL EFA PARAMETERS 13 * ---------------------------------------------------
# Number of factors
.auto_select.nfactors <- T

# Minimum factor size
.int_min.factor_size <- 3

.remove_unacceptable_MSAi.items <- T
# Underloadings and crossloadings
.remove_under_loading.items <- T
.remove_cross_loading.items <- T
.dbl_under_loading.threshold <- 0.5
.dbl_cross_loading.threshold <- 0.2

# Diagrams and tests
.show_diagrams <- T
.show_results <- T

# ----- EFA ---------------------------------------------------------------
# # FULLY AUTOMATED EFA WORKFLOW (ONLY STAGE ONE) --------------------------------------------
# # Skills
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.skill
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size
#   , .chr_rotation = .chr_rotation.skill
#   , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_skill.1
# 
# # Abilities
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.ablt
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size
#   , .chr_rotation = .chr_rotation.ablt
#   , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_ablt.1
# 
# # Knowledge
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.know
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size
#   , .chr_rotation = .chr_rotation.know
#   , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_know.1
# 
# # Work context
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.context
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size
#   , .chr_rotation = .chr_rotation.context
#   , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_context.1
# 
# # Work activities
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.activities
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size
#   , .chr_rotation = .chr_rotation.activities
#   , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_activities.1

# FULLY AUTOMATED EFA WORKFLOW (WITH TOP ITEMS) --------------------------------------------
# Skills
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.skill
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.skill
  , .chr_rotation = .chr_rotation.skill
  , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
  # Underloadings and crossloadings
  , .remove_under_loading.items = .remove_under_loading.items
  , .remove_cross_loading.items = .remove_cross_loading.items
  , .dbl_under_loading.threshold = .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
  # Diagrams and tests
  , .show_diagrams = .show_diagrams
  , .show_results = .show_results
) -> EFA_skill

# Abilities
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.ablt
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.ablt
  , .chr_rotation = .chr_rotation.ablt
  , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
  # Underloadings and crossloadings
  , .remove_under_loading.items = .remove_under_loading.items
  , .remove_cross_loading.items = .remove_cross_loading.items
  , .dbl_under_loading.threshold = .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
  # Diagrams and tests
  , .show_diagrams = .show_diagrams
  , .show_results = .show_results
) -> EFA_ablt

# Knowledge
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.know
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.know
  , .chr_rotation = .chr_rotation.know
  , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
  # Underloadings and crossloadings
  , .remove_under_loading.items = .remove_under_loading.items
  , .remove_cross_loading.items = .remove_cross_loading.items
  , .dbl_under_loading.threshold = .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
  # Diagrams and tests
  , .show_diagrams = .show_diagrams
  , .show_results = .show_results
) -> EFA_know

# Work context
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.context
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.context
  , .chr_rotation = .chr_rotation.context
  , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
  # Underloadings and crossloadings
  , .remove_under_loading.items = .remove_under_loading.items
  , .remove_cross_loading.items = .remove_cross_loading.items
  , .dbl_under_loading.threshold = .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
  # Diagrams and tests
  , .show_diagrams = .show_diagrams
  , .show_results = .show_results
) -> EFA_context

# Work activities
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.activities
  , .auto_select.nfactors = .auto_select.nfactors
  , .int_min.factor_size = .int_min.factor_size
  , .int_n.items.total = .int_n.items.total.activities
  , .chr_rotation = .chr_rotation.activities
  , .remove_unacceptable_MSAi.items = .remove_unacceptable_MSAi.items
  # Underloadings and crossloadings
  , .remove_under_loading.items = .remove_under_loading.items
  , .remove_cross_loading.items = .remove_cross_loading.items
  , .dbl_under_loading.threshold = .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
  # Diagrams and tests
  , .show_diagrams = .show_diagrams
  , .show_results = .show_results
) -> EFA_activities

# # FIX FIELDS OF KNOWLEDGE EFA --------------------------------------------
# # Knowledge
# fun_best.model.top.items.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.know
#   , .auto_select.nfactors = T
#   , .int_min.factor_size = 3
#   # , .int_n.items.total = 12
#   , .int_n.items.total = 16
#   , .chr_rotation = 'promax'
#   # , .chr_rotation = 'varimax'
#   # promax > varimax
#   , .remove_unacceptable_MSAi.items = T
#   # Underloadings and crossloadings
#   # , .remove_under_loading.items = T
#   , .remove_under_loading.items = T
#   , .remove_cross_loading.items = T
#   # , .dbl_under_loading.threshold = 0.45 #no
#   # , .dbl_under_loading.threshold = 0.4 #no
#   # , .dbl_under_loading.threshold = 0.6 #no
#   # , .dbl_under_loading.threshold = 0.7 #no
#   , .dbl_under_loading.threshold = 0.5
#   , .dbl_cross_loading.threshold = 0.2
#   # Diagrams and tests
#   , .show_diagrams = T
#   , .show_results = T
# ) -> EFA_know
# 
# # Internal consistency
# EFA_know$best.model$EFA.top.items$reliability.evaluation
# 
# EFA_know$best.models.evaluation %>% view()
# EFA_know$all.models.evaluation %>% view()
# 
# EFA_know$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
# EFA_know$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()
# EFA_know$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation %>% view()
# EFA_know$EFA.workflow$EFA.top.items$EFA.5Factors$reliability.evaluation %>% view()
# 
# # Rotation
# EFA_know$best.model$EFA.top.items$suggested.rotation
# 
# EFA_know$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
# EFA_know$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation
# EFA_know$EFA.workflow$EFA.top.items$EFA.4Factors$suggested.rotation
# 
# # Top items
# EFA_know$EFA.workflow$top.items$EFA.2Factors %>% view()
# EFA_know$EFA.workflow$top.items$EFA.3Factors %>% view()
# EFA_know$EFA.workflow$top.items$EFA.4Factors %>% view()
# EFA_know$EFA.workflow$top.items$EFA.5Factors %>% view()
# 
# # Removed items
# EFA_know$EFA.workflow$EFA.top.items$EFA.4Factors$removed.items
# EFA_know$EFA.workflow$EFA$EFA$EFA.4Factors$removed.items
# EFA_know$EFA.workflow$EFA$EFA$EFA.4Factors$cross_loading.items
# EFA_know$EFA.workflow$EFA$EFA$EFA.4Factors$under_loading.items
# EFA_know$EFA.workflow$EFA$EFA$EFA.4Factors$loadings.long %>% view

# FIX WORK CONTEXTS EFA --------------------------------------------
# Work contexts
fun_best.model.top.items.workflow(
  # Basic
  .df_data.numeric = df_occupations.numeric.context
  , .auto_select.nfactors = T
  , .int_min.factor_size = 3
  # , .int_n.items.total = 24
  # , .int_n.items.total = 24 #shit
  # , .int_n.items.total = 21 #works well with lower thresholds
  # , .int_n.items.total = 18 #doesn't work with knowledge-optimizing parameters
  , .int_n.items.total = 12
  # , .int_n.items.total = 15 #no
  # , .chr_rotation = 'promax'
  , .chr_rotation = 'varimax'
  # promax > varimax
  , .remove_unacceptable_MSAi.items = T
  # Underloadings and crossloadings
  , .remove_under_loading.items = T
  , .remove_cross_loading.items = T
  # , .dbl_under_loading.threshold = 0.4 #no
  , .dbl_under_loading.threshold = 0.5 #optimizes fields of knowledge
  , .dbl_cross_loading.threshold = 0.2 #optimizes fields of knowledge
  # Diagrams and tests
  , .show_diagrams = T
  , .show_results = T
) -> EFA_context

# Internal consistency
EFA_context$best.model$EFA.top.items$reliability.evaluation

EFA_context$best.models.evaluation %>% view()
EFA_context$all.models.evaluation %>% view()

EFA_context$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_context$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()
EFA_context$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation %>% view()
EFA_context$EFA.workflow$EFA.top.items$EFA.5Factors$reliability.evaluation %>% view()

# Rotation
EFA_context$best.model$EFA.top.items$suggested.rotation

EFA_context$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_context$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation
EFA_context$EFA.workflow$EFA.top.items$EFA.4Factors$suggested.rotation

# Top items
EFA_context$EFA.workflow$top.items$EFA.2Factors %>% view()
EFA_context$EFA.workflow$top.items$EFA.3Factors %>% view()
EFA_context$EFA.workflow$top.items$EFA.4Factors %>% view()
EFA_context$EFA.workflow$top.items$EFA.5Factors %>% view()

# Removed items
EFA_context$EFA.workflow$EFA.top.items$EFA.4Factors$removed.items
EFA_context$EFA.workflow$EFA$EFA$EFA.4Factors$removed.items
EFA_context$EFA.workflow$EFA$EFA$EFA.4Factors$cross_loading.items
EFA_context$EFA.workflow$EFA$EFA$EFA.4Factors$under_loading.items
EFA_context$EFA.workflow$EFA$EFA$EFA.4Factors$loadings.long %>% view

# # FIX WORK ACTIVITIES EFA --------------------------------------------
# # Work contexts
# fun_best.model.top.items.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.activities
#   , .auto_select.nfactors = T
#   , .int_min.factor_size = 3
#   , .int_n.items.total = 15 #works best
#   # , .int_n.items.total = 12 #no
#   # , .int_n.items.total = 9 #no
#   , .chr_rotation = 'promax'
#   # , .chr_rotation = 'varimax'
#   # promax > varimax
#   , .remove_unacceptable_MSAi.items = T
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = T
#   , .remove_cross_loading.items = T
#   # , .dbl_under_loading.threshold = 0.4 #no
#   , .dbl_under_loading.threshold = 0.5 #optimizes fields of knowledge
#   , .dbl_cross_loading.threshold = 0.2 #optimizes fields of knowledge
#   # Diagrams and tests
#   , .show_diagrams = T
#   , .show_results = T
# ) -> EFA_activities
# 
# # Internal consistency
# EFA_activities$best.model$EFA.top.items$reliability.evaluation
# 
# EFA_activities$best.models.evaluation %>% view()
# EFA_activities$all.models.evaluation %>% view()
# 
# EFA_activities$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
# EFA_activities$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()
# EFA_activities$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation %>% view()
# EFA_activities$EFA.workflow$EFA.top.items$EFA.5Factors$reliability.evaluation %>% view()
# 
# # Rotation
# EFA_activities$best.model$EFA.top.items$suggested.rotation
# 
# EFA_activities$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
# EFA_activities$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation
# EFA_activities$EFA.workflow$EFA.top.items$EFA.4Factors$suggested.rotation
# 
# # Top items
# EFA_activities$EFA.workflow$top.items$EFA.2Factors %>% view()
# EFA_activities$EFA.workflow$top.items$EFA.3Factors %>% view()
# EFA_activities$EFA.workflow$top.items$EFA.4Factors %>% view()
# EFA_activities$EFA.workflow$top.items$EFA.5Factors %>% view()
# 
# # Removed items
# EFA_activities$EFA.workflow$EFA.top.items$EFA.4Factors$removed.items
# EFA_activities$EFA.workflow$EFA$EFA$EFA.4Factors$removed.items
# EFA_activities$EFA.workflow$EFA$EFA$EFA.4Factors$cross_loading.items
# EFA_activities$EFA.workflow$EFA$EFA$EFA.4Factors$under_loading.items
# EFA_activities$EFA.workflow$EFA$EFA$EFA.4Factors$loadings.long %>% view

# ----- EVALUATION --------------------------------------------------------
# COMPARING ONE STAGE WITH TWO STAGE EFA --------------------------------
# Skills
EFA_skill$best.model$EFA.top.items$reliability.evaluation
EFA_skill.1$best.model$reliability.evaluation

EFA_skill$best.models.evaluation %>% view()
EFA_skill.1$best.models.evaluation %>% view()
EFA_skill$all.models.evaluation %>% view()

EFA_skill$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()

# Abilities
EFA_ablt$best.model$EFA.top.items$reliability.evaluation
EFA_ablt.1$best.model$reliability.evaluation

EFA_ablt$best.models.evaluation %>% view()
EFA_ablt.1$best.models.evaluation %>% view()
EFA_ablt$all.models.evaluation %>% view()

EFA_ablt$best.model$EFA.top.items$reliability.evaluation %>% view()
EFA_ablt$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_ablt$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()
EFA_ablt$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation %>% view()

# Fields of Knowledge
EFA_know$best.model$EFA.top.items$reliability.evaluation
EFA_know.1$best.model$reliability.evaluation

EFA_know$best.models.evaluation %>% view()
EFA_know.1$best.models.evaluation %>% view()
EFA_know$all.models.evaluation %>% view()

EFA_know$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_know$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()
EFA_know$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation %>% view()
EFA_know$EFA.workflow$EFA.top.items$EFA.5Factors$reliability.evaluation %>% view()

# Work context
EFA_context$best.model$EFA.top.items$reliability.evaluation
EFA_context.1$best.model$reliability.evaluation

EFA_context$best.models.evaluation %>% view()
EFA_context.1$best.models.evaluation %>% view()
EFA_context$all.models.evaluation %>% view()

EFA_context$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_context$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()

# Work activities
EFA_activities$best.model$EFA.top.items$reliability.evaluation
EFA_activities.1$best.model$reliability.evaluation

EFA_activities$best.models.evaluation %>% view()
EFA_activities.1$best.models.evaluation %>% view()
EFA_activities$all.models.evaluation %>% view()

EFA_activities$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation %>% view()
EFA_activities$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation %>% view()

# FACTOR ADEQUACY TESTS --------------------------------
# Skills
EFA_skill$best.model$EFA.top.items$adequacy.tests
EFA_skill.1$best.model$adequacy.tests

EFA_skill$EFA.workflow$EFA.top.items$EFA.2Factors$adequacy.tests

# Abilities
EFA_ablt$best.model$EFA.top.items$adequacy.tests
EFA_ablt.1$best.model$adequacy.tests

EFA_ablt$best.model$EFA.top.items$adequacy.tests
EFA_ablt$EFA.workflow$EFA.top.items$EFA.2Factors$adequacy.tests
EFA_ablt$EFA.workflow$EFA.top.items$EFA.3Factors$adequacy.tests
EFA_ablt$EFA.workflow$EFA.top.items$EFA.4Factors$adequacy.tests

# Fields of Knowledge
EFA_know$best.model$EFA.top.items$adequacy.tests
EFA_know.1$best.model$adequacy.tests

EFA_know$EFA.workflow$EFA.top.items$EFA.2Factors$adequacy.tests
EFA_know$EFA.workflow$EFA.top.items$EFA.3Factors$adequacy.tests
EFA_know$EFA.workflow$EFA.top.items$EFA.4Factors$adequacy.tests

# Work context
EFA_context$best.model$EFA.top.items$adequacy.tests
EFA_context.1$best.model$adequacy.tests

EFA_context$EFA.workflow$EFA.top.items$EFA.2Factors$adequacy.tests
EFA_context$EFA.workflow$EFA.top.items$EFA.3Factors$adequacy.tests

# Work activities
EFA_activities$best.model$EFA.top.items$adequacy.tests
EFA_activities.1$best.model$adequacy.tests

EFA_activities$EFA.workflow$EFA.top.items$EFA.2Factors$adequacy.tests
EFA_activities$EFA.workflow$EFA.top.items$EFA.3Factors$adequacy.tests

# FACTOR CORRELATION AND REDUNDANCY --------------------------------
# Skills
EFA_skill$best.model$EFA.top.items$factor.correlation
EFA_skill.1$best.model$factor.correlation

EFA_skill$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation

# Abilities
EFA_ablt$best.model$EFA.top.items$factor.correlation
EFA_ablt.1$best.model$factor.correlation

EFA_ablt$best.model$EFA.top.items$factor.correlation
EFA_ablt$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_ablt$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation
EFA_ablt$EFA.workflow$EFA.top.items$EFA.4Factors$factor.correlation

# Fields of Knowledge
EFA_know$best.model$EFA.top.items$factor.correlation
EFA_know.1$best.model$factor.correlation

EFA_know$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_know$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation
EFA_know$EFA.workflow$EFA.top.items$EFA.4Factors$factor.correlation

# Work context
EFA_context$best.model$EFA.top.items$factor.correlation
EFA_context.1$best.model$factor.correlation

EFA_context$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_context$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation

# Work activities
EFA_activities$best.model$EFA.top.items$factor.correlation
EFA_activities.1$best.model$factor.correlation

EFA_activities$EFA.workflow$EFA.top.items$EFA.2Factors$factor.correlation
EFA_activities$EFA.workflow$EFA.top.items$EFA.3Factors$factor.correlation

# SUGGESTED ROTATION --------------------------------
# Skills
EFA_skill$best.model$EFA.top.items$suggested.rotation
EFA_skill.1$best.model$suggested.rotation

EFA_skill$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation

# Abilities
EFA_ablt$best.model$EFA.top.items$suggested.rotation
EFA_ablt.1$best.model$suggested.rotation

EFA_ablt$best.model$EFA.top.items$suggested.rotation
EFA_ablt$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_ablt$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation
EFA_ablt$EFA.workflow$EFA.top.items$EFA.4Factors$suggested.rotation

# Fields of Knowledge
EFA_know$best.model$EFA.top.items$suggested.rotation
EFA_know.1$best.model$suggested.rotation

EFA_know$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_know$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation
EFA_know$EFA.workflow$EFA.top.items$EFA.4Factors$suggested.rotation

# Work context
EFA_context$best.model$EFA.top.items$suggested.rotation
EFA_context.1$best.model$suggested.rotation

EFA_context$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_context$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation

# Work activities
EFA_activities$best.model$EFA.top.items$suggested.rotation
EFA_activities.1$best.model$suggested.rotation

EFA_activities$EFA.workflow$EFA.top.items$EFA.2Factors$suggested.rotation
EFA_activities$EFA.workflow$EFA.top.items$EFA.3Factors$suggested.rotation

# TOP ITEMS ---------------------------------------------------------------
# Skills
# EFA_skill$best.model$top.items %>% view()
EFA_skill$EFA.workflow$top.items$EFA.2Factors %>% view()
# EFA_skill$EFA.workflow$top.items$EFA.3Factors %>% view()

# The two factors model is most reliable and most interpretable.

# Abilities
# EFA_ablt$best.model$top.items %>% view()
# EFA_ablt$EFA.workflow$top.items$EFA.2Factors %>% view()
EFA_ablt$EFA.workflow$top.items$EFA.3Factors %>% view()

# EFA_ablt$EFA.workflow$top.items$EFA.4Factors %>% view()

# All models are highly reliable. However, the four factors is most interpretable.

# Knowledge
# EFA_know$best.model$top.items %>% view()

# EFA_know$EFA.workflow$top.items$EFA.2Factors %>% view()
# EFA_know$EFA.workflow$top.items$EFA.3Factors %>% view()
EFA_know$EFA.workflow$top.items$EFA.4Factors %>% view()
# EFA_know$EFA.workflow$top.items$EFA.5Factors %>% view()

# Work context
# EFA_context$best.model$top.items %>% view()
EFA_context$EFA.workflow$top.items$EFA.3Factors %>% view()

# Both the two and three factors models are internally consistent.
# However, the three factors model is more interpretable and has the correct amount of items (15).

# Work activities
# EFA_activities$EFA.workflow$top.items$EFA.2Factors %>% view()
EFA_activities$EFA.workflow$top.items$EFA.3Factors %>% view()

# Both the two and three factors models are internally consistent.
# However, the three factors model is more interpretable and has the correct amount of items (15).

# ----- CHOOSE MODELS AND EXPORT ------------------------------------------------
# CHOSEN MODELS ------------------------------------------------------------------
# Revised and selected models
# Skills => 2 factors
chr_skill.model <- 'EFA.2Factors'
# Abilities => 3 factors
chr_ablt.model <- 'EFA.3Factors'
# Fields of Knowledge => 4 factors
chr_know.model <- 'EFA.4Factors'
# Work Contexts => 3 factors
chr_context.model <- 'EFA.3Factors'
# Work Activities => 3 factors
chr_activities.model <- 'EFA.3Factors'

# OUTPUT ------------------------------------------------------------------
# Revised and selected models
# Skills
chr_skill.items <- EFA_skill$EFA.workflow$top.items[[chr_skill.model]]$Item
# Abilities
chr_ablt.items <- EFA_ablt$EFA.workflow$top.items[[chr_ablt.model]]$Item 
# Fields of Knowledge
chr_know.items <- EFA_know$EFA.workflow$top.items[[chr_know.model]]$Item
# Work Contexts
chr_context.items <- EFA_context$EFA.workflow$top.items[[chr_context.model]]$Item
# Work Activities
chr_activities.items <- EFA_activities$EFA.workflow$top.items[[chr_activities.model]]$Item

# ----- RETAINED VARIANCE VS DIMENSIONALITY REDUCTION ---------------------
# RETAINED VARIANCE ------------------------------------------------
df_occupations.numeric %>%
  select(all_of(c(
    chr_skill.items
    , chr_ablt.items
    , chr_know.items
    , chr_context.items
    , chr_activities.items
  ))
  ) -> df_occupations.numeric.items

df_occupations.numeric.items %>% 
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_occupations.var.items

df_occupations.numeric.skill %>%
  select(all_of(c(
    chr_skill.items
  ))
  ) %>% 
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_skills.var.items

df_occupations.numeric.ablt %>%
  select(all_of(c(
    chr_ablt.items
  ))
  ) %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_ablt.var.items

df_occupations.numeric.know %>%
  select(all_of(c(
    chr_know.items
  ))
  ) %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_know.var.items

df_occupations.numeric.context %>%
  select(all_of(c(
    chr_context.items
  ))
  ) %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_context.var.items

df_occupations.numeric.activities %>%
  select(all_of(c(
    chr_activities.items
  ))
  ) %>%
  summarise(
    across(
      .cols = everything()
      ,.fns = var
    )
  ) %>%
  rowSums() -> dbl_activities.var.items

# Retained variance vs. Dimensionality (Overall)
dbl_occupations.var.total
dbl_occupations.var.items

ncol(df_occupations.numeric)
ncol(df_occupations.numeric.items)

dbl_occupations.var.items / dbl_occupations.var.total
ncol(df_occupations.numeric.items) / ncol(df_occupations.numeric)

dbl_occupations.var.total / dbl_occupations.var.items
ncol(df_occupations.numeric) / ncol(df_occupations.numeric.items)

# Retained variance vs. Dimensionality (Skills)
dbl_skills.var.total
dbl_skills.var.items

ncol(df_occupations.numeric.skill)
length(chr_skill.items)

dbl_skills.var.items / dbl_skills.var.total
length(chr_skill.items) / ncol(df_occupations.numeric.skill)

dbl_skills.var.total / dbl_skills.var.items
ncol(df_occupations.numeric.skill) / length(chr_skill.items)

# Retained variance vs. Dimensionality (Abilities)
dbl_ablt.var.total
dbl_ablt.var.items

ncol(df_occupations.numeric.ablt)
length(chr_ablt.items)

dbl_ablt.var.items / dbl_ablt.var.total
length(chr_ablt.items) / ncol(df_occupations.numeric.ablt)

dbl_ablt.var.total / dbl_ablt.var.items
ncol(df_occupations.numeric.ablt) / length(chr_ablt.items)

# Retained variance vs. Dimensionality (Knowledge)
dbl_know.var.total
dbl_know.var.items

ncol(df_occupations.numeric.know)
length(chr_know.items)

dbl_know.var.items / dbl_know.var.total
length(chr_know.items) / ncol(df_occupations.numeric.know)

dbl_know.var.total / dbl_know.var.items
ncol(df_occupations.numeric.know) / length(chr_know.items)

# Retained variance vs. Dimensionality (Work contexts)
dbl_context.var.total
dbl_context.var.items

ncol(df_occupations.numeric.context)
length(chr_context.items)

dbl_context.var.items / dbl_context.var.total
length(chr_context.items) / ncol(df_occupations.numeric.context)

dbl_context.var.total / dbl_context.var.items
ncol(df_occupations.numeric.context) / length(chr_context.items)

# Retained variance vs. Dimensionality (Work activities)
dbl_activities.var.total
dbl_activities.var.items

ncol(df_occupations.numeric.activities)
length(chr_activities.items)

dbl_activities.var.items / dbl_activities.var.total
length(chr_activities.items) / ncol(df_occupations.numeric.activities)

dbl_activities.var.total / dbl_activities.var.items
ncol(df_occupations.numeric.activities) / length(chr_activities.items)
