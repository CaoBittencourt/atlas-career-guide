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
    # , -ends_with('.I') #Using recommended levels
    , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.skill

# # Only numeric variables
# df_occupations %>%
#   select(
#     where(function(x){str_detect(attributes(x)$label, 'Basic')}) #Basic Skills only
#     , -ends_with('.I') #Using recommended levels
#     # , -ends_with('.L') #Using importance levels
#   ) %>% 
#   mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
#     across(
#       .fns = function(x){x/100}
#     )
#   ) -> df_occupations.numeric.basic
# 
# # Only numeric variables
# df_occupations %>%
#   select(
#     where(function(x){str_detect(attributes(x)$label, 'Cross')}) #Cross functional Skills only
#     , -ends_with('.I') #Using recommended levels
#     # , -ends_with('.L') #Using importance levels
#   ) %>% 
#   mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
#     across(
#       .fns = function(x){x/100}
#     )
#   ) -> df_occupations.numeric.cross

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'Abilities.')}) #Abilities only
    # , -ends_with('.I') #Using recommended levels
    , -ends_with('.L') #Using importance levels
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
    # , -ends_with('.I') #Using recommended levels
    , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.know

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
# .show_results <- F

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
# .show_results <- F

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
# .show_results <- F

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
# .show_results <- F

# GLOBAL EFA PARAMETERS FIRST TEST ---------------------------------------------------
# Number of factors
.auto_select.nfactors <- T
# int_nfactors.vector <- seq(1,5)

# Minimum factor size
# .int_min.factor_size.basic <- 2
.int_min.factor_size <- 3

# Top items
# .int_n.items.total.basic <- 5
# .int_n.items.total.cross <- 10

.int_n.items.total.skill <- 8
.int_n.items.total.ablt <- 12 #don't edit
.int_n.items.total.know <- 12

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

# # GLOBAL EFA PARAMETERS FIRST TEST ---------------------------------------------------
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
# 
# # Rotation (Oblique)
# # .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# 
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.3 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- F
# 
# # GLOBAL EFA PARAMETERS FIRST TEST ---------------------------------------------------
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
# 
# # Rotation (Oblique)
# # .chr_rotation <- 'promax'
# # .chr_rotation <- 'oblimin'
# # Rotation (Orthogonal)
# .chr_rotation <- 'varimax'
# .remove_unacceptable_MSAi.items <- T
# 
# # Underloadings and crossloadings
# .remove_under_loading.items <- T
# .remove_cross_loading.items <- T
# .dbl_under_loading.threshold <- 0.5 #Lesser than 0.4 loading <- under loading
# # .dbl_cross_loading.threshold <- 0.05 #Lesser than 0.05 loading difference <- cross loading
# .dbl_cross_loading.threshold <- 0.35
# 
# # Diagrams and tests
# .show_diagrams <- T
# .show_results <- F

# FULLY AUTOMATED EFA WORKFLOW (WITH TOP ITEMS) --------------------------------------------
# # Basic Skills
# fun_best.model.top.items.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.basic
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size.basic
#   , .int_n.items.total = .int_n.items.total.cross
#   , .chr_rotation = .chr_rotation
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_Basic
#
# # Cross Functional Skills
# fun_best.model.top.items.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.cross
#   , .auto_select.nfactors = .auto_select.nfactors
#   , .int_min.factor_size = .int_min.factor_size.basic
#   , .int_n.items.total = .int_n.items.total.cross
#   , .chr_rotation = .chr_rotation
#   # Underloadings and crossloadings
#   , .remove_under_loading.items = .remove_under_loading.items
#   , .remove_cross_loading.items = .remove_cross_loading.items
#   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
#   # Diagrams and tests
#   , .show_diagrams = .show_diagrams
#   , .show_results = .show_results
# ) -> EFA_Cross

# All Skills
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

# # FULLY AUTOMATED EFA WORKFLOW (ONLY STAGE ONE) --------------------------------------------
# # Basic Skills
# # fun_best.model.workflow(
# #   # Basic
# #   .df_data.numeric = df_occupations.numeric.basic
# #   , .auto_select.nfactors = .auto_select.nfactors
# #   , .int_min.factor_size = .int_min.factor_size.basic
# #   , .chr_rotation = .chr_rotation
# #   # Underloadings and crossloadings
# #   , .remove_under_loading.items = .remove_under_loading.items
# #   , .remove_cross_loading.items = .remove_cross_loading.items
# #   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
# #   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
# #   # Diagrams and tests
# #   , .show_diagrams = .show_diagrams
# #   , .show_results = .show_results
# # ) -> EFA_Basic.1
# #
# # # Cross Functional Skills
# # fun_best.model.workflow(
# #   # Basic
# #   .df_data.numeric = df_occupations.numeric.cross
# #   , .auto_select.nfactors = .auto_select.nfactors
# #   , .int_min.factor_size = .int_min.factor_size.basic
# #   , .chr_rotation = .chr_rotation
# #   # Underloadings and crossloadings
# #   , .remove_under_loading.items = .remove_under_loading.items
# #   , .remove_cross_loading.items = .remove_cross_loading.items
# #   , .dbl_under_loading.threshold = .dbl_under_loading.threshold
# #   , .dbl_cross_loading.threshold = .dbl_cross_loading.threshold
# #   # Diagrams and tests
# #   , .show_diagrams = .show_diagrams
# #   , .show_results = .show_results
# # ) -> EFA_Cross.1
# 
# # All Skills
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.skill
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
# ) -> EFA_Skill.1
# 
# # Abilities
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.ablt
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
# ) -> EFA_Ablt.1
# 
# # Knowledge
# fun_best.model.workflow(
#   # Basic
#   .df_data.numeric = df_occupations.numeric.know
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
# ) -> EFA_Know.1
# 

# # COMPARING ONE STAGE WITH TWO STAGE EFA --------------------------------
# # Skills
# EFA_Skill$best.model$EFA.top.items$reliability.evaluation
# EFA_Skill.1$best.model$reliability.evaluation
# 
# EFA_Skill$best.models.evaluation %>% view()
# EFA_Skill.1$best.models.evaluation %>% view()
# 
# EFA_Skill$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation
# EFA_Skill$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation
# 
# # Abilities
# EFA_Ablt$best.model$EFA.top.items$reliability.evaluation
# EFA_Ablt.1$best.model$reliability.evaluation
# 
# EFA_Ablt$best.model$EFA.top.items$reliability.evaluation
# EFA_Ablt$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation
# EFA_Ablt$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation
# 
# EFA_Ablt$best.models.evaluation %>% view()
# EFA_Ablt.1$best.models.evaluation %>% view()
# 
# # Fields of Knowledge
# EFA_Know$best.model$EFA.top.items$reliability.evaluation
# EFA_Know.1$best.model$reliability.evaluation
# 
# EFA_Know$best.models.evaluation %>% view()
# EFA_Know.1$best.models.evaluation %>% view()
# 
# EFA_Know$EFA.workflow$EFA.top.items$EFA.2Factors$reliability.evaluation
# EFA_Know$EFA.workflow$EFA.top.items$EFA.3Factors$reliability.evaluation
# EFA_Know$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation

# # MOST INTERPRETABLE CONSISTENT MODELS 2 ------------------------------------
# # Skills
# # The two factor model is the most consistent and very interpretable.
# # Factor composition
# EFA_Skill$best.model$top.items
# 
# # Factor 1 is composed of cognitive, non-technical, general skills (general).
# # Factor 2 is composed of technical, hands-on, specialist skills (technical).
# 
# # Abilities
# # All models have excellent consistency, on average. The differences are very slight.
# # Therefore, although the one factor model was selected as "best model" in terms of consistency,
# # the other models are just as reliable, and interpretability will be given more importance.
# # In the first stage EFA, the two factor model is the most consistent. Thus, it is clear that the single factor model
# # was selected as "best model" because the top items selection increased its internal consistency metrics.
# # This is confirmed when inspecting the selected top items: all of cognitive and social abilities are missing.
# # The single factor model is composed of bodily abilities only. That is, this model is only more consistent because
# # the maximum amount of items allowed for this single factor to be consistent when all othe items are excluded.
# # Obviously, this is not a desirable result and the single factor model should be disconsidered.
# 
# # Factor composition
# EFA_Ablt$best.model$top.items
# # Factor 1 is composed of bodily abilities (overall body constitution).
# 
# EFA_Ablt$EFA.workflow$top.items$EFA.2Factors
# # Factor 1 is composed of bodily abilities (overall body constitution).
# # Factor 2 is composed of cognitive abilities (intelligence).
# 
# EFA_Ablt$EFA.workflow$top.items$EFA.3Factors
# # Factor 1 is composed of cognitive abilities (intelligence).
# # Factor 2 is composed of manual abilities (dexterity).
# # Factor 3 is composed of perceptual abilities (perception).
# # P.S.: Speed of limb movement loads way less to this factor than the other items, and also crossloads significantly to other factors.
# # It should be considered to raise the crossloading deletion threshold and rerun the analysis.
# # Also, Factor 3 has 6 top items, while the other factors have 7.
# # The other models should be prefered over the three factor model.
# 
# EFA_Ablt$EFA.workflow$top.items$EFA.4Factors
# # Factor 1 is composed of cognitive abilities (intelligence).
# # Factor 2 is composed of bodily robustness, potency, and coordination (overall constitution).
# # Factor 3 is composed of manual abilities (dexterity).
# # Factor 4 is composed of perceptual abilities (perception).
# 
# # Overall, the four factor model seems more interpretable and nuanced.
# # It is also very close to the other models in terms of consistency (i.e. excellent).
# 
# # Fields of Knowledge
# # The two factor model is significantly more consistent than the other models.
# # It is also interpretable, although some fields of knowledge seem out of place.
# # It should be considered to raise the underloading and crossloading deletion thresholds and rerun the analysis.
# EFA_Know$best.model$top.items
# 
# # Factor composition
# # Factor 1 is composed of humanities.
# # P.S.: Administrative knowledge seems out of place.
# 
# # Factor 2 is composed of technical / building oriented fields of knowledge.
# 
# # Overall, the two factor model does not provide satisfactory nuance,
# # even though it is the most consistent model.
# # Another analysis should be made. Maybe removing problematic fields of knowledge upfront could help.
# 

# # MOST INTERPRETABLE CONSISTENT MODELS 3 ------------------------------------
# # Skills
# # The two factor model is the most consistent and very interpretable.
# # Factor composition
# EFA_Skill$best.model$top.items
# 
# # Factor 1 is composed of cognitive, non-technical, general skills (general).
# # Factor 2 is composed of technical, hands-on, specialist skills (technical).
# 
# # Abilities
# # All models have excellent consistency, on average. The differences are very slight.
# # Therefore, although the one factor model was selected as "best model" in terms of consistency,
# # the other models are just as reliable, and interpretability will be given more importance.
# # In the first stage EFA, the two factor model is the most consistent. Thus, is is clear that the single factor model
# # was selected as "best model" because the top items selection increased its internal consistency metrics.
# # This is confirmed when inspecting the selected top items: all of cognitive and social abilities are missing.
# # The single factor model is composed of bodily abilities only. That is, this model is only more consistent because
# # the maximum amount of items allowed for this single factor to be consistent when all othe items are excluded.
# # Obviously, this is not a desirable result and the single factor model should be disconsidered.
# 
# # Factor composition
# EFA_Ablt$best.model$top.items
# # Factor 1 is composed of bodily abilities (overall body constitution).
# 
# EFA_Ablt$EFA.workflow$top.items$EFA.2Factors
# # Factor 1 is composed of bodily abilities (overall body constitution).
# # Factor 2 is composed of cognitive abilities (intelligence).
# 
# EFA_Ablt$EFA.workflow$top.items$EFA.3Factors
# # Factor 1 is composed of cognitive abilities (intelligence).
# # Factor 2 is composed of manual abilities (dexterity).
# # Factor 3 is composed of perceptual abilities (perception).
# # P.S.: with an increased crossloading deletion threshold,
# # and reduction of item count in the abilities category,
# # problematic items are gone. The three factor model is now viable.
# 
# EFA_Ablt$EFA.workflow$top.items$EFA.4Factors
# # Factor 1 is composed of cognitive abilities (intelligence).
# # Factor 2 is composed of bodily robustness, potency, and coordination (overall constitution).
# # Factor 3 is composed of manual abilities (dexterity).
# # Factor 4 is composed of perceptual abilities (perception).
# 
# # Overall, the four factor model seems more interpretable and nuanced.
# # It is also almost identical to the other models in terms of consistency (i.e. excellent).
# 
# # Fields of Knowledge
# # The two factor model is significantly more consistent than the other models.
# # It is also interpretable, although some fields of knowledge seem out of place.
# # It should be considered to raise the underloading and crossloading deletion thresholds and rerun the analysis.
# EFA_Know$best.model$top.items
# 
# # Factor composition
# # Factor 1 is composed of humanities.
# # Factor 2 is composed of technical / building oriented fields of knowledge.
# 
# # P.S.: with reduced item count in the knowledge category, as well as 
# # increased crossloading deletion threshold, the problematic items are gone.
# # The two factor model is more viable than before, although it still lacks nuance.
# 
# # The three factor model has unequal top items distribution: 
# # Factor 1 and 2 have 4 top items, but Factor 3 has only 3.
# # The factors are also not as interpretable as in the other models.
# 
# EFA_Know$EFA.workflow$top.items$EFA.4Factors
# # Although the least of the best models in terms of consistency,
# # the four factor model is definitely the most interpretable.
# EFA_Know$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.evaluation
# EFA_Know$EFA.workflow$EFA.top.items$EFA.4Factors$reliability.metrics
# 
# # Factor 1 is composed of engineering / building-related fields of knowledge (build).
# # Factor 2 is composed of financial and enterprising fields of knowledge (FGV).
# # Factor 3 is composed of health-related fields of knowledge (health).
# # Factor 4 is composed of arts and humanities (arts and humanities).
# 
# # The least internally consistent of the four is Factor 4, which has mostly acceptable consistency.
# # But overall the four factor model has good internal consistency.
# 
# # It is the most interpretable model and has sufficient consistency. Thus, it is selected as the current model.

# OUTPUT ------------------------------------------------------------------
# # Top items in the "best" (most internally consistent) model 
# EFA_Skill$best.model$top.items$Item -> chr_Skill.Items
# EFA_Ablt$best.model$top.items$Item -> chr_Ablt.Items
# EFA_Know$best.model$top.items$Item -> chr_Know.Items

# Revised and selected models
chr_Skill.Items <- EFA_Skill$EFA.workflow$top.items$EFA.2Factors$Item
chr_Ablt.Items <- EFA_Ablt$EFA.workflow$top.items$EFA.4Factors$Item 
chr_Know.Items <- EFA_Know$EFA.workflow$top.items$EFA.4Factors$Item 
