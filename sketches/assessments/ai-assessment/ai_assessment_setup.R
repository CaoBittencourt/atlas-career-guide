# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  # , 'openxlsx' #Export excel
  , 'readxl' #Export excel
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# R profile to load packages at the start of the project
pkg -> chr_profile

# - Functions -------------------------------------------------------------
# EFA-based exogenous impact analysis
source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_fa_impact.R')
c(chr_profile, pkg) -> chr_profile
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
c(chr_profile, pkg) -> chr_profile
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')
c(chr_profile, pkg) -> chr_profile
# Dictionary evaluation
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_dictionary.R')
c(chr_profile, pkg) -> chr_profile
# Dynamic text imputation
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_dynamic_text.R')
c(chr_profile, pkg) -> chr_profile

unique(chr_profile) -> chr_profile

# - Parameters ------------------------------------------------------------
# Scale bounds
.dbl_scale.lb <- 0
.dbl_scale.ub <- 100

# Immunity range
.dbl_immune.lb <- 0
.dbl_immune.ub <- 33

# OECD impact estimation
dbl_impact.oecd <- -0.09

# - Data ------------------------------------------------------------------
# EFA model
read_rds(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/efa_model_equamax_15_factors.rds'
) -> efa_model

# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas.complete_equamax_15_factors.csv'
) -> df_occupations

setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# Dynamic texts
map(
  # excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/ai_assessment_texts.xlsx')
  excel_sheets('./ai_assessment_texts.xlsx')
  # , ~ read_excel('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/ai_assessment_texts.xlsx', sheet = .x)
  , ~ read_excel('./ai_assessment_texts.xlsx', sheet = .x)
) -> list_df_text

# names(list_df_text) <- excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/ai_assessment_texts.xlsx')
names(list_df_text) <- excel_sheets('./ai_assessment_texts.xlsx')

# Remove carriage returns
list_df_text %>%
  map(function(df){
    
    df %>% 
      mutate(across(
        where(is.character)
        , ~ str_remove_all(.x, "\r") %>% 
          str_remove_all("\\\\n") %>% 
          str_replace_all("\n", "  \n")
      ))
    
  }) -> list_df_text

# Remove reference dictionary
list_df_text$
  dictionary <- NULL

# Filter text by language
list_df_text %>% 
  map(
    ~ .x %>% 
      filter(
        language ==
          chr_language
      )
  ) -> list_df_text

# - Impact ----------------------------------------------------------
# Exogenous impact vector
# LLMs (e.g. chat gpt, google ai)
set_names(
  c(
    'factor1' = 33
    , 'factor2' = 0
    # , 'factor3' = -17/2
    , 'factor3' = -17
    , 'factor4' = 0
    , 'factor5' = -17/2
    # , 'factor5' = -17
    , 'factor6' = 0
    , 'factor7' = 0
    , 'factor8' = -17/2
    # , 'factor8' = -17
    , 'factor9' = 0
    , 'factor10' = -50
    , 'factor11' = -17/2
    # , 'factor11' = -17
    , 'factor12' = 0
    , 'factor13' = 0
    , 'factor14' = 0
    , 'factor15' = 0
  )
  , list_df_text$
    factor.model$
    factor.name
) -> dbl_factors.impact

# [RESULTS] ----------------------------------------------
# - Estimate exogenous impact (US labor market) ---------------------------------------------
fun_efa.impact(
  .df_data =
    df_occupations %>% 
    select(
      occupation
      , ends_with('.l')
    )
  , .dbl_weights =
    df_occupations$
    employment2
  , .efa_model = efa_model
  , .dbl_factors.impact =
    dbl_factors.impact
  , .dbl_scale.lb =
    .dbl_scale.lb 
  , .dbl_scale.ub =
    .dbl_scale.ub 
  , .dbl_immune.lb =
    .dbl_immune.lb
  , .dbl_immune.ub =
    .dbl_immune.ub
  , .lgc_aggregate = T
) -> list_ai.impact

# - Item impact -----------------------------------------------------------
list_ai.impact$
  items.impact %>% 
  arrange(desc(
    item.impact
  )) %>% 
  slice(
    1:10
    , (n() - 9): n()
  ) %>% 
  mutate(
    impact.type =
      if_else(
        item.impact >= 0
        , 'Positive Impact'
        , 'Negative Impact'
      )
  ) %>% 
  group_by(
    impact.type
  ) %>% 
  arrange(desc(
    abs(item.impact)
  )) -> df_items.top_bot

# - Most affected occupation -----------------------------------------
list_ai.impact$
  aggregate.impact %>% 
  filter(
    aggregate.impact ==
      min(aggregate.impact)
  ) -> df_max.aggregate

list_ai.impact$
  individual.impact %>% 
  filter(
    occupation ==
      df_max.aggregate$
      occupation
  ) -> df_max.individual

# - Aggregate impact by occupation -----------------------------------------------------
list_ai.impact$
  aggregate.impact %>% 
  mutate(
    weight = 
      weight / 
      min(weight)
    , weight = 
      ceiling(weight)
  ) %>% 
  group_by(
    occupation
  ) %>% 
  slice(rep(
    1, weight
  )) %>% 
  ungroup() -> df_impact.hist

# [EXPORT] ----------------------------------------------------------------
# - Save workspace image --------------------------------------------------
save.image('ai_assessment_image.RData')
