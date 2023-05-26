# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'openxlsx' #Export excel
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Data ------------------------------------------------------------------
# EFA model
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/efa_output.R')

# - Functions -------------------------------------------------------------
# EFA-based exogenous impact analysis
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_efa_impact.R')

# - Parameters ------------------------------------------------------------
# Exogenous impact vector
# LLMs (e.g. chat gpt, google ai)
set_names(
  c(
    'factor1' = 0
    , 'factor2' = 0
    , 'factor3' = -17/2
    , 'factor4' = 0
    , 'factor5' = -17/4
    , 'factor6' = -17/4
    , 'factor7' = -17/2
    , 'factor8' = -17/2
    , 'factor9' = 0
    # , 'factor10' = -67
    , 'factor10' = -50
    , 'factor11' = -17/2
    , 'factor12' = 0
    , 'factor13' = 0
    , 'factor14' = 0
    , 'factor15' = 0
  )
  , df_factor.names %>% 
    pull(factor.name)  
) -> dbl_factors.impact

# # Risk of automation
# set_names(
#   c(
#     'factor1' = 0
#     , 'factor2' = -17
#     , 'factor3' = -17/2
#     , 'factor4' = -17
#     , 'factor5' = 0
#     , 'factor6' = -17/4
#     , 'factor7' = -17/2
#     , 'factor8' = -17/2
#     , 'factor9' = -33
#     , 'factor10' = -67
#     , 'factor11' = -17
#     , 'factor12' = 0
#     , 'factor13' = -50
#     , 'factor14' = 0
#     , 'factor15' = -33
#   )
#   , df_factor.names %>% 
#     pull(factor.name)  
# ) -> dbl_factors.impact

# [DATA] ------------------------------------------------------------------
# - Occupations data frame on a 0 to 100 scale ------------------------------
df_occupations.efa %>%
  mutate(across(
    .cols = ends_with('.l')
    ,.fns = ~ .x * 100
  )) -> df_occupations.ai

# [RESULTS] ----------------------------------------------
# - Estimate exogenous impact ---------------------------------------------
fun_efa.impact(
  .df_data = 
    df_occupations.ai
  , .dbl_weights = 
    df_occupations %>%
    pull(employment2)
  , .efa_model = 
    list_efa.equamax.15$
    EFA.workflow$
    EFA$
    EFA.15factors$
    model
  , .dbl_factors.impact = 
    dbl_factors.impact
  # , .dbl_impact.ub = 0
  , .lgc_aggregate = T
) -> list_ai.impact

# - Factors impact -----------------------------------------------------------
list_ai.impact$
  factors.impact %>% 
  full_join(
    df_factor.names
  ) %>% 
  relocate(
    !where(is.numeric)
    , where(is.numeric)
  ) %>% 
  print(n = nrow(.))

# - Item impact -----------------------------------------------------------
list_ai.impact$
  items.impact %>% 
  arrange(desc(
    item.impact
  )) %>% 
  print(n = nrow(.))

# - Detailed impact by occupation -----------------------------------------
list_ai.impact$
  individual.impact

# - Aggregate impact by occupation -----------------------------------------------------
list_ai.impact$
  aggregate.impact %>% 
  arrange(desc(
    aggregate.impact
  )) %>% 
  print(n = nrow(.))

# - Overall impact --------------------------------------------------------
list_ai.impact$
  overall.impact
