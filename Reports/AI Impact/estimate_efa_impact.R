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

# - Impact scale ----------------------------------------------------------------
# Impact levels for reference
seq(-1, 0, length.out = 7) %>%
  round(2) %>%
  as_tibble() %>% 
  rename(impact = 1) %>% 
  mutate(
    impact = 100 * impact
    , impact = paste(
      impact
      , dplyr::lag(
        impact
        , default = 
          first(impact)
      )
      , sep = ' to '
    )
  ) %>% 
  slice(-1) %>% 
  mutate(
    level = n() - row_number() + 1
    , desc = 
      c(
        'complete replacement'
        , 'severe replacement'
        , 'partial replacement'
        , 'high impact'
        , 'medium impact'
        , 'minor impact'
      )
    , desc.title = 
      c(
        'fatal'
        , 'massive'
        , 'huge'
        , 'disruptive'
        , 'auxiliary'
        , 'irrelevant'
      )
  ) %>% 
  relocate(
    level
    , desc.title
    , desc
    , impact
  ) -> df_impact.desc

df_impact.desc

# - Parameters ------------------------------------------------------------
# Factors
df_loadings.factors %>% 
  select(
    item
    , factor.name
  ) %>% 
  print(n = nrow(.))

# Exogenous impact vector
# LLMs (e.g. chat gpt, google ai)
set_names(
  c(
    'factor1' = 67
    , 'factor2' = 0
    , 'factor3' = -33
    , 'factor4' = 0
    , 'factor5' = -17
    , 'factor6' = 0
    , 'factor7' = 0
    , 'factor8' = -17
    , 'factor9' = 0
    , 'factor10' = -75
    , 'factor11' = -33
    , 'factor12' = 0
    , 'factor13' = 0
    , 'factor14' = 0
    , 'factor15' = 0
  )
  , df_factor.names %>%
    pull(factor.name)
) -> dbl_factors.impact

set_names(
  c(
    # 'factor1' = 33
    # 'factor1' = 33 * 1.25
    'factor1' = 50
    , 'factor2' = 0
    # , 'factor2' = -17/2
    # , 'factor3' = -17/2
    , 'factor3' = -17
    , 'factor4' = 0
    # , 'factor5' = -17/4
    , 'factor5' = -17/2
    # , 'factor5' = -17
    # , 'factor6' = -17/4
    , 'factor6' = 0
    # , 'factor7' = -17/2
    , 'factor7' = 0
    # , 'factor8' = -17/2
    , 'factor8' = -17
    , 'factor9' = 0
    # , 'factor10' = -50
    , 'factor10' = -67
    # , 'factor10' = mean(c(-67,-83))
    # , 'factor11' = -17/2
    , 'factor11' = -17
    # , 'factor12' = -17/4
    , 'factor12' = 0
    , 'factor13' = 0
    # , 'factor14' = 0
    # , 'factor14' = -17/2
    # , 'factor14' = -17/4
    , 'factor14' = 0
    , 'factor15' = 0
  )
  , df_factor.names %>%
    pull(factor.name)
) -> dbl_factors.impact

sort(dbl_factors.impact)

# [DATA] ------------------------------------------------------------------
# - Occupations data frame on a 0 to 100 scale ------------------------------
df_occupations %>%
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
    .dbl_weights
  , .efa_model =
    list_efa.equamax.15$
    EFA.workflow$
    EFA$
    EFA.15factors$
    model
  , .dbl_factors.impact =
    dbl_factors.impact
  # , .dbl_impact.ub = 0
  , .dbl_immune.lb = 0
  , .dbl_immune.ub = 33
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
  ) -> df_factors.impact

df_factors.impact

# - Item impact -----------------------------------------------------------
list_ai.impact$
  items.impact %>% 
  arrange(desc(
    item.impact
  )) %>% 
  print(n = nrow(.))

list_ai.impact$
  items.impact %>% 
  filter(str_detect(
    item, 'thinking|programming|physical'
  ))

# - Detailed impact by occupation -----------------------------------------
df_occupations.ai %>% 
  pull(occupation) %>%
  sample(1) -> chr_occupation.sample

list_ai.impact$
  aggregate.impact %>% 
  filter(
    occupation ==
      chr_occupation.sample
  )

list_ai.impact$
  individual.impact %>% 
  filter(
    occupation ==
      chr_occupation.sample
  ) %>% 
  view

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

# - Expected unemployment -------------------------------------------------
df_occupations %>% 
  mutate(
    .after = 1
    , employment2 = 
      .dbl_weights
  ) %>% 
  select(
    occupation
    , employment2
  ) %>% 
  full_join(
    list_ai.impact$
      aggregate.impact
  ) %>% 
  mutate(
    unemployment.rate = 
      - aggregate.impact
    , unemployment = 
      unemployment.rate * 
      employment2
  ) %>% 
  relocate(
    occupation
    , unemployment
    , unemployment.rate
    , everything()
  ) -> df_aggregate.unemployment

df_aggregate.unemployment %>% 
  reframe(across(
    .cols = c(
      employment2
      , unemployment
    )
    ,.fns = sum
  )) %>% 
  mutate(
    unemployment.rate =  
      unemployment / 
      employment2
  ) -> df_overall.unemployment

df_aggregate.unemployment

df_overall.unemployment %>% 
  mutate(across(
    .cols = c(
      employment2
      , unemployment
    )
    ,.fns = ~ 
      scales::dollar(
        .x, prefix = ''
      )
  ))

