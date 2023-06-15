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

# User data
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
) %>% 
  mutate(across(
    .cols = ends_with('.l')
    ,.fns = ~ .x * 100
  )) -> df_input

# - Functions -------------------------------------------------------------
# EFA-based exogenous impact analysis
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_efa_impact.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')
# Dictionary
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_dictionary.R')

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

read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vRQCoYHwlfowFhlz3k0tXw1oAPxJ7H1-4MKvEXWVnPrwrehjFwoLy_zrZM-pKRWOl8l98WaF7haYYHE/pub?gid=792495051&single=true&output=csv'
) -> df_impact

df_impact %>% 
  arrange(
    interval.lb
  ) %>% 
  mutate(
    .after = interval
    # .after = interval.lb
    , interval2 =
      findInterval(
        interval.lb
        , interval.lb
      )
    # , vec = runif(13, -1, 2)
    # , interval2 =
    #   findInterval(
    #     vec
    #     , interval.lb
    #   )
  )

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
  , df_factor.names %>%
    pull(factor.name)
) -> dbl_factors.impact

# set_names(
#   c(
#     'factor1' = 67
#     , 'factor2' = 0
#     , 'factor3' = -33
#     , 'factor4' = 0
#     , 'factor5' = -17
#     , 'factor6' = 0
#     , 'factor7' = 0
#     , 'factor8' = -17
#     , 'factor9' = 0
#     , 'factor10' = -75
#     , 'factor11' = -33
#     , 'factor12' = 0
#     , 'factor13' = 0
#     , 'factor14' = 0
#     , 'factor15' = 0
#   )
#   , df_factor.names %>%
#     pull(factor.name)
# ) -> dbl_factors.impact

# set_names(
#   c(
#     # 'factor1' = 33
#     # 'factor1' = 33 * 1.25
#     'factor1' = 50
#     , 'factor2' = 0
#     # , 'factor2' = -17/2
#     # , 'factor3' = -17/2
#     , 'factor3' = -17
#     , 'factor4' = 0
#     # , 'factor5' = -17/4
#     , 'factor5' = -17/2
#     # , 'factor5' = -17
#     # , 'factor6' = -17/4
#     , 'factor6' = 0
#     # , 'factor7' = -17/2
#     , 'factor7' = 0
#     # , 'factor8' = -17/2
#     , 'factor8' = -17
#     , 'factor9' = 0
#     # , 'factor10' = -50
#     , 'factor10' = -67
#     # , 'factor10' = mean(c(-67,-83))
#     # , 'factor11' = -17/2
#     , 'factor11' = -17
#     # , 'factor12' = -17/4
#     , 'factor12' = 0
#     , 'factor13' = 0
#     # , 'factor14' = 0
#     # , 'factor14' = -17/2
#     # , 'factor14' = -17/4
#     , 'factor14' = 0
#     , 'factor15' = 0
#   )
#   , df_factor.names %>%
#     pull(factor.name)
# ) -> dbl_factors.impact

sort(dbl_factors.impact)

# Immunity range
.dbl_immune.lb <- 0
.dbl_immune.ub <- 33

# US labor force (2023)


# [DATA] ------------------------------------------------------------------
# - Occupations data frame on a 0 to 100 scale ------------------------------
df_occupations.efa %>%
  mutate(across(
    .cols = ends_with('.l')
    ,.fns = ~ .x * 100
  )) -> df_occupations.ai

# [RESULTS] ----------------------------------------------
# - Estimate exogenous impact (US labor market) ---------------------------------------------
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
  , .dbl_immune.lb =
    .dbl_immune.lb
  , .dbl_immune.ub =
    .dbl_immune.ub
  , .lgc_aggregate = T
) -> list_ai.impact

# - Estimate exogenous impact (user) ---------------------------------------------
fun_efa.impact(
  .df_data =
    df_input
  , .dbl_weights = NULL
  , .efa_model =
    list_efa.equamax.15$
    EFA.workflow$
    EFA$
    EFA.15factors$
    model
  , .dbl_factors.impact =
    dbl_factors.impact
  , .dbl_immune.lb =
    .dbl_immune.lb
  , .dbl_immune.ub =
    .dbl_immune.ub
  , .lgc_aggregate = T
) -> list_ai.impact.user

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
  slice(
    1:10
    , (n() - 9): n()
  ) %>% 
  print(n = nrow(.))

list_ai.impact$
  items.impact %>% 
  filter(str_detect(
    item, 'thinking|programming|physical'
  ))

# - Most affected occupation -----------------------------------------
list_ai.impact$
  aggregate.impact %>% 
  filter(
    aggregate.impact ==
      max(aggregate.impact)
  ) %>% 
  pull(
    occupation
  ) -> chr_occupation.max

list_ai.impact$
  individual.impact %>% 
  filter(
    occupation ==
      chr_occupation.max
  ) -> df_occupation.max

# - Aggregate impact by occupation -----------------------------------------------------
list_ai.impact$
  aggregate.impact %>% 
  arrange(desc(
    aggregate.impact
  )) %>% 
  print(n = nrow(.))

list_ai.impact$
  aggregate.impact %>% 
  arrange(desc(
    aggregate.impact
  )) %>% 
  slice(1, n())

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
  mutate(
    unemployment = 
      161 * 1000000 / 
      employment2 * 
      unemployment
    , employment2 = 
      161 * 1000000
  ) %>%
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

# [TEXT REPORT] -----------------------------------------------------------
# - Generate dynamic texts ------------------------------------------------
fun_dictionary()
# Preliminary values for analyses
flatten(list(
  username =
    df_input$
    username
  , nrow_occupations = 
    df_occupations.ai %>%
    nrow()
  , nitems.complete =
    list_questionnaires$
    atlas.complete %>% 
    nrow()
  , nfactors = 
    df_factor.names %>% 
    nrow()
  , factor.names = 
    df_factor.names$
    factor.name %>% 
    fun_text.commas(
      .chr_last.sep =
        chr_comma.last
    )
  , chr_automation.negative = 
    df_impact %>% 
    mutate(
      impact = 
        list_ai.impact$
        items.impact %>% 
        filter(
          item.impact < 0 
        ) %>% 
        reframe(
          item.impact = 
            mean(item.impact) / 
            100
        ) %>% 
        pull() %>% 
        findInterval(
          interval.lb
        ) 
    ) %>% 
    filter(
      interval == 
        impact
    ) %>% 
    pull(
      interval.title
    )
  , chr_most_affected.occupation = 
    df_impact.max$
    occupation
  , pct_most_affected.impact = 
    df_impact.max$
    aggregate.impact %>% 
    percent(
      accuracy = .01
    )
  , impact_desc.text1
  , impact_desc.text2
  , chr_analysis.panorama
  , chr_is.isnot
  , pct_overall.impact
  , impact_desc.text3
  , int_unemployment
  , chr_oecd.comparison
  , chr_user.analysis
  , pct_user.impact
  , chr_market.comparison
))

# Impute dynamic text
map_if(
  list_df.text
  , ~ !any(.x$complexity == 'complex', na.rm = T)
  , ~ fun_text.dynamic(.x, list_text)
) -> list_df.text

# Text list
as.list(list_df.text$sections$text) -> list_report.texts

# Section titles
list_df.text$sections.title %>% 
  mutate(title = paste(strrep('#', level), title)) %>% 
  pull(title) %>%
  as.list() -> list_report.titles

# Captions
list_df.text$plots %>% 
  pull(plot.caption) %>% 
  unique() %>% 
  as.list() -> list_plots.caption

# Text elements
list_df.text$text.elements %>% 
  pull(title) %>% 
  as.list() -> list_text.elements
# - Output / Render R Markdown report ----------------------------------------------

