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

# - Functions -------------------------------------------------------------
# EFA-based exogenous impact analysis
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_efa_impact.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')
# Dictionary evaluation
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_dictionary.R')
# Dynamic text imputation
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_dynamic_text.R')

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

# OECD impact estimation
dbl_impact.oecd <- -0.09

# Language
chr_language <- 'en'

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

# Dynamic texts
map(
  excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/ai_impact_report.xlsx')
  , ~ read_excel('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/ai_impact_report.xlsx', sheet = .x)
) -> list_df_text

names(list_df_text) <- excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/AI Impact/ai_impact_report.xlsx')

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

list_df_text$
  plots <- NULL

# Filter text by language
list_df_text %>% 
  map(
    ~ .x %>% 
      filter(
        language ==
          chr_language
      )
  ) -> list_df_text

# Section list
list_df_text$sections$text %>% 
  as.list() -> list_sections

names(list_sections) <- list_df_text$sections$section

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
  .df_data = df_input
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

# # - Factors impact -----------------------------------------------------------
# list_ai.impact$
#   factors.impact %>% 
#   full_join(
#     df_factor.names
#   ) %>% 
#   relocate(
#     !where(is.numeric)
#     , where(is.numeric)
#   ) -> df_factors.impact
# 
# df_factors.impact

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

df_max.individual %>%
  filter(
    item %in% 
      list_questionnaires$
      atlas.mini$
      item
  ) %>% 
  pivot_longer(
    cols = contains('item.score')
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  mutate(
    metric = 
      recode(
        metric
        , 'item.score' = 'Now'
        , 'item.score2' = 'After AI'
      )
  )

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

df_impact.hist %>% 
  fun_plot.histogram(aes(
    x = aggregate.impact
  )
  , .list_axis.x.args =
    list(
      limits = c(-1.1/2, 0)
      , breaks = seq(-1/2,0,length.out = 4)
    )
  , .fun_format.x = percent
  )

# # - Overall impact --------------------------------------------------------
# list_ai.impact$
#   overall.impact

# # - Expected unemployment -------------------------------------------------
# list_ai.impact$
#   aggregate.impact %>%
#   reframe(
#     unemployment =
#       sum(
#         -aggregate.impact *
#           weight
#       )
#     , unemployment =
#       dollar(
#         unemployment
#         , prefix = ''
#       )
#   )

# [TEXT REPORT] -----------------------------------------------------------
# - Generate dynamic texts ------------------------------------------------
# Preliminary values for analyses
list(
  username =
    df_input$
    occupation
  # username
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
        list_df_text$
        last.comma$
        text
    )
  , chr_most_affected.occupation =
    df_max.aggregate$
    occupation %>%
    fun_text.commas()
  , pct_most_affected.impact = 
    df_max.aggregate$
    aggregate.impact %>% 
    percent(
      accuracy = .01
    )
  , pct_overall.impact = 
    list_ai.impact$
    overall.impact$
    aggregate.impact %>% 
    percent(
      accuracy = .01
    )
  , int_unemployment = 
    list_ai.impact$
    aggregate.impact %>%
    reframe(sum(
      -aggregate.impact *
        weight
    )) %>% 
    pull() %>% 
    dollar(prefix = '')
  , pct_user.impact = 
    list_ai.impact.user$
    overall.impact$
    aggregate.impact %>% 
    percent(
      accuracy = .01
    )
) -> list_text

# List for dictionary evaluation
list(
  chr_automation.negative = 
    list_ai.impact$
    items.impact %>% 
    filter(
      item.impact < 0 
    ) %>% 
    reframe(
      item.impact = 
        mean(item.impact)
    ) %>% 
    pull()
  , impact_desc.text1 =
    df_max.aggregate$
    aggregate.impact
  , impact_desc.text2 = 
    df_max.aggregate$
    aggregate.impact
  , chr_analysis.panorama = 
    list_ai.impact$
    overall.impact$
    aggregate.impact
  , chr_is.isnot = 
    list_ai.impact$
    overall.impact$
    aggregate.impact
  , impact_desc.text3 = 
    list_ai.impact.user$
    overall.impact$
    aggregate.impact
  , chr_oecd.comparison = 
    list_ai.impact$
    overall.impact$
    aggregate.impact /
    dbl_impact.oecd - 1
  , chr_user.analysis =
    list_ai.impact.user$
    overall.impact$
    aggregate.impact
  , chr_market.comparison = 
    list_ai.impact.user$
    overall.impact$
    aggregate.impact / 
    list_ai.impact$
    overall.impact$
    aggregate.impact
) -> list_scores

# Dictionary evaluation
c(
  list_text
  , fun_dictionary.list(
    .df_dictionary.long = 
      list_df_text$
      dictionary.eval
    , .list_dbl_score.eval = 
      list_scores
  )
) -> list_text

# Impute dynamic text
map_if(
  list_df_text
  , ~ !any(.x$complexity == 'complex', na.rm = T)
  , ~ fun_text.dynamic(.x, list_text)
) -> list_df_text

# Text list
list_df_text$
  sections$
  text %>%
  as.list() -> list_report.texts

# Section titles
list_df_text$
  sections.title %>% 
  mutate(
    title = 
      paste(
        strrep('#', level)
        , title
      )) %>% 
  pull(title) %>%
  as.list() -> list_report.titles

# Captions
list_df_text$
  plots %>% 
  pull(plot.caption) %>% 
  unique() %>% 
  as.list() -> list_plots.caption

# Text elements
list_df_text$
  text.elements %>% 
  pull(title) %>% 
  as.list() -> list_text.elements

# - Output / Render R Markdown report ----------------------------------------------

