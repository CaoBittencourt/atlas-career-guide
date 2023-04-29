# -------- SETUP ----------------------------------------------------------
# Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'systemfit' #Solve simultaneous equations
  # , 'openxlsx' #Export excel
  # , 'httr' #API
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# Functions ------------------------------------------------------
# Plotting functions
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')

# Capital flexibility function
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')

# Factor scores function
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')

# Data --------------------------------------------------------------------
# POPULATION-WEIGHTED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')

# ACRONYMS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_acronyms.R')

# AI IMPACT DATA FRAME
df_ai.impact <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ444mHPocwbJ2_gTTC62FM8gs61zAsnR0E_mdePLg52-hJx4yG3gNqbWM2szcf2oBTDErKY6iri4w9/pub?gid=747659524&single=true&output=csv')

# APIs
# GET()

# Parameters --------------------------------------------------------------
# Colors
list(
  'green' = '#4AF7B0'
  , 'purple1' = '#753AF9'
  , 'purple2' = '#301866'
  , 'purple3' = '#3854FB'
  , 'blue1' = '#56D0F5'
  , 'blue2' = '#ABF4D4'
  , 'blue3' = '#43DED1'
  , 'blue4' = '#182766'
  , 'red' = '#CE3527'
  
  , 'abilities' = '#C92618'
  , 'knowledge' = '#FF9E1F'
  , 'skills' = '#50915D'
  
  , 'black' = '#212121'
  , 'grey' = '#D4D5D8'
) -> list_pal.atlas

# -------- AI IMPACT ---------------------------------------------
# DATA WRANGLING -----------------------------------------------------------
# Fix variable names
df_ai.impact %>% 
  slice(-1) %>% 
  rename_with(~ tolower(.x)) %>% 
  rename_with(~ str_remove_all(.x, '\\(')) %>% 
  rename_with(~ str_remove_all(.x, '\\)')) %>%
  rename_with(~ str_replace_all(.x, ' ', '_')) %>%
  mutate(ai_impact_cb = as.numeric(ai_impact_cb) / 100) %>% 
  rename(
    item.ai = ai_impact_cb
    , ai.commentary = ai_commentary_cb
    , item.name = attribute_title
  ) %>% 
  select(!starts_with('ai_impact')) -> df_ai.impact

# Fix item names
df_acronyms$item.name[!(
  df_acronyms$item.name %in% 
    df_ai.impact$item.name
)] <- c(
  'Judgment and Decision Making'
  , 'Problem Solving'
)

# Pivot occupations data frame to long
df_occupations.efa.comp %>% 
  pivot_longer(
    cols = ends_with('.l')
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  full_join(df_acronyms) %>% 
  full_join(df_ai.impact) -> df_occupations.ai

# Capital flexibility of each attribute -----------------------------------
map_df(
  df_occupations.pop.efa.comp %>% 
    select(ends_with('.l'))
  , fun_capital.flex
) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'item.kflex'
  ) -> df_kflex

# Capital flexibility per occupation -----------------------------------------
df_occupations.efa.comp %>% 
  pivot_longer(
    cols = ends_with('.l')
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  full_join(
    df_kflex
    , multiple = 'all'
  ) -> df_occupations.kflex

# AI impact & capital flexibility occupations data frame ------------------
# Join data frames
df_occupations.kflex %>% 
  full_join(
    df_occupations.ai
    , multiple = 'all'
  ) %>% 
  filter(
    !is.na(category)
  ) -> df_occupations.ai

# AI-adjusted capital flexibility  ----------------------------------------
df_occupations.ai %>% 
  mutate(
    item.kflex.ai = 
      item.kflex * (1 - item.ai)
  ) -> df_occupations.ai

# # AI impact & capital flexibility per factor ------------------------------
# # AI impact factor scores
# df_occupations.pop.efa.ai %>% 
#   select(item, item.ai) %>% 
#   unique %>% 
#   pivot_wider(
#     names_from = item
#     , values_from = item.ai
#   ) %>% 
#   fun_factor.scores2(
#     list_factors.competencies
#     , .lgc_pivot.long = T
#   ) -> list_ai.scores
# 
# # Capital flexibility factor scores
# df_occupations.pop.efa.ai %>% 
#   select(item, item.kflex) %>% 
#   unique %>% 
#   pivot_wider(
#     names_from = item
#     , values_from = item.kflex
#   ) %>% 
#   fun_factor.scores2(
#     list_factors.competencies
#     , .lgc_pivot.long = T
#   ) -> list_kflex.scores

# AI impact & capital flexibility per occupation -------------------------------------------------------------------------
# Aggregate AI impact and capital flexibility
df_occupations.ai %>%
  group_by(
    occupation
  ) %>% 
  reframe(
    kflex.pct = sum(item.kflex * item.score) / sum(item.score)
    , ai.pct = sum(item.ai * item.score) / sum(item.score)
    , kflex_ai.weighted = sum(
      item.kflex * (1 - item.ai) * item.score
    ) / sum(item.score)
  ) %>% 
  full_join(
    df_occupations
  ) %>% 
  arrange(
    desc(ai.pct)
  ) -> df_ai.pct

# Discrete impact scale
df_ai.pct %>% 
  mutate(
    ai.pct.fill = 
      findInterval(
        ai.pct
        , round(seq(0,1,length.out = 7), 2)
      ) %>% 
      recode(
        '1' = 'No impact'
        , '2' = 'Very modest impact'
        , '3' = 'Modest impact'
        , '4' = 'Medium impact'
        , '5' = 'High impact'
        , '6' = 'Partial replacement'
        , '7' = 'Total replacement'
      )
  ) -> df_ai.pct

# AI impact per cluster -------------------------------------------------------------------------
df_occupations.ai %>%
  group_by(
    career_cluster
  ) %>%
  reframe(
    employment.total = sum(employment)
    , wage.median = median(annual_wage_2021)
    , kflex.pct = sum(item.kflex * item.score) / sum(item.score)
    , ai.pct = sum(item.ai * item.score) / sum(item.score)
    , kflex_ai.weighted = sum(
      item.kflex * (1 - item.ai) * item.score
    ) / sum(item.score)
  ) %>%
  drop_na() %>%
  arrange(
    desc(ai.pct)
  ) -> df_ai.pct.clusters

# Discrete impact scale
df_ai.pct.clusters %>% 
  mutate(
    ai.pct.fill = 
      findInterval(
        ai.pct
        , round(seq(0,1,length.out = 7), 2)
      ) %>% 
      recode(
        '1' = 'No impact'
        , '2' = 'Very modest impact'
        , '3' = 'Modest impact'
        , '4' = 'Medium impact'
        , '5' = 'High impact'
        , '6' = 'Partial replacement'
        , '7' = 'Total replacement'
      )
  ) -> df_ai.pct.clusters

# Population-weighted AI impact data frame --------------------------------
df_ai.pct %>% 
  mutate(
    employment = 
      employment / min(employment)
    , employment = round(employment)
  ) %>%
  group_by(occupation) %>%
  slice(rep(
    1:n(), first(employment)
  )) %>% 
  ungroup() -> df_ai.pct.pop

# -------- VISUALIZATIONS -------------------------------------------------
# AI impact on each attribute ---------------------------------------------
df_occupations.ai %>% 
  group_by(item.acronym) %>% 
  slice(1) %>% 
  ungroup() %>% 
  fun_plot.bar(aes(
    x = item.name
    , y = item.ai
    , label = item.acronym
    , fill = category
  )
  , .theme = theme_ridges(center_axis_labels = T) +
    theme(
      title = element_text(hjust = 0.5)
      , plot.title.position = 'plot'
      , legend.position = 'bottom'
      , legend.justification = 'center'
      , legend.key.size = unit(0.5,'cm')
      , legend.key.width = unit(2,'cm')
      , plot.margin = margin(1, 1, 1, 1,'cm')
    )
  , .fun_format.y = function(y){percent(y,accuracy = 1)}
  , .coord_polar = T
  , .fun_polar.labels = percent
  , .list_axis.y.args = list(
    breaks = seq(0, 1, length.out = 5)
  )
  , .list_geom.param = list(
    position = c(position_dodge2(0.5, 'single'))
    , width = 0.5
  )
  , .list_labels.param = list(
    color = list_pal.atlas$black
  )
  , .chr_manual.pal = set_names(
    c(
      list_pal.atlas$abilities
      , list_pal.atlas$knowledge
      , list_pal.atlas$skills
    )
    , unique(df_occupations.ai$category)
  )
  , .list_labs = list(
    y = 'AI Impact'
    , fill = NULL
  )
  )

# Capital flexibility of each attribute ---------------------------------------------
df_occupations.ai %>%
  group_by(item.acronym) %>%
  slice(1) %>%
  ungroup() %>%
  fun_plot.bar(aes(
    x = item.name
    , y = item.kflex
    , label = item.acronym
    , fill = item.ai
  )
  , .scale_colors = list(
    scale_fill_viridis(
      option = 'plasma'
      , limits = c(0,1)
      , breaks = round(seq(0,1, length.out = 7), 2)
      , labels = function(x){percent(x,1)}
    )
  )
  , .theme = theme_ridges(center_axis_labels = T) +
    theme(
      title = element_text(hjust = 0.5)
      , plot.title.position = 'plot'
      , legend.position = 'bottom'
      , legend.justification = 'center'
      , legend.key.size = unit(0.5,'cm')
      # , plot.margin = margin(1, 1, 1, 1,'cm')
    )
  , .fun_format.y = function(y){percent(y,accuracy = 1)}
  , .coord_polar = T
  , .fun_polar.labels = percent
  , .list_axis.y.args = list(
    breaks = seq(0, 1, length.out = 5)
  )
  , .list_geom.param = list(
    position = c(position_dodge2(0.5, 'single'))
    , width = 0.5
  )
  , .list_labels.param = list(
    color = list_pal.atlas$black
  )
  , .list_legend = list(
    guides(
      fill = guide_colorbar(
        title.position = 'top'
        , title.hjust = 0.5
        , barwidth = unit(5, 'cm')
      )))
  , .list_labs = list(
    y = 'Capital Flexibility'
    , fill = 'AI Impact'
  ))

# AI-adjusted capital flexibility of each attribute ---------------------------------------------
df_occupations.ai %>%
  group_by(item.acronym) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    item.name = fct_reorder(
      item.name
      , item.kflex.ai
      , .desc = T
    )) %>% 
  pivot_longer(
    cols = c(
      item.ai
      , item.kflex
      , item.kflex.ai
    )
  ) %>% 
  mutate(
    name = factor(
      name
      , levels = c(
        'item.kflex'
        , 'item.ai'
        , 'item.kflex.ai'
      )
    )
    , name = 
      recode(
        name
        , 'item.ai' = 'AI Impact'
        , 'item.kflex' = 'Capital Flexibility'
        , 'item.kflex.ai' = 'AI-adjusted Capital Flexibility'
      )
    , name = factor(name)
  ) -> df_occupations.ai.long

df_occupations.ai.long %>% 
  fun_plot.dumbbell2(aes(
    x = value
    , y = item.name
    , color = name
  )
  , .sym_facets = item.name
  , .int_facets = 2
  , .chr_scales = 'free_y'
  , .list_labs = list(
    title = 'AI-adjusted Capital Flexibility'
    , subtitle = 'What is the degree of carryover of each professional attributed after accounting for AI impact?'
    , x = 'Capital Flexibility'
    , y = NULL
    , color = NULL
  )
  # , .reorder_desc = T
  , .reorder_fct = F
  , .chr_manual.pal = set_names(
    c(
      list_pal.atlas$red
      , list_pal.atlas$blue4
      , list_pal.atlas$green
    )
    , unique(df_occupations.ai.long$name)
  )
  , .list_axis.x.args = list(
    limits = c(-0.1, 1.1)
    , breaks = seq(0, 1, 0.25)
  )
  , .fun_format.x = percent_format(accuracy = 1)
  , .theme = theme_ridges(center_axis_labels = T) +
    theme(
      title = element_text(hjust = 0.5)
      , plot.title.position = 'plot'
      , axis.text.y = element_blank()
      , axis.ticks.y = element_blank()
      , legend.position = 'bottom'
      , legend.justification = 'center'
      , strip.background = element_blank()
      , plot.margin = margin(1, 1, 1, 1,'cm')
    ))

# AI impact on each factor ------------------------------------------------

# AI impact density (overall) -------------------------------------------
df_ai.pct.pop %>% 
  fun_plot.histogram(aes(
    x = ai.pct
  )
  , .list_axis.x.args = list(
    limits = c(0,1)
    , breaks = seq(0, 1, length.out = 7)
  )
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'AI Impact Distribution'
    , subtitle = 'How much does the advent of \"Artificial Intelligence\" should impact the labor market?'
    , x = 'AI Impact'
    , y = 'Frequency'
  )) + 
  annotate(
    x = mean(c(0.67, 0.83))
    , y = 200000
    , geom = 'text'
    , label = str_wrap(
      '
    We estimate the impact of \"Artificial Intelligence\" on the labor market 
    to be relatively modest (at least in comparison with the usual alarmist evaluations).
    Indeed, if we were to determine a threshold of 67 to 83% as so-called \"replacement-level\" impact, then
    no occupation whatsoever would be severely affected.
    On the other hand, it is also evident that almost all occupations
    should experience some \"auxiliary-level\" exposure to AI (i.e., on the 17 to 33% impact range).
    '
      , 40
    ))

# AI impact density (per cluster) -------------------------------------------
df_ai.pct.pop %>% 
  fun_plot.histogram(aes(
    x = ai.pct
  )
  , .list_axis.x.args = list(
    limits = c(0,1)
    , breaks = seq(0, 1, length.out = 7)
  )
  , .sym_facets = career_cluster
  , .reorder_fct = T
  , .reorder_desc = T
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'AI Impact Distribution by Career Cluster'
    , subtitle = 'How much does the advent of \"Artificial Intelligence\" should impact the labor market?'
    , x = 'AI Impact'
    , y = 'Frequency'
  )) + 
  geom_vline(
    xintercept = 0.67
    , color = list_pal.atlas$red
    , linetype = 'dashed'
  )
# geom_textvline(
#   xintercept = 0.67
#   , label = 'Replacement'
#   , color = list_pal.atlas$red
#   , fontface = 'bold'
#   , linetype = 1
#   , linewidth = 1.35
#   , hjust = 0.125
#   , vjust = -0.5
# )

# AI impact on each occupation -------------------------------------------
df_ai.pct %>% 
  fun_plot.bar(aes(
    x = occupation
    , y = ai.pct
    , fill = ai.pct.fill
  )
  , .theme = theme_ridges(center_axis_labels = T) +
    theme(
      title = element_text(hjust = 0.5)
      , plot.title.position = 'plot'
      , legend.position = 'bottom'
      , legend.justification = 'center'
      , legend.key.size = unit(0.5,'cm')
      , legend.key.width = unit(2,'cm')
      , plot.margin = margin(1, 1, 1, 1,'cm')
    )
  , .fun_format.y = function(y){percent(y,accuracy = 1)}
  , .coord_polar = T
  , .fun_polar.labels = percent
  , .list_axis.y.args = list(
    breaks = seq(0, 1, length.out = 5)
  )
  , .chr_manual.pal = setNames(
    plasma(n = 7)
    , c(
      'No impact'
      , 'Very modest impact'
      , 'Modest impact'
      , 'Medium impact'
      , 'High impact'
      , 'Partial replacement'
      , 'Total replacement'
    )
  )
  , .list_geom.param = list(
    position = c(position_dodge2(0.5, 'single'))
    , width = 0.5
    , fill = list_pal.atlas$purple3
  )
  , .list_labs = list(
    y = 'AI Impact'
    , fill = NULL
  )
  )

# AI impact vs. Capital flexibility ---------------------------------------


# # -------- EXPORT DATA ----------------------------------------------------
# # # EXCEL -------------------------------------------------------------------
# # # Attributes
# # df_kflex.long %>%
# #   arrange(desc(
# #     capital.flex
# #   )) %>%
# #   write.xlsx(
# #     'df_attributes.kflex.xlsx'
# #   )
# # 
# # # Occupations
# # df_occupations.kflex %>%
# #   select(
# #     occupation
# #     , code
# #     , code.variants
# #     , employment
# #     , entry_level_education
# #     , annual_wage_2021
# #     , capital.flex.pct
# #   ) %>%
# #   arrange(desc(
# #     capital.flex.pct
# #   )) %>%
# #   write.xlsx(
# #     'df_occupations.kflex.xlsx'
# #   )
# FLEXIBLE CAPITAL PER OCCUPATION -----------------------------------------
df_occupations %>%
  # select(1:126) %>% 
  pivot_longer(
    cols = ends_with('.l')
    , names_to = 'attribute'
    , values_to = 'level'
  ) %>% 
  full_join(
    df_kflex.long
  ) %>% 
  group_by(
    occupation
  ) %>% 
  summarise(
    capital.flex.pct = sum(capital.flex * level) / sum(level)
  ) %>% 
  full_join(
    df_occupations
  ) %>% 
  arrange(
    desc(capital.flex.pct)
  ) -> df_occupations.kflex
