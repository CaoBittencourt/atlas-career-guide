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

# - Data --------------------------------------------------------------------
# Occupations data frame
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.R')
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')

# - Functions ---------------------------------------------------------------
# EFA
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/auto_efa_fa.R')

# Knn matching function
source("C:/Users/Cao/Documents/Github/Atlas-Research/Functions/KNN_Matching.R")

# - Parameters --------------------------------------------------------------
# EFA parameters
# Sample weights
df_occupations %>% 
  pull(
    employment2
  ) -> .dbl_weights

# Orthogonal rotations
.chr_rotation <- 'equamax'
# .chr_rotation <- 'varimax'
# .chr_rotation <- 'varimin'
# .chr_rotation <- 'quartimax'

# Oblique rotations
# .chr_rotation <- 'oblimin'
# .chr_rotation <- 'Promax'
# .chr_rotation <- 'promax'
# .chr_rotation <- 'bentlerQ'
# .chr_rotation <- 'cluster'

# Number of factors
# Models tested: from 1 to 22+ factors
.int_nfactors <- 15
# .auto_select.nfactors <- T
.auto_select.nfactors <- F

# Minimum factor size
.int_min.factor_size <- 3

.remove_unacceptable_MSAi.items <- F
# Underloadings and crossloadings
.remove_under_loading.items <- F
.remove_cross_loading.items <- F
.dbl_under_loading.threshold <- 0.5
.dbl_cross_loading.threshold <- 0.2

# Diagrams and tests
.show_diagrams <- T
.show_results <- T

# [DATA] -----------------------------------------------------
# - Problematic items -----------------------------------------------------
list(
  'near_vision.l'
  , 'foreign_language.l'
  , 'duration_of_typical_work_week.l'
  # , 'telephone.l'
  # , 'electronic_mail.l'
  , 'letters_and_memos.l'
  , 'food_production.l'
  , 'structured_versus_unstructured_work.l'
  , 'establishing_and_maintaining_interpersonal_relationships.l'
  , 'communicating_with_supervisors_peers_or_subordinates.l'
  , 'speaking.l'
  , 'writing.l'
  , 'reading_comprehension.l'
  , 'degree_of_automation.l'
  , 'work_schedules.l'
  , 'indoors_environmentally_controlled.l'
  , 'impact_of_decisions_on_co_workers_or_company_results.l'
  , 'spend_time_sitting.l'
  , 'law_and_government.l'
) -> list_items.remove

# - Select items for EFA --------------------------------------------------
df_occupations %>%
  select(
    occupation
    , ends_with('.l')
  ) %>%
  select(
    !list_items.remove %>%
      flatten_chr()
  ) -> df_occupations.efa

# [EFA] -----------------------
# - Run factor analysis on the whole data frame ---------------------------
fun_efa.bestmodel(
  .df_data.numeric = 
    df_occupations.efa
  , .dbl_weights = 
    .dbl_weights
  , .chr_rotation =
    .chr_rotation
  , .auto_select.nfactors =
    .auto_select.nfactors
  , .int_nfactors.vector =
    .int_nfactors
  , .int_min.factor_size =
    .int_min.factor_size
  , .remove_unacceptable_MSAi.items =
    .remove_unacceptable_MSAi.items
  , .remove_under_loading.items =
    .remove_under_loading.items
  , .remove_cross_loading.items =
    .remove_cross_loading.items
  , .dbl_under_loading.threshold =
    .dbl_under_loading.threshold
  , .dbl_cross_loading.threshold =
    .dbl_cross_loading.threshold
  , .show_diagrams =
    .show_diagrams
  , .show_results =
    .show_results
) -> list_efa.equamax.15

# - Factor names ----------------------------------------------------------
tibble(
  factor = 
    paste0('factor', seq(1,.int_nfactors))
  , factor.name = 
    c(
      'discernment'
      , 'mechanical skills'
      , 'health science'
      , 'transportation / vehicle operation / operation'
      , 'management'
      , 'social skills'
      , 'analytical skills'
      , 'business'
      , 'dexterity'
      , 'administrative'
      , 'building'
      , 'intelligence'
      , 'industrial / job hazards'
      , 'arts & humanities'
      , 'robustness'
    )
) -> df_factor.names

list_efa.equamax.15$
  best.model$
  loadings.long.factors %>%
  full_join(df_factor.names) %>%
  group_by(factor) %>% 
  mutate(
    nitems = n()
  ) %>% 
  ungroup() %>% 
  select(
    factor
    , factor.name
    , nitems
    , item
    , loading
  ) %>%
  ungroup() -> df_loadings.factors

# - Top items (all education levels) -------------------------------------------------------------
# Top items selection
list(
  'atlas.mini' = 0.25
  , 'atlas.pro' = 0.5
  , 'atlas.complete' = 1
) %>%
  map(
    ~ df_occupations.efa %>% 
      select(ends_with('.l')) %>% 
      ncol() * .x
  ) %>% 
  map(
    ~ fun_efa.topitems(
      .df_data.numeric = 
        df_occupations.efa
      , .dbl_weights = 
        df_occupations %>% 
        pull(employment2)
      , .efa_model = 
        list_efa.equamax.15$
        EFA.workflow$
        EFA$
        EFA.15factors$
        model
      , .int_n.items.total = .x
      , .lgc_uneven.factors = T
      , .int_min.factor_size = 3
    ) %>% 
      full_join(df_factor.names) %>% 
      relocate(
        factor
        , factor.name
        , factor.items
        , item
        , everything()
      )
  ) -> list_questionnaires

list_questionnaires

list_questionnaires %>% 
  map(
    ~ .x %>%
      group_by(
        factor
        , factor.name
      ) %>% 
      tally() %>% 
      arrange(desc(n))
  )

list_questionnaires$atlas.mini
list_questionnaires$atlas.pro
list_questionnaires$atlas.complete

# - Top items (highly qualified) -------------------------------------------------------------
# Top items selection
list(
  'atlas.mini.high_edu' = 0.25
  , 'atlas.pro.high_edu' = 0.5
  , 'atlas.complete.high_edu' = 1
) %>%
  map(
    ~ df_occupations.efa %>%
      select(ends_with('.l')) %>%
      ncol() * .x
  ) %>%
  map(
    ~ fun_efa.topitems(
      .df_data.numeric =
        df_occupations %>%
        filter(str_detect(
          entry_level_education %>%
            str_to_lower()
          , 'master|bachelor|doct|post'
        )) %>% 
        select(
          df_occupations.efa %>%
            names()
        )
      , .dbl_weights =
        df_occupations %>%
        filter(str_detect(
          entry_level_education %>%
            str_to_lower()
          , 'master|bachelor|doct|post'
        )) %>% 
        pull(
          employment2
        )
      , .efa_model =
        list_efa.equamax.15$
        EFA.workflow$
        EFA$
        EFA.15factors$
        model
      , .int_n.items.total = .x
      , .lgc_uneven.factors = T
      , .int_min.factor_size = 3
    ) %>%
      inner_join(df_factor.names) %>%
      relocate(
        factor
        , factor.name
        , factor.items
        , item
        , everything()
      )
  ) -> list_questionnaires.high_edu

list_questionnaires.high_edu

list_questionnaires.high_edu %>%
  map(
    ~ .x %>%
      group_by(
        factor
        , factor.name
      ) %>%
      tally() %>%
      arrange(desc(n))
  )

list_questionnaires.high_edu$atlas.mini
list_questionnaires.high_edu$atlas.pro
list_questionnaires.high_edu$atlas.complete

# - Top items (poorly qualified) -------------------------------------------------------------
# Top items selection
list(
  'atlas.mini.low_edu' = 0.25
  , 'atlas.pro.low_edu' = 0.5
  , 'atlas.complete.low_edu' = 1
) %>%
  map(
    ~ df_occupations.efa %>%
      select(ends_with('.l')) %>%
      ncol() * .x
  ) %>%
  map(
    ~ fun_efa.topitems(
      .df_data.numeric =
        df_occupations %>%
        filter(!str_detect(
          entry_level_education %>%
            str_to_lower()
          , 'master|bachelor|doct|post'
        )) %>% 
        select(
          df_occupations.efa %>%
            names()
        )
      , .dbl_weights =
        df_occupations %>%
        filter(!str_detect(
          entry_level_education %>%
            str_to_lower()
          , 'master|bachelor|doct|post'
        )) %>% 
        pull(
          employment2
        )
      , .efa_model =
        list_efa.equamax.15$
        EFA.workflow$
        EFA$
        EFA.15factors$
        model
      , .int_n.items.total = .x
      , .lgc_uneven.factors = T
      , .int_min.factor_size = 3
    ) %>%
      inner_join(df_factor.names) %>%
      relocate(
        factor
        , factor.name
        , factor.items
        , item
        , everything()
      )
  ) -> list_questionnaires.low_edu

list_questionnaires.low_edu

list_questionnaires.low_edu %>%
  map(
    ~ .x %>%
      group_by(
        factor
        , factor.name
      ) %>%
      tally() %>%
      arrange(desc(n))
  )

list_questionnaires.low_edu$
  atlas.pro %>% 
  pull(item) %in%
  (
    list_questionnaires.high_edu$
      atlas.pro %>% 
      pull(item)
  )

list_questionnaires.low_edu$atlas.pro
list_questionnaires.low_edu$atlas.complete

# # - dsds ------------------------------------------------------------------
# list_questionnaires.high_edu$
#   atlas.pro %>% 
#   pull(item) %>%
#   dplyr::setdiff(
#     list_questionnaires$
#       atlas.pro %>% 
#       pull(item)
#   ) %>% 
#   c(
#     list_questionnaires$
#       atlas.pro %>% 
#       pull(item) %>%
#       dplyr::setdiff(
#         list_questionnaires.high_edu$
#           atlas.pro %>% 
#           pull(item)
#       )
#   ) %>% 
#   unique() %>% 
#   length()
#   writeClipboard()
# 
# list_questionnaires.low_edu$
#   atlas.pro %>% 
#   pull(item) %>%
#   dplyr::setdiff(
#     list_questionnaires$
#       atlas.pro %>% 
#       pull(item)
#   ) %>% 
#   c(
#     list_questionnaires$
#       atlas.pro %>% 
#       pull(item) %>%
#       dplyr::setdiff(
#         list_questionnaires.low_edu$
#           atlas.pro %>% 
#           pull(item)
#       )
#   ) %>% 
#   unique() %>% 
#   length()
#   writeClipboard()
# 
# list_questionnaires.high_edu$
#   atlas.pro %>% 
#   pull(item) %>%
#   dplyr::setdiff(
#     list_questionnaires.low_edu$
#       atlas.pro %>% 
#       pull(item)
#   ) %>% 
#   c(
#     list_questionnaires.low_edu$
#       atlas.pro %>% 
#       pull(item) %>%
#       dplyr::setdiff(
#         list_questionnaires.high_edu$
#           atlas.pro %>% 
#           pull(item)
#       )
#   ) %>% 
#   unique() %>% 
#   length()
#   writeClipboard()

# [TESTING] ---------------------------------------------------------------
# - Use questionnaires for knn matching -----------------------------------
# Sample occupation
df_occupations.efa %>%
  slice_sample(
    n = 1
  ) -> df_sample

# Apply questionnaires
list_questionnaires %>% 
  map(
    ~ fun_KNN.matching(
      .df_data.numeric = 
        df_occupations.efa %>% 
        select(
          occupation
          , .x$item
        )
      , .vec_query.numeric =
        df_sample %>% 
        select(.x$item)
      , .int_k = nrow(df_occupations.efa)
      , .imput.over_qualification = T
      , .dbl_over_qualification.threshold = 0
    ) %>% 
      select(
        rank
        , occupation
        , similarity
      ) %>% 
      slice(
        seq(1, 7, 1)
        , seq(n() - 7 + 1, n())
      )
  )

list_questionnaires$atlas.mini
list_questionnaires$atlas.pro
list_questionnaires$atlas.complete

# - Real questionnaires ------------------------------------------
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
) -> df_sample

source("C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R")

fun_KNN.matching(
  .df_data.numeric =
    df_occupations.efa %>%
    select(
      occupation
      , list_questionnaires$
        atlas.mini$
        item
    )
  , .vec_query.numeric =
    df_sample
  , .int_k = nrow(df_occupations.efa)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = 0
) %>%
  full_join(
    df_occupations.pop %>%
      group_by(occupation) %>%
      slice(1) %>%
      ungroup() %>%
      select(
        occupation
        , contains('wage')
        , contains('entry_level')
      )
  ) %>%
  add_row(
    df_sample
    , .before = 1
  ) %>%
  mutate(
    across(
      .cols = ends_with('.l')
      ,.fns = ~ .x - first(.x)
    )
    , across(
      .cols = ends_with('.l')
      ,.fns = ~ pmax(.x, 0)
    )
  ) %>%
  rowwise() %>%
  mutate(
    .after = occupation
    , attribute.gap =
      sum(c_across(ends_with('.l')))
  ) %>%
  ungroup() %>%
  select(
    rank
    , occupation
    , contains('entry_level')
    , contains('wage')
    , attribute.gap
    , similarity
  ) -> df_knn.matching

df_knn.matching %>%
  slice(-1) %>%
  rename(
    wage = annual_wage_2021
  ) %>%
  mutate(
    .after = occupation
    , career.coef =
      sqrt(
        (wage / max(wage)) *
          similarity
      ) / (1 + attribute.gap)
  ) %>%
  arrange(desc(
    career.coef
  )) %>%
  print(n = 22)

df_knn.matching %>%
  mutate(
    recommended = ifelse(
      round(similarity, 2) >= 0.67
      | is.na(similarity)
      , 'Recommended'
      , 'Not Recommended'
    )
  ) %>%
  fun_plot.bar(aes(
    x = rank
    , y = similarity
    , fill = recommended
  )
  , .theme = theme_ridges(center_axis_labels = T) +
    theme(
      title = element_text(hjust = 0.5)
      , plot.title.position = 'plot'
      , legend.position = 'bottom'
      , legend.justification = 'center'
      , legend.key.size = unit(0.5,'cm')
      , legend.key.width = unit(2,'cm')
      # , plot.margin = margin(1, 1, 1, 1,'cm')
      , plot.margin = margin(0, 0, 0, 0,'cm')
    )
  , .fun_format.y = function(x){percent(x,accuracy = 1)}
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
  , .chr_manual.pal = setNames(
    c(
      list_pal.atlas$purple3
      , list_pal.atlas$grey
    )
    , c(
      'Recommended'
      , 'Not Recommended'
    )
  )
  , .chr_manual.aes = c(
    'fill', 'color'
  )
  , .list_legend = list(
    color = 'none'
  )
  , .list_labs = list(
    y = 'Professional Compatibility'
    , fill = NULL
  ))

df_knn.matching %>%
  fun_plot.histogram(aes(
    x = similarity
    , y = after_stat(density)
  )
  , .dbl_limits.y = c(0,1.25*max(density(df_knn.matching$similarity)$y))
  , .list_axis.x.args = list(
    limits = c(-0.1,1.1)
    , breaks = seq(0,1,.25)
  )
  , .fun_format.x = percent_format(accuracy = 1)
  , .list_labs = list(
    title = NULL
    , subtitle = NULL
    , x = 'Professional Compatibility'
    , y = NULL
  )
  , .theme = ggridges::theme_ridges(font_size = 11, center_axis_labels = T) +
    theme(
      plot.margin = margin(0, 0, 0, 0)
      , axis.text.y = element_blank()
    )
  ) +
  geom_density(aes(
    x = similarity
  )
  , size = 1.2
  ) +
  geom_textvline(
    xintercept = 0.67
    , label = 'Recommended'
    , color = list_pal.atlas$green
    , fontface = 'bold'
    , linetype = 1
    , linewidth = 1.35
    , hjust = 0.125
    , vjust = -0.5
  )

# [OUTPUT] ----------------------------------------------------------------
# - Export to excel -------------------------------------------------------
# Model with main factor loadings
df_loadings.factors %>% 
  openxlsx::write.xlsx(
    file = 'df_efa.equamax.15factors.xlsx'
  )

map2(
  .x = list_questionnaires
  , .y = names(list_questionnaires)
  , .f = 
    ~ .x %>%
    openxlsx::write.xlsx(
      file = paste0('questionnaire_', .y,'.xlsx')
    )
)

map2(
  .x = list_questionnaires.high_edu
  , .y = names(list_questionnaires.high_edu)
  , .f = 
    ~ .x %>%
    openxlsx::write.xlsx(
      file = paste0('questionnaire_', .y,'.xlsx')
    )
)

map2(
  .x = list_questionnaires.low_edu
  , .y = names(list_questionnaires.low_edu)
  , .f = 
    ~ .x %>%
    openxlsx::write.xlsx(
      file = paste0('questionnaire_', .y,'.xlsx')
    )
)

# list_questionnaires$
#   atlas.mini %>% 
#   openxlsx::write.xlsx(
#     file = 'df_atlas_mini_questionnaire.xlsx'
#   )

# Questionnaires

# # dsds -----------------------
# # Occupations data frame on a 0 to 100 scale
# map(
#   set_names(
#     c(
#       'oblimin'
#       , 'Promax'
#       , 'promax'
#       # , 'simplimax' [no]
#       , 'bentlerQ'
#       , 'cluster'
#     )
#     , c(
#       'oblimin'
#       , 'Promax'
#       , 'promax'
#       # , 'simplimax' [no]
#       , 'bentlerQ'
#       , 'cluster'
#     )
#   )
#   , ~
#     fun_efa.bestmodel(
#       .df_data.numeric =
#         df_occupations %>%
#         select(ends_with('.l')) #%>%
#       # select(1:120)
#       , .dbl_weights =
#         df_occupations %>%
#         pull(employment2)
#       , .auto_select.nfactors = T
#       , .int_min.factor_size =
#         .int_min.factor_size
#       , .chr_rotation = .x
#       , .remove_unacceptable_MSAi.items =
#         .remove_unacceptable_MSAi.items
#       , .remove_under_loading.items =
#         .remove_under_loading.items
#       , .remove_cross_loading.items =
#         .remove_cross_loading.items
#       , .dbl_under_loading.threshold =
#         .dbl_under_loading.threshold
#       , .dbl_cross_loading.threshold =
#         .dbl_cross_loading.threshold
#       , .show_diagrams =
#         .show_diagrams
#       , .show_results =
#         .show_results
#     )
# ) -> list_efa.oblique
# 
# list_efa.oblique %>%
#   map(~ .x$best.models.evaluation) %>%
#   bind_rows(.id = 'rotation') %>% view
# 
# list_efa.orthogonal %>%
#   map(~ .x$best.models.evaluation) %>%
#   bind_rows(.id = 'rotation') %>% view
# 
# list_efa.oblique %>%
#   list_flatten() %>%
#   map(
#     ~ .x$EFA
#   ) %>%
#   list_flatten() %>%
#   map(
#     ~ .x$suggested.rotation
#   ) %>%
#   compact() %>%
#   bind_rows(
#     .id = 'model'
#   ) %>%
#   pivot_longer(
#     cols = everything()
#     , names_to = 'model'
#     , values_to = 'suggested.rotation'
#   ) %>%
#   mutate(
#     .before = 1
#     , rotation =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,1]
#     , model =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,2]
#   ) -> df_rotation.oblique
# 
# list_efa.oblique %>%
#   list_flatten() %>%
#   map(
#     ~ .x$EFA
#   ) %>%
#   list_flatten() %>%
#   map(
#     ~ .x$suggested.rotation
#   ) %>%
#   compact() %>%
#   bind_rows(
#     .id = 'model'
#   ) %>%
#   pivot_longer(
#     cols = everything()
#     , names_to = 'model'
#     , values_to = 'suggested.rotation'
#   ) %>%
#   mutate(
#     .before = 1
#     , rotation =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,1]
#     , model =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,2]
#   ) -> df_rotation.oblique
# 
# list_efa.orthogonal %>%
#   list_flatten() %>%
#   map(
#     ~ .x$EFA
#   ) %>%
#   list_flatten() %>%
#   map(
#     ~ .x$suggested.rotation
#   ) %>%
#   compact() %>%
#   bind_rows(
#     .id = 'model'
#   ) %>%
#   pivot_longer(
#     cols = everything()
#     , names_to = 'model'
#     , values_to = 'suggested.rotation'
#   ) %>%
#   mutate(
#     .before = 1
#     , rotation =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,1]
#     , model =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,2]
#   ) -> df_rotation.orthogonal
# 
# sum(
#   nrow(df_rotation.oblique)
#   , nrow(df_rotation.orthogonal)
# )
# 
# df_rotation.oblique %>%
#   drop_na() %>%
#   group_by(suggested.rotation) %>%
#   tally()
# 
# df_rotation.oblique %>%
#   drop_na() %>%
#   filter(
#     suggested.rotation ==
#       'oblique'
#   )
# 
# list_efa.oblique %>%
#   list_flatten() %>%
#   map(
#     ~ .x$EFA
#   ) %>%
#   list_flatten() %>%
#   map(
#     ~ .x$loadings.long.factors
#   ) %>%
#   compact() %>%
#   bind_rows(
#     .id = 'model'
#   ) -> df_items.oblique
# 
# df_items.oblique %>%
#   mutate(
#     .after = 1
#     , rotation =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,1]
#     , nfactors =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,2] %>%
#       parse_number()
#   ) %>%
#   filter(
#     rotation == 'Promax'
#     # rotation == 'promax' [no]
#     # rotation == 'oblimin' [ok]
#     # rotation == 'oblimin'
#     # rotation == 'bentlerQ'
#     # , nfactors == 16
#     # , nfactors == 14
#     , nfactors == 15
#   ) %>%
#   select(1:6) %>%
#   view
# 
# 
# df_items.oblique %>%
#   mutate(
#     model =
#       model %>%
#       str_remove_all(
#         '_EFA.workflow_EFA'
#       )
#   ) %>%
#   select(1:4) %>%
#   openxlsx::write.xlsx(file = 'df_items.oblique2.xlsx')
# 
# map(
#   set_names(
#     c(
#       'varimax'
#       , 'quartimax'
#       , 'equamax'
#     )
#     , c(
#       'varimax'
#       , 'quartimax'
#       , 'equamax'
#     )
#   )
#   , ~
#     fun_efa.bestmodel(
#       .df_data.numeric =
#         df_occupations %>%
#         select(ends_with('.l')) #%>%
#       # select(1:120)
#       , .dbl_weights =
#         df_occupations %>%
#         pull(employment2)
#       , .auto_select.nfactors = T
#       , .int_min.factor_size =
#         .int_min.factor_size
#       , .chr_rotation = .x
#       , .remove_unacceptable_MSAi.items =
#         .remove_unacceptable_MSAi.items
#       , .remove_under_loading.items =
#         .remove_under_loading.items
#       , .remove_cross_loading.items =
#         .remove_cross_loading.items
#       , .dbl_under_loading.threshold =
#         .dbl_under_loading.threshold
#       , .dbl_cross_loading.threshold =
#         .dbl_cross_loading.threshold
#       , .show_diagrams =
#         .show_diagrams
#       , .show_results =
#         .show_results
#     )
# ) -> list_efa.orthogonal
# 
# list_efa.orthogonal %>%
#   map(~ .x$best.models.evaluation) %>%
#   bind_rows(.id = 'rotation') %>% view
# 
# list_efa.orthogonal %>%
#   list_flatten() %>%
#   map(
#     ~ .x$EFA
#   ) %>%
#   list_flatten() %>%
#   map(
#     ~ .x$loadings.long.factors
#   ) %>%
#   compact() %>%
#   bind_rows(
#     .id = 'model'
#   ) -> df_items.orthogonal
# 
# # df_occupations$near_vision.l
# # df_occupations$foreign_language.l
# # df_occupations$duration_of_typical_work_week.l
# # df_occupations$telephone.l
# # df_occupations$electronic_mail.l
# # df_occupations$letters_and_memos.l
# # df_occupations$food_production.l
# 
# # list(
# #   'near_vision.l'
# #   # , 'foreign_language.l'
# #   , 'duration_of_typical_work_week.l'
# #   # , 'telephone.l'
# #   , 'electronic_mail.l'
# #   # , 'letters_and_memos.l'
# #   # , 'food_production.l'
# #   , 'structured_versus_unstructured_work.l'
# #   # , 'establishing_and_maintaining_interpersonal_relationships.l'
# #   # , 'communicating_with_supervisors_peers_or_subordinates.l'
# #   # , 'speaking.l'
# #   # , 'writing.l'
# #   # , 'reading_comprehension.l'
# #   # , 'degree_of_automation.l'
# #   , 'work_schedules.l'
# #   , 'indoors_environmentally_controlled.l'
# #   # , 'impact_of_decisions_on_co_workers_or_company_results.l'
# #   , 'spend_time_sitting.l'
# # ) -> list_items.remove
# 
# list(
#   'near_vision.l'
#   , 'foreign_language.l'
#   , 'duration_of_typical_work_week.l'
#   # , 'telephone.l'
#   # , 'electronic_mail.l'
#   , 'letters_and_memos.l'
#   , 'food_production.l'
#   , 'structured_versus_unstructured_work.l'
#   , 'establishing_and_maintaining_interpersonal_relationships.l'
#   , 'communicating_with_supervisors_peers_or_subordinates.l'
#   , 'speaking.l'
#   , 'writing.l'
#   , 'reading_comprehension.l'
#   , 'degree_of_automation.l'
#   , 'work_schedules.l'
#   , 'indoors_environmentally_controlled.l'
#   , 'impact_of_decisions_on_co_workers_or_company_results.l'
#   , 'spend_time_sitting.l'
#   , 'law_and_government.l'
# ) -> list_items.remove
# 
# list_items.remove %>%
#   flatten_chr() %>%
#   writeClipboard()
# set_names(
#   list_items.remove %>%
#     flatten_chr()
# ) %>%
#   map_df(
#     ~ df_occupations %>%
#       select(ends_with('.l')) %>%
#       weights::wtd.cors(
#         weight =
#           df_occupations %>%
#           pull(employment2)
#       ) %>%
#       as_tibble(
#         rownames = 'item'
#       ) %>%
#       filter(item == .x) %>%
#       pivot_longer(
#         cols = -item
#         , names_to = 'items'
#         , values_to = 'correlation'
#       ) %>%
#       filter(items != .x) %>%
#       arrange(desc(
#         correlation
#       ))
#   ) -> df_items.problematic
# 
# df_items.problematic %>%
#   filter(str_detect(
#     item, 'law'
#   )) %>%
#   mutate(correlation = round(correlation, 4)) %>% view
# 
# df_items.problematic %>%
#   group_by(item) %>%
#   slice(1,n()) %>%
#   ungroup() %>% view
# 
# df_occupations %>%
#   select(contains(
#     'comprehension'
#   )) %>%
#   names
# 
# df_occupations %>%
#   select(contains(
#     'expression'
#   )) %>%
#   names
# 
# df_occupations %>%
#   select(contains(
#     'read'
#   )) %>%
#   names
# 
# weights::wtd.cors(
#   x = df_occupations$deductive_reasoning.l
#   , y = df_occupations$inductive_reasoning.l
#   , weight = df_occupations$employment2
# )
# 
# df_occupations %>%
#   select(ends_with('.l')) %>%
#   select(
#     !list_items.remove %>%
#       flatten_chr()
#   ) %>%
#   fun_efa.nfactors()
# 
# fun_efa.bestmodel(
#   .df_data.numeric =
#     df_occupations %>%
#     select(ends_with('.l')) %>%
#     select(
#       !list_items.remove %>%
#         flatten_chr()
#     )
#   , .dbl_weights =
#     df_occupations %>%
#     pull(employment2)
#   # , .int_nfactors.vector = c(4,5,8,seq(13,16))
#   , .int_nfactors.vector = seq(10,16)
#   , .auto_select.nfactors = F
#   # .auto_select.nfactors
#   , .int_min.factor_size =
#     .int_min.factor_size
#   # , .chr_rotation = 'varimin' [no]
#   , .chr_rotation = 'equamax'
#   # , .chr_rotation = 'varimax' [no]
#   # , .chr_rotation = 'quartimax' [no]
#   # , .chr_rotation = 'oblimin' [no]
#   # , .chr_rotation = 'Promax' [no]
#   # , .chr_rotation = 'promax' [no]
#   , .remove_unacceptable_MSAi.items =
#     .remove_unacceptable_MSAi.items
#   , .remove_under_loading.items =
#     .remove_under_loading.items
#   , .remove_cross_loading.items =
#     .remove_cross_loading.items
#   , .dbl_under_loading.threshold =
#     .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold =
#     .dbl_cross_loading.threshold
#   , .show_diagrams =
#     .show_diagrams
#   , .show_results =
#     .show_results
# ) -> list_efa.equamax
# 
# list_efa.equamax$
#   best.models.evaluation %>%
#   view
# 
# map(
#   list_efa.equamax$
#     EFA.workflow$
#     EFA
#   , ~ .x$loadings.long.factors
# ) -> dsds
# 
# map_chr(
#   dsds
#   , ~ .x %>%
#     filter(str_detect(item, 'medicine')) %>%
#     # filter(str_detect(item, 'law')) %>%
#     pull(factor) %>%
#     as.character()
# ) -> lalala
# 
# Map(
#   function(ds,la){
#     
#     ds %>%
#       filter(factor == la) %>%
#       select(item, factor, loading)
#     
#   }
#   , ds = dsds
#   , la = lalala
# ) -> dsdsds
# 
# dsdsds %>%
#   map(
#     ~ .x %>%
#       pull(item) %>%
#       str_detect('philosophy|theology|sociology') %>%
#       # str_detect('philosophy|theology|sociology|law') %>%
#       any()
#   )
# 
# dsds$
#   # EFA.10factors %>%
#   # EFA.6factors [no] %>%
#   # EFA.12factors [no] %>%
#   # EFA.13factors [no] %>%
#   # EFA.14factors [no] %>%
#   # EFA.15factors [no] %>%
#   # EFA.16factors [no] %>%
#   # EFA.17factors [no] %>%
#   
#   # EFA.12factors [no] %>%
#   # EFA.13factors [no] %>%
# # EFA.14factors [no] %>%
# # EFA.15factors %>%
# # EFA.16factors %>%
# 
# # oblimin
# # EFA.11factors [no] %>%
# # EFA.12factors [no] %>%
# # EFA.13factors [no] %>%
# # EFA.14factors %>%
# # EFA.15factors %>%
# # EFA.16factors %>%
# 
# #equamax no gov
# # EFA.12factors %>%
# # EFA.13factors [no] %>%
# # EFA.14factors [ok] %>%
# EFA.15factors #[this one] %>%
# # EFA.16factors %>%
# # EFA.16factors %>%
# select(1:3) %>%
#   group_by(factor) %>%
#   mutate(
#     .before = factor
#     , nitems = n()
#   ) -> df_factors
# 
# df_factors %>% view
# 
# # df_factors %>%
# #   filter(str_detect(
# #     item, 'law'
# #   ))
# 
# # df_factors %>%
# #   split(.$factor) %>%
# #   pluck(4) %>%
# #   print(n = nrow(.))
# 
# df_factors %>%
#   split(.$factor)
# 
# df_factors %>%
#   # slice(1:3) %>%
#   # ungroup() %>%
#   split(.$factor) %>%
#   set_names(
#     c(
#       'discernment'
#       , 'mechanical skills'
#       , 'health science'
#       , 'transportation / operation'
#       , 'management'
#       , 'social skills'
#       , 'analytical skills'
#       , 'business'
#       , 'dexterity'
#       , 'administrative'
#       , 'building'
#       , 'intelligence'
#       , 'industrial / job hazards'
#       , 'arts & humanities'
#       , 'robustness'
#     )
#   ) %>%
#   bind_rows(.id = 'factor.name') %>%
#   openxlsx::write.xlsx(file = 'df_efa.equamax.15factors.xlsx')
# 
# map2(
#   .x = dsds
#   , .y = lalala
#   ~ .x %>%
#     ungroup() %>%
#     filter(factor == .y)
# )
# 
# list_efa.equamax$
#   EFA.workflow$
#   EFA$
#   EFA.5factors$
#   loadings.long.factors %>%
#   filter(str_detect(item, 'medicine')) %>%
#   pull(factor)
# 
# list_efa.equamax.13_17$
#   EFA.workflow$
#   EFA %>%
#   map(
#     ~ .x$removed.items
#   )
# 
# # fun_efa.mfa(
# #   .df_data.numeric =
# #     df_occupations %>%
# #     select(ends_with('.l'))
# #   , .dbl_weights =
# #     df_occupations %>%
# #     pull(employment2)
# #   , .int_nfactors.vector = seq(20,23)
# #   , .chr_rotation = 'equamax'
# #   # , .chr_rotation = 'oblimin'
# #   , .remove_unacceptable_MSAi.items =
# #     .remove_unacceptable_MSAi.items
# #   , .remove_under_loading.items =
# #     .remove_under_loading.items
# #   , .remove_cross_loading.items =
# #     .remove_cross_loading.items
# #   , .dbl_under_loading.threshold =
# #     .dbl_under_loading.threshold
# #   , .dbl_cross_loading.threshold =
# #     .dbl_cross_loading.threshold
# #   , .show_diagrams =
# #     .show_diagrams
# #   , .show_results =
# #     .show_results
# # ) -> list_efa.20_23.equamax
# 
# list_efa.orthogonal$
#   equamax$
#   EFA.workflow$
#   EFA$
#   EFA.15factors$
#   loadings %>%
#   filter(
#     item == 'establishing_and_maintaining_interpersonal_relationships.l'
#   ) %>%
#   # rowwise() %>%
#   mutate(
#     loading.max = max(c_across(-item))
#   )
# 
# list_efa.equamax.13_16$
#   best.models.evaluation %>%
#   view
# 
# list_efa.equamax.13_16$
#   best.model$
#   n.factors
# 
# list_efa.equamax.13_17$
#   EFA.workflow$
#   EFA$
#   EFA.13factors$
#   # EFA.14factors$
#   # EFA.15factors$
#   # EFA.16factors$
#   loadings.long.factors %>%
#   select(1:3) %>%
#   group_by(factor) %>%
#   mutate(
#     .before = factor
#     , nitems = n()
#   ) %>%
#   ungroup() %>%
#   view
# 
# # list_efa.20_23.equamax$
# #   EFA$
# #   EFA.22factors$
# #   loadings.long.factors %>%
# #   select(1:3) %>%
# #   group_by(factor) %>%
# #   mutate(
# #     .before = factor
# #     , nitems = n()
# #   ) %>%
# #   ungroup() %>%
# #   view
# 
# df_items.orthogonal %>%
#   mutate(
#     .after = 1
#     , rotation =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,1]
#     , nfactors =
#       str_split_fixed(
#         model
#         , '_EFA.workflow_EFA.'
#         , n = 2
#       )[,2] %>%
#       parse_number()
#   ) %>%
#   filter(
#     # rotation == 'quartimax' [no]
#     # rotation == 'equamax' [ok]
#     rotation == 'equamax'
#     # rotation == 'varimax' [no]
#     , nfactors == 16
#   ) %>%
#   select(1:6) %>% view
# 
# list_efa.equamax.13_16$
#   EFA.workflow$
#   EFA$
#   EFA.13factors$
#   loadings.long.factors %>%
#   select(1:3) %>%
#   group_by(factor) %>%
#   mutate(
#     factor.items = n()
#   ) %>%
#   ungroup() %>%
#   view
# 
# 
# fun_efa.topitems(
#   .df_data.numeric =
#     list_efa.equamax.13_16$
#     EFA.workflow$
#     EFA$
#     EFA.15factors$
#     data
#   , .dbl_weights =
#     df_occupations$
#     employment2
#   , .efa_model =
#     list_efa.equamax.13_16$
#     EFA.workflow$
#     EFA$
#     EFA.15factors$
#     model
#   , .int_n.items.total = 60
#   , .lgc_uneven.factors = T
#   , .int_min.factor_size = 3
# ) %>%
#   arrange(desc(
#     factor.items
#   )) %>%
#   view
# 
# df_items.orthogonal %>%
#   mutate(
#     model =
#       model %>%
#       str_remove_all(
#         '_EFA.workflow_EFA'
#       )
#   ) %>%
#   select(1:4) %>%
#   openxlsx::write.xlsx(file = 'df_items.orthogonal2.xlsx')
# 
# # fun_efa.bestmodel(
# #   .df_data.numeric =
# #     df_occupations %>%
# #     select(ends_with('.l')) %>%
# #     select(1:120)
# #   , .dbl_weights =
# #     df_occupations %>%
# #     pull(employment2)
# #   , .int_nfactors.vector = c(3,5,8,10)
# #   , .auto_select.nfactors = F
# #   , .int_min.factor_size =
# #     .int_min.factor_size
# #   , .chr_rotation =
# #     'promax'
# #     # 'oblimin' [no]
# #     # 'simplimax' [no]
# #     # .chr_rotation
# #     # 'equamax'
# #   , .remove_unacceptable_MSAi.items =
# #     .remove_unacceptable_MSAi.items
# #   , .remove_under_loading.items =
# #     .remove_under_loading.items
# #   , .remove_cross_loading.items =
# #     .remove_cross_loading.items
# #   , .dbl_under_loading.threshold =
# #     .dbl_under_loading.threshold
# #   , .dbl_cross_loading.threshold =
# #     .dbl_cross_loading.threshold
# #   , .show_diagrams =
# #     .show_diagrams
# #   , .show_results =
# #     .show_results
# # ) -> list_efa.models
# 
# list_efa.models$
#   all.models.evaluation %>% view
# 
# list_efa.models$
#   best.models.evaluation %>% view
# 
# list_efa.models$EFA.workflow$EFA$EFA.10factors$n.factors
# 
# list_efa.models$EFA.workflow$EFA$EFA.1factor$loadings.long.factors
# list_efa.models$EFA.workflow$EFA$EFA.10factor$loadings.long.factors
# 
# list_efa.rotations$
#   oblimin$
#   best.models.evaluation %>% view
# 
# fun_efa.topitems(
#   .df_data.numeric =
#     df_occupations %>%
#     select(ends_with('.l')) %>%
#     select(1:120)
#   , .efa_model =
#     list_efa.rotations$
#     oblimin$
#     EFA.workflow$
#     EFA$
#     EFA.2factors$
#     model
#   , .dbl_weights =
#     df_occupations %>%
#     pull(employment2)
#   # , .int_n.items.total = 120
#   , .lgc_uneven.factors = T
#   , .int_n.items.total = 60
#   , .int_min.factor_size = 3
# ) -> df_questionnaire
# 
# df_questionnaire %>% tally
# 
# fun_KNN.matching(
#   .df_data.numeric =
#     df_occupations %>%
#     select(
#       occupation
#       , df_questionnaire %>%
#         pull(item)
#     )
#   , .vec_query.numeric =
#     df_input.user %>%
#     select(
#       df_questionnaire %>%
#         pull(item)
#     )
#   , .int_k = nrow(df_occupations)
#   , .imput.over_qualification = T
#   , .dbl_over_qualification.threshold = 0.17
# ) %>%
#   select(
#     rank
#     , occupation
#     , similarity
#   ) %>%
#   print(n = 20)
# 
# list_efa.models$
#   EFA.workflow$
#   EFA$
#   EFA.10factor$
#   reliability.evaluation %>%
#   view
# 
# list(
#   # 'MR1' = 'Discernment'
#   'MR1' = 'Problem Solving'
#   , 'MR2' = 'Dexterity'
#   , 'MR3' = '(Health) Science'
#   # , 'MR4' = 'Business'
#   , 'MR4' = 'Office'
#   , 'MR5' = 'Perception'
#   , 'MR6' = 'Building'
#   # , 'MR7' = 'Intelligence'
#   , 'MR7' = 'Mathematics'
#   , 'MR8' = 'Physical Fitness'
#   , 'MR9' = 'Arts & Humanities'
#   , 'MR10' = 'Management'
# )
# 
# 
# df_questionnaire %>%
#   ungroup() %>%
#   mutate(
#     item.purity.norm =
#       item.purity -
#       item.purity.norm
#     , item.
#   ) %>% view
# 
# fun_efa.impact(
#   .df_data =
#     df_occupations %>%
#     select(
#       occupation
#       , ends_with('.l')
#     ) %>%
#     select(1:121)
#   , .df_factors.impact =
#     tibble(
#       'factor' =
#         loadings(
#           list_efa.models$
#             EFA.workflow$
#             EFA$
#             EFA.2factors$
#             model
#         )[,] %>%
#         colnames()
#       , 'factor.impact' = c(
#         'Discernment' = (-17/4)
#         , 'Technical Skills' = -50
#       )
#     )
#   , .efa_model =
#     list_efa.models$
#     EFA.workflow$
#     EFA$
#     EFA.2factors$
#     model
#   , .dbl_scale.lb = 0
#   , .dbl_scale.ub = 100
#   , .dbl_impact.lb = -Inf
#   , .dbl_impact.ub = Inf
#   , .lgc_aggregate = T
# ) -> list_efa.impact
# 
# list_efa.impact$individual.impact %>% view
# list_efa.impact$items.impact %>% view
# list_efa.impact$aggregate.impact %>% view
# list_efa.impact$overall.impact %>% view
# 
# list_efa.impact %>%
#   mutate(
#     factor.impact.wgt =
#       factor.impact *
#       factor.loading
#   ) %>% view
# group_by(item) %>%
#   reframe(
#     item.score =
#       item.score %>%
#       unique()
#     , factor.impact.wgt =
#       factor.impact.wgt %>%
#       sum()
#   )
# 
# list_efa.models$
#   EFA.workflow$
#   EFA$
#   EFA.2factors$
#   loadings %>%
#   arrange(desc(
#     factor2
#   )) %>%
#   print(n = nrow(.))
# 
# 
# list_efa.impact$individual.impact %>% view
# list_efa.impact$factors.impact
# 
# fun_top.items.multi.workflow(
#   .df_data.numeric =
#     df_occupations %>%
#     select(ends_with('.l'))
#   , .int_n.items.total = 15 * 10
#   , .auto_select.nfactors = F
#   , .int_nfactors.vector = c(1,15)
#   , .chr_rotation = 'promax'
#   , .remove_unacceptable_MSAi.items = F
#   , .remove_under_loading.items = F
#   , .remove_cross_loading.items = F
#   , .show_diagrams = F
#   , .show_results = F
# ) -> list_efa.models
# 
# 
# dsds$sufficient.loadings
# dsds$reliability.evaluation
# 
# 
# 
# loadings(dsds)[,] %>%
#   as_tibble(
#     rownames = 'item'
#   ) %>%
#   set_names(
#     c(
#       'item'
#       , loadings(dsds) %>%
#         # as.matrix() %>%
#         colnames() %>%
#         str_extract(
#           '[[:digit:]]+'
#         ) %>%
#         paste0('Factor',.)
#     )
#   ) %>%
#   relocate(
#     item
#     , str_sort(
#       names(.)
#       , numeric = T
#     )
#   )
# 
# 
# 
# 
# # Run EFA on occupations data frame
# fun_best.model.workflow(
#   .df_data.numeric =
#     df_occupations
#   , .chr_rotation =
#     .chr_rotation
#   , .auto_select.nfactors =
#     .auto_select.nfactors
#   , .int_min.factor_size =
#     .int_min.factor_size
#   , .remove_unacceptable_MSAi.items =
#     .remove_unacceptable_MSAi.items
#   , .remove_under_loading.items =
#     .remove_under_loading.items
#   , .remove_cross_loading.items =
#     .remove_cross_loading.items
#   , .dbl_under_loading.threshold =
#     .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold =
#     .dbl_cross_loading.threshold
#   , .show_diagrams =
#     .show_diagrams
#   , .show_results =
#     .show_results
# ) -> list_efa.models
# 
# fun_EFA(
#   .df_data.numeric =
#     df_occupations
#   , .chr_rotation =
#     .chr_rotation
#   # , .int_nfactors = 21
#   , .int_nfactors = 15
#   , .remove_unacceptable_MSAi.items =
#     .remove_unacceptable_MSAi.items
#   , .remove_under_loading.items =
#     .remove_under_loading.items
#   , .remove_cross_loading.items =
#     .remove_cross_loading.items
#   , .dbl_under_loading.threshold =
#     .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold =
#     .dbl_cross_loading.threshold
#   , .show_diagrams =
#     .show_diagrams
#   , .show_results =
#     , .show_results
# ) -> list_efa.15factors
# 
# 
# fun_nfactors.selection(
#   df_occupations
# ) -> df_factors.recommended
# 
# fun_EFA.multi(
#   .df_data.numeric =
#     df_occupations
#   , .auto_select.nfactors = F
#   , .int_nfactors.vector =
#     df_factors.recommended$Factors.Suggested
#   , .chr_rotation =
#     .chr_rotation
#   , .remove_unacceptable_MSAi.items =
#     .remove_unacceptable_MSAi.items
#   , .remove_under_loading.items =
#     .remove_under_loading.items
#   , .remove_cross_loading.items =
#     .remove_cross_loading.items
#   , .dbl_under_loading.threshold =
#     .dbl_under_loading.threshold
#   , .dbl_cross_loading.threshold =
#     .dbl_cross_loading.threshold
#   , .show_diagrams =
#     .show_diagrams
#   , .show_results =
#     , .show_results
# ) -> list_efa
# 
# list_efa.models$EFA.workflow$reliability.metrics %>% view
# list_efa.models$EFA.workflow$reliability.evaluation %>% view
