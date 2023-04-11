# [SETUP] -----------------------------------------------------------------
# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')
# ACRONYMS
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_acronyms.R')
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')
# Capital flexibility
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')

library(Hmisc)
# library(ids)
library(stringi)

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

colorRampPalette(c(
  list_pal.atlas$purple2
  , list_pal.atlas$green
)) -> fun_gradient

# # [DATA] DYNAMIC TEXTS -----------------------------------------------------
# df_texts.sections <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTui3FuBxa4USYdgPYdX8sK0Ow6LnMhDLMzVKXf01oqGJ9dMcpsv0pOSv77djlbgMQmyp_3B0_kghJg/pub?gid=1661033231&single=true&output=csv')
# 
# df_texts.overall <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTui3FuBxa4USYdgPYdX8sK0Ow6LnMhDLMzVKXf01oqGJ9dMcpsv0pOSv77djlbgMQmyp_3B0_kghJg/pub?gid=780395980&single=true&output=csv')
# 
# df_texts.kflex <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vTui3FuBxa4USYdgPYdX8sK0Ow6LnMhDLMzVKXf01oqGJ9dMcpsv0pOSv77djlbgMQmyp_3B0_kghJg/pub?gid=872198732&single=true&output=csv')
# 
# df_texts.sections
# df_texts.overall
# df_texts.kflex

# [DATA] EFA-REDUCED QUERY VECTOR -----------------------------------------------
# USER INPUT DATA FRAME
df_input <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv')

# Select user
df_input %>% 
  filter(!str_detect(Name, 'Random')) %>%
  # filter(Name == 'Cao') -> df_input
  drop_na() -> df_input

# EFA-reduced data frame
df_input %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){
        recode(x + 2
               , '1' = 0.00
               , '2' = 0.17
               , '3' = 0.33
               , '4' = 0.50
               , '5' = 0.67
               , '6' = 0.83
               , '7' = 1.00
        )}
    )
  ) -> df_input

# # [FUNCTION] DATA COMPARISON ----------------------------------------------
# fun_data.comparisons.mvp <- function(
    #     
#   # Data
#   .df_data.users
#   , .df_data.comparison = NULL
#   # Factor list
#   , .list_factors = list()
#   
# ){
#   
#   # Arguments validation
#   # Data frames
#   stopifnot(
#     "'.df_data.users' must be a data frame containing the users' item scores." = 
#       is.data.frame(.df_data.users)
#   )
#   
#   stopifnot(
#     "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." = 
#       is.data.frame(.df_data.comparison) |
#       !length(.df_data.comparison)
#   )
#   
#   # # Rewrite
#   # # List
#   # stopifnot(
#   #   "'.list_factors' must be a list of factor keys." = c(
#   #     is.list(.list_factors)
#   #     | !length(.list_factors)
#   #     , !is.data.frame(.list_factors)
#   #   ))
#   
#   # Unique user id (for confidentiality purposes)
#   .df_data.users %>%
#     mutate(
#       # id.unique = Name
#       id.unique = paste0(
#         row_number()
#         , stri_rand_strings(nrow(.), 1)
#       )
#       , .before = everything()
#     ) -> .df_data.users
#   
#   # Make an users's occupation data frame
#   .df_data.comparison %>% 
#     filter(if_any(
#       .cols = !where(is.numeric)
#       ,.fns = 
#         ~ .x %in% all_of(.df_data.users$occupation)
#     )) %>% 
#     select(
#       where(
#         ~ is.numeric(.x)
#         | any(.x %in% .df_data.users$occupation)
#       )) %>% 
#     select(any_of(
#       names(.df_data.users)
#     )) %>% 
#     relocate(
#       !where(is.numeric)
#       , where(is.numeric)
#     ) %>% 
#     rename(id.unique = 1) %>% 
#     slice(
#       match(
#         .df_data.users$occupation
#         , id.unique
#       )
#     ) -> .df_data.comparison
#   
#   # Individual scores, factor scores, and averages
#   .df_data.users %>% 
#     fun_factor.scores2(
#       .list_factor.keys = .list_factors
#       , .lgc_sample.averages = T
#       , .lgc_pivot.long = T
#       , .lgc_totals = F
#     ) -> list_scores
#   
#   # Individual scores, factor scores, and averages
#   .df_data.comparison %>% 
#     fun_factor.scores2(
#       .list_factor.keys = .list_factors
#       , .lgc_sample.averages = F
#       , .lgc_pivot.long = T
#       , .lgc_totals = F
#     ) -> list_scores.occupations
#   
#   # Output
#   return(compact(list(
#     'actual' = list_scores$scores
#     , 'actual.long' = list_scores$scores.long
#     , 'average' = list_scores$scores.average
#     , 'average.long' = list_scores$scores.average.long
#   )))
#   
# }
# 
# # [FUNCTION] PLOT COMPARISON ----------------------------------------------
# fun_text.comparisons.mvp <- function(
    # 
#   # List returned from fun_data.comparison.mvp
#   .list_data.users
#   
#   # Data frame of acronyms
#   , df_acronyms
# 
# ){
# 
#   # Arguments validation
#   # Data frames
#   stopifnot(
#     "'.df_data.users' must be a data frame containing the users' item scores." =
#       is.data.frame(.df_data.users)
#   )
# 
#   stopifnot(
#     "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." =
#       is.data.frame(.df_data.comparison) |
#       !length(.df_data.comparison)
#   )
# 
#   # # Rewrite
#   # # List
#   # stopifnot(
#   #   "'.list_factors' must be a list of factor keys." = c(
#   #     is.list(.list_factors)
#   #     | !length(.list_factors)
#   #     , !is.data.frame(.list_factors)
#   #   ))
# 
#   # Acronyms
#   .list_data.users$actual.long %>%
#     # full_join(df_acronyms) -> df_scores.long
#     left_join(df_acronyms) -> df_scores.long
#     # inner_join(df_acronyms) -> df_scores.long
# 
#   .list_data.users$average.long %>%
#     # full_join(df_acronyms) -> df_average.long
#     left_join(df_acronyms) -> df_average.long
#     # inner_join(df_acronyms) -> df_average.long
# 
#   list_scores.occupations$scores.long %>%
#     # full_join(df_acronyms) -> list_scores.occupations$scores.long
#     left_join(df_acronyms) -> list_scores.occupations$scores.long
#     # inner_join(df_acronyms) -> list_scores.occupations$scores.long
# 
#   # Comparison plots
#   NULL -> plt_items
#   NULL -> list_plt_items.users
#   NULL -> plt_items.average
#   NULL -> plt_factors
#   NULL -> list_plt_factors.users
#   NULL -> plt_factors.average
# 
#   # Item by item comparison
#   df_scores.long %>%
#     fun_plot.heatmap(aes(
#       x = id.unique
#       , y = item.name
#       , fill = item.score
#     )
# 
#     , .list_labs = list(
#       title = 'Item Scores — All Users'
#       , x = 'User'
#       , y = NULL
#       , fill = 'Item Score'
#     )
# 
#     , .theme = theme_ridges(center_axis_labels = T) +
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#         , axis.text.y = element_text(vjust = 0.5)
#       )
# 
#     , .scale_colors = list(
#       # scale_fill_gradientn(
#       scale_fill_viridis(
#         # , values = round(seq(0,1,1/6),2)
#         limits = c(0,1)
#         , breaks = round(seq(0,1, length.out = 7), 2)
#         , labels = function(x){percent(x,1)}
#       )
#     )
# 
#     , .fun_axis.y = scale_y_discrete
#     , .list_axis.y.args = list(
#       position = 'right'
#     )
#     , .fun_format.y = function(y){y}
#     # , .coord_polar = T
#     ) +
#     guides(
#       fill = guide_colorbar(
#         title.position = 'top'
#         , title.hjust = 0.5
#       )
#     ) -> plt_items
# 
#   # Factor by factor comparison
#   df_scores.long %>%
#     fun_plot.heatmap(aes(
#       x = id.unique
#       , y = factor
#       , fill = factor.score
#     )
# 
#     , .list_labs = list(
#       title = 'Factor Scores — All Users'
#       , x = 'User'
#       , y = NULL
#       , fill = 'Factor Score'
#     )
# 
#     , .theme = theme_ridges(center_axis_labels = T) +
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#         , axis.text.y = element_text(vjust = 0.5)
#       )
# 
#     , .scale_colors = list(
#       # scale_fill_gradientn(
#       scale_fill_viridis(
#         # , values = round(seq(0,1,1/6),2)
#         limits = c(0,1)
#         , breaks = round(seq(0,1, length.out = 7), 2)
#         , labels = function(x){percent(x,1)}
#       )
#     )
#     # , .fun_axis.y = scale_y_discrete
#     # , .list_axis.y.args = list(
#     #   position = 'right'
#     # )
#     , .fun_format.y = function(y){y}
#     # , .coord_polar = T
#     ) +
#     guides(
#       fill = guide_colorbar(
#         title.position = 'top'
#         , title.hjust = 0.5
#       )
#     ) -> plt_factors
# 
#   # Item by item comparison (one plot per user)
#   df_average.long %>%
#     mutate(
#       id.unique = 'Average'
#     ) %>%
#     bind_rows(
#       df_scores.long
#       , list_scores.occupations$scores.long
#     ) -> df_average.long
# 
#   map(
#     setNames(
#       .df_data.users$id.unique
#       , .df_data.users$id.unique
#     )
#     , function(.x){
# 
#       df_scores.long %>%
#         filter(
#           id.unique %in% .x
#         ) %>%
#         arrange(desc(item.score)) %>%
#         pull(item.name) %>%
#         unique() -> chr_order
# 
#       c(.x, 'Average'
#         , df_average.long %>%
#           filter(id.unique %in% .x) %>%
#           pull(occupation) %>%
#           unique()
#       ) -> chr_ids
# 
#       df_average.long %>%
#         filter(id.unique %in% chr_ids) %>%
#         mutate(
#           item.name = factor(item.name, levels = chr_order)
#           , id.unique = fct_inorder(id.unique)
#         ) %>%
#         fun_plot.dumbbell2(aes(
#           x = item.score
#           , y = item.name
#           # , label = percent(item.score, accuracy = 1)
#           , color = id.unique
#         )
#         , .sym_facets = item.name
#         , .int_facets = 2
#         , .chr_scales = 'free_y'
#         , .list_labs = list(
#           title = paste0('Item Scores — ', .x)
#           , x = 'Item Score'
#           , y = NULL
#           , color = NULL
#         )
#         , .reorder_fct = F
#         , .chr_manual.pal = set_names(
#           c(
#             list_pal.atlas$purple3
#             , list_pal.atlas$blue1
#             , list_pal.atlas$green
#           )
#           , chr_ids
#         )
#         , .list_axis.x.args = list(
#           limits = c(-0.1, 1.1)
#           , breaks = seq(0, 1, 0.25)
#         )
#         , .fun_format.x = percent_format(accuracy = 1)
#         , .theme = theme_ridges(center_axis_labels = T) +
#           theme(
#             title = element_text(hjust = 0.5)
#             , plot.title.position = 'plot'
#             , axis.text.y = element_blank()
#             , axis.ticks.y = element_blank()
#             , legend.position = 'bottom'
#             , legend.justification = 'center'
#             , strip.background = element_blank()
#             , plot.margin = margin(1, 1, 1, 1,'cm')
#         ))
# 
#     }
#   ) -> list_plt_items.users
# 
#   # map(
#   #   setNames(
#   #     .df_data.users$id.unique
#   #     , .df_data.users$id.unique
#   #   )
#   #   , function(.x){
#   #
#   #     df_scores.long %>%
#   #       filter(
#   #         id.unique %in% .x
#   #       ) %>%
#   #       arrange(item.score) %>%
#   #       pull(item.name) %>%
#   #       unique() -> chr_order
#   #
#   #     df_average.long %>%
#   #       filter(
#   #         # id.unique %in% c(.x, 'Average')
#   #         id.unique %in% c(.x, 'Average'
#   #           # , df_average.long %>%
#   #           #   filter(id.unique %in% .x) %>%
#   #           #   pull(occupation)
#   #           )
#   #       ) %>%
#   #       mutate(
#   #         item.name = factor(item.name, levels = chr_order)
#   #         , id.unique = fct_inorder(id.unique)
#   #       ) %>%
#   #       group_by(id.unique) %>%
#   #       arrange(item.name) %>%
#   #       mutate(
#   #         column = row_number() < (n() / 2)
#   #         , column = factor(column)
#   #       ) %>%
#   #       ungroup() %>%
#   #       fun_plot.bar(aes(
#   #         x = item.name
#   #         , y = item.score
#   #         , label = percent(item.score, accuracy = 1)
#   #         , fill = id.unique
#   #         , color = id.unique
#   #       )
#   #       , .sym_facets = column
#   #       , .chr_scales = 'free'
#   #       , .list_labs = list(
#   #         title = paste0('Item Scores — ', .x)
#   #         , x = NULL
#   #         , y = 'Item Score'
#   #         , fill = NULL
#   #         , color = NULL
#   #       )
#   #       , .coord_polar = F
#   #       , .coord_flip = T
#   #       , .reorder_fct = F
#   #       , .chr_manual.pal = c(
#   #         list_pal.atlas$grey
#   #         , list_pal.atlas$purple3
#   #         , list_pal.atlas$green
#   #       )
#   #       , .chr_manual.aes = c(
#   #         'fill', 'color'
#   #       )
#   #       , .list_axis.y.args = list(
#   #         limits = c(-0.1, 1.1)
#   #         , breaks = seq(0, 1, 0.25)
#   #       )
#   #       , .fun_format.y = percent_format(accuracy = 1)
#   #       , .list_labels.param = list(
#   #         position = c(position_dodge2(0.5, 'single'))
#   #         , hjust = -0.15
#   #       )
#   #       , .theme = theme_ridges(center_axis_labels = T) +
#   #         theme(
#   #           title = element_text(hjust = 0.5)
#   #           , plot.title.position = 'plot'
#   #           , panel.grid.major.y = element_blank()
#   #           , axis.text.y = element_blank()
#   #           , axis.ticks.y = element_blank()
#   #           , legend.position = 'bottom'
#   #           , legend.justification = 'center'
#   #           , strip.background = element_blank()
#   #           , strip.text = element_blank()
#   #           , plot.margin = margin(1, 1, 1, 1,'cm')
#   #         )
#   #       , .list_geom.param = list(
#   #         position = c(position_dodge2(0.5, 'single'))
#   #         , width = 0.5
#   #       )
#   #       ) +
#   #       geom_text(aes(
#   #         label = item.name
#   #         , x = item.name
#   #         , y = 0
#   #       )
#   #       , vjust = -2
#   #       , hjust = 0
#   #       )
#   #
#   #   }
#   # ) -> list_plt_items.users
# 
#   # Factor by factor comparison (one plot per user)
#   map(
#     setNames(
#       .df_data.users$id.unique
#       , .df_data.users$id.unique
#     )
#     , function(.x){
# 
#       df_scores.long %>%
#         filter(
#           id.unique %in% .x
#         ) %>%
#         arrange(factor.score) %>%
#         pull(factor) %>%
#         unique() -> chr_order.fct
# 
#       df_scores.long %>%
#         filter(
#           id.unique %in% .x
#         ) %>%
#         arrange(desc(factor.score)) %>%
#         pull(category) %>%
#         unique() -> chr_order.cat
# 
#       c(.x, 'Average'
#         , df_average.long %>%
#           filter(id.unique %in% .x) %>%
#           pull(occupation) %>%
#           unique()
#       ) -> chr_ids
# 
#       df_average.long %>%
#         filter(id.unique %in% chr_ids) %>%
#         mutate(
#           factor = factor(factor, levels = chr_order.fct)
#           , category = factor(category, levels = chr_order.cat)
#           , id.unique = fct_inorder(id.unique)
#         ) %>%
#         fun_plot.dumbbell2(aes(
#           x = factor.score
#           , y = factor
#           , label = percent(factor.score, accuracy = 1)
#           , color = id.unique
#         )
#         , .sym_facets = category
#         , .int_facets = 1
#         , .chr_scales = 'free_y'
#         , .list_labs = list(
#           title = paste0('Factor Scores — ', .x)
#           , x = 'Factor Score'
#           , y = NULL
#           , color = NULL
#         )
#         , .reorder_fct = F
#         , .reorder_desc = F
#         , .chr_manual.pal = set_names(
#           c(
#             list_pal.atlas$purple3
#             , list_pal.atlas$blue1
#             , list_pal.atlas$green
#           )
#           , chr_ids
#         )
#         , .list_axis.x.args = list(
#           limits = c(-0.1, 1.1)
#           , breaks = seq(0, 1, 0.25)
#         )
#         , .fun_format.x = percent_format(accuracy = 1)
#         , .fun_format.y = function(y){y}
#         , .theme = theme_ridges(center_axis_labels = T) +
#           theme(
#             title = element_text(hjust = 0.5)
#             , plot.title.position = 'plot'
#             , legend.position = 'bottom'
#             , legend.justification = 'center'
#             , strip.background = element_blank()
#             , plot.margin = margin(1, 1, 1, 1,'cm')
#             , axis.text.y = element_text(vjust = 0.5)
#           ))
# 
#     }
#   ) -> list_plt_factors.users
# 
#   # map(
#   #   setNames(
#   #     .df_data.users$id.unique
#   #     , .df_data.users$id.unique
#   #   )
#   #   , function(.x){
#   #
#   #     df_average.long %>%
#   #       filter(
#   #         id.unique %in% c(
#   #           .x, 'Average'
#   #           , df_average.long %>%
#   #             filter(id.unique %in% .x) %>%
#   #             pull(occupation)
#   #         )
#   #       ) %>%
#   #       fun_plot.dumbbell2(aes(
#   #         x = factor.score
#   #         , y = factor
#   #         , color = id.unique
#   #       )
#   #       , .sym_facets = category
#   #       , .int_facets = 1
#   #       , .chr_scales = 'free'
#   #       , .list_labs = list(
#   #         title = paste0('Factor Scores — ', .x)
#   #         , x = 'Factor Score'
#   #         , y = NULL
#   #         , color = NULL
#   #       )
#   #       , .reorder_fct = T
#   #       , .reorder_desc = F
#   #       , .labels = F
#   #       , .list_axis.x.args = list(
#   #         limits = c(0, 1)
#   #         , breaks = seq(0, 1, 0.25)
#   #       )
#   #       , .fun_format.x = percent
#   #       , .fun_format.y = function(y){y}
#   #       , .theme = theme_ridges(center_axis_labels = T) +
#   #         theme(
#   #           title = element_text(hjust = 0.5)
#   #           , plot.title.position = 'plot'
#   #           # , panel.grid.major.y = element_blank()
#   #           , legend.position = 'bottom'
#   #           , legend.justification = 'center'
#   #           , plot.margin = margin(1, 1, 1, 1,'cm')
#   #           , axis.text.y = element_text(vjust = 0.5)
#   #           # , strip.background = element_blank()
#   #         )
#   #       )
#   #
#   #   }
#   # ) -> list_plt_factors.users
# 
#   # Average item by item comparison
#   df_average.long %>%
#     # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
#     fun_plot.bar(aes(
#       x = item.name
#       , y = item.score
#       , label = item.acronym
#       # , label = item.label
#       , fill = category
#       , color = category
#     )
#     , .theme = theme_ridges(center_axis_labels = T) +
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#       )
#     , .fun_format.y = function(x){percent(x,accuracy = 1)}
#     , .coord_polar = T
#     , .fun_polar.labels = percent
#     , .list_axis.y.args = list(
#       breaks = seq(0, 1, length.out = 5)
#     )
#     , .list_geom.param = list(
#       position = c(position_dodge2(0.5, 'single'))
#       , width = 0.5
#     )
#     , .list_labels.param = list(
#       color = list_pal.atlas$black
#     )
#     , .chr_manual.pal = set_names(
#       c(
#         list_pal.atlas$abilities
#         , list_pal.atlas$knowledge
#         , list_pal.atlas$skills
#       )
#       , unique(df_average.long$category)
#     )
#     , .chr_manual.aes = c(
#       'fill', 'color'
#     )
#     , .list_legend = list(
#       color = 'none'
#     )
#     , .list_labs = list(
#       y = 'Average Item Scores'
#       , fill = NULL
#     )
#     ) -> plt_items.average
# 
#   # Average factor by factor comparison
#   df_average.long %>%
#     select(!starts_with('item')) %>%
#     unique() %>%
#     # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
#     fun_plot.lollipop(aes(
#       x = factor
#       , y = factor.score
#       # , label = factor.acronym
#       , label = percent(factor.score, .01)
#       , color = category
#     )
#     , .reorder_fct = T
#     , .reorder_desc = F
#     , .sym_facets = category
#     , .int_facets = 1
#     , .chr_scales = 'free'
#     , .theme = theme_ridges(center_axis_labels = T) +
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#         , axis.text.y = element_text(vjust = 0.5)
#         , strip.background = element_blank()
#       )
#     , .fun_format.x = function(x){x}
#     , .fun_format.y = percent
#     , .list_axis.y.args = list(
#       breaks = seq(0, 1, length.out = 5)
#       , limits = c(-0.1, 1.1)
#     )
#     , .chr_manual.pal = set_names(
#       c(
#         list_pal.atlas$abilities
#         , list_pal.atlas$knowledge
#         , list_pal.atlas$skills
#       )
#       , unique(df_average.long$category)
#     )
#     , .list_legend = list(
#       color = 'none'
#     )
#     , .list_labs = list(
#       title = 'Average Factor Scores'
#       , x = NULL
#       , y = 'Average Factor Scores'
#     )
#     ) -> plt_factors.average
# 
#   # Output
#   return(compact(list(
#     'actual' = list_scores$scores
#     , 'actual.long' = df_scores.long
#     , 'average' = list_scores$scores.average
#     , 'average.long' = df_average.long
#     , 'items' = plt_items
#     , 'items.users' = list_plt_items.users
#     , 'items.average' = plt_items.average
#     , 'factors' = plt_factors
#     , 'factors.users' = list_plt_factors.users
#     , 'factors.average' = plt_factors.average
#   )))
# 
# }
# 
# # [FUNCTION] TEXT COMPARISON ----------------------------------------------
# fun_text.comparisons.mvp <- function(
    #     
#   # Data
#   .df_data.users
#   , .df_data.comparison = NULL
#   # Factor list
#   , .list_factors = list()
#   
# ){
#   
#   # Arguments validation
#   # Data frames
#   stopifnot(
#     "'.df_data.users' must be a data frame containing the users' item scores." = 
#       is.data.frame(.df_data.users)
#   )
#   
#   stopifnot(
#     "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." = 
#       is.data.frame(.df_data.comparison) |
#       !length(.df_data.comparison)
#   )
#   
#   # # Rewrite
#   # # List
#   # stopifnot(
#   #   "'.list_factors' must be a list of factor keys." = c(
#   #     is.list(.list_factors)
#   #     | !length(.list_factors)
#   #     , !is.data.frame(.list_factors)
#   #   ))
#   
#   # Unique user id (for confidentiality purposes)
#   .df_data.users %>%
#     mutate(
#       id.unique = Name
#       # id.unique = paste0(
#       #   row_number()
#       #   , stri_rand_strings(nrow(.), 1)
#       # )
#       , .before = everything()
#     ) -> .df_data.users
#   
#   # Make an users's occupation data frame
#   .df_data.comparison %>% 
#     filter(if_any(
#       .cols = !where(is.numeric)
#       ,.fns = 
#         ~ .x %in% all_of(.df_data.users$occupation)
#     )) %>% 
#     select(
#       where(
#         ~ is.numeric(.x)
#         | any(.x %in% .df_data.users$occupation)
#       )) %>% 
#     select(any_of(
#       names(.df_data.users)
#     )) %>% 
#     relocate(
#       !where(is.numeric)
#       , where(is.numeric)
#     ) %>% 
#     rename(id.unique = 1) %>% 
#     slice(
#       match(
#         .df_data.users$occupation
#         , id.unique
#       )
#     ) -> .df_data.comparison
#   
#   # Individual scores, factor scores, and averages
#   .df_data.users %>% 
#     fun_factor.scores2(
#       .list_factor.keys = .list_factors
#       , .lgc_sample.averages = T
#       , .lgc_pivot.long = T
#       , .lgc_totals = F
#     ) -> list_scores
#   
#   # Individual scores, factor scores, and averages
#   .df_data.comparison %>% 
#     fun_factor.scores2(
#       .list_factor.keys = .list_factors
#       , .lgc_sample.averages = F
#       , .lgc_pivot.long = T
#       , .lgc_totals = F
#     ) -> list_scores.occupations
#   
#   # Acronyms
#   list_scores$scores.long %>%
#     # full_join(df_acronyms) -> list_scores$scores.long
#     left_join(df_acronyms) -> list_scores$scores.long
#     # inner_join(df_acronyms) -> list_scores$scores.long
#   
#   list_scores$scores.average.long %>% 
#     # full_join(df_acronyms) -> list_scores$scores.average.long
#     left_join(df_acronyms) -> list_scores$scores.average.long
#     # inner_join(df_acronyms) -> list_scores$scores.average.long
#   
#   list_scores.occupations$scores.long %>%
#     # full_join(df_acronyms) -> list_scores.occupations$scores.long
#     left_join(df_acronyms) -> list_scores.occupations$scores.long
#     # inner_join(df_acronyms) -> list_scores.occupations$scores.long
#   
#   # Comparison plots
#   NULL -> plt_items
#   NULL -> list_plt_items.users
#   NULL -> plt_items.average
#   NULL -> plt_factors
#   NULL -> list_plt_factors.users
#   NULL -> plt_factors.average
#   
#   # Item by item comparison
#   list_scores$scores.long %>%
#     fun_plot.heatmap(aes(
#       x = id.unique
#       , y = item.name
#       , fill = item.score
#     )
#     
#     , .list_labs = list(
#       title = 'Item Scores — All Users'
#       , x = 'User'
#       , y = NULL
#       , fill = 'Item Score'
#     )
#     
#     , .theme = theme_ridges(center_axis_labels = T) + 
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#         , axis.text.y = element_text(vjust = 0.5)
#       )
#     
#     , .scale_colors = list(
#       # scale_fill_gradientn(
#       scale_fill_viridis(
#         # , values = round(seq(0,1,1/6),2)
#         limits = c(0,1)
#         , breaks = round(seq(0,1, length.out = 7), 2)
#         , labels = function(x){percent(x,1)}
#       )
#     )
#     
#     , .fun_axis.y = scale_y_discrete
#     , .list_axis.y.args = list(
#       position = 'right'
#     )
#     , .fun_format.y = function(y){y}
#     # , .coord_polar = T
#     ) + 
#     guides(
#       fill = guide_colorbar(
#         title.position = 'top'
#         , title.hjust = 0.5
#       )
#     ) -> plt_items
#   
#   # Factor by factor comparison
#   list_scores$scores.long %>%
#     fun_plot.heatmap(aes(
#       x = id.unique
#       , y = factor
#       , fill = factor.score
#     )
#     
#     , .list_labs = list(
#       title = 'Factor Scores — All Users'
#       , x = 'User'
#       , y = NULL
#       , fill = 'Factor Score'
#     )
#     
#     , .theme = theme_ridges(center_axis_labels = T) + 
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#         , axis.text.y = element_text(vjust = 0.5)
#       )
#     
#     , .scale_colors = list(
#       # scale_fill_gradientn(
#       scale_fill_viridis(
#         # , values = round(seq(0,1,1/6),2)
#         limits = c(0,1)
#         , breaks = round(seq(0,1, length.out = 7), 2)
#         , labels = function(x){percent(x,1)}
#       )
#     )
#     # , .fun_axis.y = scale_y_discrete
#     # , .list_axis.y.args = list(
#     #   position = 'right'
#     # )
#     , .fun_format.y = function(y){y}
#     # , .coord_polar = T
#     ) + 
#     guides(
#       fill = guide_colorbar(
#         title.position = 'top'
#         , title.hjust = 0.5
#       )
#     ) -> plt_factors
#   
#   # Item by item comparison (one plot per user)
#   list_scores$scores.average.long %>%
#     mutate(
#       id.unique = 'Average'
#     ) %>%
#     bind_rows(
#       list_scores$scores.long
#       , list_scores.occupations$scores.long
#     ) -> df_average.long
#   
#   map(
#     setNames(
#       .df_data.users$id.unique
#       , .df_data.users$id.unique
#     )
#     , function(.x){
# 
#       list_scores$scores.long %>%
#         filter(
#           id.unique %in% .x
#         ) %>%
#         arrange(desc(item.score)) %>%
#         pull(item.name) %>% 
#         unique() -> chr_order
# 
#       c(.x, 'Average'
#         , df_average.long %>%
#           filter(id.unique %in% .x) %>%
#           pull(occupation) %>% 
#           unique()
#       ) -> chr_ids
# 
#       df_average.long %>%
#         filter(id.unique %in% chr_ids) %>%
#         mutate(
#           item.name = factor(item.name, levels = chr_order)
#           , id.unique = fct_inorder(id.unique)
#         ) %>%
#         fun_plot.dumbbell2(aes(
#           x = item.score
#           , y = item.name
#           # , label = percent(item.score, accuracy = 1)
#           , color = id.unique
#         )
#         , .sym_facets = item.name
#         , .int_facets = 2
#         , .chr_scales = 'free_y'
#         , .list_labs = list(
#           title = paste0('Item Scores — ', .x)
#           , x = 'Item Score'
#           , y = NULL
#           , color = NULL
#         )
#         , .reorder_fct = F
#         , .chr_manual.pal = set_names(
#           c(
#             list_pal.atlas$purple3
#             , list_pal.atlas$blue1
#             , list_pal.atlas$green
#           )
#           , chr_ids
#         )
#         , .list_axis.x.args = list(
#           limits = c(-0.1, 1.1)
#           , breaks = seq(0, 1, 0.25)
#         )
#         , .fun_format.x = percent_format(accuracy = 1)
#         , .theme = theme_ridges(center_axis_labels = T) +
#           theme(
#             title = element_text(hjust = 0.5)
#             , plot.title.position = 'plot'
#             , axis.text.y = element_blank()
#             , axis.ticks.y = element_blank()
#             , legend.position = 'bottom'
#             , legend.justification = 'center'
#             , strip.background = element_blank()
#             , plot.margin = margin(1, 1, 1, 1,'cm')
#         ))
# 
#     }
#   ) -> list_plt_items.users
#   
#   # map(
#   #   setNames(
#   #     .df_data.users$id.unique
#   #     , .df_data.users$id.unique
#   #   )
#   #   , function(.x){
#   # 
#   #     list_scores$scores.long %>%
#   #       filter(
#   #         id.unique %in% .x
#   #       ) %>%
#   #       arrange(item.score) %>%
#   #       pull(item.name) %>%
#   #       unique() -> chr_order
#   # 
#   #     df_average.long %>%
#   #       filter(
#   #         # id.unique %in% c(.x, 'Average')
#   #         id.unique %in% c(.x, 'Average'
#   #           # , df_average.long %>%
#   #           #   filter(id.unique %in% .x) %>%
#   #           #   pull(occupation)
#   #           )
#   #       ) %>%
#   #       mutate(
#   #         item.name = factor(item.name, levels = chr_order)
#   #         , id.unique = fct_inorder(id.unique)
#   #       ) %>%
#   #       group_by(id.unique) %>%
#   #       arrange(item.name) %>%
#   #       mutate(
#   #         column = row_number() < (n() / 2)
#   #         , column = factor(column)
#   #       ) %>%
#   #       ungroup() %>%
#   #       fun_plot.bar(aes(
#   #         x = item.name
#   #         , y = item.score
#   #         , label = percent(item.score, accuracy = 1)
#   #         , fill = id.unique
#   #         , color = id.unique
#   #       )
#   #       , .sym_facets = column
#   #       , .chr_scales = 'free'
#   #       , .list_labs = list(
#   #         title = paste0('Item Scores — ', .x)
#   #         , x = NULL
#   #         , y = 'Item Score'
#   #         , fill = NULL
#   #         , color = NULL
#   #       )
#   #       , .coord_polar = F
#   #       , .coord_flip = T
#   #       , .reorder_fct = F
#   #       , .chr_manual.pal = c(
#   #         list_pal.atlas$grey
#   #         , list_pal.atlas$purple3
#   #         , list_pal.atlas$green
#   #       )
#   #       , .chr_manual.aes = c(
#   #         'fill', 'color'
#   #       )
#   #       , .list_axis.y.args = list(
#   #         limits = c(-0.1, 1.1)
#   #         , breaks = seq(0, 1, 0.25)
#   #       )
#   #       , .fun_format.y = percent_format(accuracy = 1)
#   #       , .list_labels.param = list(
#   #         position = c(position_dodge2(0.5, 'single'))
#   #         , hjust = -0.15
#   #       )
#   #       , .theme = theme_ridges(center_axis_labels = T) +
#   #         theme(
#   #           title = element_text(hjust = 0.5)
#   #           , plot.title.position = 'plot'
#   #           , panel.grid.major.y = element_blank()
#   #           , axis.text.y = element_blank()
#   #           , axis.ticks.y = element_blank()
#   #           , legend.position = 'bottom'
#   #           , legend.justification = 'center'
#   #           , strip.background = element_blank()
#   #           , strip.text = element_blank()
#   #           , plot.margin = margin(1, 1, 1, 1,'cm')
#   #         )
#   #       , .list_geom.param = list(
#   #         position = c(position_dodge2(0.5, 'single'))
#   #         , width = 0.5
#   #       )
#   #       ) +
#   #       geom_text(aes(
#   #         label = item.name
#   #         , x = item.name
#   #         , y = 0
#   #       )
#   #       , vjust = -2
#   #       , hjust = 0
#   #       )
#   # 
#   #   }
#   # ) -> list_plt_items.users
# 
#   # Factor by factor comparison (one plot per user)
#   map(
#     setNames(
#       .df_data.users$id.unique
#       , .df_data.users$id.unique
#     )
#     , function(.x){
#       
#       list_scores$scores.long %>%
#         filter(
#           id.unique %in% .x
#         ) %>%
#         arrange(factor.score) %>%
#         pull(factor) %>% 
#         unique() -> chr_order.fct
#       
#       list_scores$scores.long %>%
#         filter(
#           id.unique %in% .x
#         ) %>%
#         arrange(desc(factor.score)) %>%
#         pull(category) %>% 
#         unique() -> chr_order.cat
#       
#       c(.x, 'Average'
#         , df_average.long %>%
#           filter(id.unique %in% .x) %>%
#           pull(occupation) %>% 
#           unique()
#       ) -> chr_ids
#       
#       df_average.long %>%
#         filter(id.unique %in% chr_ids) %>%
#         mutate(
#           factor = factor(factor, levels = chr_order.fct)
#           , category = factor(category, levels = chr_order.cat)
#           , id.unique = fct_inorder(id.unique)
#         ) %>%
#         fun_plot.dumbbell2(aes(
#           x = factor.score
#           , y = factor
#           , label = percent(factor.score, accuracy = 1)
#           , color = id.unique
#         )
#         , .sym_facets = category
#         , .int_facets = 1
#         , .chr_scales = 'free_y'
#         , .list_labs = list(
#           title = paste0('Factor Scores — ', .x)
#           , x = 'Factor Score'
#           , y = NULL
#           , color = NULL
#         )
#         , .reorder_fct = F
#         , .reorder_desc = F
#         , .chr_manual.pal = set_names(
#           c(
#             list_pal.atlas$purple3
#             , list_pal.atlas$blue1
#             , list_pal.atlas$green
#           )
#           , chr_ids
#         )
#         , .list_axis.x.args = list(
#           limits = c(-0.1, 1.1)
#           , breaks = seq(0, 1, 0.25)
#         )
#         , .fun_format.x = percent_format(accuracy = 1)
#         , .fun_format.y = function(y){y}
#         , .theme = theme_ridges(center_axis_labels = T) +
#           theme(
#             title = element_text(hjust = 0.5)
#             , plot.title.position = 'plot'
#             , legend.position = 'bottom'
#             , legend.justification = 'center'
#             , strip.background = element_blank()
#             , plot.margin = margin(1, 1, 1, 1,'cm')
#             , axis.text.y = element_text(vjust = 0.5)
#           ))
#       
#     }
#   ) -> list_plt_factors.users
#   
#   # map(
#   #   setNames(
#   #     .df_data.users$id.unique
#   #     , .df_data.users$id.unique
#   #   )
#   #   , function(.x){
#   # 
#   #     df_average.long %>%
#   #       filter(
#   #         id.unique %in% c(
#   #           .x, 'Average'
#   #           , df_average.long %>%
#   #             filter(id.unique %in% .x) %>%
#   #             pull(occupation)
#   #         )
#   #       ) %>%
#   #       fun_plot.dumbbell2(aes(
#   #         x = factor.score
#   #         , y = factor
#   #         , color = id.unique
#   #       )
#   #       , .sym_facets = category
#   #       , .int_facets = 1
#   #       , .chr_scales = 'free'
#   #       , .list_labs = list(
#   #         title = paste0('Factor Scores — ', .x)
#   #         , x = 'Factor Score'
#   #         , y = NULL
#   #         , color = NULL
#   #       )
#   #       , .reorder_fct = T
#   #       , .reorder_desc = F
#   #       , .labels = F
#   #       , .list_axis.x.args = list(
#   #         limits = c(0, 1)
#   #         , breaks = seq(0, 1, 0.25)
#   #       )
#   #       , .fun_format.x = percent
#   #       , .fun_format.y = function(y){y}
#   #       , .theme = theme_ridges(center_axis_labels = T) +
#   #         theme(
#   #           title = element_text(hjust = 0.5)
#   #           , plot.title.position = 'plot'
#   #           # , panel.grid.major.y = element_blank()
#   #           , legend.position = 'bottom'
#   #           , legend.justification = 'center'
#   #           , plot.margin = margin(1, 1, 1, 1,'cm')
#   #           , axis.text.y = element_text(vjust = 0.5)
#   #           # , strip.background = element_blank()
#   #         )
#   #       )
#   # 
#   #   }
#   # ) -> list_plt_factors.users
# 
#   # Average item by item comparison
#   list_scores$scores.average.long %>%
#     # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
#     fun_plot.bar(aes(
#       x = item.name
#       , y = item.score
#       , label = item.acronym
#       # , label = item.label
#       , fill = category
#       , color = category
#     )
#     , .theme = theme_ridges(center_axis_labels = T) +
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#       )
#     , .fun_format.y = function(x){percent(x,accuracy = 1)}
#     , .coord_polar = T
#     , .fun_polar.labels = percent
#     , .list_axis.y.args = list(
#       breaks = seq(0, 1, length.out = 5)
#     )
#     , .list_geom.param = list(
#       position = c(position_dodge2(0.5, 'single'))
#       , width = 0.5
#     )
#     , .list_labels.param = list(
#       color = list_pal.atlas$black
#     )
#     , .chr_manual.pal = set_names(
#       c(
#         list_pal.atlas$abilities
#         , list_pal.atlas$knowledge
#         , list_pal.atlas$skills
#       )
#       , unique(list_scores$scores.average.long$category)
#     )
#     , .chr_manual.aes = c(
#       'fill', 'color'
#     )
#     , .list_legend = list(
#       color = 'none'
#     )
#     , .list_labs = list(
#       y = 'Average Item Scores'
#       , fill = NULL
#     )
#     ) -> plt_items.average
# 
#   # Average factor by factor comparison
#   list_scores$scores.average.long %>%
#     select(!starts_with('item')) %>%
#     unique() %>%
#     # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
#     fun_plot.lollipop(aes(
#       x = factor
#       , y = factor.score
#       # , label = factor.acronym
#       , label = percent(factor.score, .01)
#       , color = category
#     )
#     , .reorder_fct = T 
#     , .reorder_desc = F
#     , .sym_facets = category
#     , .int_facets = 1
#     , .chr_scales = 'free'
#     , .theme = theme_ridges(center_axis_labels = T) +
#       theme(
#         title = element_text(hjust = 0.5)
#         , plot.title.position = 'plot'
#         , legend.position = 'bottom'
#         , legend.justification = 'center'
#         , legend.key.size = unit(0.5,'cm')
#         , legend.key.width = unit(2,'cm')
#         , plot.margin = margin(1, 1, 1, 1,'cm')
#         , axis.text.y = element_text(vjust = 0.5)
#         , strip.background = element_blank()
#       )
#     , .fun_format.x = function(x){x}
#     , .fun_format.y = percent
#     , .list_axis.y.args = list(
#       breaks = seq(0, 1, length.out = 5)
#       , limits = c(-0.1, 1.1)
#     )
#     , .chr_manual.pal = set_names(
#       c(
#         list_pal.atlas$abilities
#         , list_pal.atlas$knowledge
#         , list_pal.atlas$skills
#       )
#       , unique(list_scores$scores.average.long$category)
#     )
#     , .list_legend = list(
#       color = 'none'
#     )
#     , .list_labs = list(
#       title = 'Average Factor Scores'
#       , x = NULL
#       , y = 'Average Factor Scores'
#     )
#     ) -> plt_factors.average
#   
#   # Output
#   return(compact(list(
#     'actual' = list_scores$scores
#     , 'actual.long' = list_scores$scores.long
#     , 'average' = list_scores$scores.average
#     , 'average.long' = list_scores$scores.average.long
#     , 'items' = plt_items
#     , 'items.users' = list_plt_items.users
#     , 'items.average' = plt_items.average
#     , 'factors' = plt_factors
#     , 'factors.users' = list_plt_factors.users
#     , 'factors.average' = plt_factors.average
#   )))
#   
# }

# [FUNCTION] PLOT COMPARISON ----------------------------------------------
fun_plot.comparisons.mvp <- function(
    
  # Data
  .df_data.users
  , .df_data.comparison = NULL
  # Factor list
  , .list_factors = list()
  
){
  
  # Arguments validation
  # Data frames
  stopifnot(
    "'.df_data.users' must be a data frame containing the users' item scores." =
      is.data.frame(.df_data.users)
  )
  
  stopifnot(
    "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." =
      is.data.frame(.df_data.comparison) |
      !length(.df_data.comparison)
  )
  
  # # Rewrite
  # # List
  # stopifnot(
  #   "'.list_factors' must be a list of factor keys." = c(
  #     is.list(.list_factors)
  #     | !length(.list_factors)
  #     , !is.data.frame(.list_factors)
  #   ))
  
  # Unique user id (for confidentiality purposes)
  .df_data.users %>%
    mutate(
      id.unique = paste0(
        row_number()
        , stri_rand_strings(nrow(.), 1)
      )
      , .before = everything()
    ) -> .df_data.users
  
  # Make an users's occupation data frame
  .df_data.comparison %>%
    filter(if_any(
      .cols = !where(is.numeric)
      ,.fns =
        ~ .x %in% all_of(.df_data.users$occupation)
    )) %>%
    select(
      where(
        ~ is.numeric(.x)
        | any(.x %in% .df_data.users$occupation)
      )) %>%
    select(any_of(
      names(.df_data.users)
    )) %>%
    relocate(
      !where(is.numeric)
      , where(is.numeric)
    ) %>%
    rename(id.unique = 1) %>%
    slice(
      match(
        .df_data.users$occupation
        , id.unique
      )
    ) -> .df_data.comparison
  
  # Individual scores, factor scores, and averages
  .df_data.users %>%
    fun_factor.scores2(
      .list_factor.keys = .list_factors
      , .lgc_sample.averages = T
      , .lgc_pivot.long = T
      , .lgc_totals = F
    ) -> list_scores
  
  # Individual scores, factor scores, and averages
  .df_data.comparison %>%
    fun_factor.scores2(
      .list_factor.keys = .list_factors
      , .lgc_sample.averages = F
      , .lgc_pivot.long = T
      , .lgc_totals = F
    ) -> list_scores.occupations
  
  # Acronyms
  list_scores$scores.long %>%
    # full_join(df_acronyms) -> list_scores$scores.long
    left_join(df_acronyms) -> list_scores$scores.long
  # inner_join(df_acronyms) -> list_scores$scores.long
  
  list_scores$scores.average.long %>%
    # full_join(df_acronyms) -> list_scores$scores.average.long
    left_join(df_acronyms) -> list_scores$scores.average.long
  # inner_join(df_acronyms) -> list_scores$scores.average.long
  
  list_scores.occupations$scores.long %>%
    # full_join(df_acronyms) -> list_scores.occupations$scores.long
    left_join(df_acronyms) -> list_scores.occupations$scores.long
  # inner_join(df_acronyms) -> list_scores.occupations$scores.long
  
  # Comparison plots
  NULL -> plt_items
  NULL -> list_plt_items.users
  NULL -> plt_items.average
  NULL -> plt_factors
  NULL -> list_plt_factors.users
  NULL -> plt_factors.average
  
  # Item by item comparison
  list_scores$scores.long %>%
    fun_plot.heatmap(aes(
      x = id.unique
      , y = item.name
      , fill = item.score
    )
    
    , .list_labs = list(
      title = 'Item Scores — All Users'
      , x = 'User'
      , y = NULL
      , fill = 'Item Score'
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
        , axis.text.y = element_text(vjust = 0.5)
      )
    
    , .scale_colors = list(
      # scale_fill_gradientn(
      scale_fill_viridis(
        # , values = round(seq(0,1,1/6),2)
        limits = c(0,1)
        , breaks = round(seq(0,1, length.out = 7), 2)
        , labels = function(x){percent(x,1)}
      )
    )
    
    , .fun_axis.y = scale_y_discrete
    , .list_axis.y.args = list(
      position = 'right'
    )
    , .fun_format.y = function(y){y}
    # , .coord_polar = T
    ) +
    guides(
      fill = guide_colorbar(
        title.position = 'top'
        , title.hjust = 0.5
      )
    ) -> plt_items
  
  # Factor by factor comparison
  list_scores$scores.long %>%
    fun_plot.heatmap(aes(
      x = id.unique
      , y = factor
      , fill = factor.score
    )
    
    , .list_labs = list(
      title = 'Factor Scores — All Users'
      , x = 'User'
      , y = NULL
      , fill = 'Factor Score'
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
        , axis.text.y = element_text(vjust = 0.5)
      )
    
    , .scale_colors = list(
      # scale_fill_gradientn(
      scale_fill_viridis(
        # , values = round(seq(0,1,1/6),2)
        limits = c(0,1)
        , breaks = round(seq(0,1, length.out = 7), 2)
        , labels = function(x){percent(x,1)}
      )
    )
    # , .fun_axis.y = scale_y_discrete
    # , .list_axis.y.args = list(
    #   position = 'right'
    # )
    , .fun_format.y = function(y){y}
    # , .coord_polar = T
    ) +
    guides(
      fill = guide_colorbar(
        title.position = 'top'
        , title.hjust = 0.5
      )
    ) -> plt_factors
  
  # Item by item comparison (one plot per user)
  list_scores$scores.average.long %>%
    mutate(
      id.unique = 'Average'
    ) %>%
    bind_rows(
      list_scores$scores.long
      , list_scores.occupations$scores.long
    ) -> df_average.long
  
  map(
    setNames(
      .df_data.users$id.unique
      , .df_data.users$id.unique
    )
    , function(.x){
      
      list_scores$scores.long %>%
        filter(
          id.unique %in% .x
        ) %>%
        arrange(desc(item.score)) %>%
        pull(item.name) %>%
        unique() -> chr_order
      
      c(.x, 'Average'
        , df_average.long %>%
          filter(id.unique %in% .x) %>%
          pull(occupation) %>%
          unique()
      ) -> chr_ids
      
      df_average.long %>%
        filter(id.unique %in% chr_ids) %>%
        mutate(
          item.name = factor(item.name, levels = chr_order)
          , id.unique = fct_inorder(id.unique)
        ) %>%
        fun_plot.dumbbell2(aes(
          x = item.score
          , y = item.name
          # , label = percent(item.score, accuracy = 1)
          , color = id.unique
        )
        , .sym_facets = item.name
        , .int_facets = 2
        , .chr_scales = 'free_y'
        , .list_labs = list(
          title = paste0('Item Scores — ', .x)
          , x = 'Item Score'
          , y = NULL
          , color = NULL
        )
        , .reorder_fct = F
        , .chr_manual.pal = set_names(
          c(
            list_pal.atlas$purple3
            , list_pal.atlas$blue1
            , list_pal.atlas$green
          )
          , chr_ids
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
      
    }
  ) -> list_plt_items.users
  
  # map(
  #   setNames(
  #     .df_data.users$id.unique
  #     , .df_data.users$id.unique
  #   )
  #   , function(.x){
  #
  #     list_scores$scores.long %>%
  #       filter(
  #         id.unique %in% .x
  #       ) %>%
  #       arrange(item.score) %>%
  #       pull(item.name) %>%
  #       unique() -> chr_order
  #
  #     df_average.long %>%
  #       filter(
  #         # id.unique %in% c(.x, 'Average')
  #         id.unique %in% c(.x, 'Average'
  #           # , df_average.long %>%
  #           #   filter(id.unique %in% .x) %>%
  #           #   pull(occupation)
  #           )
  #       ) %>%
  #       mutate(
  #         item.name = factor(item.name, levels = chr_order)
  #         , id.unique = fct_inorder(id.unique)
  #       ) %>%
  #       group_by(id.unique) %>%
  #       arrange(item.name) %>%
  #       mutate(
  #         column = row_number() < (n() / 2)
  #         , column = factor(column)
  #       ) %>%
  #       ungroup() %>%
  #       fun_plot.bar(aes(
  #         x = item.name
  #         , y = item.score
  #         , label = percent(item.score, accuracy = 1)
  #         , fill = id.unique
  #         , color = id.unique
  #       )
  #       , .sym_facets = column
  #       , .chr_scales = 'free'
  #       , .list_labs = list(
  #         title = paste0('Item Scores — ', .x)
  #         , x = NULL
  #         , y = 'Item Score'
  #         , fill = NULL
  #         , color = NULL
  #       )
  #       , .coord_polar = F
  #       , .coord_flip = T
  #       , .reorder_fct = F
  #       , .chr_manual.pal = c(
  #         list_pal.atlas$grey
  #         , list_pal.atlas$purple3
  #         , list_pal.atlas$green
  #       )
  #       , .chr_manual.aes = c(
  #         'fill', 'color'
  #       )
  #       , .list_axis.y.args = list(
  #         limits = c(-0.1, 1.1)
  #         , breaks = seq(0, 1, 0.25)
  #       )
  #       , .fun_format.y = percent_format(accuracy = 1)
  #       , .list_labels.param = list(
  #         position = c(position_dodge2(0.5, 'single'))
  #         , hjust = -0.15
  #       )
  #       , .theme = theme_ridges(center_axis_labels = T) +
  #         theme(
  #           title = element_text(hjust = 0.5)
  #           , plot.title.position = 'plot'
  #           , panel.grid.major.y = element_blank()
  #           , axis.text.y = element_blank()
  #           , axis.ticks.y = element_blank()
  #           , legend.position = 'bottom'
  #           , legend.justification = 'center'
  #           , strip.background = element_blank()
  #           , strip.text = element_blank()
  #           , plot.margin = margin(1, 1, 1, 1,'cm')
  #         )
  #       , .list_geom.param = list(
  #         position = c(position_dodge2(0.5, 'single'))
  #         , width = 0.5
  #       )
  #       ) +
  #       geom_text(aes(
  #         label = item.name
  #         , x = item.name
  #         , y = 0
  #       )
  #       , vjust = -2
  #       , hjust = 0
  #       )
  #
  #   }
  # ) -> list_plt_items.users
  
  # Factor by factor comparison (one plot per user)
  map(
    setNames(
      .df_data.users$id.unique
      , .df_data.users$id.unique
    )
    , function(.x){
      
      list_scores$scores.long %>%
        filter(
          id.unique %in% .x
        ) %>%
        arrange(factor.score) %>%
        pull(factor) %>%
        unique() -> chr_order.fct
      
      list_scores$scores.long %>%
        filter(
          id.unique %in% .x
        ) %>%
        arrange(desc(factor.score)) %>%
        pull(category) %>%
        unique() -> chr_order.cat
      
      c(.x, 'Average'
        , df_average.long %>%
          filter(id.unique %in% .x) %>%
          pull(occupation) %>%
          unique()
      ) -> chr_ids
      
      df_average.long %>%
        filter(id.unique %in% chr_ids) %>%
        mutate(
          factor = factor(factor, levels = chr_order.fct)
          , category = factor(category, levels = chr_order.cat)
          , id.unique = fct_inorder(id.unique)
        ) %>%
        fun_plot.dumbbell2(aes(
          x = factor.score
          , y = factor
          , label = percent(factor.score, accuracy = 1)
          , color = id.unique
        )
        , .sym_facets = category
        , .int_facets = 1
        , .chr_scales = 'free_y'
        , .list_labs = list(
          title = paste0('Factor Scores — ', .x)
          , x = 'Factor Score'
          , y = NULL
          , color = NULL
        )
        , .reorder_fct = F
        , .reorder_desc = F
        , .chr_manual.pal = set_names(
          c(
            list_pal.atlas$purple3
            , list_pal.atlas$blue1
            , list_pal.atlas$green
          )
          , chr_ids
        )
        , .list_axis.x.args = list(
          limits = c(-0.1, 1.1)
          , breaks = seq(0, 1, 0.25)
        )
        , .fun_format.x = percent_format(accuracy = 1)
        , .fun_format.y = function(y){y}
        , .theme = theme_ridges(center_axis_labels = T) +
          theme(
            title = element_text(hjust = 0.5)
            , plot.title.position = 'plot'
            , legend.position = 'bottom'
            , legend.justification = 'center'
            , strip.background = element_blank()
            , plot.margin = margin(1, 1, 1, 1,'cm')
            , axis.text.y = element_text(vjust = 0.5)
          ))
      
    }
  ) -> list_plt_factors.users
  
  # map(
  #   setNames(
  #     .df_data.users$id.unique
  #     , .df_data.users$id.unique
  #   )
  #   , function(.x){
  #
  #     df_average.long %>%
  #       filter(
  #         id.unique %in% c(
  #           .x, 'Average'
  #           , df_average.long %>%
  #             filter(id.unique %in% .x) %>%
  #             pull(occupation)
  #         )
  #       ) %>%
  #       fun_plot.dumbbell2(aes(
  #         x = factor.score
  #         , y = factor
  #         , color = id.unique
  #       )
  #       , .sym_facets = category
  #       , .int_facets = 1
  #       , .chr_scales = 'free'
  #       , .list_labs = list(
  #         title = paste0('Factor Scores — ', .x)
  #         , x = 'Factor Score'
  #         , y = NULL
  #         , color = NULL
  #       )
  #       , .reorder_fct = T
  #       , .reorder_desc = F
  #       , .labels = F
  #       , .list_axis.x.args = list(
  #         limits = c(0, 1)
  #         , breaks = seq(0, 1, 0.25)
  #       )
  #       , .fun_format.x = percent
  #       , .fun_format.y = function(y){y}
  #       , .theme = theme_ridges(center_axis_labels = T) +
  #         theme(
  #           title = element_text(hjust = 0.5)
  #           , plot.title.position = 'plot'
  #           # , panel.grid.major.y = element_blank()
  #           , legend.position = 'bottom'
  #           , legend.justification = 'center'
  #           , plot.margin = margin(1, 1, 1, 1,'cm')
  #           , axis.text.y = element_text(vjust = 0.5)
  #           # , strip.background = element_blank()
  #         )
  #       )
  #
  #   }
  # ) -> list_plt_factors.users
  
  # Average item by item comparison
  list_scores$scores.average.long %>%
    # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
    fun_plot.bar(aes(
      x = item.name
      , y = item.score
      , label = item.acronym
      # , label = item.label
      , fill = category
      , color = category
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
    , .chr_manual.pal = set_names(
      c(
        list_pal.atlas$abilities
        , list_pal.atlas$knowledge
        , list_pal.atlas$skills
      )
      , unique(list_scores$scores.average.long$category)
    )
    , .chr_manual.aes = c(
      'fill', 'color'
    )
    , .list_legend = list(
      color = 'none'
    )
    , .list_labs = list(
      y = 'Average Item Scores'
      , fill = NULL
    )
    ) -> plt_items.average
  
  # Average factor by factor comparison
  list_scores$scores.average.long %>%
    select(!starts_with('item')) %>%
    unique() %>%
    # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
    fun_plot.lollipop(aes(
      x = factor
      , y = factor.score
      # , label = factor.acronym
      , label = percent(factor.score, .01)
      , color = category
    )
    , .reorder_fct = T
    , .reorder_desc = F
    , .sym_facets = category
    , .int_facets = 1
    , .chr_scales = 'free'
    , .theme = theme_ridges(center_axis_labels = T) +
      theme(
        title = element_text(hjust = 0.5)
        , plot.title.position = 'plot'
        , legend.position = 'bottom'
        , legend.justification = 'center'
        , legend.key.size = unit(0.5,'cm')
        , legend.key.width = unit(2,'cm')
        , plot.margin = margin(1, 1, 1, 1,'cm')
        , axis.text.y = element_text(vjust = 0.5)
        , strip.background = element_blank()
      )
    , .fun_format.x = function(x){x}
    , .fun_format.y = percent
    , .list_axis.y.args = list(
      breaks = seq(0, 1, length.out = 5)
      , limits = c(-0.1, 1.1)
    )
    , .chr_manual.pal = set_names(
      c(
        list_pal.atlas$abilities
        , list_pal.atlas$knowledge
        , list_pal.atlas$skills
      )
      , unique(list_scores$scores.average.long$category)
    )
    , .list_legend = list(
      color = 'none'
    )
    , .list_labs = list(
      title = 'Average Factor Scores'
      , x = NULL
      , y = 'Average Factor Scores'
    )
    ) -> plt_factors.average
  
  # Output
  return(compact(list(
    'actual' = list_scores$scores
    , 'actual.long' = list_scores$scores.long
    , 'average' = list_scores$scores.average
    , 'average.long' = list_scores$scores.average.long
    , 'items' = plt_items
    , 'items.users' = list_plt_items.users
    , 'items.average' = plt_items.average
    , 'factors' = plt_factors
    , 'factors.users' = list_plt_factors.users
    , 'factors.average' = plt_factors.average
  )))
  
}

# [FUNCTION] PLOT COMPARISON ----------------------------------------------
fun_plot.comparisons.mvp <- function(
    
  # Data
  .df_data.users
  , .df_data.comparison = NULL
  # Factor list
  , .list_factors = list()
  
){
  
  # Arguments validation
  # Data frames
  stopifnot(
    "'.df_data.users' must be a data frame containing the users' item scores." =
      is.data.frame(.df_data.users)
  )
  
  stopifnot(
    "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." =
      is.data.frame(.df_data.comparison) |
      !length(.df_data.comparison)
  )
  
  # # Rewrite
  # # List
  # stopifnot(
  #   "'.list_factors' must be a list of factor keys." = c(
  #     is.list(.list_factors)
  #     | !length(.list_factors)
  #     , !is.data.frame(.list_factors)
  #   ))
  
  # Unique user id (for confidentiality purposes)
  .df_data.users %>%
    mutate(
      id.unique = Name
      , .before = everything()
    ) -> .df_data.users
  
  # Make an users's occupation data frame
  .df_data.comparison %>%
    filter(if_any(
      .cols = !where(is.numeric)
      ,.fns =
        ~ .x %in% all_of(.df_data.users$occupation)
    )) %>%
    select(
      where(
        ~ is.numeric(.x)
        | any(.x %in% .df_data.users$occupation)
      )) %>%
    select(any_of(
      names(.df_data.users)
    )) %>%
    relocate(
      !where(is.numeric)
      , where(is.numeric)
    ) %>%
    rename(id.unique = 1) %>%
    slice(
      match(
        .df_data.users$occupation
        , id.unique
      )
    ) -> .df_data.comparison
  
  # Individual scores, factor scores, and averages
  .df_data.users %>%
    fun_factor.scores2(
      .list_factor.keys = .list_factors
      , .lgc_sample.averages = T
      , .lgc_pivot.long = T
      , .lgc_totals = F
    ) -> list_scores
  
  # Individual scores, factor scores, and averages
  .df_data.comparison %>%
    fun_factor.scores2(
      .list_factor.keys = .list_factors
      , .lgc_sample.averages = F
      , .lgc_pivot.long = T
      , .lgc_totals = F
    ) -> list_scores.occupations
  
  # Acronyms
  list_scores$scores.long %>%
    # full_join(df_acronyms) -> list_scores$scores.long
    left_join(df_acronyms) -> list_scores$scores.long
  # inner_join(df_acronyms) -> list_scores$scores.long
  
  list_scores$scores.average.long %>%
    # full_join(df_acronyms) -> list_scores$scores.average.long
    left_join(df_acronyms) -> list_scores$scores.average.long
  # inner_join(df_acronyms) -> list_scores$scores.average.long
  
  list_scores.occupations$scores.long %>%
    # full_join(df_acronyms) -> list_scores.occupations$scores.long
    left_join(df_acronyms) -> list_scores.occupations$scores.long
  # inner_join(df_acronyms) -> list_scores.occupations$scores.long
  
  # Comparison plots
  NULL -> plt_items
  NULL -> list_plt_items.users
  NULL -> plt_items.average
  NULL -> plt_factors
  NULL -> list_plt_factors.users
  NULL -> plt_factors.average
  
  # Item by item comparison
  list_scores$scores.long %>%
    fun_plot.heatmap(aes(
      x = id.unique
      , y = item.name
      , fill = item.score
    )
    
    , .list_labs = list(
      title = 'Item Scores — All Users'
      , x = 'User'
      , y = NULL
      , fill = 'Item Score'
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
        , axis.text.y = element_text(vjust = 0.5)
      )
    
    , .scale_colors = list(
      # scale_fill_gradientn(
      scale_fill_viridis(
        # , values = round(seq(0,1,1/6),2)
        limits = c(0,1)
        , breaks = round(seq(0,1, length.out = 7), 2)
        , labels = function(x){percent(x,1)}
      )
    )
    
    , .fun_axis.y = scale_y_discrete
    , .list_axis.y.args = list(
      position = 'right'
    )
    , .fun_format.y = function(y){y}
    # , .coord_polar = T
    ) +
    guides(
      fill = guide_colorbar(
        title.position = 'top'
        , title.hjust = 0.5
      )
    ) -> plt_items
  
  # Factor by factor comparison
  list_scores$scores.long %>%
    fun_plot.heatmap(aes(
      x = id.unique
      , y = factor
      , fill = factor.score
    )
    
    , .list_labs = list(
      title = 'Factor Scores — All Users'
      , x = 'User'
      , y = NULL
      , fill = 'Factor Score'
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
        , axis.text.y = element_text(vjust = 0.5)
      )
    
    , .scale_colors = list(
      # scale_fill_gradientn(
      scale_fill_viridis(
        # , values = round(seq(0,1,1/6),2)
        limits = c(0,1)
        , breaks = round(seq(0,1, length.out = 7), 2)
        , labels = function(x){percent(x,1)}
      )
    )
    # , .fun_axis.y = scale_y_discrete
    # , .list_axis.y.args = list(
    #   position = 'right'
    # )
    , .fun_format.y = function(y){y}
    # , .coord_polar = T
    ) +
    guides(
      fill = guide_colorbar(
        title.position = 'top'
        , title.hjust = 0.5
      )
    ) -> plt_factors
  
  # Item by item comparison (one plot per user)
  list_scores$scores.average.long %>%
    mutate(
      id.unique = 'Average'
    ) %>%
    bind_rows(
      list_scores$scores.long
      , list_scores.occupations$scores.long
    ) -> df_average.long
  
  map(
    setNames(
      .df_data.users$id.unique
      , .df_data.users$id.unique
    )
    , function(.x){
      
      list_scores$scores.long %>%
        filter(
          id.unique %in% .x
        ) %>%
        arrange(desc(item.score)) %>%
        pull(item.name) %>%
        unique() -> chr_order
      
      c(.x, 'Average'
        , df_average.long %>%
          filter(id.unique %in% .x) %>%
          pull(occupation) %>%
          unique()
      ) -> chr_ids
      
      df_average.long %>%
        filter(id.unique %in% chr_ids) %>%
        mutate(
          item.name = factor(item.name, levels = chr_order)
          , id.unique = fct_inorder(id.unique)
        ) %>%
        fun_plot.dumbbell2(aes(
          x = item.score
          , y = item.name
          # , label = percent(item.score, accuracy = 1)
          , color = id.unique
        )
        , .sym_facets = item.name
        , .int_facets = 2
        , .chr_scales = 'free_y'
        , .list_labs = list(
          title = paste0('Item Scores — ', .x)
          , x = 'Item Score'
          , y = NULL
          , color = NULL
        )
        , .reorder_fct = F
        , .chr_manual.pal = set_names(
          c(
            list_pal.atlas$purple3
            , list_pal.atlas$blue1
            , list_pal.atlas$green
          )
          , chr_ids
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
      
    }
  ) -> list_plt_items.users
  
  # map(
  #   setNames(
  #     .df_data.users$id.unique
  #     , .df_data.users$id.unique
  #   )
  #   , function(.x){
  #
  #     list_scores$scores.long %>%
  #       filter(
  #         id.unique %in% .x
  #       ) %>%
  #       arrange(item.score) %>%
  #       pull(item.name) %>%
  #       unique() -> chr_order
  #
  #     df_average.long %>%
  #       filter(
  #         # id.unique %in% c(.x, 'Average')
  #         id.unique %in% c(.x, 'Average'
  #           # , df_average.long %>%
  #           #   filter(id.unique %in% .x) %>%
  #           #   pull(occupation)
  #           )
  #       ) %>%
  #       mutate(
  #         item.name = factor(item.name, levels = chr_order)
  #         , id.unique = fct_inorder(id.unique)
  #       ) %>%
  #       group_by(id.unique) %>%
  #       arrange(item.name) %>%
  #       mutate(
  #         column = row_number() < (n() / 2)
  #         , column = factor(column)
  #       ) %>%
  #       ungroup() %>%
  #       fun_plot.bar(aes(
  #         x = item.name
  #         , y = item.score
  #         , label = percent(item.score, accuracy = 1)
  #         , fill = id.unique
  #         , color = id.unique
  #       )
  #       , .sym_facets = column
  #       , .chr_scales = 'free'
  #       , .list_labs = list(
  #         title = paste0('Item Scores — ', .x)
  #         , x = NULL
  #         , y = 'Item Score'
  #         , fill = NULL
  #         , color = NULL
  #       )
  #       , .coord_polar = F
  #       , .coord_flip = T
  #       , .reorder_fct = F
  #       , .chr_manual.pal = c(
  #         list_pal.atlas$grey
  #         , list_pal.atlas$purple3
  #         , list_pal.atlas$green
  #       )
  #       , .chr_manual.aes = c(
  #         'fill', 'color'
  #       )
  #       , .list_axis.y.args = list(
  #         limits = c(-0.1, 1.1)
  #         , breaks = seq(0, 1, 0.25)
  #       )
  #       , .fun_format.y = percent_format(accuracy = 1)
  #       , .list_labels.param = list(
  #         position = c(position_dodge2(0.5, 'single'))
  #         , hjust = -0.15
  #       )
  #       , .theme = theme_ridges(center_axis_labels = T) +
  #         theme(
  #           title = element_text(hjust = 0.5)
  #           , plot.title.position = 'plot'
  #           , panel.grid.major.y = element_blank()
  #           , axis.text.y = element_blank()
  #           , axis.ticks.y = element_blank()
  #           , legend.position = 'bottom'
  #           , legend.justification = 'center'
  #           , strip.background = element_blank()
  #           , strip.text = element_blank()
  #           , plot.margin = margin(1, 1, 1, 1,'cm')
  #         )
  #       , .list_geom.param = list(
  #         position = c(position_dodge2(0.5, 'single'))
  #         , width = 0.5
  #       )
  #       ) +
  #       geom_text(aes(
  #         label = item.name
  #         , x = item.name
  #         , y = 0
  #       )
  #       , vjust = -2
  #       , hjust = 0
  #       )
  #
  #   }
  # ) -> list_plt_items.users
  
  # Factor by factor comparison (one plot per user)
  map(
    setNames(
      .df_data.users$id.unique
      , .df_data.users$id.unique
    )
    , function(.x){
      
      list_scores$scores.long %>%
        filter(
          id.unique %in% .x
        ) %>%
        arrange(factor.score) %>%
        pull(factor) %>%
        unique() -> chr_order.fct
      
      list_scores$scores.long %>%
        filter(
          id.unique %in% .x
        ) %>%
        arrange(desc(factor.score)) %>%
        pull(category) %>%
        unique() -> chr_order.cat
      
      c(.x, 'Average'
        , df_average.long %>%
          filter(id.unique %in% .x) %>%
          pull(occupation) %>%
          unique()
      ) -> chr_ids
      
      df_average.long %>%
        filter(id.unique %in% chr_ids) %>%
        mutate(
          factor = factor(factor, levels = chr_order.fct)
          , category = factor(category, levels = chr_order.cat)
          , id.unique = fct_inorder(id.unique)
        ) %>%
        fun_plot.dumbbell2(aes(
          x = factor.score
          , y = factor
          # , label = percent(factor.score, accuracy = 1)
          , color = id.unique
        )
        , .sym_facets = category
        , .int_facets = 1
        , .chr_scales = 'free_y'
        , .list_labs = list(
          title = paste0('Factor Scores — ', .x)
          , x = 'Factor Score'
          , y = NULL
          , color = NULL
        )
        , .reorder_fct = F
        , .reorder_desc = F
        , .chr_manual.pal = set_names(
          c(
            list_pal.atlas$purple3
            , list_pal.atlas$blue1
            , list_pal.atlas$green
          )
          , chr_ids
        )
        , .list_axis.x.args = list(
          limits = c(-0.1, 1.1)
          , breaks = seq(0, 1, 0.25)
        )
        , .fun_format.x = percent_format(accuracy = 1)
        , .fun_format.y = function(y){y}
        , .theme = theme_ridges(center_axis_labels = T) +
          theme(
            title = element_text(hjust = 0.5)
            , plot.title.position = 'plot'
            , legend.position = 'bottom'
            , legend.justification = 'center'
            , strip.background = element_blank()
            , plot.margin = margin(1, 1, 1, 1,'cm')
            , axis.text.y = element_text(vjust = 0.5)
          ))
      
    }
  ) -> list_plt_factors.users
  
  # map(
  #   setNames(
  #     .df_data.users$id.unique
  #     , .df_data.users$id.unique
  #   )
  #   , function(.x){
  #
  #     df_average.long %>%
  #       filter(
  #         id.unique %in% c(
  #           .x, 'Average'
  #           , df_average.long %>%
  #             filter(id.unique %in% .x) %>%
  #             pull(occupation)
  #         )
  #       ) %>%
  #       fun_plot.dumbbell2(aes(
  #         x = factor.score
  #         , y = factor
  #         , color = id.unique
  #       )
  #       , .sym_facets = category
  #       , .int_facets = 1
  #       , .chr_scales = 'free'
  #       , .list_labs = list(
  #         title = paste0('Factor Scores — ', .x)
  #         , x = 'Factor Score'
  #         , y = NULL
  #         , color = NULL
  #       )
  #       , .reorder_fct = T
  #       , .reorder_desc = F
  #       , .labels = F
  #       , .list_axis.x.args = list(
  #         limits = c(0, 1)
  #         , breaks = seq(0, 1, 0.25)
  #       )
  #       , .fun_format.x = percent
  #       , .fun_format.y = function(y){y}
  #       , .theme = theme_ridges(center_axis_labels = T) +
  #         theme(
  #           title = element_text(hjust = 0.5)
  #           , plot.title.position = 'plot'
  #           # , panel.grid.major.y = element_blank()
  #           , legend.position = 'bottom'
  #           , legend.justification = 'center'
  #           , plot.margin = margin(1, 1, 1, 1,'cm')
  #           , axis.text.y = element_text(vjust = 0.5)
  #           # , strip.background = element_blank()
  #         )
  #       )
  #
  #   }
  # ) -> list_plt_factors.users
  
  # Average item by item comparison
  list_scores$scores.average.long %>%
    # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
    fun_plot.bar(aes(
      x = item.name
      , y = item.score
      , label = item.acronym
      # , label = item.label
      , fill = category
      , color = category
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
    , .chr_manual.pal = set_names(
      c(
        list_pal.atlas$abilities
        , list_pal.atlas$knowledge
        , list_pal.atlas$skills
      )
      , unique(list_scores$scores.average.long$category)
    )
    , .chr_manual.aes = c(
      'fill', 'color'
    )
    , .list_legend = list(
      color = 'none'
    )
    , .list_labs = list(
      y = 'Average Item Scores'
      , fill = NULL
    )
    ) -> plt_items.average
  
  # Average factor by factor comparison
  list_scores$scores.average.long %>%
    select(!starts_with('item')) %>%
    unique() %>%
    # mutate(item.label = paste(item.acronym, percent(item.score, accuracy = 1))) %>%
    fun_plot.lollipop(aes(
      x = factor
      , y = factor.score
      # , label = factor.acronym
      , label = percent(factor.score, .01)
      , color = category
    )
    , .reorder_fct = T
    , .reorder_desc = F
    , .sym_facets = category
    , .int_facets = 1
    , .chr_scales = 'free'
    , .theme = theme_ridges(center_axis_labels = T) +
      theme(
        title = element_text(hjust = 0.5)
        , plot.title.position = 'plot'
        , legend.position = 'bottom'
        , legend.justification = 'center'
        , legend.key.size = unit(0.5,'cm')
        , legend.key.width = unit(2,'cm')
        , plot.margin = margin(1, 1, 1, 1,'cm')
        , axis.text.y = element_text(vjust = 0.5)
        , strip.background = element_blank()
      )
    , .fun_format.x = function(x){x}
    , .fun_format.y = percent
    , .list_axis.y.args = list(
      breaks = seq(0, 1, length.out = 5)
      , limits = c(-0.1, 1.1)
    )
    , .chr_manual.pal = set_names(
      c(
        list_pal.atlas$abilities
        , list_pal.atlas$knowledge
        , list_pal.atlas$skills
      )
      , unique(list_scores$scores.average.long$category)
    )
    , .list_legend = list(
      color = 'none'
    )
    , .list_labs = list(
      title = 'Average Factor Scores'
      , x = NULL
      , y = 'Average Factor Scores'
    )
    ) -> plt_factors.average
  
  # Output
  return(compact(list(
    'actual' = list_scores$scores
    , 'actual.long' = list_scores$scores.long
    , 'average' = list_scores$scores.average
    , 'average.long' = list_scores$scores.average.long
    , 'items' = plt_items
    , 'items.users' = list_plt_items.users
    , 'items.average' = plt_items.average
    , 'factors' = plt_factors
    , 'factors.users' = list_plt_factors.users
    , 'factors.average' = plt_factors.average
  )))
  
}

# [TEST] ------------------------------------------------------------------
fun_plot.comparisons.mvp(
  .df_data.users = df_input %>% 
    mutate(
      occupation = sample(df_occupations$occupation, n())
      , .before = 1
    )
  , .df_data.comparison = df_occupations
  , .list_factors = list_factors.competencies
  # , .list_factors = unname(list_factors.competencies)
  # , .list_factors = unname(map(list_factors.competencies, unname))
) -> dsds

dsds$items
dsds$factors

dsds$items.average
dsds$factors.average

dsds$items.users %>% sample(1)
dsds$factors.users %>% sample(1)

dsds$actual.long %>% 
  bind_rows(
    tibble(
      id.unique = c('dsds', 'lalala')
      , item.score = c(0,1)
    )
  ) %>%
  select(id.unique, item.score) %>% 
  group_by(id.unique) %>%
  mutate(kflex = fun_capital.flex(item.score)) %>%
  reframe(
    n.67 = sum(item.score >= .67) / n()
    , overall.score = mean(item.score)
    , kflex = unique(kflex)
  ) %>% 
  arrange(desc(n.67)) %>% 
  mutate(
    n.67 = round(n.67, 2)
    , interval.67 = findInterval(
      n.67
      , round(seq(0, 1, 1/6), 2)[
        -c(1,length(seq(0, 1, 1/6)))
      ]
    )
    , interval.kflex = findInterval(
      kflex
      , c(.33, .33 + .17/2, .50, .50 + .17/2, .67, .67 + .17/2)
      # , round(seq(0, 1, 1/6), 2)[
      #   -c(1,length(seq(0, 1, 1/6)))
      # ]
    )
    , interval.score = findInterval(
      overall.score
      # , c(.33, .33 + .17/2, .50, .50 + .17/2, .67, .67 + .17/2)
      , round(seq(0, 1, 1/6), 2)[
        -c(1,length(seq(0, 1, 1/6)))
      ]
    )
    , diff.67 = interval.kflex - interval.67
    , diff.score = interval.kflex - interval.score
  ) %>% 
  left_join(
    df_texts.overall
    , by = c('interval.score' = 'interval')
    # ) %>%
    # )
    # left_join(
    #   df_texts.kflex
    #   , by = c('interval.kflex' = 'interval')
  ) -> lalala


lalala[is.na(lalala)] <- ''

# lalala %>% 
#   filter(id.unique == sample(id.unique,1))

lalala %>% view
  group_by(across(
    starts_with('text.')
  )
  , interval.score
  ) %>% 
  reframe(
    users = fun_text.commas(id.unique)
    , plural = n() > 1
  ) %>%
  mutate(
    noun = ifelse(plural, text.noun.plural, text.noun.singular)
    , verb = ifelse(plural, text.verb.plural, text.verb.singular)
    , text = case_when(
      text.order == 1 ~ 
        paste(text.adjective, noun, verb, users)
      , text.order == 2 ~ 
        paste(noun, text.adjective, users, noun)
      , text.order == 3 ~ 
        paste(users, verb, text.adjective)
      , text.order == 4 ~ 
        paste(noun, users, verb, text.adjective)
    )
    , text = paste0(text, '.')
    , text = str_squish(text)
  ) %>% 
  arrange(desc(interval.score)) %>% 
  pull(text) %>% 
  paste0(collapse = ' ') -> chr_text.overall

dsds$actual.long %>% 
  bind_rows(
    tibble(
      id.unique = c('dsds', 'lalala')
      , item.score = c(0,1)
    )
  ) %>%
  select(id.unique, item.score) %>% 
  group_by(id.unique) %>%
  mutate(kflex = fun_capital.flex(item.score)) %>%
  reframe(
    n.67 = sum(item.score >= .67) / n()
    , overall.score = mean(item.score)
    , kflex = unique(kflex)
  ) %>% 
  arrange(desc(n.67)) %>% 
  mutate(
    n.67 = round(n.67, 2)
    , interval.67 = findInterval(
      n.67
      , round(seq(0, 1, 1/6), 2)[
        -c(1,length(seq(0, 1, 1/6)))
      ]
    )
    , interval.kflex = findInterval(
      kflex
      , c(.33, .33 + .17/2, .50, .50 + .17/2, .67, .67 + .17/2)
      # , round(seq(0, 1, 1/6), 2)[
      #   -c(1,length(seq(0, 1, 1/6)))
      # ]
    )
    , interval.score = findInterval(
      overall.score
      # , c(.33, .33 + .17/2, .50, .50 + .17/2, .67, .67 + .17/2)
      , round(seq(0, 1, 1/6), 2)[
        -c(1,length(seq(0, 1, 1/6)))
      ]
    )
    , diff.67 = interval.kflex - interval.67
    , diff.score = interval.kflex - interval.score
  ) %>% 
  left_join(
    df_texts.kflex
    , by = c('interval.kflex' = 'interval')
  ) -> dsdsds


dsdsds[is.na(dsdsds)] <- ''

dsdsds %>% 
  group_by(across(
    starts_with('text.')
  )
  , interval.kflex
  ) %>% 
  reframe(
    users = fun_text.commas(id.unique)
    , plural = n() > 1
  ) %>% 
  mutate(
    noun = ifelse(plural, text.noun.plural, text.noun.singular)
    , verb = ifelse(plural, text.verb.plural, text.verb.singular)
    , text = case_when(
      text.order == 1 ~ 
        paste(text.adjective, noun, verb, users)
      , text.order == 2 ~ 
        paste(noun, text.adjective, users, noun)
      , text.order == 3 ~ 
        paste(users, verb, text.adjective, noun)
      , text.order == 4 ~ 
        paste(noun, users, verb, text.adjective)
    )
    , text = paste0(text, '.')
    , text = str_squish(text)
  ) %>%
  arrange(desc(interval.kflex)) %>% 
  pull(text) %>% 
  paste0(collapse = ' ') -> chr_text.kflex

chr_text.overall
chr_text.kflex

# text.order
# 1	text	noun	verb	names
# 2	noun	text	names	
# 3	names	verb	text	
# 4	noun	names	verb	text


lalala %>%
  group_by(
    across(starts_with('text.')[1])
  ) %>% 
  reframe(
    users = fun_text.commas(id.unique)
    , plural = n() > 1
  ) %>% view

dsds %>%
  group_by(
    across(starts_with('text.')[2])
  ) %>% 
  reframe(
    users = fun_text.commas(id.unique)
    , kflex = max(kflex)
    , plural = ifelse(
      n() > 1
      , 'have'
      , 'has'
    )
    , profile = 'set of competencies'
  ) %>% 
  arrange(desc(kflex)) %>% 
  unite(
    col = text
    , users
    , plural
    , starts_with('text.')
    , profile
    , sep = ' '
  ) %>% view


# ) %>% 
# unite(
#   col = 'text'
#   , starts_with('text.')
#   , na.rm = F
#   , remove = F
#   , sep = ', and '
#   ) %>% view
# group_by(text) %>% 
# tally()




# reframe(
#   n.67 = sum(item.score >= 0.67)
# ) %>% 
# arrange(desc(n.67))
# reframe(kflex = fun_capital.flex(item.score)) %>% 
# arrange(desc(kflex))

dsds$actual.long %>% 
  select(id.unique, overall.score) %>% 
  unique() %>% 
  arrange(desc(overall.score))

fun_plot.density(aes(
  x = overall.score
)
, .list_axis.x.args = list(
  limits = c(0,1)
)
)
