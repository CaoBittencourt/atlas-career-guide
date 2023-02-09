# [SETUP] -----------------------------------------------------------------
# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')

library(Hmisc)
library(stringi)

# [DATA] EFA-REDUCED QUERY VECTOR -----------------------------------------------
# USER INPUT DATA FRAME
df_input <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv')

# Select user
df_input %>% 
  # filter(Name == 'Cao') -> df_input
  drop_na() -> df_input

# EFA-reduced data frame
df_input %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){
        recode(x
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

# # [FUNCTION] PLOT COMPARISON ----------------------------------------------
# fun_plot.comparisons.mvp <- function(
    # 
#   # Data
#   .df_data.users
#   , .df_data.comparison = NULL
#   # Factor list
#   , .list_factors = list()
#   # Comparisons
#   , .lgc_compare.scores = T
#   , .lgc_compare.averages = T
# 
# ){
# 
#   # Colors
#   list(
#     'green' = '#4AF7B0'
#     , 'purple1' = '#753AF9'
#     , 'purple2' = '#301866'
#     , 'purple3' = '#3854FB'
#     , 'blue1' = '#56D0F5'
#     , 'blue2' = '#ABF4D4'
#     , 'blue3' = '#43DED1'
#     , 'blue4' = '#182766'
#     , 'red' = '#CE3527'
#     , 'black' = '#212121'
#     , 'grey' = '#D4D5D8'
#   ) -> list_atlas.pal
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
#   # List
#   stopifnot(
#     "'.list_factors' must be a list of factor keys." = c(
#       is.list(.list_factors)
#       | !length(.list_factors)
#       , !is.data.frame(.list_factors)
#     ))
# 
#   # Logical
#   stopifnot(
#     "'.lgc_compare.scores' must be either TRUE or FALSE." =
#       isTRUE(.lgc_compare.scores) |
#       !isTRUE(.lgc_compare.scores)
#   )
# 
#   stopifnot(
#     "'.lgc_compare.averages' must be either TRUE or FALSE." =
#       isTRUE(.lgc_compare.averages) |
#       !isTRUE(.lgc_compare.averages)
#   )
# 
# 
#   # Actual individual item and factor scores
#   if(length(.list_factors)){
# 
#     .df_data.users %>%
#       bind_cols(
#         fun_factor.scores(
#           .df_data.numeric = .
#           , .list_factor.keys = .list_factors
#           , .lgc_pivot.long = F
#           , .lgc_totals = F
#         )) -> df_scores.individual
# 
#   }
# 
#   # Average item and factor scores
#   df_scores.individual %>%
#     summarise(across(
#       .cols = where(is.numeric)
#       ,.fns = ~ mean(.x, na.rm = T)
#     )) -> df_scores.average
# 
#   # Comparison variables
#   if(length(.list_factors)){
# 
#     .list_factors %>%
#       flatten_chr() -> chr_items
# 
#   } else {
# 
#     .df_data.users %>%
#       select(
#         where(is.numeric)
#       ) %>%
#       names() -> chr_items
# 
#   }
# 
#   # Long data frames
#   df_scores.individual %>%
#     select(
#       -where(is.numeric)
#       , all_of(c(
#         chr_items
#       ))
#     ) %>%
#     pivot_longer(
#       cols = where(is.numeric)
#       , names_to = 'attribute'
#       , values_to = 'value'
#     ) %>%
#     rename(user = 1) -> df_scores.individual.long
# 
#   df_scores.average %>%
#     select(
#       -where(is.numeric)
#       , all_of(c(
#         chr_items
#       ))
#     ) %>%
#     pivot_longer(
#       cols = where(is.numeric)
#       , names_to = 'attribute'
#       , values_to = 'value'
#     ) -> df_scores.average.long
# 
#   df_scores.individual.long %>%
#     bind_rows(
#       df_scores.average.long %>%
#         mutate(user = 'Average')
#     ) -> df_scores.individual_average.long
# 
#   # Comparison plots
#   NULL -> plt_items
#   NULL -> plt_items.average
#   NULL -> plt_factors
#   NULL -> plt_factors.average
# 
#   # Item by item comparison
#   if(.lgc_compare.scores){
# 
#     df_scores.individual.long %>%
#       fun_plot.heatmap(aes(
#         x = user
#         , y = attribute
#         , fill = value
#       )
#       # , .coord_polar = T
#       ) -> plt_items
# 
#   }
# 
#   # User vs user by item comparison (each item, one plot, all users)
#   if(.lgc_compare.scores){
# 
#     map(
#       setNames(
#         unique(df_scores.individual.long$user)
#         , unique(df_scores.individual.long$user)
#       )
#       , ~
#         df_scores.individual_average.long %>%
#         filter(user %in% c(.x, 'Average')) %>%
#         mutate(user = fct_inorder(user)) %>%
#         fun_plot.bar(aes(
#           x = attribute
#           , y = value
#           , label = percent(value, accuracy = .01)
#           , fill = user
#           , color = user
#         )
#         , .list_labs = list(
#           x = NULL
#           , y = 'Item Score'
#           , fill = NULL
#           , color = NULL
#         )
#         , .chr_manual.pal = c(
#           list_atlas.pal$purple3
#           , list_atlas.pal$grey
#         )
#         , .chr_manual.aes = c(
#           'fill', 'color'
#         )
#         , .dbl_limits.y = c(0,1)
#         , .fun_format.y = percent_format(accuracy = 1)
#         , .reorder_fun = max
#         , .coord_flip = T
#         , .list_labels.param = list(
#           position = 'dodge'
#           , fontface = 'bold'
#           , color = '#3854FB'
#           , size = 3.33
#           # , vjust = -1.15
#           # , hjust = -0.15
#           , vjust = -1
#           , hjust = 0
# 
#         )
#         , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#           theme(panel.grid.major.y = element_blank())
#         , .list_geom.param = list(
#           position = c(position_dodge2(0.8, 'single'))
#           , width = 0.7
#         )
#         )
#     ) -> list_plt_items.users
# 
#   }
# 
#   # if(.lgc_compare.scores){
#   #
#   #   df_scores.individual.long %>%
#   #     bind_rows(
#   #       df_scores.average.long %>%
#   #         mutate(user = 'average')
#   #     ) %>%
#   #     pivot_wider(
#   #       id_cols = attribute
#   #       , names_from = user
#   #       , values_from = value
#   #     ) -> df_scores.dumbbell
#   #
#   #   map(
#   #     setNames(
#   #       unique(df_scores.individual.long$user)
#   #       , unique(df_scores.individual.long$user)
#   #     )
#   #     , ~
#   #       df_scores.dumbbell %>%
#   #       select(attribute, .x, average) %>%
#   #       fun_plot.dumbbell(aes(
#   #         x = !!sym(.x)
#   #         , xend = average
#   #         , y = attribute
#   #       )
#   #       , .list_geom.param = list(
#   #         color = 'lightgrey'
#   #         , colour_x = list_atlas.pal$blue4
#   #         , colour_xend = list_atlas.pal$red
#   #         , size_x = 5.4
#   #         , size_xend = 5.4
#   #         , size = 2
#   #       )
#   #       , .list_labels1.param = list(
#   #         fontface = 'bold'
#   #         , color = list_atlas.pal$blue4
#   #         , size = 3.33
#   #         , vjust = -1.5
#   #         , hjust = 0.5
#   #       )
#   #       , .list_labels2.param = list(
#   #         fontface = 'bold'
#   #         , color = list_atlas.pal$red
#   #         , size = 3.33
#   #         , vjust = 2.25
#   #         , hjust = 0.5
#   #       )
#   #       , .list_axis.x.args = list(
#   #         limits = c(-.1,1.1)
#   #         , breaks = seq(0,1,.25)
#   #       )
#   #       , .fun_format.x = label_percent()
#   #       , .fun_format.y = function(y){y}
#   #       , .fun_format.labels = label_percent(accuracy = .01)
#   #       , .list_labs = list(
#   #         title = NULL
#   #         , subtitle = NULL
#   #         , x = str_to_title('item score')
#   #         , y = NULL
#   #       )
#   #       )
#   #   ) -> list_plt_items.users
#   #
#   # }
# 
# 
#   # User individual profiles (each user one plot, all items)
# 
#   # Average item by item comparison
#   if(.lgc_compare.averages){
# 
#     df_scores.average.long %>%
#       fun_plot.lollipop(aes(
#         x = attribute
#         , y = value
#         , label = percent(value, accuracy = .01)
#         # , color = factor
#       )
#       , .list_labs = list(
#         x = NULL
#         , y = 'Average Item Score'
#         , color = NULL
#       )
#       , .dbl_limits.y = c(0,1)
#       , .fun_format.y = percent_format(accuracy = 1)
#       , .reorder_fun = max
#       , .coord_flip = T
#       , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#         theme(panel.grid.major.y = element_blank())
#       ) -> plt_items.average
# 
#   }
# 
#   # Factor scores comparison
#   if(length(.list_factors)){
# 
#     df_scores.individual %>%
#       select(
#         -where(is.numeric)
#         , .list_factors %>%
#           flatten() %>%
#           names()
#       ) %>%
#       pivot_longer(
#         cols = where(is.numeric)
#         , names_to = 'factor'
#         , values_to = 'value'
#       ) %>%
#       rename(user = 1) %>%
#       fun_plot.heatmap(aes(
#         x = user
#         , y = factor
#         , fill = value
#       )
#       # , .coord_polar = T
#       ) -> plt_factors
# 
#     # Average factor scores comparison
#     if(.lgc_compare.averages){
# 
#       # df_scores.average %>%
#       #   select(
#       #     -where(is.numeric)
#       #     , .list_factors %>%
#       #       flatten() %>%
#       #       names()
#       #   ) %>%
#       #   pivot_longer(
#       #     cols = where(is.numeric)
#       #     , names_to = 'factor'
#       #     , values_to = 'value'
#       #   ) %>%
#       #   rename(user = 1) %>%
#       #   fun_plot.lollipop(aes(
#       #     x = factor
#       #     , y = value
#       #     , label = percent(value, accuracy = .01)
#       #     # , color = category
#       #   )
#       #   , .list_labs = list(
#       #     x = NULL
#       #     , y = 'Average Factor Score'
#       #     , color = NULL
#       #   )
#       #   , .dbl_limits.y = c(0,1)
#       #   , .fun_format.y = percent_format(accuracy = 1)
#       #   , .reorder_fun = max
#       #   , .coord_flip = T
#       #   , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#       #     theme(panel.grid.major.y = element_blank())
#       #   ) -> plt_factors.average
# 
#     }
# 
#   }
# 
# 
#   # Output
#   return(compact(list(
#     'actual' = df_scores.individual
#     , 'actual.long' = df_scores.individual.long
#     , 'vars' = chr_items
#     , 'average' = df_scores.average
#     , 'average.long' = df_scores.average.long
#     , 'items' = plt_items
#     , 'items.users' = list_plt_items.users
#     , 'items.average' = plt_items.average
#     , 'factors' = plt_factors
#     , 'factors.average' = plt_factors.average
#   )))
# 
# }

# # [FUNCTION] PLOT COMPARISON ----------------------------------------------
# fun_plot.comparisons.mvp <- function(
    # 
#   # Data
#   .df_data.users
#   , .df_data.comparison = NULL
#   # Factor list
#   , .list_factors = list()
#   # Comparisons
#   , .lgc_compare.scores = T
#   , .lgc_compare.averages = T
# 
# ){
# 
#   # Colors
#   list(
#     'green' = '#4AF7B0'
#     , 'purple1' = '#753AF9'
#     , 'purple2' = '#301866'
#     , 'purple3' = '#3854FB'
#     , 'blue1' = '#56D0F5'
#     , 'blue2' = '#ABF4D4'
#     , 'blue3' = '#43DED1'
#     , 'blue4' = '#182766'
#     , 'red' = '#CE3527'
#     , 'black' = '#212121'
#     , 'grey' = '#D4D5D8'
#   ) -> list_atlas.pal
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
#   # List
#   stopifnot(
#     "'.list_factors' must be a list of factor keys." = c(
#       is.list(.list_factors)
#       | !length(.list_factors)
#       , !is.data.frame(.list_factors)
#     ))
# 
#   # Logical
#   stopifnot(
#     "'.lgc_compare.scores' must be either TRUE or FALSE." =
#       isTRUE(.lgc_compare.scores) |
#       !isTRUE(.lgc_compare.scores)
#   )
# 
#   stopifnot(
#     "'.lgc_compare.averages' must be either TRUE or FALSE." =
#       isTRUE(.lgc_compare.averages) |
#       !isTRUE(.lgc_compare.averages)
#   )
# 
# 
#   # Actual individual item and factor scores
#   if(length(.list_factors)){
# 
#     .df_data.users %>%
#       bind_cols(
#         fun_factor.scores(
#           .df_data.numeric = .
#           , .list_factor.keys = .list_factors
#           , .lgc_pivot.long = F
#           , .lgc_totals = F
#         )) -> df_scores.individual
# 
#   }
# 
#   # Average item and factor scores
#   df_scores.individual %>%
#     summarise(across(
#       .cols = where(is.numeric)
#       ,.fns = ~ mean(.x, na.rm = T)
#     )) -> df_scores.average
# 
#   # Comparison variables
#   if(length(.list_factors)){
# 
#     .list_factors %>%
#       flatten_chr() -> chr_items
# 
#   } else {
# 
#     .df_data.users %>%
#       select(
#         where(is.numeric)
#       ) %>%
#       names() -> chr_items
# 
#   }
# 
#   # Long data frames
#   df_scores.individual %>%
#     pivot_longer(
#       cols = where(is.numeric)
#       , names_to = 'attribute'
#       , values_to = 'value'
#     ) %>%
#     rename(user = 1) -> df_scores.individual.long
# 
#   df_scores.average %>%
#     pivot_longer(
#       cols = where(is.numeric)
#       , names_to = 'attribute'
#       , values_to = 'value'
#     ) -> df_scores.average.long
# 
#   df_scores.individual.long %>%
#     bind_rows(
#       df_scores.average.long %>%
#         mutate(user = 'Average')
#     ) -> df_scores.individual_average.long
# 
#   # Comparison plots
#   NULL -> plt_items
#   NULL -> plt_items.average
#   NULL -> plt_factors
#   NULL -> plt_factors.average
# 
#   # Item by item comparison
#   if(.lgc_compare.scores){
# 
#     df_scores.individual.long %>%
#       filter(attribute %in% chr_items) %>%
#       fun_plot.heatmap(aes(
#         x = user
#         , y = attribute
#         , fill = value
#       )
#       # , .coord_polar = T
#       ) -> plt_items
# 
#   }
# 
#   # User vs user by item comparison (each item, one plot, all users)
#   if(.lgc_compare.scores){
# 
#     map(
#       setNames(
#         unique(df_scores.individual.long$user)
#         , unique(df_scores.individual.long$user)
#       )
#       , ~
#         df_scores.individual_average.long %>%
#         filter(
#           attribute %in% chr_items
#           , user %in% c(.x, 'Average')
#         ) %>%
#         mutate(user = fct_inorder(user)) %>%
#         fun_plot.bar(aes(
#           x = attribute
#           , y = value
#           , label = percent(value, accuracy = .01)
#           , fill = user
#           , color = user
#         )
#         , .list_labs = list(
#           x = NULL
#           , y = 'Item Score'
#           , fill = NULL
#           , color = NULL
#         )
#         , .chr_manual.pal = c(
#           list_atlas.pal$purple3
#           , list_atlas.pal$grey
#         )
#         , .chr_manual.aes = c(
#           'fill', 'color'
#         )
#         , .dbl_limits.y = c(0,1)
#         , .fun_format.y = percent_format(accuracy = 1)
#         , .reorder_fun = max
#         , .coord_flip = T
#         , .list_labels.param = list(
#           position = 'dodge'
#           , fontface = 'bold'
#           , color = '#3854FB'
#           , size = 3.33
#           , vjust = -1.15
#           , hjust = -0.15
#           # , vjust = -1
#           # , hjust = 0
# 
#         )
#         , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#           theme(panel.grid.major.y = element_blank())
#         , .list_geom.param = list(
#           position = c(position_dodge2(0.8, 'single'))
#           , width = 0.7
#         )
#         )
#     ) -> list_plt_items.users
# 
#   }
# 
#   # if(.lgc_compare.scores){
#   #
#   #   df_scores.individual.long %>%
#   #     bind_rows(
#   #       df_scores.average.long %>%
#   #         mutate(user = 'average')
#   #     ) %>%
#   #     pivot_wider(
#   #       id_cols = attribute
#   #       , names_from = user
#   #       , values_from = value
#   #     ) -> df_scores.dumbbell
#   #
#   #   map(
#   #     setNames(
#   #       unique(df_scores.individual.long$user)
#   #       , unique(df_scores.individual.long$user)
#   #     )
#   #     , ~
#   #       df_scores.dumbbell %>%
#   #       select(attribute, .x, average) %>%
#   #       fun_plot.dumbbell(aes(
#   #         x = !!sym(.x)
#   #         , xend = average
#   #         , y = attribute
#   #       )
#   #       , .list_geom.param = list(
#   #         color = 'lightgrey'
#   #         , colour_x = list_atlas.pal$blue4
#   #         , colour_xend = list_atlas.pal$red
#   #         , size_x = 5.4
#   #         , size_xend = 5.4
#   #         , size = 2
#   #       )
#   #       , .list_labels1.param = list(
#   #         fontface = 'bold'
#   #         , color = list_atlas.pal$blue4
#   #         , size = 3.33
#   #         , vjust = -1.5
#   #         , hjust = 0.5
#   #       )
#   #       , .list_labels2.param = list(
#   #         fontface = 'bold'
#   #         , color = list_atlas.pal$red
#   #         , size = 3.33
#   #         , vjust = 2.25
#   #         , hjust = 0.5
#   #       )
#   #       , .list_axis.x.args = list(
#   #         limits = c(-.1,1.1)
#   #         , breaks = seq(0,1,.25)
#   #       )
#   #       , .fun_format.x = label_percent()
#   #       , .fun_format.y = function(y){y}
#   #       , .fun_format.labels = label_percent(accuracy = .01)
#   #       , .list_labs = list(
#   #         title = NULL
#   #         , subtitle = NULL
#   #         , x = str_to_title('item score')
#   #         , y = NULL
#   #       )
#   #       )
#   #   ) -> list_plt_items.users
#   #
#   # }
# 
# 
#   # User individual profiles (each user one plot, all items)
# 
#   # Average item by item comparison
#   if(.lgc_compare.averages){
# 
#     df_scores.average.long %>%
#       filter(attribute %in% chr_items) %>%
#       fun_plot.lollipop(aes(
#         x = attribute
#         , y = value
#         , label = percent(value, accuracy = .01)
#         # , color = factor
#       )
#       , .list_labs = list(
#         x = NULL
#         , y = 'Average Item Score'
#         , color = NULL
#       )
#       , .dbl_limits.y = c(0,1)
#       , .fun_format.y = percent_format(accuracy = 1)
#       , .reorder_fun = max
#       , .coord_flip = T
#       , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#         theme(panel.grid.major.y = element_blank())
#       ) -> plt_items.average
# 
#   }
# 
#   # Factor scores comparison
#   if(length(.list_factors)){
# 
#     df_scores.individual.long %>%
#       filter(
#         attribute %in%
#           names(flatten(.list_factors))
#       ) %>%
#       rename(factor = attribute) %>%
#       fun_plot.heatmap(aes(
#         x = user
#         , y = factor
#         , fill = value
#       )
#       , .scale_colors = list(
#         scale_color_stepsn(
#           colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
#           , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
#           , limits = c(0,1)
#         )
#         , scale_fill_stepsn(
#           colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
#           , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
#           , limits = c(0,1)
#         )
#       )
#       # , .coord_polar = T
#       ) -> plt_factors
# 
# 
#     # Average factor scores comparison
#     if(.lgc_compare.averages){
# 
#       df_scores.average %>%
#         select(
#           -where(is.numeric)
#           , .list_factors %>%
#             flatten() %>%
#             names()
#         ) %>%
#         pivot_longer(
#           cols = where(is.numeric)
#           , names_to = 'factor'
#           , values_to = 'value'
#         ) %>%
#         rename(user = 1) %>%
#         fun_plot.lollipop(aes(
#           x = factor
#           , y = value
#           , label = percent(value, accuracy = .01)
#           # , color = category
#         )
#         , .list_labs = list(
#           x = NULL
#           , y = 'Average Factor Score'
#           , color = NULL
#         )
#         , .dbl_limits.y = c(0,1)
#         , .fun_format.y = percent_format(accuracy = 1)
#         , .reorder_fun = max
#         , .coord_flip = T
#         , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#           theme(panel.grid.major.y = element_blank())
#         ) -> plt_factors.average
# 
#     }
# 
#   }
# 
# 
#   # Output
#   return(compact(list(
#     'actual' = df_scores.individual
#     , 'actual.long' = df_scores.individual.long
#     , 'vars' = chr_items
#     , 'average' = df_scores.average
#     , 'average.long' = df_scores.average.long
#     , 'items' = plt_items
#     , 'items.users' = list_plt_items.users
#     , 'items.average' = plt_items.average
#     , 'factors' = plt_factors
#     , 'factors.average' = plt_factors.average
#   )))
# 
# }

# # [FUNCTION] PLOT COMPARISON ----------------------------------------------
# fun_plot.comparisons.mvp <- function(
    #     
#   # Data
#   .df_data.users
#   , .df_data.comparison = NULL
#   # Factor list
#   , .list_factors = list()
#   # Comparisons
#   , .lgc_compare.scores = T
#   , .lgc_compare.averages = T
#   
# ){
#   
#   # Colors
#   list(
#     'green' = '#4AF7B0'
#     , 'purple1' = '#753AF9'
#     , 'purple2' = '#301866'
#     , 'purple3' = '#3854FB'
#     , 'blue1' = '#56D0F5'
#     , 'blue2' = '#ABF4D4'
#     , 'blue3' = '#43DED1'
#     , 'blue4' = '#182766'
#     , 'red' = '#CE3527'
#     , 'black' = '#212121'
#     , 'grey' = '#D4D5D8'
#   ) -> list_atlas.pal
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
#   # List
#   stopifnot(
#     "'.list_factors' must be a list of factor keys." = c(
#       is.list(.list_factors)
#       | !length(.list_factors)
#       , !is.data.frame(.list_factors)
#     ))
#   
#   # Logical
#   stopifnot(
#     "'.lgc_compare.scores' must be either TRUE or FALSE." = 
#       isTRUE(.lgc_compare.scores) |
#       !isTRUE(.lgc_compare.scores)
#   )
#   
#   stopifnot(
#     "'.lgc_compare.averages' must be either TRUE or FALSE." = 
#       isTRUE(.lgc_compare.averages) |
#       !isTRUE(.lgc_compare.averages)
#   )
#   
#   
#   # Actual individual item and factor scores
#   if(length(.list_factors)){
#     
#     .df_data.users %>% 
#       fun_factor.scores2(
#         .list_factor.keys = .list_factors
#         , .lgc_sample.averages = .lgc_compare.averages
#         , .lgc_pivot.long = T
#         , .lgc_totals = F
#       ) -> list_df_scores.individual
#     
#   } else {
#     
#     # Long data frames
#     df_scores.individual %>%
#       pivot_longer(
#         cols = where(is.numeric)
#         , names_to = 'attribute'
#         , values_to = 'value'
#       ) %>%
#       rename(user = 1) -> df_scores.individual.long
#     
#     .df_data.users %>% 
#       summarise(across(
#         .cols = where(is.numeric)
#         ,.fns = ~ mean(.x, na.rm = T))
#       )
#     
#     df_scores.average %>%
#       pivot_longer(
#         cols = where(is.numeric)
#         , names_to = 'attribute'
#         , values_to = 'value'
#       ) -> df_scores.average.long
#     
#     df_scores.individual.long %>%
#       bind_rows(
#         df_scores.average.long %>%
#           mutate(user = 'Average')
#       ) -> df_scores.individual_average.long
#     
#     
#   }
#   
#   return(list_df_scores.individual)
#   stop()
#   
#   # # Average item and factor scores
#   # list_df_scores.individual$factor.scores %>% 
#   #   summarise(across(
#   #     .cols = where(is.numeric)
#   #     ,.fns = ~ mean(.x, na.rm = T)
#   #   )) -> df_scores.average
#   # 
#   # list_df_scores.individual$factor.scores.long %>% 
#   #   group_by(category, factor, item) %>% 
#   #   summarise(across(
#   #     .cols = where(is.numeric)
#   #     ,.fns = ~ mean(.x, na.rm = T)
#   #   )) -> df_scores.average.long
#   
#   # Comparison variables
#   if(length(.list_factors)){
#     
#     .list_factors %>%
#       flatten_chr() -> chr_items
#     
#   } else {
#     
#     .df_data.users %>%
#       select(
#         where(is.numeric)
#       ) %>% 
#       names() -> chr_items
#     
#   }
#   
#   # Comparison plots
#   NULL -> plt_items
#   NULL -> plt_items.average
#   NULL -> plt_factors
#   NULL -> plt_factors.average
#   
#   # Item by item comparison
#   if(.lgc_compare.scores){
#     
#     df_scores.individual.long %>%
#       filter(attribute %in% chr_items) %>% 
#       fun_plot.heatmap(aes(
#         x = user
#         , y = attribute
#         , fill = value
#       )
#       # , .coord_polar = T
#       ) -> plt_items
#     
#   }
#   
#   # User vs user by item comparison (each item, one plot, all users)
#   if(.lgc_compare.scores){
#     
#     map(
#       setNames(
#         unique(df_scores.individual.long$user)
#         , unique(df_scores.individual.long$user)
#       )
#       , ~
#         df_scores.individual_average.long %>%
#         filter(
#           attribute %in% chr_items
#           , user %in% c(.x, 'Average')
#         ) %>%
#         mutate(user = fct_inorder(user)) %>%
#         fun_plot.bar(aes(
#           x = attribute
#           , y = value
#           , label = percent(value, accuracy = .01)
#           , fill = user
#           , color = user
#         )
#         , .list_labs = list(
#           x = NULL
#           , y = 'Item Score'
#           , fill = NULL
#           , color = NULL
#         )
#         , .chr_manual.pal = c(
#           list_atlas.pal$purple3
#           , list_atlas.pal$grey
#         )
#         , .chr_manual.aes = c(
#           'fill', 'color'
#         )
#         , .dbl_limits.y = c(0,1)
#         , .fun_format.y = percent_format(accuracy = 1)
#         , .reorder_fun = max
#         , .coord_flip = T
#         , .list_labels.param = list(
#           position = 'dodge'
#           , fontface = 'bold'
#           , color = '#3854FB'
#           , size = 3.33
#           , vjust = -1.15
#           , hjust = -0.15
#           # , vjust = -1
#           # , hjust = 0
#           
#         )
#         , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#           theme(panel.grid.major.y = element_blank())
#         , .list_geom.param = list(
#           position = c(position_dodge2(0.8, 'single'))
#           , width = 0.7
#         )
#         )
#     ) -> list_plt_items.users
#     
#   }
#   
#   # if(.lgc_compare.scores){
#   #   
#   #   df_scores.individual.long %>% 
#   #     bind_rows(
#   #       df_scores.average.long %>% 
#   #         mutate(user = 'average')
#   #     ) %>%
#   #     pivot_wider(
#   #       id_cols = attribute
#   #       , names_from = user
#   #       , values_from = value
#   #     ) -> df_scores.dumbbell
#   #   
#   #   map(
#   #     setNames(
#   #       unique(df_scores.individual.long$user)
#   #       , unique(df_scores.individual.long$user)
#   #     )
#   #     , ~ 
#   #       df_scores.dumbbell %>%
#   #       select(attribute, .x, average) %>% 
#   #       fun_plot.dumbbell(aes(
#   #         x = !!sym(.x)
#   #         , xend = average
#   #         , y = attribute
#   #       )
#   #       , .list_geom.param = list(
#   #         color = 'lightgrey'
#   #         , colour_x = list_atlas.pal$blue4
#   #         , colour_xend = list_atlas.pal$red
#   #         , size_x = 5.4
#   #         , size_xend = 5.4
#   #         , size = 2
#   #       )
#   #       , .list_labels1.param = list(
#   #         fontface = 'bold'
#   #         , color = list_atlas.pal$blue4
#   #         , size = 3.33
#   #         , vjust = -1.5
#   #         , hjust = 0.5
#   #       )
#   #       , .list_labels2.param = list(
#   #         fontface = 'bold'
#   #         , color = list_atlas.pal$red
#   #         , size = 3.33
#   #         , vjust = 2.25
#   #         , hjust = 0.5
#   #       )
#   #       , .list_axis.x.args = list(
#   #         limits = c(-.1,1.1)
#   #         , breaks = seq(0,1,.25)
#   #       )
#   #       , .fun_format.x = label_percent()
#   #       , .fun_format.y = function(y){y}
#   #       , .fun_format.labels = label_percent(accuracy = .01)
#   #       , .list_labs = list(
#   #         title = NULL
#   #         , subtitle = NULL
#   #         , x = str_to_title('item score')
#   #         , y = NULL
#   #       )
#   #       )
#   #   ) -> list_plt_items.users
#   #   
#   # } 
#   
#   
#   # User individual profiles (each user one plot, all items)
#   
#   # Average item by item comparison
#   if(.lgc_compare.averages){
#     
#     df_scores.average.long %>%
#       filter(attribute %in% chr_items) %>% 
#       fun_plot.lollipop(aes(
#         x = attribute
#         , y = value
#         , label = percent(value, accuracy = .01)
#         # , color = factor
#       )
#       , .list_labs = list(
#         x = NULL
#         , y = 'Average Item Score'
#         , color = NULL
#       )
#       , .dbl_limits.y = c(0,1)
#       , .fun_format.y = percent_format(accuracy = 1)
#       , .reorder_fun = max
#       , .coord_flip = T
#       , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#         theme(panel.grid.major.y = element_blank())
#       ) -> plt_items.average
#     
#   }
#   
#   # Factor scores comparison
#   if(length(.list_factors)){
#     
#     df_scores.individual.long %>% 
#       filter(
#         attribute %in% 
#           names(flatten(.list_factors))
#       ) %>% 
#       rename(factor = attribute) %>% 
#       fun_plot.heatmap(aes(
#         x = user
#         , y = factor
#         , fill = value
#       )
#       , .scale_colors = list(
#         scale_color_stepsn(
#           colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
#           , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
#           , limits = c(0,1)
#         )
#         , scale_fill_stepsn(
#           colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
#           , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
#           , limits = c(0,1)
#         ) 
#       )
#       # , .coord_polar = T
#       ) -> plt_factors
#     
#     
#     # Average factor scores comparison
#     if(.lgc_compare.averages){
#       
#       df_scores.average %>%
#         select(
#           -where(is.numeric)
#           , .list_factors %>%
#             flatten() %>%
#             names()
#         ) %>%
#         pivot_longer(
#           cols = where(is.numeric)
#           , names_to = 'factor'
#           , values_to = 'value'
#         ) %>%
#         rename(user = 1) %>%
#         fun_plot.lollipop(aes(
#           x = factor
#           , y = value
#           , label = percent(value, accuracy = .01)
#           # , color = category
#         )
#         , .list_labs = list(
#           x = NULL
#           , y = 'Average Factor Score'
#           , color = NULL
#         )
#         , .dbl_limits.y = c(0,1)
#         , .fun_format.y = percent_format(accuracy = 1)
#         , .reorder_fun = max
#         , .coord_flip = T
#         , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#           theme(panel.grid.major.y = element_blank())
#         ) -> plt_factors.average
#       
#     }
#     
#   }
#   
#   
#   # Output
#   return(compact(list(
#     'actual' = df_scores.individual
#     , 'actual.long' = df_scores.individual.long
#     , 'vars' = chr_items
#     , 'average' = df_scores.average
#     , 'average.long' = df_scores.average.long
#     , 'items' = plt_items
#     , 'items.users' = list_plt_items.users
#     , 'items.average' = plt_items.average
#     , 'factors' = plt_factors
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
  # Comparisons
  , .lgc_compare.scores = T
  , .lgc_compare.averages = T
  
){
  
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
    , 'black' = '#212121'
    , 'grey' = '#D4D5D8'
  ) -> list_atlas.pal
  
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
  
  # Logical
  stopifnot(
    "'.lgc_compare.scores' must be either TRUE or FALSE." = 
      isTRUE(.lgc_compare.scores) |
      !isTRUE(.lgc_compare.scores)
  )
  
  stopifnot(
    "'.lgc_compare.averages' must be either TRUE or FALSE." = 
      isTRUE(.lgc_compare.averages) |
      !isTRUE(.lgc_compare.averages)
  )
  
  # Random user id (for confidentiality purposes)
  .df_data.users %>%
    mutate(
      id.random = stri_rand_strings(nrow(.), 10)
    ) -> .df_data.users
  
  # Individual scores, factor scores, and averages
  .df_data.users %>% 
    fun_factor.scores2(
      .list_factor.keys = .list_factors
      , .lgc_sample.averages = .lgc_compare.averages
      , .lgc_pivot.long = T
      , .lgc_totals = F
    ) -> list_df_scores.individual
  
  # # Comparison variables
  # if(length(.list_factors)){
  #   
  #   .list_factors %>%
  #     flatten_chr() -> chr_items
  #   
  # } else {
  #   
  #   .df_data.users %>%
  #     select(
  #       where(is.numeric)
  #     ) %>% 
  #     names() -> chr_items
  #   
  # }
  
  # Comparison plots
  NULL -> plt_items
  NULL -> plt_items.average
  NULL -> plt_factors
  NULL -> plt_factors.average
  
  # Item by item comparison
  if(.lgc_compare.scores){
    
    list_df_scores.individual$factor.scores.long %>%
      fun_plot.heatmap(aes(
        x = user
        , y = attribute
        , fill = value
      )
      # , .coord_polar = T
      ) -> plt_items
    
  }
  
  # User vs user by item comparison (each item, one plot, all users)
  if(.lgc_compare.scores){
    
    map(
      setNames(
        unique(df_scores.individual.long$user)
        , unique(df_scores.individual.long$user)
      )
      , ~
        df_scores.individual_average.long %>%
        filter(
          attribute %in% chr_items
          , user %in% c(.x, 'Average')
        ) %>%
        mutate(user = fct_inorder(user)) %>%
        fun_plot.bar(aes(
          x = attribute
          , y = value
          , label = percent(value, accuracy = .01)
          , fill = user
          , color = user
        )
        , .list_labs = list(
          x = NULL
          , y = 'Item Score'
          , fill = NULL
          , color = NULL
        )
        , .chr_manual.pal = c(
          list_atlas.pal$purple3
          , list_atlas.pal$grey
        )
        , .chr_manual.aes = c(
          'fill', 'color'
        )
        , .dbl_limits.y = c(0,1)
        , .fun_format.y = percent_format(accuracy = 1)
        , .reorder_fun = max
        , .coord_flip = T
        , .list_labels.param = list(
          position = 'dodge'
          , fontface = 'bold'
          , color = '#3854FB'
          , size = 3.33
          , vjust = -1.15
          , hjust = -0.15
          # , vjust = -1
          # , hjust = 0
          
        )
        , .theme = ggridges::theme_ridges(center_axis_labels = T) +
          theme(panel.grid.major.y = element_blank())
        , .list_geom.param = list(
          position = c(position_dodge2(0.8, 'single'))
          , width = 0.7
        )
        )
    ) -> list_plt_items.users
    
  }
  
  # if(.lgc_compare.scores){
  #   
  #   df_scores.individual.long %>% 
  #     bind_rows(
  #       df_scores.average.long %>% 
  #         mutate(user = 'average')
  #     ) %>%
  #     pivot_wider(
  #       id_cols = attribute
  #       , names_from = user
  #       , values_from = value
  #     ) -> df_scores.dumbbell
  #   
  #   map(
  #     setNames(
  #       unique(df_scores.individual.long$user)
  #       , unique(df_scores.individual.long$user)
  #     )
  #     , ~ 
  #       df_scores.dumbbell %>%
  #       select(attribute, .x, average) %>% 
  #       fun_plot.dumbbell(aes(
  #         x = !!sym(.x)
  #         , xend = average
  #         , y = attribute
  #       )
  #       , .list_geom.param = list(
  #         color = 'lightgrey'
  #         , colour_x = list_atlas.pal$blue4
  #         , colour_xend = list_atlas.pal$red
  #         , size_x = 5.4
  #         , size_xend = 5.4
  #         , size = 2
  #       )
  #       , .list_labels1.param = list(
  #         fontface = 'bold'
  #         , color = list_atlas.pal$blue4
  #         , size = 3.33
  #         , vjust = -1.5
  #         , hjust = 0.5
  #       )
  #       , .list_labels2.param = list(
  #         fontface = 'bold'
  #         , color = list_atlas.pal$red
  #         , size = 3.33
  #         , vjust = 2.25
  #         , hjust = 0.5
  #       )
  #       , .list_axis.x.args = list(
  #         limits = c(-.1,1.1)
  #         , breaks = seq(0,1,.25)
  #       )
  #       , .fun_format.x = label_percent()
  #       , .fun_format.y = function(y){y}
  #       , .fun_format.labels = label_percent(accuracy = .01)
  #       , .list_labs = list(
  #         title = NULL
  #         , subtitle = NULL
  #         , x = str_to_title('item score')
  #         , y = NULL
  #       )
  #       )
  #   ) -> list_plt_items.users
  #   
  # } 
  
  
  # User individual profiles (each user one plot, all items)
  
  # Average item by item comparison
  if(.lgc_compare.averages){
    
    df_scores.average.long %>%
      filter(attribute %in% chr_items) %>% 
      fun_plot.lollipop(aes(
        x = attribute
        , y = value
        , label = percent(value, accuracy = .01)
        # , color = factor
      )
      , .list_labs = list(
        x = NULL
        , y = 'Average Item Score'
        , color = NULL
      )
      , .dbl_limits.y = c(0,1)
      , .fun_format.y = percent_format(accuracy = 1)
      , .reorder_fun = max
      , .coord_flip = T
      , .theme = ggridges::theme_ridges(center_axis_labels = T) +
        theme(panel.grid.major.y = element_blank())
      ) -> plt_items.average
    
  }
  
  # Factor scores comparison
  if(length(.list_factors)){
    
    df_scores.individual.long %>% 
      filter(
        attribute %in% 
          names(flatten(.list_factors))
      ) %>% 
      rename(factor = attribute) %>% 
      fun_plot.heatmap(aes(
        x = user
        , y = factor
        , fill = value
      )
      , .scale_colors = list(
        scale_color_stepsn(
          colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
          , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
          , limits = c(0,1)
        )
        , scale_fill_stepsn(
          colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
          , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
          , limits = c(0,1)
        ) 
      )
      # , .coord_polar = T
      ) -> plt_factors
    
    
    # Average factor scores comparison
    if(.lgc_compare.averages){
      
      df_scores.average %>%
        select(
          -where(is.numeric)
          , .list_factors %>%
            flatten() %>%
            names()
        ) %>%
        pivot_longer(
          cols = where(is.numeric)
          , names_to = 'factor'
          , values_to = 'value'
        ) %>%
        rename(user = 1) %>%
        fun_plot.lollipop(aes(
          x = factor
          , y = value
          , label = percent(value, accuracy = .01)
          # , color = category
        )
        , .list_labs = list(
          x = NULL
          , y = 'Average Factor Score'
          , color = NULL
        )
        , .dbl_limits.y = c(0,1)
        , .fun_format.y = percent_format(accuracy = 1)
        , .reorder_fun = max
        , .coord_flip = T
        , .theme = ggridges::theme_ridges(center_axis_labels = T) +
          theme(panel.grid.major.y = element_blank())
        ) -> plt_factors.average
      
    }
    
  }
  
  
  # Output
  return(compact(list(
    'actual' = df_scores.individual
    , 'actual.long' = df_scores.individual.long
    , 'vars' = chr_items
    , 'average' = df_scores.average
    , 'average.long' = df_scores.average.long
    , 'items' = plt_items
    , 'items.users' = list_plt_items.users
    , 'items.average' = plt_items.average
    , 'factors' = plt_factors
    , 'factors.average' = plt_factors.average
  )))
  
}

# [TEST] ------------------------------------------------------------------
fun_plot.comparisons.mvp(
  .df_data.users = df_input
  , .chr_var.id = 'Name'
  # df_input %>% 
  # select(
  #   Name
  #   , flatten_chr(list_factors.competencies)
  # )
  , .df_data.comparison = NULL
  , .list_factors = list_factors.competencies
  , .lgc_compare.averages = T
) -> dsds

dsds$factor.names %>% view
dsds$factor.scores %>% view
dsds$factor.scores.long %>% view
dsds$factor.scores.average %>% view
dsds$factor.scores.average.long %>% view

dsds[[1]] %>% view
dsds[[2]] %>% view

dsds$items
dsds$items.users$Random
dsds$items.average
dsds$factors

# [TEST] PLOT -------------------------------------------------------------
dsds$actual %>% 
  select(
    term
    # , ends_with('.l')
    , all_of(c(
      list_factors.competencies %>%
        flatten() %>%
        flatten_chr()
    ))) %>%
  pivot_longer(
    cols = -term
  ) %>%
  fun_plot.bar(aes(
    x = name
    , y = value
    , fill = term
  )
  , .reorder_desc = F
  , .reorder_fun = max
  # , .reorder_fun = first
  , .coord_flip = T
  , .list_geom.param = list(
    position = c(
      position_dodge(width = 0.8)
    )
    , width = 0.7
    , fill = '#3854FB'
  )
  , .theme = ggridges::theme_ridges(
    grid = F
    # line_size = 0
    , center_axis_labels = T
  )
  , .fun_format.y = percent_format(accuracy = 1)
  # ) -> lalala
  )

dsds$average %>%
  select(
    term
    # , ends_with('.l')
    , all_of(c(
      list_factors.competencies %>% 
        flatten_chr()
    ))) %>% 
  mutate(across(
    .cols = where(is.numeric)
    ,.fns = ~ 1
  )) %>% 
  pivot_longer(
    cols = -term
  )

# # [TEST] ------------------------------------------------------------------
# fun_plot.comparisons(
#   .df_data.users = df_input
#   , .df_data.comparison = df_occupations
#   , sample(df_occupations$occupation, 1)
#   , sample(df_occupations$occupation, 10)
#   , sample(df_occupations$occupation, 2)
#   , sample(df_occupations$occupation, 1)
#   , df_occupations %>%
#     filter(
#       career_cluster %in% "Finance"
#       , entry_level_education %in% c(
#         "Bachelor's degree"
#         , "Postsecondary nondegree award"
#         , "Master's degree"
#         , "Doctoral or professional degree"
#       )) %>% 
#     pull(occupation)
#   , 'Finance'
#   , 'Financial Managers'
#   # , lgc_factors.heatmap = F
#   # ) %>% view
# ) -> dsds
# 
# dsds$actual %>% view
# 
# dsds$actual %>% view
# dsds$average %>% view
# 
# dsds$user
# dsds$data
# dsds$scores %>% view
# 
# dsds$average.scores %>% view
# 
# dsds$individual.scores %>% view
# dsds$average.scores %>% view
# 
# dsds$items.average
# dsds$factor_scores
# dsds$factor_scores.average
# 
# dsds$factor.scores %>% view
# 
# dsds$factor.scores$`Financial Managers`
# df_occupations %>% 
#   filter(occupation == 'Financial Managers') %>% 
#   pull(annual_wage_2021)
# 
# dsds$factor.scores$Finance
# dsds$factor.scores
# 
# dsds$factor.scores$Finance %>% view
# 
# intersect(
#   'Financial'
#   , df_occupations$occupation
# )
# 
# intersect(
#   df_occupations$career_cluster
#   , df_occupations$occupation
# )
# 
# 
# 
# sample(df_occupations$occupation, 10) %>% 
#   fun_text.commas(.lgc_quote = F) %>% 
#   
#   
#   str_trunc(
#     width = 40
#     , ellipsis = ' ...'
#   )
# 
# # fun_factor.scores()
# # scoreVeryFast()
# fun_plot.comparisons(
#   .df_data = tibble()
#   , c(
#     # df_occupations$occupation %>%
#     #   unique() %>%
#     #   sample(2),
#     df_occupations$entry_level_education %>% 
#       unique()# %>%
#     # sample(2)
#   )
#   , df_occupations$occupation %>%
#     unique() %>%
#     sample(2)
# )
# 
# 
# 
# df_occupations %>% 
#   group_by(across(where(
#     ~ !is.numeric(.x)
#   ))) %>% 
#   summarise(across(
#     .cols = where(is.numeric)
#     ,.fns = mean
#   )) -> lalala
# 
# 
# bind_cols(
#   lalala
#   , fun_factor.scores(
#     .df_data.numeric = lalala
#     , .list_factor.keys = list_factors
#   ) %>% 
#     first()
# ) -> lala
# 
# 
# fun_factor.scores(
#   .list_factor.keys = list_factors
# )
# 
# 
# fun_plot.comparisons(
#   .df_data = tibble()
#   , c(
#     # df_occupations$occupation %>% 
#     #   unique() %>%
#     #   sample(2), 
#     df_occupations$entry_level_education %>% 
#       unique() %>%
#       sample(2)
#   )
# ) %>% 
#   map(
#     function(x){
#       x %>%
#         pull(1) %>% 
#         unique()
#       
#     }
#   )
# 
# 
# # ) %>%
# first() %>% 
#   fun_factor.scores(
#     .list_factor.keys = list_factors
#     , .lgc_pivot.long = T
#   )
# 
# df_occupations %>% 
#   filter(if_any(
#     .cols = everything()
#     ,.fns = 
#       ~ .x %in% all_of(
#         c(
#           df_occupations$occupation %>%
#             unique() %>%
#             sample(2)
#           , df_occupations$entry_level_education %>% 
#             unique() %>%
#             sample(2)
#         )
#       ))) %>% view()
# 
# 
