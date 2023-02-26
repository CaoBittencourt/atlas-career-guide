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

library(Hmisc)
# library(ids)
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

# [FUNCTION] PLOT COMPARISON ----------------------------------------------
fun_plot.comparisons.mvp <- function(
    
  # Data
  .df_data.users
  , .df_data.comparison = NULL
  # Factor list
  , .list_factors = list()
  
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
    
    , 'abilities' = '#C92618'
    , 'knowledge' = '#FF9E1F'
    , 'skills' = '#50915D'
    
    , 'black' = '#212121'
    , 'grey' = '#D4D5D8'
  ) -> list_pal.atlas
  
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
  
  # Individual scores, factor scores, and averages
  .df_data.users %>% 
    fun_factor.scores2(
      .list_factor.keys = .list_factors
      , .lgc_sample.averages = T
      , .lgc_pivot.long = T
      , .lgc_totals = F
    ) -> list_scores
  
  # Acronyms
  list_scores$scores.long %>%
    # full_join(df_acronyms) -> list_scores$scores.long
    left_join(df_acronyms) -> list_scores$scores.long
  
  list_scores$scores.average.long %>% 
    # full_join(df_acronyms) -> list_scores$scores.average.long
    left_join(df_acronyms) -> list_scores$scores.average.long
  
  # Comparison plots
  NULL -> plt_items
  NULL -> list_plt_items.users
  NULL -> plt_items.average
  NULL -> plt_factors
  NULL -> list_plt_factors.users
  NULL -> plt_factors.average
  
  # Item by item comparison
  list_scores$scores.long %>%
    # full_join(df_acronyms) %>%
    fun_plot.heatmap(aes(
      x = id.unique
      , y = item.name
      , fill = item.score
    )
    , .fun_format.y = function(x){x}
    # , .coord_polar = T
    ) -> plt_items
  
  # Item by item comparison (one plot per user)
  list_scores$scores.average.long %>%
    mutate(
      id.unique = 'Average'
    ) %>%
    bind_rows(
      list_scores$scores.long
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
        arrange(item.score) %>%
        pull(item.name) %>%
        unique() -> chr_order

      df_average.long %>%
        filter(
          id.unique %in% c(.x, 'Average')
        ) %>%
        mutate(
          item.name = factor(item.name, levels = chr_order)
          , id.unique = fct_inorder(id.unique)
        ) %>%
        group_by(id.unique) %>%
        arrange(item.name) %>%
        mutate(
          column = row_number() < (n() / 2)
          , column = factor(column)
        ) %>%
        ungroup() %>%
        fun_plot.bar(aes(
          x = item.name
          , y = item.score
          , label = percent(item.score, accuracy = 1)
          , fill = id.unique
          , color = id.unique
        )
        , .sym_facets = column
        , .chr_scales = 'free'
        , .list_labs = list(
          title = paste0('Item scores — ', .x)
          , x = NULL
          , y = 'Item Score'
          , fill = NULL
          , color = NULL
        )
        , .coord_polar = F
        , .coord_flip = T
        , .reorder_fct = F
        , .chr_manual.pal = c(
          list_pal.atlas$grey
          , list_pal.atlas$purple3
        )
        , .chr_manual.aes = c(
          'fill', 'color'
        )
        , .list_axis.y.args = list(
          limits = c(0, 1.1)
          , breaks = seq(0, 1, 0.25)
        )
        , .fun_format.y = percent_format(accuracy = 1)
        , .list_labels.param = list(
          position = c(position_dodge2(0.5, 'single'))
          , hjust = -0.15
        )
        , .theme = theme_ridges(center_axis_labels = T) +
          theme(
            panel.grid.major.y = element_blank()
            , axis.text.y = element_blank()
            , axis.ticks.y = element_blank()
            , legend.position = 'bottom'
            , strip.background = element_blank()
            , strip.text = element_blank()
            , plot.margin = margin(1, 1, 1, 1,'cm')
          )
        , .list_geom.param = list(
          position = c(position_dodge2(0.5, 'single'))
          , width = 0.5
        )
        ) +
        geom_text(aes(
          label = item.name
          , x = item.name
          , y = 0
        )
        , vjust = -2
        , hjust = 0
        )

    }
  ) -> list_plt_items.users
  
  # Factor by factor comparison (one plot per user)
  list_scores$scores.average.long %>%
    mutate(
      id.unique = 'Average'
    ) %>%
    bind_rows(
      list_scores$scores.long
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
        arrange(factor.score) %>%
        pull(factor) %>%
        unique() -> chr_order

      df_average.long %>%
        filter(
          id.unique %in% c(.x, 'Average')
        ) %>%
        mutate(
          factor = factor(factor, levels = chr_order)
          , id.unique = fct_inorder(id.unique)
        ) %>%
        group_by(id.unique) %>%
        arrange(factor) %>%
        mutate(
          column = row_number() < (n() / 2)
          , column = factor(column)
        ) %>%
        ungroup() %>%
        select(
          id.unique
          , factor
          , category
          , factor.score
        ) %>%
        unique() %>%
        pivot_wider(
          names_from = id.unique
          , values_from = factor.score
        ) %>%
        fun_plot.dumbbell(aes(
          x = !!sym(.x)
          , xend = Average
          , y = factor
        )
        , .list_geom.param = list(
          color = 'lightgrey'
          , colour_x = list_pal.atlas$blue4
          , colour_xend = list_pal.atlas$grey
          , size_x = 5.4
          , size_xend = 5.4
          , size = 2
        )
        , .list_labels1.param = list(
          fontface = 'bold'
          , color = list_pal.atlas$blue4
          , size = 3.33
          , vjust = -1.5
          , hjust = 0.5
        )
        , .list_labels2.param = list(
          fontface = 'bold'
          , color = list_pal.atlas$grey
          , size = 3.33
          , vjust = 2.25
          , hjust = 0.5
        )
        , .list_axis.x.args = list(
          limits = c(-.1,1.1)
          , breaks = seq(0,1,.25)
        )
        , .fun_format.x = label_percent()
        , .fun_format.y = function(y){y}
        , .fun_format.labels = label_percent(accuracy = 1)
        , .list_labs = list(
          title = paste0('Factor scores — ', .x)
          , x = 'Factor Score'
          , y = NULL
          , fill = NULL
          , color = NULL
        )
        )

    }
  ) -> list_plt_factors.users
  
  # Average item by item comparison
  list_scores$scores.average.long %>% 
    # full_join(df_acronyms) %>% 
    fun_plot.bar(aes(
      x = item.name
      , y = item.score
      , label = item.acronym
      , fill = category
      , color = category
    )
    , .coord_polar = T
    , .fun_polar.labels = percent
    , .list_axis.y.args = list(
      breaks = seq(0, 1, length.out = 5)
    )
    , .list_labels.param = list(
      color = list_pal.atlas$black
    )
    , .chr_manual.pal = c(
      list_pal.atlas$abilities
      , list_pal.atlas$knowledge
      , list_pal.atlas$skills
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
  
  
  # if(.lgc_compare.averages){
  #   
  #   list_scores$scores.average.long %>%
  #     filter(attribute %in% chr_items) %>% 
  #     fun_plot.lollipop(aes(
  #       x = attribute
  #       , y = value
  #       , label = percent(value, accuracy = .01)
  #       # , color = factor
  #     )
  #     , .list_labs = list(
  #       x = NULL
  #       , y = 'Average Item Score'
  #       , color = NULL
  #     )
  #     , .dbl_limits.y = c(0,1)
  #     , .fun_format.y = percent_format(accuracy = 1)
  #     , .reorder_fun = max
  #     , .coord_flip = T
  #     , .theme = ggridges::theme_ridges(center_axis_labels = T) +
  #       theme(panel.grid.major.y = element_blank())
  #     ) -> plt_items.average
  #   
  # }
  
  # # Factor scores comparison
  # if(length(.list_factors)){
  #   
  #   list_scores$scores.average.long %>% 
  #     filter(
  #       attribute %in% 
  #         names(flatten(.list_factors))
  #     ) %>% 
  #     rename(factor = attribute) %>% 
  #     fun_plot.heatmap(aes(
  #       x = id.unique
  #       , y = factor
  #       , fill = value
  #     )
  #     , .scale_colors = list(
  #       scale_color_stepsn(
  #         colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
  #         , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
  #         , limits = c(0,1)
  #       )
  #       , scale_fill_stepsn(
  #         colors = viridis(length(c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)))
  #         , breaks = c(0, 0.17, 0.33, 0.5, 0.67, 0.83, 1)
  #         , limits = c(0,1)
  #       ) 
  #     )
  #     # , .coord_polar = T
  #     ) -> plt_factors
  #   
  #   
  #   # Average factor scores comparison
  #   # if(.lgc_compare.averages){
  #   #   
  #   #   df_scores.average %>%
  #   #     select(
  #   #       -where(is.numeric)
  #   #       , .list_factors %>%
  #   #         flatten() %>%
  #   #         names()
  #   #     ) %>%
  #   #     pivot_longer(
  #   #       cols = where(is.numeric)
  #   #       , names_to = 'factor'
  #   #       , values_to = 'value'
  #   #     ) %>%
  #   #     fun_plot.lollipop(aes(
  #   #       x = factor
  #   #       , y = value
  #   #       , label = percent(value, accuracy = .01)
  #   #       # , color = category
  #   #     )
  #   #     , .list_labs = list(
  #   #       x = NULL
  #   #       , y = 'Average Factor Score'
  #   #       , color = NULL
  #   #     )
  #   #     , .dbl_limits.y = c(0,1)
  #   #     , .fun_format.y = percent_format(accuracy = 1)
  #   #     , .reorder_fun = max
  #   #     , .coord_flip = T
  #   #     , .theme = ggridges::theme_ridges(center_axis_labels = T) +
  #   #       theme(panel.grid.major.y = element_blank())
  #   #     ) -> plt_factors.average
  #   #   
  #   # }
  #   
  # }
  # 
  
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
  .df_data.users = df_input
  , .df_data.comparison = NULL
  , .list_factors = list_factors.competencies
  # , .list_factors = unname(list_factors.competencies)
  # , .list_factors = unname(map(list_factors.competencies, unname))
) -> dsds

dsds$items.average
dsds$items
sample(dsds$items.users, 1)
sample(dsds$factors.users, 1)

df_occupations %>%
  slice_sample(n = 10) %>%
  # slice(1:50) %>% 
  fun_plot.bar(aes(
    x = occupation
    , y = economics_and_accounting.l
    , label = economics_and_accounting.l
  )
  # , .list_labels.param = list(
  #   size = 1
  #   , color = '#212121'
  #   # , hjust = 2
  # )
  , .coord_flip = T
  , .coord_polar = T
  , .fun_polar.labels = percent
  , .list_axis.y.args = list(
    breaks = seq(0, 1, length.out = 5)
  )
  , .list_labs = list(
    y = 'Economics & Accounting'
  )
  )
  # ) + 
  geom_textpath(
    aes(
      x = occupation
      , y = economics_and_accounting.l
      , label = economics_and_accounting.l
    )
    # , nudge_y = 0.1
    , angle = -90
    , hjust = 1.15
    , size = 2
  )

lalala$layers[[4]]$aes_params
lalala$layers[[4]]$geom_params


df_occupations %>%
  slice_sample(n = 150) %>% 
  mutate(
    relevance = case_when(
      economics_and_accounting.l >= 0.83 ~ 'Obligatory'
      , between(economics_and_accounting.l, 0.5, 0.83) ~ 'Dominant'
      , between(economics_and_accounting.l, 0.17, 0.5) ~ 'Auxiliary'
      , economics_and_accounting.l <= 0.17 ~ 'Irrelevant'
    )
  ) %>%
  fun_plot.bar(aes(
    x = occupation
    , y = economics_and_accounting.l
    , fill = relevance
  )
  , .chr_manual.pal = c(
    'Obligatory' = viridis(4)[4]
    , 'Dominant' = viridis(4)[3]
    , 'Auxiliary' = 'lightgrey'
    , 'Irrelevant' = 'red'
  )
  , .coord_polar = T
  , .fun_polar.labels = percent
  , .list_axis.y.args = list(
    breaks = seq(0, 1, length.out = 5)
  )
  , .list_labs = list(
    y = 'Economics & Accounting'
    , fill = 'Role'
  )
  )

df_occupations %>% 
  slice_sample(n = 50) %>% 
  mutate(
    dsds = economics_and_accounting.l > 0.1
  ) %>%
  fun_plot.bar(aes(
    x = occupation
    , y = economics_and_accounting.l
    , fill = dsds
    # , fill = economics_and_accounting.l > 0.1
    # , label = n
  )
  , .coord_polar = T
  , .reorder_fct = T
  , .fun_polar.labels = percent
  , .list_axis.y.args = list(
    breaks = seq(0, 1, length.out = 5)
  )
  , .list_labs = list(
    y = 'Economics & Accounting'
    , fill = 'dsds'
  )
  )

dsdsds %>% view

if(
  all(
    length(dsdsds$fill)
    , last(as.character(dsdsds$fill)) %in% 
    names(df_occupations)
    
  )
  ){
  
  'lalala'
  
  
} else {
  
  'dsds'
  
}


dsds$average.long %>% 
  # select(-c(
  #   item
  #   , item.score
  # )) %>%
  fun_plot.bar(aes(
    # x = factor
    # , y = factor.score
    x = item
    , y = item.score
    # , label = percent(item.score)
    # , color = item.score > 0.5
    , fill = item.score > 0.5
  )
  , .coord_polar = T
  , .reorder_fct = F
  , .reorder_desc = T
  , .reorder_fun = max
  , .list_labs = list(
    y = 'dsds'
  )
  # , .scale_colors = list(
  #   scale_fill_viridis(discrete = T, na.translate = T)
  #   , scale_color_viridis(discrete = T, na.translate = T)
  # )
  # , .dbl_limits.y = c(0,1)
  , .list_axis.y.args = list(
    breaks = seq(0,1,length.out = 5)
  )
  , .fun_polar.labels = percent
  )

df_occupations %>% 
  fun_plot.bar(aes(
    x = occupation
    , y = annual_wage_2021
    # x = item
    # , y = item.score
    # , label = percent(item.score)
    # , color = item.score > 0.5
    # , fill = item.score > 0.5
  )
  , .coord_polar = T
  , .reorder_fct = F
  , .reorder_desc = T
  , .reorder_fun = max
  , .list_labs = list(
    y = 'dsds'
  )
  # , .scale_colors = list(
  #   scale_fill_viridis(discrete = T, na.translate = T)
  #   , scale_color_viridis(discrete = T, na.translate = T)
  # )
  # , .dbl_limits.y = c(0,1)
  , .list_axis.y.args = list(
    breaks = seq(0,max(df_occupations$annual_wage_2021),length.out = 5)
  )
  , .fun_polar.labels = dollar
  )

empty_bar <- 4
to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$group), ncol(data)) )
colnames(to_add) <- colnames(data)
to_add$group <- rep(levels(data$group), each=empty_bar)
data <- rbind(data, to_add)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))

# # [TEST] ------------------------------------------------------------------
# fun_plot.comparisons.mvp(
#   .df_data.users = df_input
#   , .df_data.comparison = NULL
#   , .list_factors = map(list_factors.competencies, unname) 
#   , .lgc_compare.averages = T
# ) -> dsds
# 
# dsds$items.users[[1]]
# last(dsds$items.users)
# 
# dsds$actual
# dsds$actual.long
# dsds$average
# dsds$average.long
# dsds$items + 
#   geomtextpath::coord_curvedpolar()
# 
# dsds[[1]]
# dsds[[2]]
# dsds[[3]]
# dsds[[4]]
# 
# fun_factor.scores2(
#   .df_data = df_occupations
#   , .list_factor.keys = list_factors.competencies
#   , .lgc_sample.averages = T
#   , .lgc_pivot.long = T
# ) -> dsdsds
# 
# dsdsds$scores.long %>%
#   mutate(
#     item.score = case_when(
#       item.score >= 0.67 ~ 'Dominant'
#       , item.score < 0.67 & 
#         item.score >= 0.33 ~ 'Auxiliary'
#       , T ~ 'Irrelevant'
#     ) 
#   ) %>% 
#   group_by(occupation, factor) %>% 
#   mutate(factor.score = sum(str_count(item.score, 'Dominant')) / n()) %>% 
#   ungroup() %>% 
#   group_by(occupation, category) %>% 
#   mutate(category.score = sum(str_count(item.score, 'Dominant')) / n()) %>% 
#   ungroup() %>%
#   mutate(across(
#     .cols = c(category.score, factor.score)
#     ,.fns = ~ ifelse(.x >= 0.67, 'Dominant', 'Auxiliary')
#   )) %>% 
#   filter(item.score != 'Irrelevant') %>% view
# 
# # mutate(across(
# #   .cols = ends_with('.score')
# #   ,.fns = ~ case_when(
# #     .x < 0.67 ~ 'Auxiliary'
# #     , .x >= 0.67 ~ 'Dominant'
# #   )
# # )) %>% view
# 
# 
# 
# dsds[[1]]
# 
# dsds %>% 
#   filter(
#     id.unique %in% c(last(id.unique), 'Average')
#   ) %>% 
#   mutate(id.unique = fct_inorder(id.unique)) %>%
#   ungroup() %>%
#   fun_plot.bar(aes(
#     x = item
#     , y = item.score
#     , label = percent(item.score, accuracy = .01)
#     , fill = id.unique
#     , color = id.unique
#   )
#   , .list_labs = list(
#     x = NULL
#     , y = 'Item Score'
#     , fill = NULL
#     , color = NULL
#   )
#   , .reorder_fct = T
#   , .chr_manual.pal = c(
#     list_pal.atlas$grey
#     , list_pal.atlas$purple3
#   )
#   , .chr_manual.aes = c(
#     'fill', 'color'
#   )
#   , .dbl_limits.y = c(0,1)
#   , .fun_format.y = percent_format(accuracy = 1)
#   , .reorder_fun = max
#   # , .coord_flip = F
#   , .list_labels.param = list(
#     position = 'dodge'
#     , fontface = 'bold'
#     , color = '#3854FB'
#     , size = 3.33
#     , vjust = -1.15
#     , hjust = -0.15
#     # , vjust = -1
#     # , hjust = 0
#     
#   )
#   , .theme = ggridges::theme_ridges(center_axis_labels = T) +
#     theme(panel.grid.major.y = element_blank())
#   , .list_geom.param = list(
#     position = c(position_dodge2(0.8, 'single'))
#     , width = 0.7
#   )
#   )
# 
# 
# 
# dsds$factor.names %>% view
# dsds$factor.scores %>% view
# dsds$factor.scores.long %>% view
# dsds$factor.scores.average %>% view
# dsds$factor.scores.average.long %>% view
# 
# dsds[[1]] %>% view
# dsds[[2]] %>% view
# 
# dsds$items
# dsds$items.users$Random
# dsds$items.average
# dsds$factors

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
