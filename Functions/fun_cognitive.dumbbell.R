# [SETUP] -----------------------------------------------------------------
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# Output name
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_unique_name.R')
# REGULAR OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.R')

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

# [FUNCTION] PLOT COMPARISON ----------------------------------------------
fun_cognitive.dumbbell <- function(
    
  # Data
  .df_data.user
  , .df_data.comparison
  
){
  
  # Arguments validation
  # Data frames
  stopifnot(
    "'.df_data.user' must be a data frame containing the users' item scores." = 
      is.data.frame(.df_data.user)
  )
  
  stopifnot(
    "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." = 
      is.data.frame(.df_data.comparison) |
      !length(.df_data.comparison)
  )
  
  # User's name
  .df_data.user %>% 
    pull(user.name) %>%
    unique() -> chr_user
  
  # Add occupation to user data
  .df_data.comparison %>% 
    filter(if_any(
      .cols = !where(is.numeric)
      ,.fns = 
        ~ .x %in% all_of(.df_data.user$occupation)
    )) %>% 
    select(
      where(
        ~ is.numeric(.x)
        | any(.x %in% .df_data.user$occupation)
      )) %>% 
    select(any_of(
      names(.df_data.user)
    )) %>% 
    relocate(
      !where(is.numeric)
      , where(is.numeric)
    ) %>% 
    rename(
      user.name = occupation
    ) %>% 
    slice(
      match(
        .df_data.user$occupation
        , user.name
      )
    ) %>%
    mutate(
      user.name = .df_data.user$occupation.title
    ) %>% 
    bind_rows(
      .df_data.user
    ) -> .df_data.user
  
  # Pretty names
  .df_data.user %>% 
    pivot_longer(
      cols = where(is.numeric)
      , names_to = 'item'
      , values_to = 'item.score'
    ) %>%
    mutate(
      item = str_replace_all(item, '_', ' ')
      , item = str_remove_all(item, '\\.l')
      , item = str_to_title(item)
    ) -> .df_data.user
  
  # Comparison plots
  # Item by item comparison
  .df_data.user %>%
    filter(
      user.name %in% chr_user
    ) %>%
    arrange(desc(item.score)) %>%
    pull(item) %>% 
    unique() -> chr_order
  
  c(
    chr_user
    , .df_data.user %>%
      filter(user.name != chr_user) %>%
      pull(user.name) %>% 
      unique()
  ) -> chr_ids
  
  .df_data.user %>%
    mutate(
      item = factor(item, levels = chr_order)
      , user.name = fct_inorder(user.name)
    ) %>%
    fun_plot.dumbbell2(aes(
      x = item.score
      , y = item
      , color = user.name
    )
    , .sym_facets = item
    , .int_facets = 2
    , .chr_scales = 'free_y'
    , .list_labs = list(
      title = paste0('Cognitive Assessment — ', chr_user)
      , x = 'Item Score'
      , y = NULL
      , color = NULL
    )
    , .reorder_fct = F
    , .chr_manual.pal = set_names(
      c(
        list_pal.atlas$purple3
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
        , panel.grid = element_line(linewidth = unit(0.1, 'cm'))
        , legend.position = 'bottom'
        , legend.justification = 'center'
        , strip.background = element_blank()
        , strip.text =  element_text(
          margin = margin(0, 0, 0.25, 0, 'cm')
        )
        , panel.spacing.x = unit(0.5, 'cm')
        , panel.spacing.y = unit(1, 'cm')
        , plot.margin = margin(1, 1, 1, 1,'cm')
      )) -> plt_items
  
  # Output
  # File name
  fun_name.unique(
    chr_suffix = '.png'
  ) -> chr_filename
  
  # Save plot
  ggplot2::ggsave(
    filename = chr_filename
    , bg = 'white'
    , plot = plt_items
    , height = 11.69
    , width = 8.27
    , units = 'in'
    , device = 'png'
    , dpi = 700
  )
  
  return(
    paste0(
      getwd()
      , chr_filename
    ))
  
}


# # [DATA] LIST OF COGNITIVE ABILITIES --------------------------------------
# list(
#   'time_sharing.l'
#   , 'selective_attention.l'
#   , 'problem_sensitivity.l'
#   , 'fluency_of_ideas.l'
#   , 'originality.l'
#   , 'deductive_reasoning.l'
#   , 'inductive_reasoning.l'
#   , 'category_flexibility.l'
#   , 'information_ordering.l'
#   , 'perceptual_speed.l'
#   , 'flexibility_of_closure.l'
#   , 'speed_of_closure.l'
#   , 'number_facility.l'
#   , 'mathematical_reasoning.l'
#   , 'spatial_orientation.l'
#   , 'visualization.l'
#   , 'written_expression.l'
#   , 'oral_expression.l'
#   , 'oral_comprehension.l'
#   , 'written_comprehension.l'
#   , 'memorization.l'
# ) -> list_abilities.cognitive

# # [TEST] --------------------------------------------------------------------
# fun_cognitive.dumbbell(
#   .df_data.user =
#     df_input %>%
#     select(!c(1,2)) %>%
#     slice_sample(n = 1) %>%
#     mutate(
#       occupation = sample(df_occupations$occupation, 1)
#       , occupation.title = stringi::stri_rand_shuffle(occupation)
#       , .after = 1
#     ) %>%
#     rename_with(
#       ~ str_to_lower(.x)
#     ) %>%
#     rename(user.name = name) %>% 
#     select(
#       1:3
#       , flatten_chr(list_abilities.cognitive)
#     )
#   , .df_data.comparison = df_occupations
# )
# 
