# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
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

# KFLEX FUNCTION ------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')

# PLOTTING FUNCTIONS ------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')

# DATA --------------------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')

# -------- CAPITAL FLEXIBLITY ---------------------------------------------
# APPLY FUNCTION -----------------------------------------------------------
df_occupations.pop %>% 
  select(1:127) %>% 
  summarise(
    across(
      .cols = ends_with('.l')
      ,.fns = fun_capital.flex
    )) %>%
  pivot_longer(
    cols = everything()
    , names_to = 'attribute'
    , values_to = 'capital.flex'
  ) %>% 
  arrange(desc(
    capital.flex
  )) -> df_kflex.long

# FLEXIBLE CAPITAL PER OCCUPATION -----------------------------------------
df_occupations %>%
  select(1:126) %>% 
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

# -------- VISUALIZATIONS -------------------------------------------------
# # CAPITAL FLEXIBILITY DISTRIBUTION -----------------------------------------------------
# # Density
# df_kflex.long %>%
#   fun_plot.histogram(aes(
#     x = capital.flex
#     , y = ..density..
#   )
#   , .list_labs = list(
#     title = str_to_title('distribution of capital flexibility')
#     , subtitle = str_to_title('understanding the degree of transferability of professional attributes')
#     , x = str_to_title('capital flexiblity score')
#     , y = str_to_title('density')
#   )
#   , .list_axis.x.args = list(
#     breaks = seq(0,1,.25)
#     , limits = c(-.1,1.05)
#   )
#   ) + 
#   stat_function(
#     fun = dnorm
#     , args = list(
#       mean = mean(df_kflex.long$capital.flex)
#       , sd = sd(df_kflex.long$capital.flex)
#     )
#     , col = '#212121'
#     , size = 1.5
#   ) +
#   annotate(
#     geom = 'text'
#     , x = median(c(0.75, 1))
#     , y = 2.8
#     , label = str_wrap(
#       'Capital Flexibility is somewhat normally distributed. 
#       This means that most values concentrate in the middle of the Capital Flexibility bell curve.
#       Thus, attributes have a fair degree of transferability to different professions.'
#       , width = 28
#     )
#     , fontface = 'plain'
#   ) -> plt_kflex.dist

# CAPITAL FLEXIBILITY DISTRIBUTION -----------------------
df_kflex.long %>%
  fun_plot.histogram(aes(
    x = capital.flex
    , y = after_stat(density)
  )
  , .dbl_limits.y = c(0,1.25*max(density(df_kflex.long$capital.flex)$y))
  , .list_axis.x.args = list(
    limits = c(-0.1,1.1)
    , breaks = seq(0,1,.25)
  )
  , .fun_format.x = percent_format(accuracy = 1)
  , .list_labs = list(
    title = str_to_title('distribution of capital flexibility')
    , subtitle = str_to_title('understanding the degree of transferability of professional attributes')
    , x = str_to_title('human capital flexiblity score')
    , y = str_to_title('frequency')
  )
  , .theme = ggridges::theme_ridges(font_size = 11, center_axis_labels = T) +
    theme(
      plot.margin = margin(0, 0, 0, 0)
      , axis.text.y = element_blank()
    )
  ) +
  geom_density(aes(
    x = capital.flex
  )
  , size = 1.2
  ) + 
  annotate(
    geom = 'text'
    , x = median(c(0.7, 1))
    , y = mean(c(0, 1.25*max(density(df_kflex.long$capital.flex)$y)))
    , label = str_wrap(
      'Human Capital Flexibility follows a somewhat right-skewed bimodal distribution.
      This means that most values concentrate on the lower end of the scale,
      clustering around two basic groups: 
      one with medium-low transferability, 
      and the other with even lower.
      Thus, job-related attributes with high carry-over across multiple occupations are not common.'
      , width = 40
    )
    , fontface = 'plain'
  )

# CAPITAL FLEXIBILITY OF EACH ATTRIBUTE ------------------------------------
# Backup
df_kflex.long2 <- df_kflex.long

# Add lines to the initial dataset
empty_bar <- 27
to_add <- matrix(NA, empty_bar, ncol(df_kflex.long))
colnames(to_add) <- colnames(df_kflex.long)
df_kflex.long2 <- rbind(to_add, df_kflex.long2)
df_kflex.long2$id <- factor(seq(1, nrow(df_kflex.long2)))

# Circular bar plot
df_kflex.long2 %>% 
  fun_plot.bar(aes(
    # x = occupation
    x = id
    , y = capital.flex
  )
  , .list_labs = list(
    title = str_to_title('capital flexibility by attribute')
    , subtitle = str_to_title("how much of each attribute is transferable across occupations")
  )
  , .coord_polar = T
  , .reorder_fct = T
  , .fun_axis.y = scale_y_continuous
  , .list_axis.y.args = list(
    limits = c(-0.55,1.1)
  )
  , .theme = ggridges::theme_ridges() + 
    theme(
      plot.title = element_text(hjust = 0.5)
      , axis.title = element_blank()
      , axis.text = element_blank()
      , axis.ticks = element_blank()
      , panel.grid = element_blank()
      , plot.subtitle = element_text(hjust = 0.5)
    ) 
  ) + 
  coord_polar(
    # start = -6.12
    start = -12.21
  ) +
  map(
    seq(0,1,0.25)
    , function(y){
      
      annotate(
        x = '14'
        , y = y + 0.1
        , label = percent(y) 
        , geom = 'text'
        , color = '#212121'
        , fontface = 'bold'
        , size = 3
      )
      
    }) + 
  annotate(
    x = 1
    , y = -.55
    , label = str_wrap(
      'Many skills have reasonable carryover to other activities.'
      , width = 20
    ) 
    , geom = 'text'
    , color = '#212121'
    , fontface = 'bold'
    , size = 3.33
  ) -> plt_attributes.kflex

plt_attributes.kflex$layers <- c(
  geom_hline(
    yintercept = c(0, 0.25, 0.5, 0.75)
    , color = '#D4D5D8'
    , size = 0.5
  )
  , plt_attributes.kflex$layers
)

plt_attributes.kflex$layers <- c(
  geom_hline(
    yintercept = 1
    , color = '#D4D5D8'
    , size = 2
  )
  , plt_attributes.kflex$layers
)

plt_attributes.kflex

# CAPITAL FLEXIBILITY IN EACH OCCUPATION ------------------------------------
# Backup
df_occupations.kflex2 <- df_occupations.kflex

# Add lines to the initial dataset
empty_bar <- 51
to_add <- matrix(NA, empty_bar, ncol(df_occupations.kflex))
colnames(to_add) <- colnames(df_occupations.kflex)
df_occupations.kflex2 <- rbind(to_add, df_occupations.kflex2)
df_occupations.kflex2$id <- factor(seq(1, nrow(df_occupations.kflex2)))

# Circular bar plot
df_occupations.kflex2 %>%
  fun_plot.bar(aes(
    # x = occupation
    x = id
    , y = capital.flex.pct
  )
  , .list_labs = list(
    title = str_to_title('capital flexibility by occupation')
    , subtitle = str_to_title("how much of each occupation's requirements is transferable to other occupations")
  )
  , .coord_polar = T
  , .reorder_fct = T
  , .fun_axis.y = scale_y_continuous
  , .list_axis.y.args = list(
    limits = c(-0.55,1.1)
  )
  , .theme = ggridges::theme_ridges() + 
    theme(
      plot.title = element_text(hjust = 0.5)
      , axis.title = element_blank()
      , axis.text = element_blank()
      , axis.ticks = element_blank()
      , panel.grid = element_blank()
      , plot.subtitle = element_text(hjust = 0.5)
    ) 
  ) + 
  coord_polar(
    # start = -6.12
    start = -6.1
  ) +
  map(
    seq(0,1,0.25)
    , function(y){
      
      annotate(
        x = '26'
        , y = y + 0.1
        , label = percent(y)
        , geom = 'text'
        , color = '#212121'
        , fontface = 'bold'
        , size = 3
      )
      
    }) + 
  annotate(
    x = 1
    , y = -.55
    , label = str_wrap(
      'HALF OF ALL PROFESSIONS IS COMMON SENSE!'
      , width = 20
    ) 
    , geom = 'text'
    , color = '#212121'
    , fontface = 'bold'
    , size = 3.33
  ) -> plt_occupations.kflex

plt_occupations.kflex$layers <- c(
  geom_hline(
    yintercept = c(0, 0.25, 0.5, 0.75)
    , color = '#D4D5D8'
    , size = 0.5
  )
  , plt_occupations.kflex$layers
)

plt_occupations.kflex$layers <- c(
  geom_hline(
    yintercept = 1
    , color = '#D4D5D8'
    , size = 2
  )
  , plt_occupations.kflex$layers
)


plt_occupations.kflex

# -------- EXPORT DATA ----------------------------------------------------
# # EXCEL -------------------------------------------------------------------
# # Attributes
# df_kflex.long %>%
#   arrange(desc(
#     capital.flex
#   )) %>%
#   write.xlsx(
#     'df_attributes.kflex.xlsx'
#   )
# 
# # Occupations
# df_occupations.kflex %>%
#   select(
#     occupation
#     , code
#     , code.variants
#     , employment
#     , entry_level_education
#     , annual_wage_2021
#     , capital.flex.pct
#   ) %>%
#   arrange(desc(
#     capital.flex.pct
#   )) %>%
#   write.xlsx(
#     'df_occupations.kflex.xlsx'
#   )