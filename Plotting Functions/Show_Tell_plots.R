# -------- SETUP ----------------------------------------------------------
# FUNCTIONS & PACKAGES ----------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Plotting Functions')

source('./Auto_plots.R')

# DATA --------------------------------------------------------------------
data('diamonds')

# COLOR PALETTES -----------------------------------------------------------
# Manual
pal_manual <- c()

# External
list(
  ggthemes::scale_color_gdocs()
  , ggthemes::scale_fill_gdocs()
) -> pal_gdocs

list(
  ggthemes::scale_color_economist()
  , ggthemes::scale_fill_economist()
) -> pal_econ

list(
  ggthemes::scale_fill_few()
  , ggthemes::scale_color_few()
) -> pal_few

# -------- SHOW & TELL ----------------------------------------------------
# HISTOGRAM / DENSITY --------------------------------------------------------------
# Basic plot
diamonds %>% 
  fun_dist.plot(
    .mapping = aes(x = price)
    , .histogram = T
    , .density = F
  )

# Basic plot + labels + color mapping (default palette)
diamonds %>% 
  fun_dist.plot(
    # Plot
    .mapping = aes(x = price, fill = clarity)
    , .histogram = T
    , .density = F
    # Labels
    , .list_labs = list(
      title = 'Diamond price vs quality of cut'
      , subtitle = 'How does diamond cut affect diamond prices?'
      , x = 'Price (USD)'
      , y = 'Frequency'
      , fill = 'Cut'
    )
    # Formatting
    , .chr_format.x = 'usd'
    , .int_decimals.y = 0
    , .int_breaks.y = 8
  )

# Basic plot + labels + color mapping (external palette, gdocs)
diamonds %>% 
  fun_dist.plot(
    # Plot
    .mapping = aes(x = price, fill = clarity)
    , .histogram = T
    , .density = F
    # Labels
    , .list_labs = list(
      title = 'Diamond price vs quality of cut'
      , subtitle = 'How does diamond cut affect diamond prices?'
      , x = 'Price (USD)'
      , y = 'Frequency'
      , fill = 'Cut'
    )
    # Formatting
    , .chr_format.x = 'R$'
    # , .chr_format.x = 'r$'
    # , .chr_format.x = 'BRL'
    # , .chr_format.x = 'brl'
    , .int_decimals.y = 0
    , .int_breaks.y = 8
    # Colors 
    , .scale_colors = pal_gdocs
  )

# [CONVESÃO PELA TAXA DE CÂMBIO PARA UMA VERSÃO FUTURA]

# Basic plot + labels + color mapping (external palette, The Economist)
diamonds %>% 
  fun_dist.plot(
    # Plot
    .mapping = aes(x = price, fill = clarity)
    , .histogram = T
    , .density = F
    # Labels
    , .list_labs = list(
      title = 'Diamond price vs quality of cut'
      , subtitle = 'How does diamond cut affect diamond prices?'
      , x = 'Price (USD)'
      , y = 'Frequency'
      , fill = 'Cut'
    )
    # Formatting
    , .chr_format.x = 'usd' #REVISTA INTERNACIONAL => US DOLLAR
    , .int_decimals.y = 0
    , .int_breaks.x = 3
    , .int_breaks.y = 8
    # Colors 
    , .scale_colors = pal_econ
    # Theme
    , .theme = ggthemes::theme_economist()
  )

# Basic plot + labels + color mapping (external palette, The Economist)
diamonds %>% 
  mutate(
    clarity = fct_reorder(clarity, carat)
  ) %>% 
  fun_dist.plot(
    # Plot
    .mapping = aes(x = carat, color = clarity)
    , .histogram = F
    , .density = T
    # Facets
    , .sym_facets = clarity
    , .int_facets = 4
    # Labels
    , .list_labs = list(
      title = 'Diamond weight vs clarity'
      , subtitle = 'How do diamonds of different weights compare in terms of clarity?'
      , x = 'Weight (carat)'
      , y = 'Frequency'
      , fill = 'Clarity'
    )
    # Formatting
    , .int_decimals.x = 0
    , .int_decimals.y = 0
    # Colors 
    , .scale_colors = pal_few 
    # Theme
    , .theme = ggthemes::theme_fivethirtyeight()
  )

# Basic plot + labels + color mapping (external palette, The Economist)
diamonds %>% 
  mutate(
    clarity = fct_reorder(clarity, carat)
  ) %>%
  fun_dist.plot(
    # Plot
    .mapping = aes(x = carat, fill = clarity)
    , .histogram = F
    , .density = T
    # Facets
    , .sym_facets = clarity
    , .int_facets = 2
    # Labels
    , .list_labs = list(
      title = 'Diamond weight vs clarity'
      , subtitle = 'How do diamonds of different weights compare in terms of clarity?'
      , x = 'Weight (carat)'
      , y = 'Frequency'
      , fill = 'Clarity'
    )
    # Formatting
    , .int_decimals.x = 0
    , .int_decimals.y = 0
    # Colors 
    , .scale_colors = pal_few 
    # Theme
    , .theme = ggthemes::theme_fivethirtyeight()
  )


# BAR CHARTS --------------------------------------------------------------
# Basic plot
diamonds %>% 
  fun_bar.plot(
    .mapping = aes(
      x = clarity
      , y = price
    )
    , .bar = T
    , .lollipop = F
  )

# Basic plot + labels + color etc
diamonds %>% 
  group_by(clarity, cut) %>% 
  summarise(price = mean(price)) %>% 
  ungroup() %>% 
  mutate(clarity = fct_reorder(
    clarity, price, .desc = T)
  ) %>% 
  fun_bar.plot(
    # Plot
    .mapping = aes(
      x = cut
      , y = price
      , fill = cut
      , color = cut
    )
    , .bar = T
    , .lollipop = F
    , .coord_flip = T
    # Facets
    , .sym_facets = clarity
    , .int_facets = 4
    # Labels
    , .list_labs = list(
      title = 'Diamond prices vs clarity'
      , subtitle = 'What is the impact of diamond clarity on prices?'
      , x = 'Quality of cut'
      , y = 'Price (USD)'
    )
    # Legends
    , .list_legend = list(fill = 'none', color = 'none')
    # Formatting
    , .chr_format.y = '$'
    , .int_decimals.y = 0
    # Theme
    , .theme = ggthemes::theme_gdocs()
  )

# LOLLIPOP CHARTS ---------------------------------------------------------
# Basic plot + labels
diamonds %>% 
  group_by(clarity) %>% 
  summarise(price = mean(price)) %>% 
  ungroup() %>% 
  mutate(clarity = fct_reorder(
    clarity, price)
  ) %>% 
  fun_bar.plot(
    # Plot
    .mapping = aes(
      x = clarity
      , xend = clarity
      , y = price
      , yend = price
      , color = clarity
    )
    , .bar = F
    , .lollipop = T
    , .coord_flip = T
    # Labels
    , .list_labs = list(
      title = 'Diamond prices vs clarity'
      , subtitle = 'What is the impact of diamond clarity on prices?'
      , x = 'Clarity'
      , y = 'Price (USD)'
    )
    # Legends
    , .list_legend = list(fill = 'none')
    # Formatting
    , .chr_format.y = '$'
    , .int_decimals.y = 2
    # Theme
    , .theme = ggthemes::theme_gdocs()
  )

