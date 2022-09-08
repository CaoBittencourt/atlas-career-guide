# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'ggridges', 'gghighlight', 'scales'#, 'paletteer' #Data visualization
  # , 'hrbrthemes', 'extrafont' #Data visualization
  # , 'ggthemr' #Data visualization
  , 'tidyverse', 'rlang' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# # [TO DO] FONTS -------------------------------------------------------------------
# font_import(prompt = F)
# loadfonts(device = 'win')
# hrbrthemes::
# hrbrthemes::import_roboto_condensed()

# ------- PLOT ELEMENTS ---------------------------------------------------
# [DONE] DYNAMIC FACETS ----------------------------------------------------------
fun_facets <- function(
    
  # Facetting variables quosure
  .enq_facets = NULL
  # Number of columns in facet_wrap
  , .int_facets = NULL
  
){
  
  # Quosure
  if(!is_quosure(.enq_facets)){
    
    stop("'.enq_facets' must be a quosure.")
    
  }
  
  # Numeric
  if(!(is.null(.int_facets) | is.numeric(.int_facets))){
    
    stop("'.int_facets' must be an integer.")
    
  }
  
  # Convert to integers
  if(is.numeric(.int_facets)){
    
    round(.int_facets) -> .int_facets
    
  }
  
  # Number of facets
  .enq_facets %>% 
    quo_get_expr() %>% 
    length() - 1 -> int_n.args
  
  # Max facets = 2
  if(int_n.args >= 2){
    
    # For facets = 2 => facet_grid
    facet_grid(
      rows = vars(!!call_args(.enq_facets)[[1]])
      , cols = vars(!!call_args(.enq_facets)[[2]])
    ) -> plt_facets
    
  } else if(int_n.args == 1){
    
    # For facets = 1 => facet_wrap
    facet_wrap(
      facets = vars(!!.enq_facets)
      , ncol = .int_facets
    ) -> plt_facets
    
  } else {
    
    NULL -> plt_facets
    
  }
  
  return(plt_facets)
  
}

# [DONE] DYNAMIC COLORS ---------------------------------------------------
fun_colors <- function(
    
  # External scale
  .scale_color = NULL
  # Manual scale
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'color'
  
){
  
  # Scale
  if(!(
    is.null(.scale_color)
    | 'scale' %in% tolower(class(.scale_color))
    | (
      'list' %in% class(.scale_color) & 
      all(
        sapply(.scale_color, function(scale){
          
          'scale' %in% tolower(class(scale))
          
        })
      ))
  )){
    
    stop("'.scale_color' must be a ggproto scale object or a list of ggproto scale objects.")
    
  }
  
  # Character
  if(!(is.null(.chr_manual.pal) | is.character(.chr_manual.pal))){
    
    stop("'.chr_manual.pal' must be a character vector.")
    
  }
  
  if(!(
    is.null(.chr_manual.aes)
    | is.character(.chr_manual.aes)
    | length(intersect(
      tolower(.chr_manual.aes)
      , c('fill', 'color', 'colour')
    )) == 0
  )){
    
    stop("'.chr_manual.aes' must be a character vector containing 'fill' and/or 'color'.")
    
  }
  
  
  # Manual palette
  if(!is.null(.chr_manual.pal)){
    
    scale_fill_manual(
      values = .chr_manual.pal
      , aesthetics = .chr_manual.aes
    ) -> plt_scale
    
    
  } else {
    
    .scale_color -> plt_scale
    
  }
  
  
  # Scale
  return(plt_scale)
  
}

# [TO DO] DYNAMIC HIGHLIGHT -------------------------------------------------------

# [DONE] DYNAMIC AXIS ----------------------------------------------------------
fun_axis.format <- function(
    
  # Number of breaks
  .int_breaks.x = 5
  , .int_breaks.y = 5
  # Decimal places
  , .int_decimals.x = 2
  , .int_decimals.y = 2
  # Format
  , .chr_format.x = NULL
  , .chr_format.y = NULL
  
){
  
  # Character
  if(!(is.null(.chr_format.x) | is.character(.chr_format.x))){
    
    stop("'.chr_format.x' must be a character.")
    
  }
  
  if(!(is.null(.chr_format.y) | is.character(.chr_format.y))){
    
    stop("'.chr_format.y' must be a character.")
    
  }
  
  # Numeric
  if(!(is.null(.int_breaks.x) | is.numeric(.int_breaks.x))){
    
    stop("'.int_breaks.x' must be an integer.")
    
  }
  
  if(!(is.null(.int_breaks.y) | is.numeric(.int_breaks.y))){
    
    stop("'.int_breaks.y' must be an integer.")
    
  }
  
  if(!(is.null(.int_decimals.x) | is.numeric(.int_decimals.x))){
    
    stop("'.int_decimals.x' must be an integer.")
    
  }
  
  if(!(is.null(.int_decimals.y) | is.numeric(.int_decimals.y))){
    
    stop("'.int_decimals.y' must be an integer.")
    
  }
  
  # Convert to integers
  if(is.numeric(.int_breaks.x)){
    
    round(.int_breaks.x) -> .int_breaks.x
    
  }
  
  if(is.numeric(.int_breaks.y)){
    
    round(.int_breaks.y) -> .int_breaks.y
    
  }
  
  if(is.numeric(.int_decimals.x)){
    
    round(.int_decimals.x) -> .int_decimals.x
    
  }
  
  if(is.numeric(.int_decimals.y)){
    
    round(.int_decimals.y) -> .int_decimals.y
    
  }
  
  # Accuracy
  if(is.numeric(.int_decimals.x)){
    
    10 ^ (- .int_decimals.x) -> dbl_accuracy.x
    
  } else {
    
    NULL -> dbl_accuracy.x
    
  }
  
  if(is.numeric(.int_decimals.y)){
    
    10 ^ (- .int_decimals.y) -> dbl_accuracy.y
    
  } else {
    
    NULL -> dbl_accuracy.y
    
  }
  
  
  # X axis labels
  if(is.null(.chr_format.x)){
    
    scale_x_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) == 'percent' | tolower(.chr_format.x) == '%'){
    
    scale_x_continuous(
      labels = scales::percent_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) == 'usd' | tolower(.chr_format.x) == '$'){
    
    scale_x_continuous(
      breaks = scales::breaks_extended(n = .int_breaks.x)
      , labels = scales::label_dollar(
        accuracy = dbl_accuracy.x
        , prefix = '$'
        , big.mark = ','
        , decimal.mark = '.'
      )
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) == 'brl' | tolower(.chr_format.x) == 'R$'){
    
    scale_x_continuous(
      breaks = scales::breaks_extended(n = .int_breaks.x)
      , labels = scales::label_dollar(
        accuracy = dbl_accuracy.x
        , prefix = 'R$'
        , big.mark = '.'
        , decimal.mark = ','
      )
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) == 'text' | tolower(.chr_format.x) == 'text'){
    
    NULL -> plt_axis.x
    
  } else {
    
    scale_x_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
    ) -> plt_axis.x
    
  }
  
  
  # Y axis labels
  if(is.null(.chr_format.y)){
    
    scale_y_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) == 'percent' | tolower(.chr_format.y) == '%'){
    
    scale_y_continuous(
      labels = scales::percent_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) == 'usd' | tolower(.chr_format.y) == '$'){
    
    scale_y_continuous(
      labels = scales::label_dollar(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) == 'brl' | tolower(.chr_format.y) == 'R$'){
    
    scale_y_continuous(
      labels = scales::label_dollar(accuracy = dbl_accuracy.y, prefix = 'R$')
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) == 'text' | tolower(.chr_format.y) == 'text'){
    
    NULL -> plt_axis.y
    
  } else {
    
    scale_y_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  }
  
  
  # Output
  return(
    list(
      'plt_format.x' = plt_axis.x
      , 'plt_format.y' = plt_axis.y
    ))
  
}

# [TO DO] DYNAMIC POLAR AXIS (CIRCULAR PLOTS) -----------------------------

# [DONE] DYNAMIC LEGENDS ----------------------------------------------------------
fun_legends <- function(.list_legend){
  
  # List
  if(!is.list(.list_legend)){
    
    stop("'.list_legend' must be a named list of logical elements.")
    
  }
  
  # Character
  lapply(
    .list_legend
    , function(args){
      
      if(!(is.null(args) | is.character(args))){
        
        stop("'.list_legend' must be a named list of character elements.")
        
      }
      
    }
  )
  
  guides(!!!.list_legend) %>% 
    return()
  
}

# [DONE] DYNAMIC LABS ------------------------------------------------------------
fun_labs <- function(.list_labs){
  
  # List
  if(!is.list(.list_labs)){
    
    stop("'.list_labs' must be a named list of character elements.")
    
  }
  
  # Character
  lapply(
    .list_labs
    , function(args){
      
      if(!(is.null(args) | is.character(args))){
        
        stop("'.list_labs' must be a named list of character elements.")
        
      }
      
    }
  )
  
  # Labs
  labs(!!!.list_labs) %>% 
    return()
  
}

# [DONE] DYNAMIC THEME -----------------------------------------------------------
fun_theme <- function(.theme){
  
  # Theme
  if(!is.theme(.theme)){
    
    stop("'.theme' must be a valid theme.")
    
  }
  
  
  # Output
  return(.theme)
}

# ------- PLOTS -----------------------------------------------------------
# HISTOGRAM / DENSITY FUNCTION --------------------------------------------
fun_dist.plot <- function(
    
  # Data
  .df_data
  , .mapping
  
  # Labels
  , .list_labs = list(NULL)
  
  # Plots
  , .density = T
  , .histogram = F
  , .int_bins = NULL
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T)
    , viridis::scale_fill_viridis(discrete = T)
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = c('fill','color')
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.x = 5
  , .int_breaks.y = 5
  , .int_decimals.x = 2
  , .int_decimals.y = 2
  , .chr_format.x = NULL
  , .chr_format.y = NULL
  
  # Theme
  , .theme = ggthemes::theme_few()
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  # Logical
  if(!is.logical(.density)){
    
    stop("'.density' must be either TRUE or FALSE.")
    
  }
  
  if(!is.logical(.histogram)){
    
    stop("'.histogram' must be either TRUE or FALSE.")
    
  }
  
  # Numeric
  if(!(is.null(.int_bins) | is.numeric(.int_bins))){
    
    stop("'.int_bins' must be an integer.")
    
  }
  
  
  # Convert to integers
  if(is.numeric(.int_bins)){
    
    round(.int_bins) -> .int_bins
    
  }
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Colors
  fun_colors(
    .scale_color = .scale_colors
    , .chr_manual.pal = .chr_manual.pal
    , .chr_manual.aes = .chr_manual.aes
  ) -> plt_colors
  
  # Axis format
  fun_axis.format(
    .int_breaks.x = .int_breaks.x
    , .int_breaks.y = .int_breaks.y
    , .int_decimals.x = .int_decimals.x
    , .int_decimals.y = .int_decimals.y
    , .chr_format.x = .chr_format.x
    , .chr_format.y = .chr_format.y
  ) -> list_axis.format
  
  # Theme
  fun_theme(.theme) -> plt_theme
  
  # Legend
  fun_legends(.list_legend) -> plt_legend
  
  # Labs
  fun_labs(.list_labs) -> plt_labs
  
  
  # Density plot
  if(.density){
    
    .df_data %>%
      # Plot
      ggplot(.mapping) +
      geom_density(size = 1.22) +
      plt_facets +
      # Colors
      plt_colors + 
      # Axis
      list_axis.format$plt_format.x +
      list_axis.format$plt_format.y +
      # Theme
      plt_theme +
      plt_legend +
      # Labels
      plt_labs -> plt_density
    
  } else {
    
    NULL -> plt_density
    
  }
  
  
  # Histogram plot
  if(.histogram){
    
    .df_data %>%
      # Plot
      ggplot(.mapping) +
      geom_histogram(bins = .int_bins) +
      plt_facets +
      # Colors
      plt_colors + 
      # Axis
      list_axis.format$plt_format.x +
      list_axis.format$plt_format.y +
      # Theme
      plt_theme +
      plt_legend +
      # Labels
      plt_labs -> plt_histogram
    
  } else {
    
    NULL -> plt_histogram
    
  }
  
  
  # Output
  return(
    compact(
      list(
        'density' = plt_density
        , 'histogram' = plt_histogram
      )))
  
}

# BAR / LOLLIPOP FUNCTION --------------------------------------------
fun_bar.plot <- function(
    
  # Data
  .df_data
  , .mapping
  
  # Labels
  , .list_labs = list(NULL)
  
  # Plots
  , .bar = F
  , .lollipop = T
  , .dbl_lollipop.size = 3
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T)
    , viridis::scale_fill_viridis(discrete = T)
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = c('fill','color')
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.y = 5
  , .int_decimals.y = 2
  , .chr_format.y = NULL
  , .coord_flip = T
  , .coord_polar = F
  
  # Theme
  , .theme = ggthemes::theme_few()
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  # Logical
  if(!is.logical(.bar)){
    
    stop("'.bar' must be either TRUE or FALSE.")
    
  }
  
  if(!is.logical(.lollipop)){
    
    stop("'.lollipop' must be either TRUE or FALSE.")
    
  }
  
  # Numeric
  if(!(is.null(.dbl_lollipop.size) | is.numeric(.dbl_lollipop.size))){
    
    stop("'.dbl_lollipop.size' must be an integer.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Colors
  fun_colors(
    .scale_color = .scale_colors
    , .chr_manual.pal = .chr_manual.pal
    , .chr_manual.aes = .chr_manual.aes
  ) -> plt_colors
  
  # Axis format
  fun_axis.format(
    # X axis is categorical
    .chr_format.x = 'text'
    # Y axis is numerical
    , .int_breaks.y = .int_breaks.y
    , .int_decimals.y = .int_decimals.y
    , .chr_format.y = .chr_format.y
  ) -> list_axis.format
  
  # Coordinates
  if(.coord_flip){
    
    coord_flip() -> plt_coord
    
  } else { 
    
    NULL -> plt_coord
    
  }
  
  # Theme
  fun_theme(.theme) -> plt_theme
  
  # Legend
  fun_legends(.list_legend) -> plt_legend
  
  # Labs
  fun_labs(.list_labs) -> plt_labs
  
  
  # Bar chart
  if(.bar){
    
    .df_data %>%
      # Plot
      ggplot(.mapping) +
      geom_col() +
      plt_facets +
      # Colors
      plt_colors + 
      # Axis
      list_axis.format$plt_format.x +
      list_axis.format$plt_format.y +
      # Coordinates
      plt_coord + 
      # Theme
      plt_theme +
      plt_legend +
      # Labels
      plt_labs -> plt_bar
    
  } else {
    
    NULL -> plt_bar
    
  }
  
  
  # Lollipop chart
  if(.lollipop){
    
    .df_data %>%
      # Plot
      ggplot(.mapping) +
      # geom_segment(aes(x=group ,xend=group, y=0, yend=val), color="grey") +
      # geom_point(size=3, color="#69b3a2") +
      plt_facets +
      # Colors
      plt_colors + 
      # Axis
      list_axis.format$plt_format.x +
      list_axis.format$plt_format.y +
      # Coordinates
      plt_coord + 
      # Theme
      plt_theme +
      plt_legend +
      # Labels
      plt_labs -> plt_lollipop
    
  } else {
    
    NULL -> plt_lollipop
    
  }
  
  
  # Output
  return(
    compact(
      list(
        'bar' = plt_bar
        , 'lollipop' = plt_lollipop
      )))
  
}

# TEST --------------------------------------------------------------------
data("diamonds")

diamonds %>%
  fun_dist.plot(
    .mapping = aes(
      x = price
      # , color = color
      , fill = color
    )
    # , .sym_facets = c(clarity, color)
    , .theme = ggridges::theme_ridges()
    , .list_labs = list(
      title = 'Diamond Pricing'
      , subtitle = 'How diamond princing varies with clarity and color'
      , x = 'lalala'
      , y = 'Frequency'
    )
    , .scale_colors = list(
      scale_color_viridis_d(option = 'cividis')
      , scale_fill_viridis_d(option = 'cividis')
    )
    
    , .list_legend = list(fill = 'none')
    , .int_decimals.x = 0
    , .int_decimals.y = 4
    , .int_breaks.x = 3
    , .int_breaks.y = 4
    , .chr_format.y = NULL
    , .chr_format.x = 'brl'
    , .histogram = T
    , .density = F
  ) -> dsds

diamonds %>% 
  ggplot(aes(
    x = color
    , y = price
    # , fill = clarity
  )) + 
  geom_col() + 
  fun_axis.format(.chr_format.x = 'text')


diamonds %>% 
  fun_bar.plot(
    .mapping = aes(
      x = cut
      , y = price
      , fill = color
    )
    , .lollipop = F
    , .bar = T
    , .coord_flip = F
    , .list_labs = list(
      title = 'dsds'
      , subtitle = 'lalala'
      , x = 'Cut'
      , y = 'Price (USD)'
    )
    , .chr_format.y = 'usd'
    , .int_breaks.y = 15
    , .theme = ggridges::theme_ridges()
    # , .sym_facets = NULL
  )

