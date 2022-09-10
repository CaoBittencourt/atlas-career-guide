# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'ggridges', 'gghighlight', 'scales' #, 'paletteer' #Data visualization
  # , 'ggalt', 'hrbrthemes', 'extrafont' #Data visualization
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
  if(!(!length(.int_facets) | is.numeric(.int_facets))){
    
    stop("'.int_facets' must be an integer.")
    
  }
  
  # Convert to integers
  if(is.numeric(.int_facets)){
    
    round(.int_facets) -> .int_facets
    
  }
  
  # Number of facets
  .enq_facets %>% 
    quo_get_expr() %>% 
    length() -> int_n.args
  
  if(int_n.args > 1){
    
    int_n.args - 1 -> int_n.args
    
  }
  
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
  
  
  # Output
  return(plt_facets)
  
}

# # [TO DO] DYNAMIC LABELS (GEOM_TEXT) ----------------------------------------------------------
# fun_labels <- function(
    #     
#   # Labeling variables quosure
#   .enq_labels = NULL
#   # Spacing between geom and label
#   , .dbl_spacing = .22
#   # Flipped coordinates
#   , .coord_flip = T
#   
# ){
#   
#   # Quosure
#   if(!is_quosure(.enq_labels)){
#     
#     stop("'.enq_labels' must be a quosure.")
#     
#   }
#   
#   # Logical
#   if(!(
#     is.logical(.coord_flip) & 
#     !is.na(.coord_flip)
#   )){
#     
#     stop("'.coord_flip' must be either TRUE or FALSE.")
#     
#   }
#   
#   # Numeric
#   if(!is.numeric(.dbl_spacing)){
#     
#     stop("'.dbl_spacing' must be an integer.")
#     
#   }
#   
#   # Number of labeling variables
#   .enq_facets %>% 
#     quo_get_expr() %>% 
#     length() - 1 -> int_n.labels
#   
#   # Max labels = 1
#   if(int_n.args == 1){
#     
#     # Labels if coordinates are flipped
#     if(.coord_flip){
#       
#       geom_text(
#         aes(label = !!.enq_labels)
#         , hjust = .dbl_spacing
#       ) -> plt_text
#     
#     } else { 
#       
#       # Labels if coordinates are not flipped
#       
#       
#       }
#     
#   } else {
#     
#     NULL -> plt_text
#     
#   }
#   
# 
#  # Output
#    return(plt_text)
# 
# }

# [DONE] DYNAMIC GEOM (DEFAULT PARAMETERS) ----------------------------------------------------
fun_geom.params <- function(
    
  # Geom function
  .fun_geom = NULL
  # Default parameters (no mapping)
  , .list_default = list()
  # Aes to override default parameters (mapping)
  , .aes_mapping = NULL
  
){
  
  # Function
  if(!(
    !length(.fun_geom)
    | 'function' %in% tolower(class(.fun_geom))
  )){
    
    stop(".fun_geom must be a function that generates a ggproto geom object.")
    
  }
  
  # Geom Function
  if(!('layerinstance' %in% tolower(class(do.call(.fun_geom, args = list()))))){
    
    stop(".fun_geom must be a function that generates a ggproto geom object.")
    
  }
  
  # List
  if(!(!length(.list_default) | is.list(.list_default))){
    
    stop("'.list_default' must be a list with the default parameters for .fun_geom.")
    
  }
  
  # Aes
  if(!(!length(.aes_mapping) | 'uneval' %in% tolower(class(.aes_mapping)))){
    
    stop("'.aes_mapping' must be an aes() mapping.")
    
  }
  
  # Default parameters (any non-null parameters in .list_default)
  compact(.list_default) -> .list_default
  
  # Only one default parameter per variable
  map(.list_default, 1) -> .list_default
  
  # Override default parameters with aes mapping
  .list_default[
    setdiff(
      names(.list_default)
      , names(.aes_mapping))
  ] -> .list_default
  
  
  # Geom with default parameters
  do.call(
    .fun_geom
    , .list_default
  ) -> plt_geom
  
  
  # Output
  return(plt_geom)
  
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
    !length(.scale_color)
    | 'scale' %in% tolower(class(.scale_color))
    | (
      'list' %in% tolower(class(.scale_color)) &
      all(
        sapply(.scale_color, function(scale){
          
          'scale' %in% tolower(class(scale))
          
        })
      ))
  )){
    
    stop("'.scale_color' must be a ggproto scale object or a list of ggproto scale objects.")
    
  }
  
  # Character
  if(!(!length(.chr_manual.pal) | is.character(.chr_manual.pal))){
    
    stop("'.chr_manual.pal' must be a character vector.")
    
  }
  
  if(!(
    !length(.chr_manual.aes)
    | is.character(.chr_manual.aes)
    | !length(intersect(
      tolower(.chr_manual.aes)
      , c('fill', 'color', 'colour')
    ))
  )){
    
    stop("'.chr_manual.aes' must be a character vector containing 'fill' and/or 'color'.")
    
  }
  
  
  # Manual palette
  if(!!length(.chr_manual.pal)){
    
    scale_fill_manual(
      values = .chr_manual.pal
      , aesthetics = .chr_manual.aes
    ) -> plt_scale
    
    
  } else {
    
    .scale_color -> plt_scale
    
  }
  
  
  # Output
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
  if(!(!length(.chr_format.x) | is.character(.chr_format.x))){
    
    stop("'.chr_format.x' must be a character.")
    
  }
  
  if(!(!length(.chr_format.y) | is.character(.chr_format.y))){
    
    stop("'.chr_format.y' must be a character.")
    
  }
  
  # Numeric
  if(!(!length(.int_breaks.x) | is.numeric(.int_breaks.x))){
    
    stop("'.int_breaks.x' must be an integer.")
    
  }
  
  if(!(!length(.int_breaks.y) | is.numeric(.int_breaks.y))){
    
    stop("'.int_breaks.y' must be an integer.")
    
  }
  
  if(!(!length(.int_decimals.x) | is.numeric(.int_decimals.x))){
    
    stop("'.int_decimals.x' must be an integer.")
    
  }
  
  if(!(!length(.int_decimals.y) | is.numeric(.int_decimals.y))){
    
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
  if(!length(.chr_format.x)){
    
    scale_x_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) %in% 'percent' | tolower(.chr_format.x) %in% '%'){
    
    scale_x_continuous(
      labels = scales::percent_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) %in% 'usd' | tolower(.chr_format.x) %in% '$'){
    
    scale_x_continuous(
      breaks = scales::breaks_extended(n = .int_breaks.x)
      , labels = scales::label_dollar(
        accuracy = dbl_accuracy.x
        , prefix = '$'
        , big.mark = ','
        , decimal.mark = '.'
      )
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) %in% 'brl' | tolower(.chr_format.x) %in% 'r$'){
    
    scale_x_continuous(
      breaks = scales::breaks_extended(n = .int_breaks.x)
      , labels = scales::label_dollar(
        accuracy = dbl_accuracy.x
        , prefix = 'R$'
        , big.mark = '.'
        , decimal.mark = ','
      )
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) %in% 'text' | tolower(.chr_format.x) %in% 'text'){
    
    NULL -> plt_axis.x
    
  } else {
    
    scale_x_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
    ) -> plt_axis.x
    
  }
  
  
  # Y axis labels
  if(!length(.chr_format.y)){
    
    scale_y_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'percent' | tolower(.chr_format.y) %in% '%'){
    
    scale_y_continuous(
      labels = scales::percent_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'usd' | tolower(.chr_format.y) %in% '$'){
    
    scale_y_continuous(
      labels = scales::label_dollar(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'brl' | tolower(.chr_format.y) %in% 'R$'){
    
    scale_y_continuous(
      labels = scales::label_dollar(accuracy = dbl_accuracy.y, prefix = 'R$')
      , breaks = scales::breaks_extended(n = .int_breaks.y)
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'text' | tolower(.chr_format.y) %in% 'text'){
    
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

# [TO DO] DYNAMIC COORDINATES (GENERAL) -----------------------------------
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
      
      if(!(!length(args) | is.character(args))){
        
        stop("'.list_legend' must be a named list of character elements.")
        
      }
      
    }
  )
  
  
  # Guides
  guides(!!!.list_legend) -> plt_guides
  
  
  # Output
  return(plt_guides)
  
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
      
      if(!(!length(args) | is.character(args))){
        
        stop("'.list_labs' must be a named list of character elements.")
        
      }
      
    }
  )
  
  
  # Labs
  labs(!!!.list_labs) -> plt_labs  
  
  
  # Output
  return(plt_labs)
  
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

# [DONE] DYNAMIC MAPPING -----------------------------------------------------------
fun_aes.map <- function(.aes_mapping){
  
  # Aes
  if(!(!length(.aes_mapping) | 'uneval' %in% tolower(class(.aes_mapping)))){
    
    stop("'.aes_mapping' must be an aes() mapping.")
    
  }
  
  
  # Output
  return(.aes_mapping)
}

# ------- PLOTS -----------------------------------------------------------
# HISTOGRAM FUNCTION --------------------------------------------
fun_plot.histogram <- function(
    
  # Data
  .df_data
  , .mapping
  
  # Labels
  , .list_labs = list(NULL)
  
  # Geom default parameters
  , .list_geom.param = list(
    fill = '#3854FB'
    , bins = 30
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  
  # Color Mapping
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T)
    , viridis::scale_fill_viridis(discrete = T)
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.x = 5
  , .int_breaks.y = 5
  , .int_decimals.x = 2
  , .int_decimals.y = 0
  , .chr_format.x = NULL
  , .chr_format.y = NULL
  
  # Theme
  , .theme = ggridges::theme_ridges()
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Color mapping
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
  
  
  # Histogram plot
  # geom_histogram with default parameters
  fun_geom.params(
    .fun_geom = geom_histogram
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot(aes_mapping) +
    plt_geom + 
    plt_facets +
    # Colors
    plt_colors + 
    # Axis
    list_axis.format +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_histogram
  
  
  # Output
  return(plt_histogram)
  
}

# DENSITY FUNCTION --------------------------------------------
fun_plot.density <- function(
    
  # Data
  .df_data
  , .mapping
  
  # Labels
  , .list_labs = list(NULL)
  
  # Geom default parameters
  , .list_geom.param = list(
    color = '#212121'
    , fill = '#3854FB'
    , alpha = 0.8
    , size = 1.22
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  
  # Color Mapping
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T)
    , viridis::scale_fill_viridis(discrete = T)
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.x = 5
  , .int_breaks.y = 5
  , .int_decimals.x = 2
  , .int_decimals.y = 4
  , .chr_format.x = NULL
  , .chr_format.y = NULL
  
  # Theme
  , .theme = ggridges::theme_ridges()
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Color mapping
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
  # geom_density with default parameters
  fun_geom.params(
    .fun_geom = geom_density
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot(aes_mapping) +
    plt_geom + 
    plt_facets +
    # Colors
    plt_colors + 
    # Axis
    list_axis.format +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_density
  
  
  # Output
  return(plt_density)
  
}

# BAR CHART FUNCTION --------------------------------------------
fun_plot.bar <- function(
    
  # Data
  .df_data
  , .mapping
  
  # Labels
  , .list_labs = list(NULL)
  
  # Geom default parameters
  , .list_geom.param = list(
    fill = '#3854FB'
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T)
    , viridis::scale_fill_viridis(discrete = T)
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.y = 5
  , .int_decimals.y = 2
  , .chr_format.y = NULL
  , .coord_flip = T
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges()
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Color mapping
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
  # geom_col with default parameters
  fun_geom.params(
    .fun_geom = geom_col
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot(aes_mapping) +
    plt_geom + 
    plt_facets +
    # Colors
    plt_colors + 
    # Axis
    list_axis.format +
    # Coordinates
    plt_coord + 
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_bar
  
  
  # Output
  return(plt_bar)
  
}

# LOLLIPOP CHART FUNCTION --------------------------------------------
fun_plot.lollipop <- function(
    
  # Data
  .df_data
  , .mapping
  
  # Labels
  , .list_labs = list(NULL)
  
  # Geom default parameters
  , .list_geom.param = list(
    color = '#3854FB'
    , size = 5
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T)
    , viridis::scale_fill_viridis(discrete = T)
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.y = 5
  , .int_decimals.y = 2
  , .chr_format.y = NULL
  , .coord_flip = T
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges()
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Color mapping
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
  
  
  # Lollipop chart
  # geom_point with default parameters
  fun_geom.params(
    .fun_geom = geom_point
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # geom_segment with hard-set parameters
  geom_segment(
    y = 0
    , size = 1.21
    , color = '#212121'
  ) -> plt_segment
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot(aes_mapping) +
    plt_segment + 
    plt_geom + 
    plt_facets +
    # Colors
    plt_colors + 
    # Axis
    list_axis.format +
    # Coordinates
    plt_coord + 
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_lollipop
  
  
  # Output
  return(plt_lollipop)
  
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
    , .list_geom.param = list()
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
    , .sym_facets = c(cut, clarity)
  )


diamonds %>% 
  fun_plot.histogram(
    .mapping = aes(
      x = price
      , fill = clarity
    )
    # , .list_geom.param = list(
    #   bins = 30
    #   , fill = '#3854FB'
    # )
    # , .sym_facets = clarity
    # , .int_facets = 4
  )
  


aes.test1 <- aes(x = price, fill = clarity)
aes.test2 <- aes(x = price)

diamonds %>% 
  ggplot(aes.test1) -> dsds
  # ggplot(aes.test2) -> dsds

fun_geom.params(
  .fun_geom = geom_histogram
  , .list_default = list(
    fill = '#3854FB'
    , bins = 30
  )
  , .aes_mapping = aes.test1
  # , .aes_mapping = aes.test2
) -> lalala

dsds + lalala + 
  list(
    scale_x_continuous(labels = dollar_format())
    , scale_y_continuous(labels = percent_format())
  )

fun_aes.map

diamonds %>%
  group_by(clarity, cut) %>%
  summarise(price = mean(price)) %>%
  ungroup() -> dsds

diamonds %>% 
  fun_plot.bar(
    .mapping = aes(
      x = cut
      , y = price
      , fill = clarity
    )
    , 
  )


diamonds %>% 
  group_by(cut) %>%
  summarise(
    price = mean(price)
  ) %>%
  fun_plot.lollipop(
    aes(
      x = fct_reorder(cut, price)
      , xend = fct_reorder(cut, price)
      , y = price
      , yend = price
    )
  )
  # ggplot(aes(
  #   x = cut
  #   , xend = cut
  #   , y = price
  #   , yend = price
  # )) + 
  # geom_segment(y = 0, size = 1.22) + 
  # geom_point(size = 5.4)


tibble(
  x = paste0('x',seq(1,50))
  , y = rnorm(50, 12.8, 1.5)
) %>% 
  mutate(
    x = fct_reorder(x, y)
  ) %>%
  ggplot(aes(
    x = x
    , xend = x
    , y = y
    , yend = y
  )) +
  
  fun_plot.lollipop(
    aes(
      x = x
      , xend = x
      , y = y
      , yend = y
    )
  )
