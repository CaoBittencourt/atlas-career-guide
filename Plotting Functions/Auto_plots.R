# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'ggridges', 'gghighlight', 'scales' , 'viridis', 'ggalt' #'paletteer' #Data visualization
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

# [DONE] DYNAMIC LABELS --------------------------------------------------
fun_labels <- function(
    
  # Aes containg labelling variables, etc (mapping)
  .aes_mapping = NULL
  # Whether to use geom_label instead of geom_text
  , .geom_label = F
  # Logical indicating if coordinates are flipped
  , .coord_flip = F 
  # Logical indicating if coordinates are circular
  , .coord_polar = F
  # Default parameters for labels (no mapping)
  , .list_default = list(
    position = c('dodge', 'stack', 'fill', 'identity')
    , vjust = -1.15
    , hjust = -0.15
    , size = 3.33
    , fontface = 'bold'
    , color = '#3854FB'
  )
  
){
  
  # Logical
  if(!(
    is.logical(.geom_label) &
    !is.na(.geom_label)
  )){
    
    stop("'.geom_label' must be either TRUE or FALSE.")
    
  }
  
  if(!(
    is.logical(.coord_flip) &
    !is.na(.coord_flip)
  )){
    
    stop("'.coord_flip' must be either TRUE or FALSE.")
    
  }
  
  if(!(
    is.logical(.coord_polar) &
    !is.na(.coord_polar)
  )){
    
    stop("'.coord_polar' must be either TRUE or FALSE.")
    
  }
  
  # List
  if(!(!length(.list_default) | is.list(.list_default))){
    
    stop("'.list_default' must be a list with the default labelling parameters.")
    
  }
  
  # Aes
  if(!(!length(.aes_mapping) | 'uneval' %in% tolower(class(.aes_mapping)))){
    
    stop("'.aes_mapping' must be an aes() mapping.")
    
  }
  
  
  # Default => labels = NULL
  NULL -> plt_labels
  
  # If label aes() available, generate labels
  if(length(.aes_mapping)){
    
    if('label' %in% tolower(names(.aes_mapping))){
      
      # Standardize names
      names(.list_default) <- standardise_aes_names(names(.list_default))
      
      names(.aes_mapping) <- standardise_aes_names(names(.aes_mapping))
      
      # Only one default parameter per variable
      map(.list_default, 1) -> .list_default
      
      # Override default parameters with aes mapping
      .list_default[
        setdiff(
          standardise_aes_names(names(.list_default))
          , standardise_aes_names(names(.aes_mapping))
        )] -> .list_default
      
      # Add label mapping
      .list_default$mapping <- .aes_mapping['label']
      
      chr_label.position <- tolower(.list_default$position)
      
      # Label position
      if('stack' == chr_label.position){
        # Position stack  
        .list_default$position <- position_stack(vjust = 0.5)
        
        .list_default$vjust <- NULL
        
        .list_default$hjust <- NULL
        
      } else if('fill' == chr_label.position){
        # Position fill
        .list_default$position <- position_fill(vjust = 0.5)
        
        .list_default$vjust <- NULL
        
        .list_default$hjust <- NULL
        
      } else if('dodge' == chr_label.position){
        
        .list_default$position <- position_dodge(width = 0.9)
        
      } else {
        
        .list_default$position <- position_identity()
        
      }
      
      
      # Label orientation
      if(.coord_flip){
        
        if('stack' == chr_label.position){
          # Position stack
          .list_default$angle <- 90
          
        } else if('fill' == chr_label.position){
          # Position fill
          .list_default$angle <- 90
          
        } else { 
          # If coord_flip, drop vjust. 
          .list_default$vjust <- 0.5
          
        } 
        
      } else { 
        # If coord_flip == F, drop hjust
        .list_default$hjust <- 0.5
        
      }
      
      
      # If coord_polar ...
      
      
      # Label
      if(.geom_label){
        # If geom_label, use geom_label
        do.call(
          geom_label
          , .list_default
        ) -> plt_labels
        
      } else {
        # Otherwise, use geom_text
        do.call(
          geom_text
          , .list_default
        ) -> plt_labels
        
      }
      
    } 
    
  }
  
  
  # Output
  return(plt_labels)
  
}

# [DONE] DYNAMIC GEOM ----------------------------------------------------
fun_geom.params <- function(
    
  # Geom function
  .fun_geom = NULL
  # Default parameters (no mapping)
  , .list_default = list()
  # Aes to override default parameters (mapping)
  , .aes_mapping = aes()
  
){
  
  # Function
  if(!(
    !length(.fun_geom)
    | 'function' %in% tolower(class(.fun_geom))
  )){
    
    stop("'.fun_geom' must be a function that generates a ggproto geom object.")
    
  }
  
  # Geom Function
  if(!('layerinstance' %in% tolower(class(do.call(.fun_geom, args = list()))))){
    
    stop("'.fun_geom' must be a function that generates a ggproto geom object.")
    
  }
  
  # List
  if(!(!length(.list_default) | is.list(.list_default))){
    
    stop("'.list_default' must be a list with the default parameters for '.fun_geom'.")
    
  }
  
  # Aes
  if(!(!length(.aes_mapping) | 'uneval' %in% tolower(class(.aes_mapping)))){
    
    stop("'.aes_mapping' must be an aes() mapping.")
    
  }
  
  # Standardize names
  names(.list_default) <- standardise_aes_names(names(.list_default)) 
  
  names(.aes_mapping) <- standardise_aes_names(names(.aes_mapping))
  
  # Default parameters (any non-null parameters in .list_default)
  compact(.list_default) -> .list_default
  
  # Only one default parameter per variable
  map(.list_default, 1) -> .list_default
  
  .list_default[
    setdiff(
      names(.list_default)
      , names(.aes_mapping)
    )] -> .list_default
  
  
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
    
  # Format
  .chr_format.x = NULL
  , .chr_format.y = NULL
  # Limits
  , .dbl_limits.x = NULL
  , .dbl_limits.y = NULL
  # Number of breaks
  , .int_breaks.x = 5
  , .int_breaks.y = 5
  # Decimal places
  , .int_decimals.x = 2
  , .int_decimals.y = 2
  
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
  
  # Numeric vector with length == 2
  if(!(
    !length(.dbl_limits.x)
    | (is.numeric(.dbl_limits.x)
       & length(.dbl_limits.x) == 2)
  )){
    
    stop("'.dbl_limits.x' must be numeric vector with length two.")
    
  }
  
  if(!(
    !length(.dbl_limits.y)
    | (is.numeric(.dbl_limits.y)
       & length(.dbl_limits.y) == 2)
  )){
    
    stop("'.dbl_limits.y' must be numeric vector with length two.")
    
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
      , limits = .dbl_limits.x
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) %in% 'percent' | tolower(.chr_format.x) %in% '%'){
    
    scale_x_continuous(
      labels = scales::percent_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
      , limits = .dbl_limits.x
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
      , limits = .dbl_limits.x
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
      , limits = .dbl_limits.x
    ) -> plt_axis.x
    
  } else if(tolower(.chr_format.x) %in% 'text' | tolower(.chr_format.x) %in% 'text'){
    
    scale_x_discrete(
      labels = function(x){str_wrap(x,10)}
    ) -> plt_axis.x
    
  } else {
    
    scale_x_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.x)
      , breaks = scales::breaks_extended(n = .int_breaks.x)
      , limits = .dbl_limits.x
    ) -> plt_axis.x
    
  }
  
  
  # Y axis labels
  if(!length(.chr_format.y)){
    
    scale_y_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
      , limits = .dbl_limits.y
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'percent' | tolower(.chr_format.y) %in% '%'){
    
    scale_y_continuous(
      labels = scales::percent_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
      , limits = .dbl_limits.y
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'usd' | tolower(.chr_format.y) %in% '$'){
    
    scale_y_continuous(
      labels = scales::label_dollar(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
      , limits = .dbl_limits.y
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'brl' | tolower(.chr_format.y) %in% 'R$'){
    
    scale_y_continuous(
      labels = scales::label_dollar(accuracy = dbl_accuracy.y, prefix = 'R$')
      , breaks = scales::breaks_extended(n = .int_breaks.y)
      , limits = .dbl_limits.y
    ) -> plt_axis.y
    
  } else if(tolower(.chr_format.y) %in% 'text' | tolower(.chr_format.y) %in% 'text'){
    
    scale_y_discrete(
      labels = function(y){str_wrap(y,10)}
    ) -> plt_axis.y
    
  } else {
    
    scale_y_continuous(
      labels = scales::number_format(accuracy = dbl_accuracy.y)
      , breaks = scales::breaks_extended(n = .int_breaks.y)
      , limits = .dbl_limits.y
    ) -> plt_axis.y
    
  }
  
  
  # Output
  return(
    list(
      'plt_format.x' = plt_axis.x
      , 'plt_format.y' = plt_axis.y
    ))
  
}

# [DONE] DYNAMIC COORDINATES -----------------------------------
fun_coordinates <- function(
    
  # xlim
  .dbl_limits.x = NULL
  # ylim
  , .dbl_limits.y = NULL
  # Coord_flip
  , .coord_flip = F
  # Coord_polar
  , .coord_polar = F
  
){
  
  # Numeric vector with length == 2
  if(!(
    !length(.dbl_limits.x)
    | (is.numeric(.dbl_limits.x)
       & length(.dbl_limits.x) == 2)
  )){
    
    stop("'.dbl_limits.x' must be numeric vector with length two.")
    
  }
  
  if(!(
    !length(.dbl_limits.y)
    | (is.numeric(.dbl_limits.y)
       & length(.dbl_limits.y) == 2)
  )){
    
    stop("'.dbl_limits.y' must be numeric vector with length two.")
    
  }
  
  # Logical
  if(!(
    is.logical(.coord_flip) &
    !is.na(.coord_flip)
  )){
    
    stop("'.coord_flip' must be either TRUE or FALSE.")
    
  }
  
  if(!(
    is.logical(.coord_polar) &
    !is.na(.coord_polar)
  )){
    
    stop("'.coord_polar' must be either TRUE or FALSE.")
    
  }
  
  
  # Coordinates
  if(.coord_flip){
    # If coord_flip, use coord_flip
    coord_flip(
      xlim = .dbl_limits.x
      , ylim = .dbl_limits.y
    ) -> plt_coord
    
  } else if(.coord_polar){
    # Else, if coord_polar, use coord_polar
    coord_polar() -> plt_coord
    
  } else {
    # Else, use coord_cartesian
    coord_cartesian(
      xlim = .dbl_limits.x
      , ylim = .dbl_limits.y
    ) -> plt_coord
    
  }
  
  
  # Output
  return(plt_coord)
  
}

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

# [DONE] DYNAMIC FACTOR REORDER -------------------------------------------
fun_reorder <- function(
    
  # Data
  .df_data = tibble()
  # Factor variable (quo)
  , .enq_var.fct = NULL
  # Reordering variable (quo)
  , .enq_var.dbl = NULL
  # Ordering function
  , .fun_ord = sum
  # Descending
  , .desc = F
  
){
  
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  # Factor / Character
  if(!(
    !length(.enq_var.fct)
    | .df_data %>% pull(!!.enq_var.fct) %>% is.character()
    | .df_data %>% pull(!!.enq_var.fct) %>% is.factor()
  )){
    
    stop("'.enq_var.fct' must be a quosure of a factor or character variable.")
    
  }
  
  # Numerical
  if(!(
    !length(.enq_var.dbl)
    | .df_data %>% pull(!!.enq_var.dbl) %>% is.numeric()
  )){
    
    stop("'.enq_var.dbl' must be a quosure of a numerical variable.")
    
  }
  
  # Function
  if(!(
    !length(.fun_ord)
    | 'function' %in% tolower(class(.fun_ord))
  )){
    
    stop("'.fun_ord' must be a function for ordering the factor variable.")
    
  }
  
  # Logical
  if(!(
    is.logical(.desc) &
    !is.na(.desc)
  )){
    
    stop("'.desc' must be either TRUE or FALSE.")
    
  }
  
  
  # Reorder factor variable
  # If any missing argument, return original data frame
  if(
    length(.df_data)
    & length(.enq_var.fct)
    & length(.enq_var.dbl)
    & length(.fun_ord)
    & length(.desc)
  ){
    
    .df_data %>% 
      ungroup() %>% 
      mutate(
        !!.enq_var.fct := 
          fct_reorder(
            !!.enq_var.fct
            , !!.enq_var.dbl
            , .desc = .desc
            , .fun = .fun_ord
          )
      ) -> .df_data
    
  } 
  
  
  # Output
  return(.df_data)
  
}

# ------- PLOTS -----------------------------------------------------------
# HISTOGRAM FUNCTION --------------------------------------------
fun_plot.histogram <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = sum
  
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
  , .dbl_limits.x = NULL
  , .dbl_limits.y = NULL
  
  # Theme
  , .theme = ggridges::theme_ridges(center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.aes_mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct & length(plt_facets)){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = enq_facets
      , .enq_var.dbl = aes_mapping$x
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
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
    , .dbl_limits.x = .dbl_limits.x
    , .dbl_limits.y = .dbl_limits.y
  ) -> list_axis.format
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.x = .dbl_limits.x
    , .dbl_limits.y = .dbl_limits.y
    , .coord_flip = F
    , .coord_polar = F
  ) -> plt_coord
  
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
    plt_coord + 
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
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = sum
  
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
  , .dbl_limits.x = NULL
  , .dbl_limits.y = NULL
  
  # Theme
  , .theme = ggridges::theme_ridges(center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.aes_mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct & length(plt_facets)){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = enq_facets
      , .enq_var.dbl = aes_mapping$x
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
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
    , .dbl_limits.x = .dbl_limits.x
    , .dbl_limits.y = .dbl_limits.y
  ) -> list_axis.format
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.x = .dbl_limits.x
    , .dbl_limits.y = .dbl_limits.y
    , .coord_flip = F
    , .coord_polar = F
  ) -> plt_coord
  
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
    plt_coord + 
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_density
  
  
  # Output
  return(plt_density)
  
}

# HEATMAP FUNCTION --------------------------------------------
fun_plot.heatmap <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = T
  , .reorder_fun = sum
  
  # Labels
  , .list_labs = list(NULL)
  , .geom_label = F
  
  # Default parameters
  , .list_geom.param = list(
    position = 'identity'
    , color = 'white'
    , size = 3.33
  )
  , .list_labels.param = list(
    position = 'identity'
    , fontface = 'bold'
    , color = 'white'
    , size = 3.33
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis()
    , viridis::scale_fill_viridis()
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.y = 5
  , .int_decimals.y = 2
  , .chr_format.y = NULL
  , .coord_flip = F
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges(center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.aes_mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = aes_mapping$x
      , .enq_var.dbl = aes_mapping$fill
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
  if(.reorder_fct & length(plt_facets)){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = enq_facets
      , .enq_var.dbl = aes_mapping$fill
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
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
    # X axis is categorical
    , .chr_format.y = 'text'
  ) -> list_axis.format
  
  # Coordinates
  fun_coordinates(
    .coord_flip = .coord_flip
    , .coord_polar = .coord_polar
  ) -> plt_coord
  
  # Theme
  fun_theme(.theme) -> plt_theme
  
  # Legend
  fun_legends(.list_legend) -> plt_legend
  
  # Labs
  fun_labs(.list_labs) -> plt_labs
  
  # Labels
  fun_labels(
    .list_default = .list_labels.param
    , .aes_mapping = aes_mapping
    , .coord_flip = .coord_flip
    , .geom_label = .geom_label
  ) -> plt_labels
  
  
  # Heat map
  # geom_tile with default parameters
  fun_geom.params(
    .fun_geom = geom_tile
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot(aes_mapping) +
    plt_geom + 
    plt_labels +
    plt_facets +
    # Colors
    plt_colors + 
    # Axis
    list_axis.format +
    plt_coord + 
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_heatmap
  
  
  # Output
  return(plt_heatmap)
  
}

# BAR CHART FUNCTION --------------------------------------------
fun_plot.bar <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = sum
  
  # Labels
  , .list_labs = list(NULL)
  , .geom_label = F
  
  # Default parameters
  , .list_geom.param = list(
    position = c('dodge', 'stack', 'fill', 'identity')
    , fill = '#3854FB'
  )
  , .list_labels.param = list(
    fontface = 'bold'
    , color = '#3854FB'
    , size = 3.33
    , vjust = -1.15
    , hjust = -0.15
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
  , .dbl_limits.y = NULL
  , .coord_flip = F
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges(center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.aes_mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = aes_mapping$x
      , .enq_var.dbl = aes_mapping$y
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
  if(.reorder_fct & length(plt_facets)){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = enq_facets
      , .enq_var.dbl = aes_mapping$y
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
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
    , .dbl_limits.y = .dbl_limits.y
  ) -> list_axis.format
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.y = .dbl_limits.y
    , .coord_flip = .coord_flip
    , .coord_polar = .coord_polar
  ) -> plt_coord
  
  # Theme
  fun_theme(.theme) -> plt_theme
  
  # Legend
  fun_legends(.list_legend) -> plt_legend
  
  # Labs
  fun_labs(.list_labs) -> plt_labs
  
  # Label position
  if(!length(.list_labels.param$position)){
    # Unless specified, use geom position as label position
    .list_labels.param$position <- .list_geom.param$position
    
  }
  
  # Labels
  fun_labels(
    .list_default = .list_labels.param
    , .aes_mapping = aes_mapping
    , .coord_flip = .coord_flip
    , .geom_label = .geom_label
  ) -> plt_labels
  
  
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
    plt_labels + 
    plt_facets +
    # Colors
    plt_colors + 
    # Axis
    list_axis.format +
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
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = sum
  
  # Labels
  , .list_labs = list(NULL)
  , .geom_label = F
  
  # Default parameters
  , .list_point.param = list(
    position = 'identity'
    , color = '#3854FB'
    , size = 5.4
  )
  , .list_segment.param = list(
    y = 0
    , alpha = 0.8
    , position = 'identity'
    , color = '#D4D5D8'
    , size = 2
  )
  , .list_labels.param = list(
    fontface = 'bold'
    , color = '#3854FB'
    , size = 3.33
    , vjust = -1.25
    , hjust = -0.25
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
  , .chr_manual.aes = 'color'
  
  # Legend
  , .list_legend = list(NULL)
  
  # Axis
  , .int_breaks.y = 5
  , .int_decimals.y = 2
  , .chr_format.y = NULL
  , .dbl_limits.y = NULL
  , .coord_flip = T
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges(center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(.aes_mapping) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = aes_mapping$x
      , .enq_var.dbl = aes_mapping$y
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
  if(.reorder_fct & length(plt_facets)){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = enq_facets
      , .enq_var.dbl = aes_mapping$y
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
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
    , .dbl_limits.y = .dbl_limits.y
  ) -> list_axis.format
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.y = .dbl_limits.y
    , .coord_flip = .coord_flip
    , .coord_polar = .coord_polar
  ) -> plt_coord
  
  # Theme
  fun_theme(.theme) -> plt_theme
  
  # Legend
  fun_legends(.list_legend) -> plt_legend
  
  # Labs
  fun_labs(.list_labs) -> plt_labs
  
  # Label position
  if(!length(.list_labels.param$position)){
    # Unless specified, use geom position as label position
    .list_labels.param$position <- .list_point.param$position
    
  }
  
  # Labels
  fun_labels(
    .list_default = .list_labels.param
    , .aes_mapping = aes_mapping
    , .coord_flip = .coord_flip
    , .geom_label = .geom_label
  ) -> plt_labels
  
  
  # Lollipop chart
  # geom_point with default parameters
  fun_geom.params(
    .fun_geom = geom_point
    , .list_default = .list_point.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # geom_segment with hard-set parameters
  fun_geom.params(
    .fun_geom = geom_segment
    , .list_default = .list_segment.param
    , .aes_mapping = aes()
  ) -> plt_segment
  
  # geom_segment(
  #   y = 0
  #   , size = 1.21
  #   , color = '#212121'
  #   , alpha = 0.5
  # ) -> plt_segment
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot(aes_mapping) +
    plt_segment + 
    plt_geom + 
    plt_labels +
    plt_facets +
    # Colors
    plt_colors + 
    # Axis
    list_axis.format +
    plt_coord + 
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_lollipop
  
  
  # Output
  return(plt_lollipop)
  
}

# # DUMBBELL CHART FUNCTION --------------------------------------------
# fun_plot.dumbbell <- function(
#     
#   # Data
#   .df_data
#   , .aes_mapping
#   
#   # Reorder
#   , .reorder_fct = T
#   , .reorder_desc = F
#   , .reorder_fun = max
#   
#   # Labels
#   , .list_labs = list(NULL)
#   , .geom_label = F
#   
#   # Default parameters
#   , .list_point1.param = list(
#     position = 'identity'
#     , color = '#182766'
#     , size = 5.4
#   )
#   , .list_point2.param = list(
#     position = 'identity'
#     , color = '#4AF7B0'
#     , size = 5.4
#   )
#   , .list_segment.param = list(
#     alpha = 0.8
#     , position = 'identity'
#     , color = '#D4D5D8'
#     , size = 2
#   )
#   , .list_labels1.param = list(
#     fontface = 'bold'
#     , color = '#182766'
#     , size = 3.33
#     , vjust = 2.22
#     , hjust = 0.5
#   )
#   , .list_labels2.param = list(
#     fontface = 'bold'
#     , color = '#4AF7B0'
#     , size = 3.33
#     , vjust = -1.5
#     , hjust = 0.5
#   )
#   
#   # Facets
#   , .sym_facets = NULL
#   , .int_facets = NULL
#   
#   # Legend
#   , .list_legend = list(NULL)
#   
#   # Axis
#   , .int_breaks.y = 5
#   , .int_decimals.y = 2
#   , .chr_format.y = NULL
#   , .dbl_limits.y = NULL
#   , .coord_flip = T
#   , .coord_polar = F
#   
#   # Theme
#   , .theme = ggridges::theme_ridges(center_axis_labels = T)
#   
# ){
#   
#   # Errors
#   # Data frame
#   if(!is.data.frame(.df_data)){
#     
#     stop("'.df_data' must be a data frame.")
#     
#   }
#   
#   
#   # Quo vars
#   enquo(.sym_facets) -> enq_facets
#   
#   
#   # Aes Mapping
#   fun_aes.map(.aes_mapping) -> aes_mapping
#   
#   aes() -> aes_mapping2
#   
#   aes(!!aes_mapping$yend) -> aes_mapping2$y
#   
#   # Facets
#   fun_facets(
#     .enq_facets = enq_facets
#     , .int_facets = .int_facets
#   ) -> plt_facets
#   
#   # Reordering
#   if(.reorder_fct){
#     
#     fun_reorder(
#       .df_data = .df_data
#       , .enq_var.fct = aes_mapping$x
#       , .enq_var.dbl = aes_mapping$y
#       , .fun_ord = .reorder_fun
#       , .desc = .reorder_desc
#     ) -> .df_data
#     
#   }
#   
#   if(.reorder_fct & length(plt_facets)){
#     
#     fun_reorder(
#       .df_data = .df_data
#       , .enq_var.fct = enq_facets
#       , .enq_var.dbl = aes_mapping$y
#       , .fun_ord = .reorder_fun
#       , .desc = .reorder_desc
#     ) -> .df_data
#     
#   }
#   
#   # Axis format
#   fun_axis.format(
#     # X axis is categorical
#     .chr_format.x = 'text'
#     # Y axis is numerical
#     , .int_breaks.y = .int_breaks.y
#     , .int_decimals.y = .int_decimals.y
#     , .chr_format.y = .chr_format.y
#     , .dbl_limits.y = .dbl_limits.y
#   ) -> list_axis.format
#   
#   # Coordinates
#   fun_coordinates(
#     .dbl_limits.y = .dbl_limits.y
#     , .coord_flip = .coord_flip
#     , .coord_polar = .coord_polar
#   ) -> plt_coord
#   
#   # Theme
#   fun_theme(.theme) -> plt_theme
#   
#   # Legend
#   fun_legends(.list_legend) -> plt_legend
#   
#   # Labs
#   fun_labs(.list_labs) -> plt_labs
#   
#   # Label position
#   if(!length(.list_labels1.param$position)){
#     # Unless specified, use geom position as label position
#     .list_labels1.param$position <- .list_point1.param$position
#     
#   }
#   
#   if(!length(.list_labels2.param$position)){
#     # Unless specified, use geom position as label position
#     .list_labels2.param$position <- .list_point2.param$position
#     
#   }
#   
#   # Labels
#   .list_labels1.param$mapping <- aes_mapping2
#   
#   fun_labels(
#     .list_default = .list_labels1.param
#     , .aes_mapping = aes()
#     , .coord_flip = .coord_flip
#     , .geom_label = .geom_label
#   ) -> plt_labels1
#   
#   
#   .list_labels2.param$mapping <- aes_mapping2
#   
#   fun_labels(
#     .list_default = .list_labels2.param
#     , .aes_mapping = aes()
#     , .coord_flip = .coord_flip
#     , .geom_label = .geom_label
#   ) -> plt_labels2
#   
#   
#   # Dumbbell chart
#   # geom_point 1 with default parameters
#   fun_geom.params(
#     .fun_geom = geom_point
#     , .list_default = .list_point1.param
#     # , .aes_mapping = aes_mapping['y']
#   ) -> plt_geom1
#   
#   # geom_point 2 with default parameters
#   .list_point2.param$mapping <- aes(!!aes_mapping['yend'])
#   
#   fun_geom.params(
#     .fun_geom = geom_point
#     , .list_default = .list_point2.param
#     # , .aes_mapping = aes_mapping['yend']
#   ) -> plt_geom2
#   
#   # geom_segment with hard-set parameters
#   # geom_segment with hard-set parameters
#   fun_geom.params(
#     .fun_geom = geom_segment
#     , .list_default = .list_segment.param
#     , .aes_mapping = aes()
#   ) -> plt_segment
#   
#   # ggplot
#   .df_data %>%
#     # Plot
#     ggplot(aes_mapping) +
#     plt_segment +
#     plt_geom1 +
#     plt_geom2 +
#     plt_labels1 +
#     plt_labels2 +
#     plt_facets +
#     # Axis
#     list_axis.format +
#     plt_coord +
#     # Theme
#     plt_theme +
#     plt_legend +
#     # Labels
#     plt_labs -> plt_dumbbell
#   
#   
#   # Output
#   return(plt_dumbbell)
#   
# }

# TEST --------------------------------------------------------------------
data("diamonds")

tibble(
  a = LETTERS[1:5]
  , b = runif(5, 0, 100)
  , c = runif(5, 25, 50)
  # , d = runif(5, 0, 80)
) %>%
  mutate(
    a = fct_reorder(a, b)
  ) %>%
  fun_plot.dumbbell(aes(
    x = a
    , xend = a
    , y = c
    , yend = b
  ))
  
  
  ggplot(aes(
    x = b
    , xend = c
    , y = a
  )) + 
  geom_dumbbell(
    color = 'lightgrey'
    , colour_x = viridis(4)[3]
    , colour_xend = viridis(4)[2]
    , size_x = 5.4
    , size_xend = 5.4
    , size = 2
  ) +
  theme_ridges(center_axis_labels = T) + 
  geom_text(aes(label = number(b, accuracy = .01)
                , x = b)
            , hjust = 1.5
            # , vjust = -1.5
            , color = viridis(4)[3]
            , fontface = 'bold'
  ) +
  geom_text(aes(label = number(c, accuracy = .01)
                , x = c)
            , hjust = -.5
            # , vjust = 1.5
            , color = viridis(4)[2]
            , fontface = 'bold'
  )


tibble(
  a = LETTERS[1:10]
  , b = runif(10, 0, 100)
  , c = runif(10, 0, 100)
  # , d = runif(5, 0, 80)
) %>%
  mutate(
    a = fct_reorder(a, b)
  ) %>%
  ggplot(aes(
    x = a
    , xend = a
    , y = b
    , yend = c
  )) + 
  geom_segment(
    color = 'lightgrey'
    , size = 3
    , alpha = 0.5
  ) +
  geom_point(aes(y = b), size = 5.4, color = viridis(4)[2]) +
  geom_point(aes(y = c), size = 5.4, color = viridis(4)[3]) +
  # geom_point(aes(y = d), size = 5.4, color = viridis(4)[4]) +
  coord_flip() +
  theme_ridges(center_axis_labels = T) + 
  geom_text(aes(label = number(b, accuracy = .01)
                , y = b
  )
  , color = viridis(4)[2]
  , fontface = 'bold'
  , vjust = -1.5) +
  geom_text(aes(label = number(c, accuracy = .01)
                , y = c)
            , color = viridis(4)[3]
            , fontface = 'bold'
            , vjust = 2.22)



diamonds %>% 
  group_by(clarity) %>% 
  summarise(price = mean(price)) %>% 
  ungroup() %>% 
  mutate(
    clarity = fct_reorder(clarity, price, .fun = sum, .desc = F)
  ) %>% 
  fun_plot.lollipop(aes(
    x = clarity
    , xend = clarity
    , y = price
    , yend = price
    , label = dollar(price, accuracy = .01)
    , color = clarity
  )
  , .coord_flip = T
  )



fun_plot.heatmap(
  aes(
    x = cut
    , y = as.numeric(clarity)
    , fill = price
    # , label = dollar(price, accuracy = .01)
  )
  , .coord_polar = F
) + 
  ylim(c(-8/4,8+1)) +
  # ylim(c(0, max(as.numeric(diamonds$clarity) + 15) + 0.5)) +
  # scale_y_discrete(
  #   breaks = seq_along(levels(diamonds$clarity)) + 15
  #   , labels = levels(diamonds$clarity)
  #   ) + 
  annotate(
    x = ''
    , y = 1:8
    , label = levels(diamonds$clarity)
    , size = 3.33
    , geom = 'text'
  ) +
  coord_polar(start = -0.5)



diamonds %>% 
  group_by(cut, clarity) %>% 
  summarise(price = mean(price)) %>% 
  ungroup() %>% 
  mutate(
    cut = fct_reorder(cut, price, .fun = sum, .desc = T)
    , clarity = fct_reorder(clarity, price, .fun = sum, .desc = F)
  ) %>% 
  ggplot(aes(
    x = cut
    , y = clarity
    , fill = price
    , label = dollar(price, accuracy = .01)
  )) + 
  geom_tile(
    position = position_identity()
    , color = 'white'
    , size = 3.33
  ) + 
  geom_text(
    position = position_identity()
    , fontface = 'bold'
    , color = 'white'
    , size = 3.33
  ) +
  scale_fill_viridis() + 
  theme_ridges(center_axis_labels = T) + 
  coord_polar()

diamonds %>% 
  pivot_longer(
    cols = c(x,y,z,table,carat,depth)
  ) %>%
  group_by(cut, name) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  mutate(
    cut = fct_reorder(cut, value, .fun = sum, .desc = T)
    , name = fct_reorder(name, value, .fun = sum, .desc = F)
  ) %>%
  ggplot(aes(
    x = cut
    , y = name
    , fill = value
    , label = number(value, accuracy = .01)
  )) + 
  geom_tile(
    color = 'white'
    , size = 3
  ) + 
  geom_text(
    position = 'dodge'
    , fontface = 'bold'
    , color = 'white'
    , size = 5
  ) +
  scale_fill_viridis() + 
  theme_ridges(center_axis_labels = T) +
  coord_polar()
