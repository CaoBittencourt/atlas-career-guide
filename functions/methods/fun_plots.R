# -------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'ggridges', 'gghighlight', 'scales' , 'viridis', 'ggalt', 'geomtextpath'
  # , 'circlize' #'paletteer' #Data visualization
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

# -------- PLOT ELEMENTS ---------------------------------------------------
# [DONE] DYNAMIC FACETS ----------------------------------------------------------
fun_facets <- function(
    
  # Facetting variables quosure
  .enq_facets = NULL
  # Number of columns in facet_wrap
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
){
  
  # Quosure
  if(!is_quosure(.enq_facets)){
    
    stop("'.enq_facets' must be a quosure.")
    
  }
  
  # Numeric
  if(!(!length(.int_facets) | is.numeric(.int_facets))){
    
    stop("'.int_facets' must be an integer.")
    
  }
  
  # Character
  if(!(!length(.chr_scales) | is.character(.chr_scales))){
    
    stop("'.chr_scales' must be a character.")
    
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
      , scales = .chr_scales
    ) -> plt_facets
    
  } else if(int_n.args == 1){
    
    # For facets = 1 => facet_wrap
    facet_wrap(
      facets = vars(!!.enq_facets)
      , ncol = .int_facets
      , scales = .chr_scales
    ) -> plt_facets
    
  } else {
    
    NULL -> plt_facets
    
  }
  
  
  # Output
  return(plt_facets)
  
}

# # [DONE] DYNAMIC LABELS --------------------------------------------------
# fun_labels <- function(
    #     
#   # Aes containg labelling variables, etc (mapping)
#   .aes_mapping = NULL
#   # Whether to use geom_label instead of geom_text
#   , .geom_label = F
#   # Logical indicating if coordinates are flipped
#   , .coord_flip = F 
#   # Logical indicating if coordinates are circular
#   , .coord_polar = F
#   # Default parameters for labels (no mapping)
#   , .list_default = list(
#     position = c('dodge', 'stack', 'fill', 'identity')
#     , vjust = -1.15
#     , hjust = -0.15
#     , size = 3.33
#     , fontface = 'bold'
#     , color = '#3854FB'
#   )
#   
# ){
#   
#   # Logical
#   if(!(
#     is.logical(.geom_label) &
#     !is.na(.geom_label)
#   )){
#     
#     stop("'.geom_label' must be either TRUE or FALSE.")
#     
#   }
#   
#   if(!(
#     is.logical(.coord_flip) &
#     !is.na(.coord_flip)
#   )){
#     
#     stop("'.coord_flip' must be either TRUE or FALSE.")
#     
#   }
#   
#   if(!(
#     is.logical(.coord_polar) &
#     !is.na(.coord_polar)
#   )){
#     
#     stop("'.coord_polar' must be either TRUE or FALSE.")
#     
#   }
#   
#   # List
#   if(!(!length(.list_default) | is.list(.list_default))){
#     
#     stop("'.list_default' must be a list with the default labelling parameters.")
#     
#   }
#   
#   # Aes
#   if(!(!length(.aes_mapping) | 'uneval' %in% tolower(class(.aes_mapping)))){
#     
#     stop("'.aes_mapping' must be an aes() mapping.")
#     
#   }
#   
#   
#   # Default => labels = NULL
#   NULL -> plt_labels
#   
#   # If label aes() available, generate labels
#   if('label' %in% tolower(names(.aes_mapping))){
#     
#     # Standardize names
#     names(.list_default) <- standardise_aes_names(names(.list_default))
#     
#     names(.aes_mapping) <- standardise_aes_names(names(.aes_mapping))
#     
#     # Only one default parameter per variable
#     map(.list_default, 1) -> .list_default
#     
#     # Include mapping into list of arguments
#     .list_default$mapping <- .aes_mapping
#     
#     # Remove overlaps
#     .list_default[
#       setdiff(
#         names(.list_default)
#         , names(.aes_mapping)
#       )] -> .list_default
#     
#     
#     # Label position
#     # if(is.character(.list_default$position)){
#     if(length(.list_default$position)){
#       
#       chr_label.position <- tolower(.list_default$position)
#       
#     } else {
#       
#       chr_label.position <- 'identity'
#       
#     }
#     
#     if('stack' %in% chr_label.position){
#       # Position stack  
#       .list_default$position <- position_stack(vjust = 0.5)
#       
#       .list_default$vjust <- NULL
#       
#       .list_default$hjust <- NULL
#       
#     } else if('fill' %in% chr_label.position){
#       # Position fill
#       .list_default$position <- position_fill(vjust = 0.5)
#       
#       .list_default$vjust <- NULL
#       
#       .list_default$hjust <- NULL
#       
#     } else if('dodge' %in% chr_label.position){
#       
#       .list_default$position <- position_dodge(width = 0.9)
#       
#     } else {
#       
#       .list_default$position <- position_identity()
#       
#     }
#     
#     
#     # Label orientation
#     if(.coord_flip){
#       
#       if('stack' %in% chr_label.position){
#         # Position stack
#         .list_default$angle <- 90
#         
#       } else if('fill' %in% chr_label.position){
#         # Position fill
#         .list_default$angle <- 90
#         
#       } else { 
#         # If coord_flip, drop vjust. 
#         .list_default$vjust <- 0.5
#         
#       } 
#       
#     } else { 
#       # If coord_flip == F, drop hjust
#       .list_default$hjust <- 0.5
#       
#     }
#     
#     
#     # If coord_polar ...
#     
#     
#     # Label
#     if(.geom_label){
#       # If geom_label, use geom_label
#       do.call(
#         geom_label
#         , .list_default
#       ) -> plt_labels
#       
#     } else {
#       # Otherwise, use geom_text
#       do.call(
#         geom_text
#         , .list_default
#       ) -> plt_labels
#       
#     }
#     
#   } 
#   
#   
#   # Output
#   return(plt_labels)
#   
# }

# # [DONE] DYNAMIC LABELS --------------------------------------------------
# fun_labels <- function(
    #     
#   # Aes containg labelling variables, etc (mapping)
#   .aes_mapping = NULL
#   # Whether to use geom_label instead of geom_text
#   , .geom_label = F
#   # Logical indicating if coordinates are flipped
#   , .coord_flip = F 
#   # Logical indicating if coordinates are circular
#   , .coord_polar = F
#   , .df_data = tibble()
#   # Default parameters for labels (no mapping)
#   , .list_default = list(
#     position = c('dodge', 'stack', 'fill', 'identity')
#     , vjust = -1.15
#     , hjust = -0.15
#     , size = 3.33
#     , fontface = 'bold'
#     , color = '#3854FB'
#   )
#   
# ){
#   
#   # Logical
#   if(!(
#     is.logical(.geom_label) &
#     !is.na(.geom_label)
#   )){
#     
#     stop("'.geom_label' must be either TRUE or FALSE.")
#     
#   }
#   
#   if(!(
#     is.logical(.coord_flip) &
#     !is.na(.coord_flip)
#   )){
#     
#     stop("'.coord_flip' must be either TRUE or FALSE.")
#     
#   }
#   
#   if(!(
#     is.logical(.coord_polar) &
#     !is.na(.coord_polar)
#   )){
#     
#     stop("'.coord_polar' must be either TRUE or FALSE.")
#     
#   }
#   
#   # List
#   if(!(!length(.list_default) | is.list(.list_default))){
#     
#     stop("'.list_default' must be a list with the default labelling parameters.")
#     
#   }
#   
#   # Aes
#   if(!(!length(.aes_mapping) | 'uneval' %in% tolower(class(.aes_mapping)))){
#     
#     stop("'.aes_mapping' must be an aes() mapping.")
#     
#   }
#   
#   
#   # Default => labels = NULL
#   NULL -> plt_labels
#   
#   # If label aes() available, generate labels
#   if('label' %in% tolower(names(.aes_mapping))){
#     
#     # Standardize names
#     names(.list_default) <- standardise_aes_names(names(.list_default))
#     
#     names(.aes_mapping) <- standardise_aes_names(names(.aes_mapping))
#     
#     # Only one default parameter per variable
#     map(.list_default, 1) -> .list_default
#     
#     # Include mapping into list of arguments
#     .list_default$mapping <- .aes_mapping
#     
#     # Remove overlaps
#     .list_default[
#       setdiff(
#         names(.list_default)
#         , names(.aes_mapping)
#       )] -> .list_default
#     
#     
#     # Label position
#     if(is.character(.list_default$position)){
#       # if(length(.list_default$position)){
#       
#       chr_label.position <- tolower(.list_default$position)
#       
#       if('stack' %in% chr_label.position){
#         # Position stack  
#         .list_default$position <- position_stack(vjust = 0.5)
#         
#         .list_default$vjust <- NULL
#         
#         .list_default$hjust <- NULL
#         
#       } else if('fill' %in% chr_label.position){
#         # Position fill
#         .list_default$position <- position_fill(vjust = 0.5)
#         
#         .list_default$vjust <- NULL
#         
#         .list_default$hjust <- NULL
#         
#       } else if('dodge' %in% chr_label.position){
#         
#         .list_default$position <- position_dodge(width = 0.9)
#         
#       } else {
#         
#         .list_default$position <- position_identity()
#         
#       }
#       
#       # Label orientation
#       if(.coord_flip){
#         
#         if('stack' %in% chr_label.position){
#           # Position stack
#           .list_default$angle <- 90
#           
#         } else if('fill' %in% chr_label.position){
#           # Position fill
#           .list_default$angle <- 90
#           
#         } else { 
#           # If coord_flip, drop vjust. 
#           .list_default$vjust <- 0.5
#           
#         } 
#         
#       }
#       
#     }
#     
#     # Label orientation
#     if(!.coord_flip){
#       # If coord_flip == F, drop hjust
#       .list_default$hjust <- 0.5
#       
#     } else { 
#       # If coord_flip, drop vjust. 
#       .list_default$vjust <- 0.5
#       
#     }
#     
#     
#     # If coord_polar
#     if(.coord_polar){
#       
#       .list_default$hjust <- NULL
#       
#       .list_default$vjust <- NULL
#       
#       # Error handling
#       # stopifnot(
#       #   "'.df_data' must be a data frame." = c(
#       #     length(.df_data)
#       #     , is.data.frame(.df_data)
#       #   )
#       # )
#       
#       
#       # .list_default$mapping$angle <- .df_data$angle
#       
#       # if(all(
#       #   !length(.list_default$nudge_y)
#       #   , length(.list_default$mapping$y)
#       # )){
#       #   
#       #   .df_data %>% 
#       #     pull(!!.list_default$mapping$y) -> dbl_y
#       #   
#       #   ifelse(
#       #     is.numeric(dbl_y)
#       #     , range(dbl_y, na.rm = T) %>% 
#       #       diff() * 0.05
#       #     , NULL
#       #   ) -> .list_default$vjust
#       # 
#       # }
#       
#     }
#     
#     
#     # Label
#     if(.geom_label){
#       # If geom_label, use geom_label
#       do.call(
#         geom_label
#         , .list_default
#       ) -> plt_labels
#       
#     } else {
#       # Otherwise, use geom_text
#       do.call(
#         geom_text
#         , .list_default
#       ) -> plt_labels
#       
#     }
#     
#   } 
#   
#   
#   # Output
#   return(plt_labels)
#   
# }

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
  , .df_data = tibble()
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
  if('label' %in% tolower(names(.aes_mapping))){
    
    # Standardize names
    names(.list_default) <- standardise_aes_names(names(.list_default))
    
    names(.aes_mapping) <- standardise_aes_names(names(.aes_mapping))
    
    # Only one default parameter per variable
    map(.list_default, 1) -> .list_default
    
    # Include mapping into list of arguments
    .list_default$mapping <- .aes_mapping
    
    # Remove overlaps
    .list_default[
      setdiff(
        names(.list_default)
        , names(.aes_mapping)
      )] -> .list_default
    
    
    # Label position
    if(is.character(.list_default$position)){
      # if(length(.list_default$position)){
      
      chr_label.position <- tolower(.list_default$position)
      
      if('stack' %in% chr_label.position){
        # Position stack  
        .list_default$position <- position_stack(vjust = 0.5)
        
        .list_default$vjust <- NULL
        
        .list_default$hjust <- NULL
        
      } else if('fill' %in% chr_label.position){
        # Position fill
        .list_default$position <- position_fill(vjust = 0.5)
        
        .list_default$vjust <- NULL
        
        .list_default$hjust <- NULL
        
      } else if('dodge' %in% chr_label.position){
        
        .list_default$position <- position_dodge(width = 0.9)
        
      } else {
        
        .list_default$position <- position_identity()
        
      }
      
      # Label orientation
      if(.coord_flip){
        
        if('stack' %in% chr_label.position){
          # Position stack
          .list_default$angle <- 90
          
        } else if('fill' %in% chr_label.position){
          # Position fill
          .list_default$angle <- 90
          
        } else { 
          # If coord_flip, drop vjust. 
          .list_default$vjust <- 0.5
          
        } 
        
      }
      
    }
    
    # Label orientation
    if(!.coord_flip){
      # If coord_flip == F, drop hjust
      .list_default$hjust <- 0.5
      
    } else { 
      # If coord_flip, drop vjust. 
      .list_default$vjust <- 0.5
      
    }
    
    
    # If coord_polar
    if(.coord_polar){
      
      .list_default$angle <- -90
      
      .list_default$hjust <- 1.15
      
      .list_default$vjust <- NULL
      
    }
    
    
    # Label
    if(.geom_label){
      # If geom_label, use geom_label
      do.call(
        geom_labelpath
        , .list_default
      ) -> plt_labels
      
    } else {
      # Otherwise, use geom_text
      do.call(
        geom_textpath
        , .list_default
      ) -> plt_labels
      
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
  # Aes mapping
  , .aes_mapping = aes()
  
){c
  
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
  
  # Include mapping into list of arguments
  .list_default$mapping <- .aes_mapping
  
  # Remove overlaps
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
  # , .chr_manual.lim = NULL
  
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
  
  # if(!(!length(.chr_manual.lim) | is.character(.chr_manual.lim))){
  # 
  #   stop("'.chr_manual.lim' must be a character vector.")
  # 
  # }
  
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
  if(length(.chr_manual.pal)){
    
    # if(!length(.chr_manual.lim)){
    # 
    #   names(.chr_manual.pal)[
    #     nzchar(names(.chr_manual.pal))
    #   ] -> .chr_manual.lim
    # 
    # }
    
    scale_fill_manual(
      values = .chr_manual.pal
      # , limits = .chr_manual.lim
      , aesthetics = .chr_manual.aes
      # , na.translate = F
      , drop = F
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
    
  # Axis function
  .fun_axis = scale_y_continuous
  # Axis function arguments
  , .list_axis.args = list(
    # Limits
    limits = NULL
    # Number of breaks
    , breaks = breaks_extended(5)
  )
  # Format function with arguments
  , .fun_format = label_number(accuracy = .01)
  
){
  
  # Function
  if(!(
    !length(.fun_axis)
    | 'function' %in% tolower(class(.fun_axis))
  )){
    
    stop("'.fun_axis' must be a function for generating an axis.")
    
  }
  
  if(!(
    !length(.fun_format)
    | 'function' %in% tolower(class(.fun_format))
  )){
    
    stop("'.fun_format' must be a function for formatting an x axis.")
    
  }
  
  # Scale
  if(length(.fun_axis)){
    
    if(!c('scalecontinuousposition'
          , 'scalediscreteposition') %in% 
       tolower(
         .fun_axis %>% 
         do.call(list()) %>% 
         class()
       ) %>%
       any()
    ){
      
      stop("'.fun_axis' must be a function that generates a ggproto scale object.")
      
    }
    
  }
  
  # List
  if(!is.list(.list_axis.args)){
    
    stop("'.list_axis.args' must be a list of arguments to be passed on to '.fun_axis'.")
    
  }
  
  # Default
  NULL -> plt_axis
  
  # Axis 
  if(length(.fun_axis)){
    
    # Labels
    .list_axis.args$labels <- .fun_format
    
    # Axis
    do.call(
      .fun_axis
      , .list_axis.args
    ) -> plt_axis
    
  }
  
  
  # Output
  return(plt_axis)
  
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
    coord_curvedpolar() -> plt_coord
    
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
  
  # # List
  # if(!is.list(.list_legend)){
  #   
  #   stop("'.list_legend' must be a named list of logical elements.")
  #   
  # }
  # 
  # # Character
  # lapply(
  #   .list_legend
  #   , function(args){
  #     
  #     if(!(!length(args) | is.character(args))){
  #       
  #       stop("'.list_legend' must be a named list of character elements.")
  #       
  #     }
  #     
  #   }
  # )
  
  
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
fun_aes.map <- function(.aes_mapping, .chr_required_aes){
  
  # Aes
  if(!(!length(.aes_mapping) | 'uneval' %in% tolower(class(.aes_mapping)))){
    
    stop("'.aes_mapping' must be an aes() mapping.")
    
  }
  
  # Character
  if(!(!length(.chr_required_aes) | is.character(.chr_required_aes))){
    
    stop("'.chr_required_aes' must be a character.")
    
  }
  
  # Required aes()
  if(length(.aes_mapping) & length(.chr_required_aes)){
    
    # Standardize names
    .chr_required_aes %>%
      tolower() %>%
      standardise_aes_names() -> chr_required_aes
    
    .aes_mapping %>% 
      names %>%
      tolower %>%
      standardise_aes_names() -> aes_mapping
    
    # Check if all required aesthetics are available
    if(!all(chr_required_aes %in% aes_mapping)){
      
      chr_required_aes[!(chr_required_aes %in% aes_mapping)] %>% 
        paste(collapse = ',') -> chr_missing_aes
      
      stop(paste0("The following aesthetics are missing: ", chr_missing_aes, '.'))
      
    }
    
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
    | .df_data %>%
    select(
      !!.enq_var.fct
    ) %>% 
    map_df(class) %>% 
    tail(1) %>%
    as.character() %in% 
    c('factor', 'character') %>% 
    all()
  )){
    
    stop("'.enq_var.fct' must be a quosure of a factor or character variable.")
    
  }
  
  # Numerical
  if(!(
    !length(.enq_var.dbl)
    | .df_data %>%
    select(
      !!.enq_var.dbl
    ) %>% 
    map_df(class) %>% 
    tail(1) %>%
    as.character() %in% 
    c('numeric', 'integer') %>% 
    all()
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
        across(
          .cols = !!.enq_var.fct
          ,.fns = function(x){
            fct_reorder(
              x
              , !!.enq_var.dbl
              , .desc = .desc
              , .fun = .fun_ord
            )
          }
        )) -> .df_data
    
  }
  
  
  # Output
  return(.df_data)
  
}

# -------- PLOTS -----------------------------------------------------------
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
  , .list_labs = list()
  
  # Geom default parameters
  , .list_geom.param = list(
    fill = '#3854FB'
    , bins = 30
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
  # Color Mapping
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T, na.translate = F)
    , viridis::scale_fill_viridis(discrete = T, na.translate = F)
  )
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  # , .chr_manual.lim = NULL
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_continuous
  , .fun_axis.y = scale_y_continuous
  , .list_axis.x.args = list(
    breaks = breaks_extended(5)
  )
  , .list_axis.y.args = list(
    breaks = breaks_extended(5)
  )
  , .fun_format.x = label_number(accuracy = .01)
  , .fun_format.y = label_number()
  
  # Coordinates
  , .dbl_limits.x = NULL
  , .dbl_limits.y = NULL
  
  # Theme
  , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = 'x'
  ) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
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
    # , .chr_manual.lim = .chr_manual.lim
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
  
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
    ggplot() +
    plt_geom +
    plt_facets +
    # Colors
    plt_colors +
    # Axes
    plt_axis.x +
    plt_axis.y +
    plt_coord +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_histogram
  
  
  # Output
  return(plt_histogram)
  
}

# # HISTOGRAM FUNCTION --------------------------------------------
# fun_plot.histogram <- function(
    #     
#   # Data
#   .df_data
#   , .aes_mapping
#   
#   # Overlay density
#   , .overlay_density = T
#   
#   # Reorder
#   , .reorder_fct = T
#   , .reorder_desc = F
#   , .reorder_fun = sum
#   
#   # Labels
#   , .list_labs = list()
#   
#   # Geom default parameters
#   , .list_geom.param = list(
#     fill = '#3854FB'
#     , bins = 30
#   )
#   
#   # Facets
#   , .sym_facets = NULL
#   , .int_facets = NULL
#   
#   # Color Mapping
#   , .scale_colors = list(
#     viridis::scale_color_viridis(discrete = T, na.translate = F)
#     , viridis::scale_fill_viridis(discrete = T, na.translate = F)
#   )  
#   , .chr_manual.pal = NULL
#   , .chr_manual.aes = 'fill'
#   
#   # Legend
#   , .list_legend = list()
#   
#   # Axes
#   , .fun_axis.x = scale_x_continuous
#   , .fun_axis.y = scale_y_continuous
#   , .list_axis.x.args = list(
#     breaks = breaks_extended(5)
#   )
#   , .list_axis.y.args = list(
#     breaks = breaks_extended(5)
#   )
#   , .fun_format.x = label_number(accuracy = .01)
#   , .fun_format.y = label_number()
#   
#   # Coordinates
#   , .dbl_limits.x = NULL
#   , .dbl_limits.y = NULL
#   
#   # Theme
#   , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
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
#   fun_aes.map(
#     .aes_mapping
#     , .chr_required_aes = 'x'
#   ) -> aes_mapping
#   
#   # Facets
#   fun_facets(
#     .enq_facets = enq_facets
#     , .int_facets = .int_facets
#   ) -> plt_facets
#   
#   # Reordering
#   if(.reorder_fct & length(plt_facets)){
#     
#     fun_reorder(
#       .df_data = .df_data
#       , .enq_var.fct = enq_facets
#       , .enq_var.dbl = aes_mapping$x
#       , .fun_ord = .reorder_fun
#       , .desc = .reorder_desc
#     ) -> .df_data
#     
#   }
#   
#   # Color mapping
#   fun_colors(
#     .scale_color = .scale_colors
#     , .chr_manual.pal = .chr_manual.pal
#     , .chr_manual.aes = .chr_manual.aes
#   ) -> plt_colors
#   
#   # Axes format
#   fun_axis.format(
#     .fun_axis = .fun_axis.x
#     , .list_axis.args = .list_axis.x.args
#     , .fun_format = .fun_format.x
#   ) -> plt_axis.x
#   
#   fun_axis.format(
#     .fun_axis = .fun_axis.y
#     , .list_axis.args = .list_axis.y.args
#     , .fun_format = .fun_format.y
#   ) -> plt_axis.y
#   
#   
#   # Coordinates
#   fun_coordinates(
#     .dbl_limits.x = .dbl_limits.x
#     , .dbl_limits.y = .dbl_limits.y
#     , .coord_flip = F
#     , .coord_polar = F
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
#   
#   # Histogram plot
#   # With density overlay
#   # Without density overlay
#   if(.overlay_density){ 
#     
#     # aes_mapping$y <- after_stat(density)
#     
#     fun_geom.params(
#       .fun_geom = geom_histogram
#       , .list_default = .list_geom.param
#       , .aes_mapping = aes_mapping
#     ) -> plt_geom
#     
#     # stat_function(
#     #   fun = dnorm
#     #   , args = list(
#     #     mean = .df_data %>% pull(!!aes_mapping$x) %>% mean()
#     #     , sd = .df_data %>% pull(!!aes_mapping$x) %>% sd()
#     #   )
#     #   , col = '#212121'
#     #   , size = 1.5
#     # ) -> plt_density
#     
#   } else { 
#     
#     fun_geom.params(
#       .fun_geom = geom_histogram
#       , .list_default = .list_geom.param
#       , .aes_mapping = aes_mapping
#     ) -> plt_geom
#     
#     NULL -> plt_density
#     
#     }
#   
#   # ggplot
#   .df_data %>%
#     # Plot
#     ggplot() +
#     plt_geom + 
#     plt_density + 
#     plt_facets +
#     # Colors
#     plt_colors + 
#     # Axes
#     plt_axis.x +
#     plt_axis.y +
#     plt_coord + 
#     # Theme
#     plt_theme +
#     plt_legend +
#     # Labels
#     plt_labs -> plt_histogram
#   
#   
#   # Output
#   return(plt_histogram)
#   
# }

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
  , .list_labs = list()
  
  # Geom default parameters
  , .list_geom.param = list(
    color = '#212121'
    , fill = '#3854FB'
    , alpha = 0.8
    , size = 1.23
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
  # Color Mapping
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T, na.translate = F)
    , viridis::scale_fill_viridis(discrete = T, na.translate = F)
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  # , .chr_manual.lim = NULL
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_continuous
  , .fun_axis.y = scale_y_continuous
  , .list_axis.x.args = list(
    breaks = breaks_extended(5)
  )
  , .list_axis.y.args = list(
    breaks = breaks_extended(5)
  )
  , .fun_format.x = label_number(accuracy = .01)
  , .fun_format.y = label_number(accuracy = .0001)
  
  # Coordinates
  , .dbl_limits.x = NULL
  , .dbl_limits.y = NULL
  
  # Theme
  , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = 'x'
  ) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
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
    # , .chr_manual.lim = .chr_manual.lim 
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
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
    ggplot() +
    plt_geom + 
    plt_facets +
    # Colors
    plt_colors + 
    # Axes
    plt_axis.x +
    plt_axis.y +
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
  , .list_labs = list()
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
  , .chr_scales = 'fixed'
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis()
    , viridis::scale_fill_viridis()
  )  
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  # , .chr_manual.lim = NULL
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_discrete
  , .fun_axis.y = scale_y_discrete
  , .list_axis.x.args = list()
  , .list_axis.y.args = list()
  , .fun_format.x = function(x){str_wrap(x,10)}
  , .fun_format.y = function(y){str_wrap(y,10)}
  
  # Coordinates
  , .coord_flip = F
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = c('x','y','fill')
  ) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
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
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = aes_mapping$y
      , .enq_var.dbl = aes_mapping$fill
      , .fun_ord = .reorder_fun
      , .desc = !.reorder_desc
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
    # , .chr_manual.lim = .chr_manual.lim
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
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
    ggplot() +
    plt_geom + 
    plt_labels +
    plt_facets +
    # Colors
    plt_colors + 
    # Axes
    plt_axis.x +
    plt_axis.y +
    plt_coord + 
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_heatmap
  
  
  # Output
  return(plt_heatmap)
  
}

# LINE CHART FUNCTION --------------------------------------------
fun_plot.line <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = F
  , .reorder_desc = F
  , .reorder_fun = max
  
  # Labels
  , .list_labs = list()
  , .geom_label = F
  
  # Default parameters
  , .list_geom.param = list(
    position = 'identity'
    , color = '#3854FB'
    , size = 1.2
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T, na.translate = F)
    , viridis::scale_fill_viridis(discrete = T, na.translate = F)
  )
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'color'
  # , .chr_manual.lim = .chr_manual.lim
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_continuous
  , .fun_axis.y = scale_y_continuous
  , .list_axis.x.args = list(
    breaks = breaks_extended(5)
  )
  , .list_axis.y.args = list(
    breaks = breaks_extended(5)
  )
  , .fun_format.x = label_number(accuracy = .01)
  , .fun_format.y = label_number(accuracy = .01)
  
  # Coordinates
  , .dbl_limits.x = NULL
  , .dbl_limits.y = NULL
  , .coord_flip = F
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = c('x', 'y')
  ) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
  ) -> plt_facets
  
  # Reordering
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
    # , .chr_manual.lim = .chr_manual.lim
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.x = .dbl_limits.x
    , .dbl_limits.y = .dbl_limits.y
    , .coord_flip = .coord_flip
    , .coord_polar = .coord_polar
  ) -> plt_coord
  
  # Theme
  fun_theme(.theme) -> plt_theme
  
  # Legend
  fun_legends(.list_legend) -> plt_legend
  
  # Labs
  fun_labs(.list_labs) -> plt_labs
  
  
  # Line chart
  # geom_line with default parameters
  fun_geom.params(
    .fun_geom = geom_line
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot() +
    plt_geom +
    plt_facets +
    # Colors
    plt_colors +
    # Axes
    plt_axis.x +
    plt_axis.y +
    plt_coord +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_line
  
  
  # Output
  return(plt_line)
  
}

# # BAR CHART FUNCTION --------------------------------------------
# fun_plot.bar <- function(
    #     
#   # Data
#   .df_data
#   , .aes_mapping
#   
#   # Reorder
#   , .reorder_fct = T
#   , .reorder_desc = F
#   , .reorder_fun = sum
#   
#   # Labels
#   , .list_labs = list()
#   , .geom_label = F
#   
#   # Default parameters
#   , .list_geom.param = list(
#     position = c('dodge', 'stack', 'fill', 'identity')
#     , fill = '#3854FB'
#   )
#   , .list_labels.param = list(
#     fontface = 'bold'
#     , color = '#3854FB'
#     , size = 3.33
#     , vjust = -1.15
#     , hjust = -0.15
#   )
#   
#   # Facets
#   , .sym_facets = NULL
#   , .int_facets = NULL
#   , .chr_scales = 'fixed'
#   
#   # Colors
#   , .scale_colors = list(
#     viridis::scale_color_viridis(discrete = T, na.translate = F)
#     , viridis::scale_fill_viridis(discrete = T, na.translate = F)
#   )
#   , .chr_manual.pal = NULL
#   , .chr_manual.aes = 'fill'
#   
#   # Legend
#   , .list_legend = list()
#   
#   # Axes
#   , .fun_axis.x = scale_x_discrete
#   , .fun_axis.y = scale_y_continuous
#   , .list_axis.x.args = list()
#   , .list_axis.y.args = list(
#     breaks = breaks_extended(5)
#   )
#   , .fun_format.x = function(x){str_wrap(x,10)}
#   , .fun_format.y = label_number(accuracy = .01)
#   
#   # Coordinates
#   , .dbl_limits.y = NULL
#   , .coord_flip = F
#   , .coord_polar = F
#   
#   # Labels for polar coordinates
#   , .fun_polar.labels = function(x){number(x, accuracy = .01)}
#   
#   # Theme
#   , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
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
#   fun_aes.map(
#     .aes_mapping
#     , .chr_required_aes = c('x','y')
#   ) -> aes_mapping
#   
#   if(.coord_polar){
#     
#     # Keep only unique rows
#     .df_data %>%
#       group_by(across(
#         !!!aes_mapping$fill
#       )) %>% 
#       slice(1) %>%
#       ungroup()
#     # unique() %>%
#     arrange(desc(
#       !!aes_mapping$y
#     )) -> .df_data.unique
#     
#     # Empty columns
#     int_NA <- round(nrow(.df_data.unique) * (51 / 873))
#     int_NA <- ifelse(int_NA %% 2, int_NA, int_NA + 1)
#     mtx_NA <- matrix(NA, int_NA, ncol(.df_data.unique))
#     colnames(mtx_NA) <- colnames(.df_data.unique)
#     
#     mtx_NA %>%
#       rbind(.df_data.unique) %>%
#       mutate(
#         n = row_number()
#         , angle = 90 - 360 * (row_number() - 0.5) / n()
#         # , angle = angle + pi * int_NA / (int_NA + n())
#         # , hjust = ifelse(angle < -90, 1, 0)
#         # , hjust = ifelse(angle < -90, 1.15, -0.15)
#         , hjust = ifelse(angle < -90, 1.15, -0.15)
#         , angle = ifelse(angle < -90, angle + 180, angle)
#         , n = factor(n)
#       ) -> .df_data.unique
#     
#     aes_mapping$x <- sym('n')
#     
#     # aes_mapping$hjust <- sym('hjust')
#     # 
#     # aes_mapping$angle <- sym('angle')
#     
#     # Reorder = false
#     .reorder_fct <- F
#     
#   }
#   
#   # Facets
#   fun_facets(
#     .enq_facets = enq_facets
#     , .int_facets = .int_facets
#     , .chr_scales = .chr_scales
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
#   # Color mapping
#   fun_colors(
#     .scale_color = .scale_colors
#     , .chr_manual.pal = .chr_manual.pal
#     , .chr_manual.aes = .chr_manual.aes
#     # , .chr_manual.lim = .chr_manual.lim
#   ) -> plt_colors
#   
#   # Axes format
#   fun_axis.format(
#     .fun_axis = .fun_axis.x
#     , .list_axis.args = .list_axis.x.args
#     , .fun_format = .fun_format.x
#   ) -> plt_axis.x
#   
#   fun_axis.format(
#     .fun_axis = .fun_axis.y
#     , .list_axis.args = .list_axis.y.args
#     , .fun_format = .fun_format.y
#   ) -> plt_axis.y
#   
#   # Coordinates
#   fun_coordinates(
#     .dbl_limits.y = .dbl_limits.y
#     , .coord_flip = .coord_flip
#     , .coord_polar = F
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
#   if(!length(.list_labels.param$position)){
#     # Unless specified, use geom position as label position
#     .list_labels.param$position <- .list_geom.param$position
#     
#   }
#   
#   # Labels
#   fun_labels(
#     .list_default = .list_labels.param
#     , .aes_mapping = aes_mapping
#     , .coord_flip = .coord_flip
#     , .geom_label = .geom_label
#     , .coord_polar = .coord_polar
#     , .df_data = .df_data
#   ) -> plt_labels
#   
#   # return(list(plt_labels, .df_data))
#   # stop()
#   
#   # Bar chart
#   # geom_col with default parameters
#   fun_geom.params(
#     .fun_geom = geom_col
#     , .list_default = .list_geom.param
#     , .aes_mapping = aes_mapping
#   ) -> plt_geom
#   
#   if(!.coord_polar){
#     
#     # ggplot
#     .df_data %>%
#       # Plot
#       ggplot() +
#       plt_geom +
#       plt_labels +
#       plt_facets +
#       # Colors
#       plt_colors +
#       # Axes
#       plt_axis.x +
#       plt_axis.y +
#       plt_coord +
#       # Theme
#       plt_theme +
#       plt_legend +
#       # Labels
#       plt_labs -> plt_bar
#     
#   } else {
#     
#     # Y axis
#     if(
#       length(.list_axis.y.args$breaks) &
#       is.numeric(.list_axis.y.args$breaks)
#     ){
#       
#       seq_breaks <- .list_axis.y.args$breaks
#       
#     } else if(length(.dbl_limits.y)){
#       
#       seq_breaks <- .dbl_limits.y
#       
#     } else {
#       
#       .df_data %>%
#         pull(!!aes_mapping$y) -> dbl_y
#       
#       seq(
#         min(dbl_y, na.rm = T)
#         , max(dbl_y, na.rm = T)
#         , length.out = 5
#       ) -> seq_breaks
#       
#     }
#     
#     sort(seq_breaks) -> seq_breaks
#     
#     # Axis labels
#     if(length(.fun_polar.labels)){
#       
#       do.call(
#         .fun_polar.labels
#         , list(seq_breaks)
#       ) -> chr_labels
#       
#     }
#     
#     # Circular bar plot
#     .df_data %>%
#       # ggplot
#       # Plot
#       ggplot() +
#       plt_geom +
#       plt_labels +
#       plt_facets +
#       # Colors
#       plt_colors +
#       # Axes
#       plt_axis.x +
#       plt_axis.y +
#       ylim(c(-0.55 * max(seq_breaks)), NA) +
#       plt_coord +
#       # Theme
#       # Void theme
#       theme_void() +
#       theme(
#         legend.position = c(0.5,0.05)
#         , legend.direction = 'horizontal'
#         , panel.grid = element_blank()
#         , panel.border = element_blank()
#         , plot.margin = margin(0, 0, 0, 0)
#         # , plot.title = element_blank()
#         , plot.subtitle = element_blank()
#         , axis.title = element_blank()
#         , axis.text = element_blank()
#         , axis.ticks = element_blank()
#         , axis.line = element_blank()
#       ) +
#       plt_legend +
#       # Labels
#       plt_labs +
#       # Polar coordinates
#       coord_polar(
#         start = -pi * int_NA / (int_NA + nrow(.df_data.unique))
#         # start = pi * int_NA / (int_NA + nrow(.df_data))
#       ) +
#       # y axis
#       Map(
#         function(y, y.label){
#           
#           annotate(
#             # x = as.character(round(int_NA / 2))
#             x = as.character(ceiling(int_NA / 2))
#             , y = y + max(seq_breaks) * 0.1
#             , label = y.label
#             , geom = 'text'
#             , color = '#212121'
#             , fontface = 'bold'
#             , size = 3
#           )
#           
#         }
#         , y = seq_breaks
#         , y.label = chr_labels
#       ) +
#       annotate(
#         # x = as.character(round(int_NA / 2))
#         x = as.character(ceiling(int_NA / 2))
#         , y = -0.55 * max(seq_breaks)
#         , label = do.call(.fun_format.x, list(.list_labs$y)) 
#         # , label = str_replace_all(
#         #   .list_labs$y
#         #   , ' ', '\n')
#         , geom = 'text'
#         , color = ifelse(
#           length(.list_geom.param$fill)
#           & !length(aes_mapping$fill)
#           , .list_geom.param$fill
#           , '#212121'
#         )
#         , fontface = 'bold'
#         , size = 4
#       ) -> plt_bar
#     
#     plt_bar$layers <- c(
#       geom_hline(
#         yintercept = seq_breaks[-1]
#         # , color = list_atlas.pal$grey
#         , linetype = 'dashed'
#         , color = 'lightgrey'
#         , size = 0.5
#       )
#       , plt_bar$layers
#     )
#     
#     plt_bar$layers <- c(
#       geom_hline(
#         # yintercept = seq_breaks[length(seq_breaks)]
#         yintercept = seq_breaks[1]
#         # , color = list_atlas.pal$grey
#         , color = 'lightgrey'
#         , size = 0.5
#       )
#       , plt_bar$layers
#     )
#     
#   }
#   
#   
#   # Output
#   return(plt_bar)
#   
# }

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
  , .list_labs = list()
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
  , .chr_scales = 'fixed'
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T, na.translate = F)
    , viridis::scale_fill_viridis(discrete = T, na.translate = F)
  )
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_discrete
  , .fun_axis.y = scale_y_continuous
  , .list_axis.x.args = list()
  , .list_axis.y.args = list(
    breaks = breaks_extended(5)
  )
  , .fun_format.x = function(x){str_wrap(x,10)}
  , .fun_format.y = label_number(accuracy = .01)
  
  # Coordinates
  , .dbl_limits.y = NULL
  , .coord_flip = F
  , .coord_polar = F
  
  # Labels for polar coordinates
  , .fun_polar.labels = function(x){number(x, accuracy = .01)}
  
  # Theme
  , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = c('x','y')
  ) -> aes_mapping
  
  if(.coord_polar){
    
    # Keep only unique rows
    .df_data %>%
      unique() %>%
      # mutate(across(
      #   .cols = !!aes_mapping$fill
      #   ,.fns = factor
      # )) %>%
      arrange(desc(
        !!aes_mapping$y
      )) -> .df_data
    
    # Empty columns
    int_NA <- round(nrow(.df_data) * (51 / 873))
    int_NA <- ifelse(int_NA %% 2, int_NA, int_NA + 1)
    mtx_NA <- matrix(NA, int_NA, ncol(.df_data))
    colnames(mtx_NA) <- colnames(.df_data)
    
    mtx_NA %>%
      rbind(.df_data) %>%
      mutate(
        n = row_number()
        , angle = 90 - 360 * (row_number() - 0.5) / n()
        # , angle = angle + pi * int_NA / (int_NA + n())
        # , hjust = ifelse(angle < -90, 1, 0)
        # , hjust = ifelse(angle < -90, 1.15, -0.15)
        , hjust = ifelse(angle < -90, 1.15, -0.15)
        , angle = ifelse(angle < -90, angle + 180, angle)
        , n = factor(n)
      ) -> .df_data
    
    aes_mapping$x <- sym('n')
    
    # aes_mapping$hjust <- sym('hjust')
    #
    # aes_mapping$angle <- sym('angle')
    
    # Reorder = false
    .reorder_fct <- F
    
  }
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
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
    # , .chr_manual.lim = .chr_manual.lim
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.y = .dbl_limits.y
    , .coord_flip = .coord_flip
    , .coord_polar = F
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
    , .coord_polar = .coord_polar
    , .df_data = .df_data
  ) -> plt_labels
  
  # return(list(plt_labels, .df_data))
  # stop()
  
  # Bar chart
  # geom_col with default parameters
  fun_geom.params(
    .fun_geom = geom_col
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  if(!.coord_polar){
    
    # ggplot
    .df_data %>%
      # Plot
      ggplot() +
      plt_geom +
      plt_labels +
      plt_facets +
      # Colors
      plt_colors +
      # Axes
      plt_axis.x +
      plt_axis.y +
      plt_coord +
      # Theme
      plt_theme +
      plt_legend +
      # Labels
      plt_labs -> plt_bar
    
  } else {
    
    # Y axis
    if(
      length(.list_axis.y.args$breaks) &
      is.numeric(.list_axis.y.args$breaks)
    ){
      
      seq_breaks <- .list_axis.y.args$breaks
      
    } else if(length(.dbl_limits.y)){
      
      seq_breaks <- .dbl_limits.y
      
    } else {
      
      .df_data %>%
        pull(!!aes_mapping$y) -> dbl_y
      
      seq(
        min(dbl_y, na.rm = T)
        , max(dbl_y, na.rm = T)
        , length.out = 5
      ) -> seq_breaks
      
    }
    
    sort(seq_breaks) -> seq_breaks
    
    # Axis labels
    do.call(
      .fun_format.y
      , list(seq_breaks)
    ) -> chr_labels
    
    # Circular bar plot
    .df_data %>%
      # ggplot
      # Plot
      ggplot() +
      plt_geom +
      plt_labels +
      plt_facets +
      # Colors
      plt_colors +
      # Axes
      plt_axis.x +
      plt_axis.y +
      ylim(c(-0.55 * max(seq_breaks)), NA) +
      plt_coord +
      # Theme
      # Void theme
      theme_void() +
      theme(
        # legend.position = c(0.5,0)
        # , legend.direction = 'horizontal'
        panel.grid = element_blank()
        , panel.border = element_blank()
        , plot.margin = margin(0, 0, 0, 0)
        # , plot.title = element_blank()
        , plot.subtitle = element_blank()
        , axis.title = element_blank()
        , axis.text = element_blank()
        , axis.ticks = element_blank()
        , axis.line = element_blank()
      ) +
      plt_legend +
      # Labels
      plt_labs +
      # Polar coordinates
      coord_polar(
        start = -pi * int_NA / (int_NA + nrow(.df_data))
        # start = pi * int_NA / (int_NA + nrow(.df_data))
      ) +
      # y axis
      Map(
        function(y, y.label){
          
          annotate(
            # x = as.character(round(int_NA / 2))
            x = as.character(ceiling(int_NA / 2))
            , y = y + max(seq_breaks) * 0.1
            , label = y.label
            , geom = 'text'
            , color = '#212121'
            , fontface = 'bold'
            , size = 3
          )
          
        }
        , y = seq_breaks
        , y.label = chr_labels
      ) +
      annotate(
        # x = as.character(round(int_NA / 2))
        x = as.character(ceiling(int_NA / 2))
        , y = -0.55 * max(seq_breaks)
        , label = do.call(.fun_format.x, list(.list_labs$y))
        # , label = str_replace_all(
        #   .list_labs$y
        #   , ' ', '\n')
        , geom = 'text'
        , color = ifelse(
          length(.list_geom.param$fill)
          & !length(aes_mapping$fill)
          , .list_geom.param$fill
          , '#212121'
        )
        , fontface = 'bold'
        , size = 4
      ) -> plt_bar
    
    plt_bar$layers <- c(
      geom_hline(
        yintercept = seq_breaks[-1]
        # , color = list_atlas.pal$grey
        , linetype = 'dashed'
        , color = 'lightgrey'
        , size = 0.5
      )
      , plt_bar$layers
    )
    
    plt_bar$layers <- c(
      geom_hline(
        # yintercept = seq_breaks[length(seq_breaks)]
        yintercept = seq_breaks[1]
        # , color = list_atlas.pal$grey
        , color = 'lightgrey'
        , size = 0.5
      )
      , plt_bar$layers
    )
    
  }
  
  
  # Output
  return(plt_bar)
  
}

# # BAR CHART FUNCTION --------------------------------------------
# fun_plot.bar <- function(
    #     
#   # Data
#   .df_data
#   , .aes_mapping
#   
#   # Reorder
#   , .reorder_fct = T
#   , .reorder_desc = F
#   , .reorder_fun = sum
#   
#   # Labels
#   , .list_labs = list()
#   , .geom_label = F
#   
#   # Default parameters
#   , .list_geom.param = list(
#     position = c('dodge', 'stack', 'fill', 'identity')
#     , fill = '#3854FB'
#   )
#   , .list_labels.param = list(
#     fontface = 'bold'
#     , color = '#3854FB'
#     , size = 3.33
#     , vjust = -1.15
#     , hjust = -0.15
#   )
#   
#   # Facets
#   , .sym_facets = NULL
#   , .int_facets = NULL
#   , .chr_scales = 'fixed'
#   
#   # Colors
#   , .scale_colors = list(
#     viridis::scale_color_viridis(discrete = T, na.translate = F)
#     , viridis::scale_fill_viridis(discrete = T, na.translate = F)
#   )
#   , .chr_manual.pal = NULL
#   , .chr_manual.aes = 'fill'
#   
#   # Legend
#   , .list_legend = list()
#   
#   # Axes
#   , .fun_axis.x = scale_x_discrete
#   , .fun_axis.y = scale_y_continuous
#   , .list_axis.x.args = list()
#   , .list_axis.y.args = list(
#     breaks = breaks_extended(5)
#   )
#   , .fun_format.x = function(x){str_wrap(x,10)}
#   , .fun_format.y = label_number(accuracy = .01)
#   
#   # Coordinates
#   , .dbl_limits.y = NULL
#   , .coord_flip = F
#   , .coord_polar = F
#   
#   # Labels for polar coordinates
#   , .fun_polar.labels = function(x){number(x, accuracy = .01)}
#   
#   # Theme
#   , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
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
#   fun_aes.map(
#     .aes_mapping
#     , .chr_required_aes = c('x','y')
#   ) -> aes_mapping
#   
#   if(.coord_polar){
#     
#     # Keep only unique rows
#     .df_data %>%
#       unique() %>%
#       arrange(desc(
#         !!aes_mapping$y
#       )) -> .df_data
#     
#     # Empty columns
#     int_NA <- round(nrow(.df_data) * (51 / 873))
#     int_NA <- ifelse(int_NA %% 2, int_NA, int_NA + 1)
#     mtx_NA <- matrix(NA, int_NA, ncol(.df_data))
#     colnames(mtx_NA) <- colnames(.df_data)
#     
#     # Grouping variable
#     if(all(
#       length(aes_mapping$fill)
#       , last(as.character(aes_mapping$fill)) %in% 
#       names(.df_data)
#     )
#     ){
#       
#       .df_data %>% 
#         pull(!!aes_mapping$fill) -> chr_fill
#       
#       mtx_NA %>% 
#         rbind(matrix(
#           NA
#           , int_NA * ((
#             chr_fill %>% 
#               unique() %>% 
#               length()
#           ) - 1)
#           , ncol(.df_data)
#         )) %>% 
#         as_tibble() %>%
#         mutate(
#           !!aes_mapping$fill := 
#             chr_fill %>% 
#             unique() %>% 
#             factor() %>% 
#             levels() %>% 
#             rep(each = int_NA)
#         ) -> mtx_NA
#       
#     } 
#     
#     mtx_NA %>%
#       rbind(.df_data) %>% 
#       arrange(
#         !!aes_mapping$fill
#       ) %>% 
#       mutate(
#         n = row_number()
#         , n = factor(n)
#       ) -> .df_data
#     
#     aes_mapping$x <- sym('n')
#     
#     # Reorder = false
#     .reorder_fct <- F
#     
#   }
#   
#   # Facets
#   fun_facets(
#     .enq_facets = enq_facets
#     , .int_facets = .int_facets
#     , .chr_scales = .chr_scales
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
#   # Color mapping
#   fun_colors(
#     .scale_color = .scale_colors
#     , .chr_manual.pal = .chr_manual.pal
#     , .chr_manual.aes = .chr_manual.aes
#     # , .chr_manual.lim = .chr_manual.lim
#   ) -> plt_colors
#   
#   # Axes format
#   fun_axis.format(
#     .fun_axis = .fun_axis.x
#     , .list_axis.args = .list_axis.x.args
#     , .fun_format = .fun_format.x
#   ) -> plt_axis.x
#   
#   fun_axis.format(
#     .fun_axis = .fun_axis.y
#     , .list_axis.args = .list_axis.y.args
#     , .fun_format = .fun_format.y
#   ) -> plt_axis.y
#   
#   # Coordinates
#   fun_coordinates(
#     .dbl_limits.y = .dbl_limits.y
#     , .coord_flip = .coord_flip
#     , .coord_polar = F
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
#   if(!length(.list_labels.param$position)){
#     # Unless specified, use geom position as label position
#     .list_labels.param$position <- .list_geom.param$position
#     
#   }
#   
#   # Labels
#   fun_labels(
#     .list_default = .list_labels.param
#     , .aes_mapping = aes_mapping
#     , .coord_flip = .coord_flip
#     , .geom_label = .geom_label
#     , .coord_polar = .coord_polar
#     , .df_data = .df_data
#   ) -> plt_labels
#   
#   
#   # Bar chart
#   # geom_col with default parameters
#   fun_geom.params(
#     .fun_geom = geom_col
#     , .list_default = .list_geom.param
#     , .aes_mapping = aes_mapping
#   ) -> plt_geom
#   
#   if(!.coord_polar){
#     
#     # ggplot
#     .df_data %>%
#       # Plot
#       ggplot() +
#       plt_geom + 
#       plt_labels + 
#       plt_facets +
#       # Colors
#       plt_colors + 
#       # Axes
#       plt_axis.x +
#       plt_axis.y +
#       plt_coord + 
#       # Theme
#       plt_theme +
#       plt_legend +
#       # Labels
#       plt_labs -> plt_bar
#     
#   } else {
#     
#     # Y axis
#     if(
#       length(.list_axis.y.args$breaks) &
#       is.numeric(.list_axis.y.args$breaks)
#     ){
#       
#       seq_breaks <- .list_axis.y.args$breaks
#       
#     } else if(length(.dbl_limits.y)){
#       
#       seq_breaks <- .dbl_limits.y
#       
#     } else { 
#       
#       .df_data %>% 
#         pull(!!aes_mapping$y) -> dbl_y
#       
#       seq_breaks <- seq(
#         min(dbl_y, na.rm = T)
#         , max(dbl_y, na.rm = T)
#         , length.out = 5
#       )
#       
#     }
#     
#     sort(seq_breaks) -> seq_breaks
#     
#     # Axis labels
#     if(length(.fun_polar.labels)){
#       
#       do.call(
#         .fun_polar.labels
#         , list(seq_breaks
#         )) -> chr_labels
#       
#     }
#     
#     # Circular bar plot
#     .df_data %>% 
#       # ggplot
#       # Plot
#       ggplot() +
#       plt_geom + 
#       plt_labels +
#       plt_facets +
#       # Colors
#       plt_colors + 
#       # Axes
#       plt_axis.x +
#       plt_axis.y +
#       ylim(c(-0.55 * max(seq_breaks)), NA) + 
#       plt_coord + 
#       # Theme
#       # Void theme
#       theme_void() + 
#       theme(
#         legend.position = c(0.5,0.05)
#         , legend.direction = 'horizontal'
#         , panel.grid = element_blank()
#         , panel.border = element_blank()
#         , plot.margin = margin(0, 0, 0, 0)
#         # , plot.title = element_blank()
#         , plot.subtitle = element_blank()
#         , axis.title = element_blank()
#         , axis.text = element_blank()
#         , axis.ticks = element_blank()
#         , axis.line = element_blank()
#       ) +
#       plt_legend +
#       # Labels
#       plt_labs + 
#       # Polar coordinates
#       coord_polar(
#         start = -pi * int_NA / (int_NA + nrow(.df_data))
#       ) +
#       # y axis
#       Map(
#         function(y, y.label){
#           
#           annotate(
#             # x = as.character(round(int_NA / 2))
#             x = as.character(ceiling(int_NA / 2))
#             , y = y + max(seq_breaks) * 0.1
#             , label = y.label
#             , geom = 'text'
#             , color = '#212121'
#             , fontface = 'bold'
#             , size = 3
#           )
#           
#         }
#         , y = seq_breaks
#         , y.label = chr_labels
#       ) + 
#       annotate(
#         # x = as.character(round(int_NA / 2))
#         x = as.character(ceiling(int_NA / 2))
#         , y = -0.55 * max(seq_breaks)
#         , label = str_replace_all(
#           .list_labs$y
#           , ' ', '\n')
#         , geom = 'text'
#         , color = ifelse(
#           length(.list_geom.param$fill)
#           & !length(aes_mapping$fill)
#           , .list_geom.param$fill
#           , '#212121'
#         )
#         , fontface = 'bold'
#         , size = 4
#       ) -> plt_bar
#     
#     plt_bar$layers <- c(
#       geom_hline(
#         yintercept = seq_breaks[-length(seq_breaks)]
#         # , color = list_atlas.pal$grey
#         , color = 'lightgrey'
#         , size = 0.5
#       )
#       , plt_bar$layers
#     )
#     
#     plt_bar$layers <- c(
#       geom_hline(
#         yintercept = seq_breaks[length(seq_breaks)]
#         # , color = list_atlas.pal$grey
#         , color = 'lightgrey'
#         , size = 2
#       )
#       , plt_bar$layers
#     )
#     
#   }
#   
#   
#   # Output
#   return(plt_bar)
#   
# }

# LOLLIPOP CHART FUNCTION --------------------------------------------
fun_plot.lollipop <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = max
  
  # Labels
  , .list_labs = list()
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
    # , vjust = -1.25
    , vjust = -1.5
    # , hjust = -0.25
    , hjust = -0.33
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T, na.translate = F)
    , viridis::scale_fill_viridis(discrete = T, na.translate = F)
  )
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'color'
  # , .chr_manual.lim = NULL
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_discrete
  , .fun_axis.y = scale_y_continuous
  , .list_axis.x.args = list()
  , .list_axis.y.args = list(
    breaks = breaks_extended(5)
  )
  , .fun_format.x = function(x){str_wrap(x,10)}
  , .fun_format.y = label_number(accuracy = .01)
  
  # Coordinates
  , .dbl_limits.y = NULL
  , .coord_flip = T
  , .coord_polar = F
  
  # Theme
  , .theme = 
    ggridges::theme_ridges(
      center_axis_labels = T
      , font_size = 12
    ) + 
    theme(
      axis.text.y = element_text(vjust = 0.5)
      , legend.position = 'bottom'
      , legend.direction = 'horizontal'
    )
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = c('x', 'y')
  ) -> aes_mapping
  
  aes_mapping$x -> aes_mapping$xend
  
  aes_mapping$y -> aes_mapping$yend
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
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
  
  if(all(
    .reorder_fct
    , length(plt_facets)
  )){
    
    if(.reorder_desc){
      
      .df_data %>% 
        arrange(desc(!!aes_mapping$x)) %>%
        pull(!!enq_facets) %>%
        unique() -> chr_order
      
    } else { 
      
      .df_data %>% 
        arrange(!!aes_mapping$x) %>%
        pull(!!enq_facets) %>%
        unique() -> chr_order
      
    }
    
    .df_data %>% 
      mutate(
        !!enq_facets :=
          factor(
            !!enq_facets
            , levels = chr_order
          )
      ) -> .df_data
    
    # fun_reorder(
    #   .df_data = .df_data
    #   , .enq_var.fct = enq_facets
    #   , .enq_var.dbl = aes_mapping$x
    #   , .fun_ord = .reorder_fun
    #   , .desc = .reorder_desc
    # ) -> .df_data
    
  }
  
  # if(.reorder_fct & length(plt_facets)){
  #   
  #   fun_reorder(
  #     .df_data = .df_data
  #     , .enq_var.fct = enq_facets
  #     , .enq_var.dbl = aes_mapping$y
  #     , .fun_ord = .reorder_fun
  #     , .desc = .reorder_desc
  #   ) -> .df_data
  #   
  # }
  
  # Color mapping
  fun_colors(
    .scale_color = .scale_colors
    , .chr_manual.pal = .chr_manual.pal
    , .chr_manual.aes = .chr_manual.aes
    # , .chr_manual.lim = .chr_manual.lim
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
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
    , .aes_mapping = aes_mapping[c('x','xend','yend')]
  ) -> plt_segment
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot() +
    plt_segment +
    plt_geom +
    plt_labels +
    plt_facets +
    # Colors
    plt_colors +
    # Axes
    plt_axis.x +
    plt_axis.y +
    plt_coord +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_lollipop
  
  
  # Output
  return(plt_lollipop)
  
}

# DUMBBELL CHART FUNCTION --------------------------------------------
fun_plot.dumbbell <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = max
  
  # Labels
  , .labels = T
  , .fun_format.labels = function(x){
    number(x, accuracy = .01)
  }
  , .list_args.labels = list(
    accuracy = .01
  )
  , .list_labs = list()
  , .geom_label = F
  
  # Default parameters
  , .list_geom.param = list(
    color = 'lightgrey'
    , colour_x = '#182766'
    , colour_xend = '#4AF7B0'
    , size_x = 5.4
    , size_xend = 5.4
    , size = 2
  )
  , .list_labels1.param = list(
    fontface = 'bold'
    , color = '#182766'
    , size = 3.33
    , vjust = -1.5
    , hjust = 0.5
  )
  , .list_labels2.param = list(
    fontface = 'bold'
    , color = '#4AF7B0'
    , size = 3.33
    , vjust = 2.25
    , hjust = 0.5
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_continuous
  , .fun_axis.y = scale_y_discrete
  , .list_axis.x.args = list(
    breaks = breaks_extended(5)
  )
  , .list_axis.y.args = list()
  , .fun_format.x = label_number(accuracy = .01)
  , .fun_format.y = function(y){str_wrap(y,10)}
  
  # Coordinates
  , .dbl_limits.x = NULL
  , .coord_flip = F
  , .coord_polar = F
  
  # Theme
  , .theme = ggridges::theme_ridges(font_size = 12, center_axis_labels = T)
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  # Function
  if(!(
    !length(.fun_format.labels)
    | 'function' %in% tolower(class(.fun_format.labels))
  )){
    
    stop("'.fun_format.labels' must be a function for formatting labels.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = c('x','xend','y')
  ) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = aes_mapping$y
      , .enq_var.dbl = aes_mapping$x
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
  if(.reorder_fct & length(plt_facets)){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = enq_facets
      , .enq_var.dbl = aes_mapping$x
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.x = .dbl_limits.x
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
  if(!length(.list_labels1.param$position)){
    # Unless specified, use geom position as label position
    .list_labels1.param$position <- .list_geom.param$position
    
  }
  
  if(!length(.list_labels2.param$position)){
    # Unless specified, use geom position as label position
    .list_labels2.param$position <- .list_geom.param$position
    
  }
  
  # Labels
  if(.labels){
    
    # Compose individual aes() for each label
    aes(
      label = !!aes_mapping$x %>%
        .fun_format.labels
      , x = !!aes_mapping$x
      , y = !!aes_mapping$y
    ) -> aes_labels1
    
    aes(
      label = !!aes_mapping$xend %>%
        .fun_format.labels
      , x = !!aes_mapping$xend
      , y = !!aes_mapping$y
    ) -> aes_labels2
    
    # Call labels function
    fun_labels(
      .list_default = .list_labels1.param
      , .aes_mapping = aes_labels1
      # , .coord_flip = !.coord_flip
      , .geom_label = .geom_label
    ) -> plt_labels1
    
    fun_labels(
      .list_default = .list_labels2.param
      , .aes_mapping = aes_labels2
      # , .coord_flip = !.coord_flip
      , .geom_label = .geom_label
    ) -> plt_labels2
    
  } else {
    
    NULL -> plt_labels1
    
    NULL -> plt_labels2
    
  }
  
  
  # Dumbbell chart
  # geom_dumbbell
  fun_geom.params(
    .fun_geom = geom_dumbbell
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot() +
    plt_geom +
    plt_labels1 +
    plt_labels2 +
    plt_facets +
    # Axes
    plt_axis.x +
    plt_axis.y +
    plt_coord +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_dumbbell
  
  
  # Output
  return(plt_dumbbell)
  
}

# DUMBBELL CHART FUNCTION 2 --------------------------------------------
fun_plot.dumbbell2 <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = max
  
  # Labels
  , .labels = F
  , .fun_format.labels = function(x){
    number(x, accuracy = .01)
  }
  , .list_args.labels = list(
    accuracy = .01
  )
  , .list_labs = list()
  , .geom_label = F
  
  # Default parameters
  , .list_geom_line.param = list(
    color = 'lightgrey'
    , size = 2
  )
  , .list_geom_point.param = list(
    size = 5.4
  )
  , .list_labels1.param = list(
    fontface = 'bold'
    , color = '#182766'
    , size = 3.33
    , vjust = -1.5
    , hjust = 0.5
  )
  , .list_labels2.param = list(
    fontface = 'bold'
    , color = '#4AF7B0'
    , size = 3.33
    , vjust = 2.25
    , hjust = 0.5
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T, na.translate = F)
    , viridis::scale_fill_viridis(discrete = T, na.translate = F)
  )
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'color'
  
  # Legend
  , .list_legend = list()
  
  # Axes
  , .fun_axis.x = scale_x_continuous
  , .fun_axis.y = scale_y_discrete
  , .list_axis.x.args = list(
    breaks = breaks_extended(5)
  )
  , .list_axis.y.args = list()
  , .fun_format.x = label_number(accuracy = .01)
  , .fun_format.y = function(y){str_wrap(y,10)}
  
  # Coordinates
  , .dbl_limits.x = NULL
  , .coord_flip = F
  , .coord_polar = F
  
  # Theme
  , .theme = 
    ggridges::theme_ridges(
      center_axis_labels = T
      , font_size = 12
    ) + 
    theme(
      axis.text.y = element_text(vjust = 0.5)
      , legend.position = 'bottom'
      , legend.direction = 'horizontal'
    )
  
){
  
  # Errors
  # Data frame
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be a data frame.")
    
  }
  
  # Function
  if(!(
    !length(.fun_format.labels)
    | 'function' %in% tolower(class(.fun_format.labels))
  )){
    
    stop("'.fun_format.labels' must be a function for formatting labels.")
    
  }
  
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  
  # Aes Mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = c('x','color','y')
  ) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct){
    
    .df_data %>% 
      filter(
        !!aes_mapping$colour %in% (
          .df_data %>%
            slice_max(!!aes_mapping$x) %>% 
            pull(!!aes_mapping$colour)
        )
      ) -> chr_order
    
    if(!.reorder_desc){
      
      # Correct this code
      # Order = max with min diff to max diff
      
      chr_order %>%
        arrange(desc(!!aes_mapping$x)) %>%
        pull(!!aes_mapping$y) %>%
        unique() -> chr_order
      
    } else { 
      
      chr_order %>%
        arrange(!!aes_mapping$x) %>%
        pull(!!aes_mapping$y) %>%
        unique() -> chr_order
      
    }
    
    .df_data %>% 
      mutate(
        !!aes_mapping$y := 
          factor(
            !!aes_mapping$y
            , levels = chr_order
          )
        # , !!aes_mapping$colour := 
        #   fct_reorder(
        #     !!aes_mapping$colour
        #     , !!aes_mapping$x
        #     , .reorder_fun
        #     , .desc = .reorder_desc
        #   )
      ) -> .df_data
    
    # fun_reorder(
    #   .df_data = .df_data
    #   , .enq_var.fct = aes_mapping$y
    #   , .enq_var.dbl = aes_mapping$x
    #   , .fun_ord = .reorder_fun
    #   , .desc = .reorder_desc
    # ) -> .df_data
    
  }
  
  if(all(
    .reorder_fct
    , length(plt_facets)
  )){
    
    if(.reorder_desc){
      
      .df_data %>% 
        arrange(desc(!!aes_mapping$x)) %>%
        pull(!!enq_facets) %>%
        unique() -> chr_order
      
    } else { 
      
      .df_data %>% 
        arrange(!!aes_mapping$x) %>%
        pull(!!enq_facets) %>%
        unique() -> chr_order
      
    }
    
    .df_data %>% 
      mutate(
        !!enq_facets :=
          factor(
            !!enq_facets
            , levels = chr_order
          )
      ) -> .df_data
    
    # fun_reorder(
    #   .df_data = .df_data
    #   , .enq_var.fct = enq_facets
    #   , .enq_var.dbl = aes_mapping$x
    #   , .fun_ord = .reorder_fun
    #   , .desc = .reorder_desc
    # ) -> .df_data
    
  }
  
  # Color mapping
  fun_colors(
    .scale_color = .scale_colors
    , .chr_manual.pal = .chr_manual.pal
    , .chr_manual.aes = .chr_manual.aes
    # , .chr_manual.lim = .chr_manual.lim
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.x = .dbl_limits.x
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
  if(!length(.list_labels1.param$position)){
    # Unless specified, use geom position as label position
    .list_labels1.param$position <- .list_geom_point.param$position
    
  }
  
  if(!length(.list_labels2.param$position)){
    # Unless specified, use geom position as label position
    .list_labels2.param$position <- .list_geom_point.param$position
    
  }
  
  # Labels
  if(.labels){
    
    # Compose individual aes() for each label
    aes(
      label = !!aes_mapping$x %>%
        .fun_format.labels
      , x = !!aes_mapping$x
      , y = !!aes_mapping$y
    ) -> aes_labels1
    
    aes(
      label = !!aes_mapping$xend %>%
        .fun_format.labels
      , x = !!aes_mapping$xend
      , y = !!aes_mapping$y
    ) -> aes_labels2
    
    # Call labels function
    fun_labels(
      .list_default = .list_labels1.param
      , .aes_mapping = aes_labels1
      # , .coord_flip = !.coord_flip
      , .geom_label = .geom_label
    ) -> plt_labels1
    
    fun_labels(
      .list_default = .list_labels2.param
      , .aes_mapping = aes_labels2
      # , .coord_flip = !.coord_flip
      , .geom_label = .geom_label
    ) -> plt_labels2
    
  } else {
    
    NULL -> plt_labels1
    
    NULL -> plt_labels2
    
  }
  
  
  # Dumbbell chart
  # geom_line
  fun_geom.params(
    .fun_geom = geom_line
    , .list_default = .list_geom_line.param
    , .aes_mapping = aes(
      x = !!aes_mapping$x
      , y = !!aes_mapping$y
    )
  ) -> plt_line
  
  # geom_dumbbell
  fun_geom.params(
    .fun_geom = geom_point
    , .list_default = .list_geom_point.param
    , .aes_mapping = aes_mapping
  ) -> plt_point
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot() +
    plt_line +
    plt_point +
    plt_labels1 +
    plt_labels2 +
    plt_facets +
    # Colors
    plt_colors +
    # Axes
    plt_axis.x +
    plt_axis.y +
    plt_coord +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_dumbbell
  
  
  # Output
  return(plt_dumbbell)
  
}

# - Ridge plot function ---------------------------------------------------
fun_plot.ridges <- function(
    
  # Data
  .df_data
  , .aes_mapping
  
  # Reorder
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = median
  
  # Labels
  , .list_labs = list(y = NULL)
  , .geom_label = F
  
  # Default parameters
  , .list_geom.param = list(
    position = 'identity'
    , fill = '#3854FB'
    , color = '#212121'
    , size = 1.1
    , alpha = 0.8
  )
  
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  , .chr_scales = 'fixed'
  
  # Colors
  , .scale_colors = list(
    viridis::scale_color_viridis(discrete = T, na.translate = F)
    , viridis::scale_fill_viridis(discrete = T, na.translate = F)
  )
  , .chr_manual.pal = NULL
  , .chr_manual.aes = 'fill'
  # , .chr_manual.lim = NULL
  
  # Legend
  , .list_legend = list(fill = 'none')
  
  # Axes
  , .fun_axis.x = scale_x_continuous
  , .fun_axis.y = scale_y_discrete
  , .list_axis.x.args = list(
    breaks = breaks_extended(5)
  )
  , .list_axis.y.args = list(
    expand = c(0.01,0)
  )
  , .fun_format.x = label_number(accuracy = .01)
  , .fun_format.y = function(y){str_wrap(y,10)}
  
  # Coordinates
  , .dbl_limits.x = NULL
  , .coord_flip = F
  , .coord_polar = F
  
  # Theme
  , .theme = 
    ggridges::theme_ridges(
      center_axis_labels = T
      , font_size = 12
    ) + 
    theme(
      axis.text.y = element_text(vjust = 0.5)
      , legend.position = 'bottom'
      , legend.direction = 'horizontal'
    )
  
){
  
  # Arguments validation
  stopifnot(
    "'.df_data' must be a data frame." = 
      is.data.frame(.df_data)
  )
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  # Aes mapping
  fun_aes.map(
    .aes_mapping
    , .chr_required_aes = c('x', 'y')
  ) -> aes_mapping
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
    , .chr_scales = .chr_scales
  ) -> plt_facets
  
  # Reordering
  if(.reorder_fct){
    
    fun_reorder(
      .df_data = .df_data
      , .enq_var.fct = aes_mapping$y
      , .enq_var.dbl = aes_mapping$x
      , .fun_ord = .reorder_fun
      , .desc = .reorder_desc
    ) -> .df_data
    
  }
  
  if(all(
    .reorder_fct
    , length(plt_facets)
  )){
    
    if(.reorder_desc){
      
      .df_data %>% 
        arrange(desc(!!aes_mapping$y)) %>%
        pull(!!enq_facets) %>%
        unique() -> chr_order
      
    } else { 
      
      .df_data %>% 
        arrange(!!aes_mapping$y) %>%
        pull(!!enq_facets) %>%
        unique() -> chr_order
      
    }
    
    .df_data %>% 
      mutate(
        !!enq_facets :=
          factor(
            !!enq_facets
            , levels = chr_order
          )
      ) -> .df_data
    
  }
  
  # Color mapping
  fun_colors(
    .scale_color = .scale_colors
    , .chr_manual.pal = .chr_manual.pal
    , .chr_manual.aes = .chr_manual.aes
    # , .chr_manual.lim = .chr_manual.lim
  ) -> plt_colors
  
  # Axes format
  fun_axis.format(
    .fun_axis = .fun_axis.x
    , .list_axis.args = .list_axis.x.args
    , .fun_format = .fun_format.x
  ) -> plt_axis.x
  
  fun_axis.format(
    .fun_axis = .fun_axis.y
    , .list_axis.args = .list_axis.y.args
    , .fun_format = .fun_format.y
  ) -> plt_axis.y
  
  # Coordinates
  fun_coordinates(
    .dbl_limits.x = .dbl_limits.x
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
  # if(!length(.list_labels.param$position)){
  #   # Unless specified, use geom position as label position
  #   .list_labels.param$position <- .list_geom.param$position
  #   
  # }
  # 
  # # Labels
  # fun_labels(
  #   .list_default = .list_labels.param
  #   , .aes_mapping = aes_mapping
  #   , .coord_flip = .coord_flip
  #   , .geom_label = .geom_label
  # ) -> plt_labels
  
  # Ridges chart
  # geom_density_ridges with default parameters
  fun_geom.params(
    .fun_geom = geom_density_ridges
    , .list_default = .list_geom.param
    , .aes_mapping = aes_mapping
  ) -> plt_geom
  
  # ggplot
  .df_data %>%
    # Plot
    ggplot() +
    plt_geom +
    # plt_labels +
    plt_facets +
    # Colors
    plt_colors +
    # Axes
    plt_axis.x +
    plt_axis.y +
    plt_coord +
    # Theme
    plt_theme +
    plt_legend +
    # Labels
    plt_labs -> plt_ridges
  
  # Output
  return(plt_ridges)
  
}

# # TEST --------------------------------------------------------------------
# data("diamonds")
# 
# set.seed(123)
# mat1 = rbind(cbind(matrix(rnorm(50*5, mean = 1), nr = 50), 
#                    matrix(rnorm(50*5, mean = -1), nr = 50)),
#              cbind(matrix(rnorm(50*5, mean = -1), nr = 50), 
#                    matrix(rnorm(50*5, mean = 1), nr = 50))
# )
# rownames(mat1) = paste0("R", 1:100)
# colnames(mat1) = paste0("C", 1:10)
# mat1 = mat1[sample(100, 100), ] # randomly permute rows
# split = sample(letters[1:5], 100, replace = TRUE)
# split = factor(split, levels = letters[1:5])
# col_fun1 <- colorRamp2(c(-2, 0, 2), c("red", "white", "blue"))
# 
# circos.clear()
# 
# circos.heatmap(
#   mat = mat1
#   # , split = split
#   , col = col_fun1
# )
# 
# tibble(
#   a = LETTERS[1:5]
#   , b = runif(5, 0, 100)
#   , c = runif(5, 25, 50)
#   # , d = runif(5, 0, 80)
# ) %>%
#   fun_plot.dumbbell(
#     aes(
#       y = a
#       , x = b
#       , xend = c
#     )
#     , .dbl_limits.x = c(0,100)
#     , .reorder_fct = T
#     , .labels = T
#   )
# 
# diamonds %>%
#   group_by(clarity) %>% 
#   summarise(price = mean(price)) %>% 
#   mutate(clarity = paste0(clarity, 'dsdsds lalalala dsds')) %>% 
#   fun_plot.lollipop(aes(
#     x = clarity
#     , y = price
#     # , color = clarity
#     , label = dollar(price,.01)
#   )
#   , .fun_format.y = label_dollar(.01)
#   )
# 
# diamonds %>% 
#   fun_plot.density(aes(
#     x = price
#     , fill = clarity
#   )
#   , .sym_facets = clarity
#   , .int_facets = 4
#   , .reorder_fct = T
#   , .reorder_desc = T
#   , .dbl_limits.x = c(-1000,1000)
#   , .fun_format.x = dollar_format(accuracy = .01)
#   )
# 
# 
# diamonds %>%
#   group_by(cut, clarity) %>% 
#   summarise(price = mean(price)) %>%
#   ungroup() %>% 
#   fun_plot.heatmap(
#     aes(
#       x = cut
#       # , y = as.numeric(clarity)
#       , y = clarity
#       , fill = price
#       , label = dollar(price, accuracy = .01)
#     )
#     # , .sym_facets = c(cut, clarity)
#     , .coord_polar = F
#     , .reorder_fct = T
#     , .reorder_desc = T
#     , .reorder_fun = sum
#   )