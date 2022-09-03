# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'ggthemes' #Data visualization
  # , 'hrbrthemes', 'extrafont' #Data visualization
  # , 'ggthemr' #Data visualization
  , 'tidyverse', 'glue', 'rlang' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# # FONTS -------------------------------------------------------------------
# font_import(prompt = F)
# loadfonts(device = 'win')
# hrbrthemes::
# hrbrthemes::import_roboto_condensed()

# ------- PLOT ELEMENTS ---------------------------------------------------
# DYNAMIC FACETS ----------------------------------------------------------
fun_facets <- function(
    
  .enq_facets = NULL
  , .int_facets = NULL
  
){
  
  if(!is_quosure(.enq_facets)){
    
    stop("'.enq_facets' must be a quosure.")
    
  }
  
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
  
  } else {
    
    # For facets = 1 => facet_wrap
    facet_wrap(
      facets = vars(!!.enq_facets)
      , ncol = .int_facets
    ) -> plt_facets
    
  }
  
  return(plt_facets)
  
}

# DYNAMIC COLORS ---------------------------------------------------


# DYNAMIC FILL ---------------------------------------------------


# DYNAMIC LABELS ----------------------------------------------------------


# DYNAMIC LEGENDS ----------------------------------------------------------


# ------- PLOTS -----------------------------------------------------------
# HISTOGRAM / DENSITY FUNCTION --------------------------------------------
fun_dist.plot <- function(
    
  # Data
  .df_data
  , .mapping
  # Plots
  , .density = T
  , .histogram = F
  , .int_bins = NULL
  # Facets
  , .sym_facets = NULL
  , .int_facets = NULL
  # Theme
  , .theme = 'hc'
  , .chr_color = c(
    'gdocs'
    , 'viridis'
    , 'plasma'
    , 'magma'
    , 'inferno'
    , 'cividis'
  )
  , .labels = c(
    'normal'
    , 'percent'
    , 'usd'
    , 'brl'
  )
  
){
  
  # Errors 
  if(!is.data.frame(.df_data)){
    
    stop("'.df_data' must be either a data frame.")
    
  }
  
  if(!is.logical(.density)){
    
    stop("'.density' must be either TRUE or FALSE.")
    
  }
  
  if(!is.logical(.histogram)){
    
    stop("'.histogram' must be either TRUE or FALSE.")
    
  }
  
  # if(
  #   !(is.null(.int_facets)) |
  #   !(is.numeric(.int_facets))
  # ){
  #   
  #   stop("'.int_facets' must be an integer.")
  #   
  # }
  # 
  # if(
  #   !(is.null(.int_facets)) |
  #   !(is.numeric(.int_bins))){
  #   
  #   stop("'.int_bins' must be an integer.")
  #   
  # }
  
  # Integers
  if(is.numeric(.int_facets)){
    
    .int_facets %>% 
      round() -> .int_facets
    
  }
  
  # Integers
  if(is.numeric(.int_bins)){
    
    .int_bins %>% 
      round() -> .int_bins
    
  }
  
  # Quo vars
  enquo(.sym_facets) -> enq_facets
  
  # Facets
  fun_facets(
    .enq_facets = enq_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # # Colors
  # if(length(.chr_color) > 1){
  #
  #   .chr_color <- sample(.chr_color,1)
  #
  # } else {
  #
  #   case_when(
  #
  #     .chr_color ==
  #
  #   )
  #
  # }
  
  
  # Density plot
  if(.density){
    
    .df_data %>%
      ggplot(.mapping) +
      geom_density(size = 1.22) +
      plt_facets -> plt_density
    
  } else {
    
    NULL -> plt_density
    
  }
  
  # Histogram plot
  if(.histogram){
    
    .df_data %>%
      ggplot(.mapping) +
      geom_histogram(bins = .int_bins) +
      plt_facets -> plt_histogram
    
  } else {
    
    NULL -> plt_histogram
    
  }
  
  
  return(
    compact(
      list(
        'density' = plt_density
        , 'histogram' = plt_histogram
      )))
  
}


# TEST --------------------------------------------------------------------
data("diamonds")

diamonds %>% 
  fun_dist.plot(
    .mapping = aes(
      x = price
      , color = color
      )
    , .sym_facets = c(clarity, color)
    , .int_facets = 4
  )
