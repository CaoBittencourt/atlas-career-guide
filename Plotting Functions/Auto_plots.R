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
# # DYNAMIC FACETS ----------------------------------------------------------
# fun_facets <- function(
    #     
#   .data
#   , .sym_facets = NULL
#   , .int_facets = NULL
#   
# ){
#   
#   # If empty, facets = NULL
#   if(!is_empty(.sym_facets)){
#     
#     # Max facets = 2
#     if(length(.sym_facets) > 2){
#       
#       .sym_facets[1:2] -> .sym_facets
#       
#     }
#     
#     # For facets = 2 => facet_grid
#     if(length(.sym_facets) > 1){
#       
#       facet_grid(
#         rows = vars(.data$.sym_facets[[1]])
#         , cols = vars(.data$.sym_facets[[2]])
#       ) -> plt_facets
#       
#     } else {
#       
#       # For facets = 1 => facet_wrap
#       facet_wrap(
#         facets = vars(.data$.sym_facets)
#         , ncol = .int_facets
#       ) -> plt_facets
#       
#     }
#     
#   } else {
#     
#     NULL -> plt_facets
#     
#   }
#   
#   return(plt_facets)
#   
# }

# # DYNAMIC FACETS ----------------------------------------------------------
# fun_facets <- function(
    # 
#   .sym_facets = NULL
#   , .int_facets = NULL
# 
# ){
# 
#   # If empty, facets = NULL
#   if(!is_empty(.sym_facets)){
# 
#     # Max facets = 2
#     if(length(.sym_facets) > 2){
# 
#       .sym_facets[1:2] -> .sym_facets
# 
#     }
# 
#     # For facets = 2 => facet_grid
#     if(length(.sym_facets) == 2){
# 
#       facet_grid(
#         rows = vars(!!sym(.sym_facets[[1]]))
#         , cols = vars(!!sym(.sym_facets[[2]]))
#       ) -> plt_facets
# 
#     } else {
# 
#       # For facets = 1 => facet_wrap
#       facet_wrap(
#         facets = vars(!!sym(.sym_facets))
#         , ncol = .int_facets
#       ) -> plt_facets
# 
#     }
# 
#   } else {
# 
#     NULL -> plt_facets
# 
#   }
# 
#   return(plt_facets)
# 
# }

# # DYNAMIC FACETS ----------------------------------------------------------
# fun_facets <- function(
    # 
#   .sym_facets = NULL
#   , .int_facets = NULL
# 
# ){
# 
#   # Quo vars
#   rlang::enquos(.sym_facets) -> enq_facets
# 
#   # If empty facets = NULL
#   if(!(length(enq_facets) == 0)){
# 
#     # Max facets = 2
#     if(length(.sym_facets) > 2){
# 
#       .sym_facets[[1]] %>%
#         enquo() -> enq_facets1
# 
#       .sym_facets[[2]] %>%
#         enquo() -> enq_facets2
# 
#     }
# 
#     # For facets = 2 => facet_grid
#     if(length(.sym_facets) == 2){
# 
#       facet_grid(
#         rows = vars(!!enq_facets1)
#         , cols = vars(!!enq_facets2)
#       ) -> plt_facets
# 
#     } else {
# 
#       # For facets = 1 => facet_wrap
#       facet_wrap(
#         facets = vars(!!enq_facets)
#         , ncol = .int_facets
#       ) -> plt_facets
# 
#     }
# 
#   } else {
# 
#     NULL -> plt_facets
# 
#   }
# 
#   return(plt_facets)
# 
# }

# DYNAMIC FACETS ----------------------------------------------------------
fun_facets <- function(
    
  .sym_facets = NULL
  , .int_facets = NULL
  
){
  
  # Max facets = 2
  if(length(.sym_facets) >= 2){
    
    .sym_facets[[1]] %>%
      # ensym() -> enq_facets1
      enquo() -> enq_facets1
    
    .sym_facets[[2]] %>%
      # ensym() -> enq_facets2
      enquo() -> enq_facets2
    
  }
  
  # For facets = 2 => facet_grid
  if(length(.sym_facets) >= 2){
    
    facet_grid(
      rows = vars(!!enq_facets1)
      , cols = vars(!!enq_facets2)
    ) -> plt_facets
    
  } else {
    
    .sym_facets %>%
      # ensym() -> enq_facets
      enquo() -> enq_facets
    
    # For facets = 1 => facet_wrap
    facet_wrap(
      facets = vars(!!enq_facets)
      , ncol = .int_facets
    ) -> plt_facets
    
  }
  
  return(plt_facets)
  
}

# DYNAMIC FACETS ----------------------------------------------------------
fun_facets <- function(
    
  .sym_facets = NULL
  , .int_facets = NULL
  
){
  
  # Max facets = 2
  # if(length(.sym_facets) >= 2){
  #   
  #   .sym_facets[[1]] -> .sym_facets1
  #   
  #   .sym_facets[[2]] -> .sym_facets2
  #   
  # }
  # 
  # # For facets = 2 => facet_grid
  # if(length(.sym_facets) >= 2){
  #   
  #   facet_grid(
  #     rows = vars({{.sym_facets1}})
  #     , cols = vars({{.sym_facets2}})
  #   ) -> plt_facets
  #   
  # } else {
    
    # For facets = 1 => facet_wrap
    facet_wrap(
      facets = vars({{.sym_facets}})
      , ncol = .int_facets
    ) -> plt_facets
    
  # }
  
  return(plt_facets)
  
}

# # DYNAMIC FACETS ----------------------------------------------------------
# fun_facets <- function(
    # 
#   .sym_facets = NULL
#   , .int_facets = NULL
# 
# ){
# 
#   # Max facetting vars = 2
#   # => If 2+ vars, get the first 2 vars
#   if(length(.sym_facets) > 2){
# 
#     .sym_facets[1:2] -> .sym_facets
# 
#   }
# 
#   # Quo vars
#   rlang::enquo(.sym_facets) -> enq_facets
# 
#   # If empty, facet = NULL
#   if(!rlang::quo_is_null(enq_facets)){
# 
#     if(length(.sym_facets) == 2){
# 
#       # Facet grid for 2 facetting vars
#       facet_grid(
#         rows = vars(!!enquo(.sym_facets[[1]]))
#         , cols = vars(!!enquo(.sym_facets[[2]]))
#       ) -> plt_facets
# 
#     } else {
# 
#       # Facet wrap for 1 facetting var
#       facet_wrap(
#         facets = vars(!!enq_facets)
#         , ncol = .int_facets
#       ) -> plt_facets
# 
#     }
# 
#   } else {
# 
#     NULL -> plt_facets
# 
#   }
# 
# }

# DYNAMIC COLORS ---------------------------------------------------


# DYNAMIC FILL ---------------------------------------------------


# DYNAMIC LABELS ----------------------------------------------------------


# DYNAMIC LEGENDS ----------------------------------------------------------


# ------- PLOTS -----------------------------------------------------------
# HISTOGRAM / DENSITY FUNCTION --------------------------------------------
fun_dist.plot <- function(
    
  # Data
  .data
  , .mapping
  # Plots
  , .density = T
  , .histogram = F
  , .int_bins = NULL
  # Facets
  , .chr_facets = NULL
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
  
  # return( enexprs(.chr_facets) )
  # return( ensyms(.chr_facets) )
  # return( enquo(.chr_facets) )
  
  # .chr_facets %>% 
  #   enquo() %>% 
  #   quo_get_expr() -> expr_facets
  
  # Facets
  # if(length( as_string(.chr_facets) ) > 1){
  if(length( as_label(enquo(.chr_facets)) ) > 1){

  facet_grid(
    # rows = vars(!!sym(.chr_facets[[1]]))
    rows = vars({{.chr_facets[[1]]}})
    , cols = vars({{.chr_facets[[2]]}})
  ) -> plt_facets

  } else {

    facet_wrap(
      facets = vars({{.chr_facets}})
      , ncol = .int_facets
    ) -> plt_facets

  }

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

    .data %>%
      ggplot(.mapping) +
      geom_density(size = 1.22) +
      plt_facets -> plt_density

  } else {

    NULL -> plt_density

  }

  # Histogram plot
  if(.histogram){

    .data %>%
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


# HISTOGRAM / DENSITY FUNCTION --------------------------------------------
fun_dist.plot <- function(
    
  # Data
  .data
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
  
  # Facets
  fun_facets(
    # .sym_facets = {{.sym_facets}}
    .sym_facets = .sym_facets
    , .int_facets = .int_facets
  ) -> plt_facets
  
  # # Max facets = 2
  # if(length(.sym_facets) >= 2){
  #   
  #   .sym_facets[[1]] %>%
  #     # ensym() -> enq_facets1
  #     enquo() -> enq_facets1
  #   
  #   .sym_facets[[2]] %>%
  #     # ensym() -> enq_facets2
  #     enquo() -> enq_facets2
  #   
  # }
  # 
  # # For facets = 2 => facet_grid
  # if(length(.sym_facets) >= 2){
  #   
  #   facet_grid(
  #     rows = vars(!!enq_facets1)
  #     , cols = vars(!!enq_facets2)
  #   ) -> plt_facets
  #   
  # } else {
  #   
  #   .sym_facets %>%
  #     # ensym() -> enq_facets
  #     enquo() -> enq_facets
  #   
  #   # For facets = 1 => facet_wrap
  #   facet_wrap(
  #     facets = vars(!!enq_facets)
  #     , ncol = .int_facets
  #   ) -> plt_facets
  #   
  # }
  
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
    
    .data %>%
      ggplot(.mapping) + 
      geom_density(size = 1.22) + 
      plt_facets -> plt_density
    
  } else {
    
    NULL -> plt_density 
    
  }
  
  # Histogram plot
  if(.histogram){
    
    .data %>%
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

# # HISTOGRAM / DENSITY FUNCTION --------------------------------------------
# fun_dist.plot <- function(
    #     
#   # Data
#   .data
#   , .mapping
#   # Plots
#   , .density = T
#   , .histogram = F
#   , .int_bins = NULL
#   # Facets
#   , .chr_facets = NULL
#   , .int_facets = NULL
#   # Theme
#   , .theme = 'hc'
#   , .chr_color = c(
#     'gdocs'
#     , 'viridis'
#     , 'plasma'
#     , 'magma'
#     , 'inferno'
#     , 'cividis'
#   )
#   , .labels = c(
#     'normal'
#     , 'percent'
#     , 'usd'
#     , 'brl'
#   )
#   
# ){
#   
#   # Eval
#   syms(.chr_facets) -> sym_facets
#   
#   
#   if(rlang::quo_is_null(sym_facets)){
#     
#     rows <- vars()
#     
#   } else {
#     rows <- vars(!!facet_quo)
#   }
#   
#   # Facets
#   if(!is_empty(.chr_facets)){
#     
#     if(length(.chr_facets) > 2){
#       
#       .chr_facets[1:2] -> .chr_facets
#       
#     }
#     
#     if(length(.chr_facets) > 1){
#       
#       facet_grid(
#         rows = vars(!!sym(.chr_facets[[1]]))
#         , cols = vars(!!sym(.chr_facets[[2]]))
#       ) -> plt_facets
#       
#     } else {
#       
#       facet_wrap(
#         facets = vars(!!sym(.chr_facets))
#         , ncol = .int_facets
#       ) -> plt_facets
#       
#     }
#     
#   } else {
#     
#     NULL -> plt_facets
#     
#   }
#   
#   # # Colors 
#   # if(length(.chr_color) > 1){
#   #   
#   #   .chr_color <- sample(.chr_color,1)
#   #   
#   # } else {
#   #   
#   #   case_when(
#   #     
#   #     .chr_color == 
#   #     
#   #   )
#   #   
#   # }
#   
#   
#   # Density plot
#   if(.density){
#     
#     .data %>%
#       ggplot(.mapping) + 
#       geom_density(size = 1.22) + 
#       plt_facets -> plt_density
#     
#   } else {
#     
#     NULL -> plt_density 
#     
#   }
#   
#   # Histogram plot
#   if(.histogram){
#     
#     .data %>%
#       ggplot(.mapping) + 
#       geom_histogram(bins = .int_bins) + 
#       plt_facets -> plt_histogram
#     
#   } else {
#     
#     NULL -> plt_histogram
#     
#   } 
#   
#   
#   return(
#     compact(
#       list(
#         'density' = plt_density
#         , 'histogram' = plt_histogram
#       )))
#   
# }
# 

# # HISTOGRAM / DENSITY FUNCTION --------------------------------------------
# fun_dist.plot <- function(
    #     
#   # Data
#   .data
#   , .mapping
#   # Plots
#   , .density = T
#   , .histogram = F
#   , .int_bins = NULL
#   # Facets
#   , .chr_facets = NULL
#   , .int_facets = NULL
#   # Theme
#   , .theme = 'hc'
#   , .chr_color = c(
#     'gdocs'
#     , 'viridis'
#     , 'plasma'
#     , 'magma'
#     , 'inferno'
#     , 'cividis'
#   )
#   , .labels = c(
#     'normal'
#     , 'percent'
#     , 'usd'
#     , 'brl'
#   )
#   
# ){
#   
#   # Eval
#   enquos(.chr_facets) -> sym_facets
#   
#   facet_grid(
#     rows = switch(
#       rlang::quo_is_null(sym_facets[[1]])
#       , vars(!!sym_facets[[1]])
#       , NULL
#     )
#     , cols = switch(
#       rlang::quo_is_null(sym_facets[[2]])
#       , vars(!!sym_facets[[2]])
#       , NULL
#     )
#   ) -> plt_facets
#   
#   return(plt_facets)
#   
#   #   if(rlang::quo_is_null(sym_facets)){
#   #     
#   #     rows <- vars()
#   #     
#   #   } else {
#   #     rows <- vars(!!facet_quo)
#   #   }
#   #   
#   #   # Facets
#   #   if(length(.chr_facets) > 1){
#   #     
#   #     facet_grid(
#   #       rows = vars(!!sym(.chr_facets[[1]]))
#   #       , cols = vars(!!sym(.chr_facets[[2]]))
#   #     ) -> plt_facets
#   #     
#   #   } else {
#   #     
#   #     facet_wrap(
#   #       facets = vars(!!sym(.chr_facets))
#   #       , ncol = .int_facets
#   #     ) -> plt_facets
#   #     
#   #   }
#   #   
#   # } else {
#   #   
#   #   NULL -> plt_facets
#   #   
#   # }
#   
#   # # Colors 
#   # if(length(.chr_color) > 1){
#   #   
#   #   .chr_color <- sample(.chr_color,1)
#   #   
#   # } else {
#   #   
#   #   case_when(
#   #     
#   #     .chr_color == 
#   #     
#   #   )
#   #   
#   # }
#   
#   
#   # # Density plot
#   # if(.density){
#   #   
#   #   .data %>%
#   #     ggplot(.mapping) + 
#   #     geom_density(size = 1.22) + 
#   #     plt_facets -> plt_density
#   #   
#   # } else {
#   #   
#   #   NULL -> plt_density 
#   #   
#   # }
#   # 
#   # # Histogram plot
#   # if(.histogram){
#   #   
#   #   .data %>%
#   #     ggplot(.mapping) + 
#   #     geom_histogram(bins = .int_bins) + 
#   #     plt_facets -> plt_histogram
#   #   
#   # } else {
#   #   
#   #   NULL -> plt_histogram
#   #   
#   # } 
#   # 
#   # 
#   # return(
#   #   compact(
#   #     list(
#   #       'density' = plt_density
#   #       , 'histogram' = plt_histogram
#   #     )))
#   
# }
# 

# TEST --------------------------------------------------------------------
data("diamonds")

diamonds %>% 
  fun_dist.plot(
    .mapping = aes(
      x = price
      , color = color)
    , .chr_facets = clarity
    # , .sym_facets = clarity
    , .int_facets = 4
  )

dsds %>% quo_get_expr() %>%  class()


fun_dist.plot(
  .data = diamonds
  , .mapping = aes(
    x = price
    , color = color)
  
  , .sym_facets = color
  , .int_facets = 4
)


fun_facets(
  .sym_facets = clarity
  , .int_facets = 4
)

lalala$density
lalala$histogram +
  facet_wrap(
    facets = vars(!!sym(NULL))
  )


gg_histo(facet = color) -> dsds

diamonds %>%
  fun_dist.plot(
    .mapping = aes(
      x = price
      , color = clarity
      , fill = clarity
    )
    # , .chr_facets = clarity
    , .density = F
    , .histogram = T
    , .chr_facets = c('color', 'clarity')
  ) -> dsds

fun_test <- function(df, vars){ 
  
  enquo(vars) %>% 
    return(.)  
  
}

diamonds %>%
  fun_test(vars = c(color, clarity)) -> dsds


dsds %>% summary()

dsds %>%
  # rlang::quo_get_expr()
  rlang::quo_name() 

diamonds %>% 
  ggplot(aes(
    x = price
    , fill = clarity
  )) + 
  geom_histogram() -> dsdsds

dsdsds + 
  labs(
    fill = 'Clarity'
    , x = 'Price (US$)'
    , y = 'Frequency'
    , title = 'Diamond Price vs Clarity'
    , subtitle = 'How does diamond clarity affect prices?'
  )

facet_grid(
  rows = vars(!!sym('color'))
  , cols = vars(!!sym('clarity'))
) -> kkk

dsdsds + kkk

# gg_histo(
#   facet = clarity
# )
# facet_wrap(
#   # facets = vars(diamonds$'clarity')
#   # facets = diamonds$'clarity'
#   # facets = vars(diamonds[['clarity']])
#   # facets = diamonds[['clarity']]
#   # facets = vars(diamonds['clarity']) #no
#   # facets = vars(diamonds$clarity)
#   facets = diamonds$clarity
#   # facets = vars(diamonds[[clarity]]) #no
#   , ncol = 4
# )
