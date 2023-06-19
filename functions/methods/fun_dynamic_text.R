# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'tidyverse', 'glue' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# -------- FUNCTIONS ------------------------------------------------------
# # DYNAMIC TEXT FUNCTION ---------------------------------------------------
# fun_text.dynamic <- function(.chr_text, .chr_pattern, ...){
#   
#   # Dynamic dots
#   list2(...) -> list_imput
#   
#   
#   # Character
#   if(!(!length(.chr_text) | is.character(.chr_text))){
#     
#     stop("'.chr_text' must be a character.")
#     
#   }
#   
#   if(!(!length(.chr_pattern) | is.character(.chr_pattern))){
#     
#     stop("'.chr_pattern' must be a character.")
#     
#   }
#   
#   # Coerce into character
#   map(list_imput, as.character) -> list_imput
#   
#   
#   # Replace blanks
#   map_chr(
#     list_imput
#     , function(imput){
#       
#       str_replace(
#         .chr_text
#         , .chr_pattern
#         , imput
#       ) ->> .chr_text
#       
#     }
#   ) 
#   
#   
#   # Output
#   return(.chr_text)
#   
# }

# DYNAMIC TEXT DATA FRAME FUNCTION --------------------------------------
fun_text.dynamic <- function(
    
  # Data frame
  .df_text = tibble()
  # Input list
  , .list_input = list()
  # NA action
  , .chr_na = ''
  
){
  
  # Data types
  stopifnot(
    "'.df_text' must be a data frame." = 
      is.data.frame(.df_text)
  )
  
  stopifnot(
    "'.list_input' must be a list of textual inputs." = 
      c(
        is.list(.list_input)
        , !is.data.frame(.list_input)
      )
  )
  
  stopifnot(
    "'.chr_na' must be either NULL or a character element." =
      !length(.chr_na) |
      length(.chr_na) &
      is.character(.chr_na)
  )
  
  # Glue texts
  .df_text %>% 
    rowwise() %>%
    mutate(across(
      .cols = !where(is.numeric)
      ,.fns = 
        ~ glue_data(
          .list_input
          , .x
          , .na = .chr_na
          , .trim = T
        )
      # ,.fns = function(text){
      #   
      #   glue_data(
      #     .list_input
      #     , text
      #     , .na = .chr_na
      #     , .trim = T
      #   )
      #   
      # })) %>%
      )) %>%
    ungroup() %>% 
    return()
  
}
