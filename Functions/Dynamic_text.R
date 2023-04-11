# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'tidyverse', 'stringi' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# -------- FUNCTIONS ------------------------------------------------------
# DYNAMIC TEXT FUNCTION ---------------------------------------------------
fun_text.dynamic <- function(.chr_text, .chr_pattern, ...){
  
  # Dynamic dots
  list2(...) -> list_imput
  
  
  # Character
  if(!(!length(.chr_text) | is.character(.chr_text))){
    
    stop("'.chr_text' must be a character.")
    
  }
  
  if(!(!length(.chr_pattern) | is.character(.chr_pattern))){
    
    stop("'.chr_pattern' must be a character.")
    
  }
  
  # Coerce into character
  map(list_imput, as.character) -> list_imput
  
  
  # Replace blanks
  map_chr(
    list_imput
    , function(imput){
      
      str_replace(
        .chr_text
        , .chr_pattern
        , imput
      ) ->> .chr_text
      
    }
  ) 
  
  
  # Output
  return(.chr_text)
  
}

# QUOTES AND COMMAS FUNCTIONS ---------------------------------------------------
fun_text.commas <- function(..., .chr_last.comma = ', and ', .lgc_quote = T){
  
  # Dynamic dots
  c(...) -> chr_text
  
  
  # Logical
  if(!(
    is.logical(.lgc_quote) &
    !is.na(.lgc_quote)
  )){
    
    stop("'.lgc_quote' must be either TRUE or FALSE.")
    
  }
  
  
  # Coerce into character
  map(chr_text, as.character) -> chr_text
  
  as.character(.chr_last.comma) -> chr_last.comma
  
  
  # Add quotes and commas
  if(length(chr_text)){
    
    if(.lgc_quote){
      
      chr_text %>% 
        paste0('"',., '"') %>% 
        paste0(collapse = ', ') -> chr_text
      
    } else {
      
      chr_text %>% 
        paste0(collapse = ', ') -> chr_text
    }
    
    if(length(chr_last.comma)){
      
      chr_text %>%
        stri_replace_last_fixed(', ', chr_last.comma) -> chr_text
    }
    
  } else { 
    
    '' -> chr_text
    
  }
  
  
  # Output
  return(chr_text)
  
}
