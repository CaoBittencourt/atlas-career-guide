# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# -------- FUNCTION ------------------------------------------------------
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
