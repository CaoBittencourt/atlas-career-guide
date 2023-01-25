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
# QUOTES AND COMMAS FUNCTIONS ---------------------------------------------------
fun_text.commas <- function(
    ...
    , .chr_sep = ', ' 
    , .chr_last.sep = ', and '
    , .lgc_quote = T
){
  
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
  
  as.character(.chr_sep) -> chr_sep
  
  as.character(.chr_last.sep) -> chr_last.comma
  
  
  # Add quotes and commas
  if(.lgc_quote){
    
    chr_text %>% 
      paste0('"',., '"') %>% 
      paste0(collapse = chr_sep) -> chr_text
    
  } else {
    
    chr_text %>% 
      paste0(collapse = chr_sep) -> chr_text
  }
  
  if(length(chr_last.comma)){
    
    chr_text %>%
      stri_replace_last_fixed(chr_sep, chr_last.comma) -> chr_text
  }
  
  
  # Output
  return(chr_text)
  
}
