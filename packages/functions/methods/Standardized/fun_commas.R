# [SETUP] ----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  # 'tidyverse', 'stringi' #Data wrangling
  'purrr', 'dplyr', 'stringi' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTIONS] ------------------------------------------------------
# - Quotes and commas function ---------------------------------------------------
fun_text_commas <- function(
    ...
    , chr_sep = ',' 
    , chr_last_sep = ', and'
    , lgc_quote = T
){
  
  # Dynamic dots
  c(...) -> chr_text
  
  # Arguments validation
  stopifnot(
    "'lgc_quote' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_quote),
        !is.na(lgc_quote)
      )
  )
  
  # Coerce into character
  map(chr_text, as.character) -> chr_text
  
  paste(chr_sep, '') -> chr_sep
  
  paste(chr_last_sep, '') -> chr_last_comma
  
  # Add quotes and commas
  if(lgc_quote){
    
    chr_text %>% 
      paste0('"', ., '"') %>% 
      paste0(
        collapse = 
          chr_sep
      ) -> chr_text
    
  } else {
    
    chr_text %>% 
      paste0(
        collapse = 
          chr_sep
      ) -> chr_text
  }
  
  if(length(chr_last_comma)){
    
    chr_text %>%
      stri_replace_last_fixed(
        chr_sep, 
        chr_last_comma
      ) -> chr_text
  }
  
  # Output
  return(chr_text)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Test ------------------------------------------------------------------
# fun_text_commas(1:5)