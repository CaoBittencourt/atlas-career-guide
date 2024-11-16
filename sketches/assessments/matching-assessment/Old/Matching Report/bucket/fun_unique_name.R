# --- FUNCTION -----------------------------------------------------------
# --- RANDOM NAME FUNCTION ---------------------------------------------------------
fun_name.unique <- function(
    int_length = 16
    , chr_preffix = NULL
    , chr_suffix = NULL
    , chr_sep = '_'
){
  
  # Timestamp
  Sys.time() -> posixct_timestamp
  
  gsub(' ', as.character(chr_sep), posixct_timestamp) -> chr_timestamp
  gsub(':', '-', chr_timestamp) -> chr_timestamp
  
  # Set seed
  runif(1) * 1000 * as.numeric(posixct_timestamp) -> dbl_seed
  
  # Generate random characters
  stri_rand_strings(1, int_length) -> chr_unique
  
  paste0(
    chr_timestamp
    , as.character(chr_sep)
    , as.character(chr_preffix)
    , chr_unique
    , as.character(chr_suffix)
  ) -> chr_unique
  
  # Output
  return(chr_unique)
  
}
