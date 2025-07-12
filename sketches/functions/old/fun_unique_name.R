# --- RANDOM NAME FUNCTION ---------------------------------------------------------
# GENERATE A UNIQUE RANDOM NAME -------------------------------------------
fun_name.unique <- function(
    int_chr = 8
    , int_num = 8
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
  sum(rbinom(
    n = int_chr
    , size = 1
    , prob = 0.5
    )) -> int_upper
  
  sample(
    x = LETTERS
    , size = int_upper
    , replace = T
    ) -> chr_upper
  
  sample(
    x = letters
    , size = int_chr - int_upper
    , replace = T
    ) -> chr_lower
  
  # Generate random numbers
  sample(
    x = seq(0,9,1)
    , size = int_num
    , replace = T
  ) -> int_numbers
  
  # Mix it up
  paste0(
    sample(
      x = c(chr_upper, chr_lower, int_numbers)
      , replace = F
    )
    , collapse = ''
  ) -> chr_unique
  
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
