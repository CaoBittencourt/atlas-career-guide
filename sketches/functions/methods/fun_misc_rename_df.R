# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'dplyr', 'stringr' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTIONS] -------------------------------------------------------------
# - Rename data frame -----------------------------------------------------
fun_misc_rename_df <- function(df_data){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame." = 
      is.data.frame(df_data)
  )
  
  # Rename columns
  df_data %>% 
    rename_with(
      .cols = everything()
      , .fn = ~ .x %>% 
        str_trim() %>%
        str_to_lower() %>%
        str_replace_all(
          '\\.', '_'
        ) %>%
        str_replace_all(
          ' ', '_'
        ) %>%
        str_replace_all(
          '-', '_'
        ) %>% 
        str_replace_all(
          '/', '_'
        ) %>% 
        str_remove_all(
          ','
        ) %>%
        str_remove_all(
          "'"
        )
    ) -> df_data
  
  # Output
  return(df_data)
  
}
