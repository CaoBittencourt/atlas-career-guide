# ------ CODE -------------------------------------------------------------
# --- SETUP -------------------------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
# Load all the packages that will be used in every file
# This way, packages are loaded only once
pkg <- c(
  'jsonify' #Work with JSON (faster than jsonlite)
  , 'ggthemes' #Data visualization
  , 'tidyverse' #Data wrangling
  , 'scales'
  , 'viridis'
  , 'jsonify'
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# library(grid)

# BUCKETS -------------------------------------------------------------
chr_bucket.fun <- 's3://atlas-platform-dev/'
chr_bucket.data <- 's3://atlas-platform-dev/'

# # OUTPUT NAME -------------------------------------------------------------
# save_object(
#   'R/fun_unique_name.R'
#   , file = '/tmp/fun_unique_name.R'
#   , bucket = chr_bucket.fun
#   , region = 'us-east-2'
# )
# 
# source('/tmp/fun_unique_name.R')
# 
# fun_name.unique(
#   chr_suffix = '.png'
# ) -> chr_file.name

# ------ HANDLER FUNCTION -------------------------------------------------
fun_handler <- function(body, ...){  
  
  # FILES --------------------------------------------------------------
  # --- DATA ----------------------------------------------------------------
  # OCCUPATIONS DATA FRAME -----------------------------------------------
  # Download data frame
  save_object(
    'db/occupations.csv'
    , file = '/tmp/occupations.csv'
    , bucket = chr_bucket.data
    , region = 'us-east-2'
  )
  
  # Read data frame
  read_csv(
    '/tmp/occupations.csv'
    , show_col_types = F
  ) %>% 
    mutate(
      across(
        .cols = where(is.numeric)
        , .fns = function(x){x/100}
      )
    ) -> df_occupations
  
  # --- S3 SOURCE (SETUP) ----------------------------------------------------------
  # File name
  save_object(
    'R/fun_unique_name.R'
    , file = '/tmp/fun_unique_name.R'
    , bucket = chr_bucket.fun
    , region = 'us-east-2'
  )
  
  source('/tmp/fun_unique_name.R')
  
  # Automated plots 
  save_object(
    'R/fun_plots.R'
    , file = '/tmp/fun_plots.R'
    , bucket = chr_bucket.fun
    , region = 'us-east-2'
  )
  
  source('/tmp/fun_plots.R')
  
  # EFA-REDUCED QUERY VECTOR -----------------------------------------------
  # User questionnaire data frame
  from_json(body) %>%
    as_tibble() %>% 
    # select(
    #   name #not user_name
    #   , occupation
    #   , all_of(
    #     list_factors %>%
    #       flatten() %>%
    #       flatten_chr()
    #   )
    # ) %>%
    mutate(
      across(
        .cols = where(is.numeric)
        , .fns = function(x){
          recode(x
                 , '1' = 0.00
                 , '2' = 0.17
                 , '3' = 0.33
                 , '4' = 0.50
                 , '5' = 0.67
                 , '6' = 0.83
                 , '7' = 1.00
          )}
      )
    ) -> df_input
  
  # --- SETUP -----------------------------------------------------------
  # PARAMETERS  --------------------------------------------------------------
  # Colors
  list(
    'green' = '#4AF7B0'
    , 'purple1' = '#753AF9'
    , 'purple2' = '#301866'
    , 'purple3' = '#3854FB'
    , 'blue1' = '#56D0F5'
    , 'blue2' = '#ABF4D4'
    , 'blue3' = '#43DED1'
    , 'blue4' = '#182766'
    , 'red' = '#CE3527'
    , 'black' = '#212121'
    , 'grey' = '#D4D5D8'
  ) -> list_atlas.pal
  
  # --- DUMBBELL PLOT --------------------------------------------------------
  fun_cognitive.dumbbell(
    .df_data.user = 
      df_input %>% 
      select(!c(1,2)) %>% 
      slice_sample(n = 1) %>% 
      mutate(
        occupation = sample(df_occupations$occupation, 1)
        , occupation.title = occupation
        , .after = 1
      ) %>%
      rename_with(
        ~ str_to_lower(.x)
      ) %>% 
      select(
        1:3
        , flatten_chr(list_abilities.cognitive)
      )
    , .df_data.comparison = 
      df_occupations %>%
      mutate(
        occupation.title = occupation
        , .after = occupation
      )
  )
  
  # ------ OUTPUT -----------------------------------------------------------
  #filename <- chr_file.name
  return(list(
    "statusCode" = 200
    , "headers" = list("Content-Type" = "application/json")
    , "body" = pretty_json(paste0('{ "filename": "', chr_s3_object , '" }' ))
  ))
  
}