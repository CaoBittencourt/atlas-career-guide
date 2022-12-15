# ------ HANDLER FUNCTION -------------------------------------------------
fun_handler <- function(body, ...){
  
  # ------ CODE -------------------------------------------------------------
  # --- SETUP -------------------------------------------------------------------------
  # PACKAGES ----------------------------------------------------------------
  # Load all the packages that will be used in every file
  # This way, packages are loaded only once
  pkg <- c(
    'psych' #Factor Analysis
    , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
    , 'jsonify' #Work with JSON (faster than jsonlite)
    , 'ggthemes' #Data visualization
    , 'tidyverse', 'stringi', 'english' #Data wrangling
    , 'tinytex' #LaTeX
    , 'modeest' #Mode
    , 'knitr' #Knitr
    , 'readxl' #Import excel (use other package?)
    , 'aws.s3' #Work with AWS Lambda and R
    # , 'lambdr'
  )
  
  # Activate / install packages
  lapply(pkg, function(x)
    if(!require(x, character.only = T))
    {install.packages(x); require(x)})
  
  # Install TinyTex
  if(!tinytex::is_tinytex()){
    tinytex::install_tinytex()
  }
  
  # Package citation
  # lapply(pkg, function(x)
  #   {citation(package = x)})
  
  # FILES --------------------------------------------------------------
  # Function files
  list(
    # KNN matching
    'fun_matching.R'
    # Factor scores
    , 'fun_scores.R'
    # Automated plotting
    , 'fun_plots.R'
    # Dynamic text
    , 'fun_texts.R'
    # Capital flexibility
    , 'fun_kflex.R'
    # Random unique ID
    , 'fun_unique_name.R'
  ) -> list_fun
  
  # Data files
  list(
    # Default texts
    'list_df_text.R'
    # Factors
    , 'list_factors.R'
  ) -> list_data
  
  # Render files
  list(
    # Render matching report
    'render_to_bucket.R'
  ) -> list_render
  
  
  # BUCKETS -------------------------------------------------------------
  # List of function files buckets
  list(
    # Only one s3 bucket
    'BUCKETNAME'
  ) -> list_bucket.fun
  
  # List of data files buckets
  list(
    # Only one s3 bucket
    'BUCKETNAME'
  ) -> list_bucket.data
  
  # List of render files buckets
  list(
    # Only one s3 bucket
    'BUCKETNAME'
  ) -> list_bucket.render
  
  # OUTPUT NAME -------------------------------------------------------------
  fun_name.unique(chr_suffix = '.pdf') -> chr_file.name
  
  # --- S3 SOURCE (SETUP) ----------------------------------------------------------
  # CREDENTIALS -------------------------------------------------------------
  # Key
  chr_key <- 'dsds'
  
  # Secret
  chr_secret <- 'lalala'
  
  # RUN SETUP FILES FROM S3 BUCKET ------------------------------------------------
  # Source function files
  map2(
    .x = list_fun
    , .y = list_bucket.fun
    , ~ s3source(
      object = .x
      , bucket = get_bucket(.y)
      , key = chr_key
      , secret = chr_secret
    )
  )
  
  # Source data files
  map2(
    .x = list_data
    , .y = list_bucket.data
    , ~ s3source(
      object = .x
      , bucket = get_bucket(.y)
      , key = chr_key
      , secret = chr_secret
    )
  )
  
  # --- DATA ----------------------------------------------------------------
  # EFA-REDUCED OCCUPATIONS DATA FRAME -----------------------------------------------
  df_occupations <- read_csv('./occupations.csv')
  
  # EFA-REDUCED QUERY VECTOR -----------------------------------------------
  # User questionnaire data frame
  from_json(body) %>%
    as_tibble() %>% 
    select(
      user_name
      , all_of(
        list_factors %>%
          flatten() %>%
          flatten_chr()
      )
    ) %>%
    mutate(
      across(
        .cols = all_of(
          list_factors %>%
            flatten() %>%
            flatten_chr()
        )
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
  
  # --- S3 SOURCE (RENDER) ----------------------------------------------------------
  # RENDER RMD FROM S3 BUCKET ------------------------------------------------
  # Source render files
  map2(
    .x = list_render
    , .y = list_bucket.render
    , ~ s3source(
      object = .x
      , bucket = get_bucket(.y)
      , key = chr_key
      , secret = chr_secret
    )
  )
  
  # ------ OUTPUT -----------------------------------------------------------
  return(list(
    "statusCode" = 200
    , "headers" = list("Content-Type" = "application/json")
    , "body" = list('filename' = chr_file.name)
  ))
  
}

