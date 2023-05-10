# [SETUP] -------------------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# Load all the packages that will be used in every file
# This way, packages are loaded only once
pkg <- c(
  'aws.s3' #Work with AWS Lambda and R
  , 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Files --------------------------------------------------------------
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
  # Occupations data frame
  'occupations.csv'
  # Factors
  # , 'list_factors.R'
) -> list_data

# Render files
list(
  # Render matching report
  'render_to_bucket.R'
) -> list_render


# - Buckets -------------------------------------------------------------
# List of function files buckets
list(
  # Only one s3 bucket
  'BUCKETNAME'
) -> list_bucket.fun

# List of data files buckets
list(
  # Only one s3 bucket
  's3://atlas-platform-dev/db/'
) -> list_bucket.data

# List of render files buckets
list(
  # Only one s3 bucket
  'BUCKETNAME'
) -> list_bucket.render

# [S3 SOURCE] ----------------------------------------------------------
# - Credentials -------------------------------------------------------------
# Key
chr_key <- 'AKIAYAUF6OUP25FNYCFX'

# Secret
chr_secret <- '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'

# Region
chr_region <- 'us-east-2'

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
    , region = chr_region
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
    , region = chr_region
  )
)

s3source(
  object = 'https://atlas-platform-dev.s3.us-east-2.amazonaws.com/R/fun_plots.R'
  , bucket = 's3://atlas-platform-dev/R/'
  , key = chr_key
  , secret = chr_secret
  , region = chr_region 
)

save_object(
  'db/occupations-v2.0.0.csv'
  , file = 'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.2023.3.csv'
  , bucket = list_bucket.data[[1]]
  , key = chr_key
  , secret = chr_secret
  , region = chr_region
)

save_object(
  'occupations.csv'
  , file = 'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.2023.2.csv'
  , bucket = 's3://atlas-platform-prod/db/'
  , key = chr_key
  , secret = chr_secret
  , region = chr_region
)



read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.2023.csv'
) -> df_occupations.2023

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.2023.2.csv'
) -> df_occupations.2023.2

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.2023.3.csv'
) -> df_occupations.2023.3


dplyr::setdiff(
  df_occupations.2023.3$name, 
  df_occupations.2023$name
)

df_occupations.2023 %>% view
df_occupations.2023 %>% names


map2(
  .x = list_data
  , .y = list_bucket.data
  , ~ aws.s3::save_object(
    object = .x
    , bucket = get_bucket(.y)
    , key = chr_key
    , secret = chr_secret
    , region = chr_region
  )
)
