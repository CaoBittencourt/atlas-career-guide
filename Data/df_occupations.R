# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  # 'labelled', 
  'tidyverse' #Data wrangling
  # , 'openxlsx' #Export excel
  # , 'blsR' #BLS API
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# DATA --------------------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# # LABELS ------------------------------------------------------------------
# # Occupations labels vector
# scan(
#   url('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=1223197022&single=true&output=csv')
#   , sep = ','
#   , what = ''
#   , quiet = T
# ) -> chr_occupations.labels
# 
# # Apply labels
# if(ncol(df_occupations) == length(chr_occupations.labels)){
#   
#   df_occupations %>%
#     labelled::set_variable_labels(
#       .labels = chr_occupations.labels
#     ) -> df_occupations
#   
# } else {
#   
#   stop("The number of labels must be same as the number of columns in the occupations data frame.")
#   
# }

# -------- DATA --------------------------------------------
# OCCUPATIONS DATA FRAME -------------------------------------------------------
# Select only necessary variables
df_occupations %>% 
  mutate(code = substr(code, 1, 7)) %>% 
  group_by(code) %>% 
  mutate(
    code.variants = n()
    , .after = code
  ) %>%
  ungroup() %>% 
  select(
    occupation
    , code
    , code.variants
    , career_cluster
    , entry_level_education
    , typical_on_the_job_training
    , annual_wage_2021
    , projected_growth_2020.2030
    , ends_with('.l') #Using recommended levels
  ) %>% 
  mutate(
    across(
      .cols = ends_with('.l') #Using recommended levels
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# -------- EXPORT ----------------------------------------------------
# # XLSX --------------------------------------------------------------------
# df_occupations %>% 
#   select(
#     code
#     , code.variants
#     , occupation
#     , annual_wage_2021
#   ) %>% 
#   write.xlsx(
#     'df_occupations.xlsx'
#   )
