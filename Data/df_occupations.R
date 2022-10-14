# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'labelled', 'tidyverse' #Data wrangling
  # , 'blsR' #BLS API
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

map(pkg, packageVersion)

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# DATA --------------------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Employment data frame
df_employment <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2VjvaVX0WrPJcuTtfYL5E4yZ6OmijSL961ytjRtxCPHb2JInjOKSHqq-pGg_m7g/pub?gid=873564137&single=true&output=csv')

# LABELS ------------------------------------------------------------------
# Occupations labels vector
scan(
  url('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=1223197022&single=true&output=csv')
  , sep = ','
  , what = ''
  , quiet = T
) -> chr_occupations.labels

# Apply labels
if(ncol(df_occupations) == length(chr_occupations.labels)){
  
  df_occupations %>%
    labelled::set_variable_labels(
      .labels = chr_occupations.labels
    ) -> df_occupations
  
} else {
  
  stop("The number of labels must be same as the number of columns in the occupations data frame.")
  
}

# Employment labels vector
scan(
  url('https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2VjvaVX0WrPJcuTtfYL5E4yZ6OmijSL961ytjRtxCPHb2JInjOKSHqq-pGg_m7g/pub?gid=1864959979&single=true&output=csv')
  , sep = ','
  , what = ''
  , quiet = T
) -> chr_employment.labels

# Apply labels
if(ncol(df_employment) == length(chr_employment.labels)){
  
  df_employment %>%
    labelled::set_variable_labels(
      .labels = chr_employment.labels
    ) -> df_employment
  
  
} else {
  
  stop("The number of labels must be same as the number of columns in the employment data frame.")
  
}

# -------- DATA --------------------------------------------
# OCCUPATIONS DATA FRAME -------------------------------------------------------
# Select only necessary variables
df_occupations %>% 
  mutate(code = substr(code, 1, 7)) %>% 
  select(
    occupation
    , code
    , entry_level_education
    , annual_wage_2021
    , ends_with('.l') #Using recommended levels
  ) %>% 
  mutate(
    across(
      .cols = ends_with('.l') #Using recommended levels
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# EMPLOYMENT DATA FRAME ---------------------------------------------------
df_employment %>% 
  rename(
    code = OCC_CODE
    , employment = TOT_EMP
  ) %>% 
  select(
    code
    , employment
  ) %>% 
  group_by(code) %>%
  summarise(
    employment = max(employment)
  ) %>% 
  right_join(
    df_occupations
  ) -> df_occupations

# # MISSING OCCUPATIONS -----------------------------------------------------
# df_occupations %>% 
#   filter(
#     code %in% 
#       setdiff(
#         df_occupations$code
#         , df_employment$OCC_CODE
#       )
#   ) %>% 
#   select(occupation) %>% view
# 
# setdiff(
#   df_employment$OCC_CODE
#   , df_occupations$code
# )

# POPULATION-WEIGHTED OCCUPATIONS DATA FRAME -------------------------------------------------------
df_occupations %>% 
  drop_na() %>% 
  group_by(code) %>% 
  mutate(employment2 = employment / n()) %>% 
  ungroup() %>%
  mutate(
    employment2 = employment2 / min(employment2, na.rm = T)
    , employment2 = round(employment2)
    ) %>% 
  group_by(occupation) %>%
  slice(rep(1:n(), first(employment2))) %>% 
  ungroup() -> df_occupations.pop
