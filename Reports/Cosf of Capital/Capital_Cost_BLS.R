# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
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

# # Employed workers data frame
# df_workers <- readr::read_csv('C:/Users/Cao/Documents/Github/Atlas-Research/Atlas_database.csv')

# Select only necessary variables
df_occupations %>%
  select(
    occupation
    , annual_wage_2021
    , ends_with('.l')
  ) %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# -------- CAPITAL FLEXIBLITY ---------------------------------------------
# COST OF CAPITAL PER OCCUPATION -----------------------------------------
df_occupations %>%
  pivot_longer(
    cols = -c(occupation, annual_wage_2021)
    , names_to = 'attribute'
    , values_to = 'level'
  ) %>% 
  group_by(
    occupation
  ) %>% 
  mutate(
    capital.cost.12m = annual_wage_2021 * (level  / sum(level))
    , capital.cost.m = capital.cost.12m / 12
  ) %>%
  ungroup() -> df_kcost.long
