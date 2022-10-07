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

# KFLEX FUNCTION ------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')

# DATA --------------------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# # Employed workers data frame
# df_workers <- readr::read_csv('C:/Users/Cao/Documents/Github/Atlas-Research/Atlas_database.csv')

# Select only necessary variables
df_occupations %>%
  select(
    occupation
    , ends_with('.l')
  ) %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# df_workers %>% view()
#   select(
#     title
#     , quick_facts.qf_number_of_jobs.value
#   ) %>% 
#   rename(
#     occupation = title
#     , workers = quick_facts.qf_number_of_jobs.value
#   ) -> df_workers
#   
#   
# df_workers %>% 
#   filter(str_detect(tolower(title), 'agricult'))
# 
# df_workers %>% 
#   group_by(occupation) %>% 
#   tally() %>% 
#   arrange(desc(n)) -> dsds
# 
# df_occupations %>% 
#   group_by(occupation) %>% 
#   tally() -> dsds
# 
# all(dsds$n == 1)

# # POPULATION-WEIGHTED DATA FRAME ------------------------------------------
# df_occupations %>% 
#   left_join(df_workers)
# 
#   mutate(workers = work.force / pmin(workers, na.rm = T)) %>% 
#   group_by(occupation) %>% 
#   slice(1:workers)

# -------- CAPITAL FLEXIBLITY ---------------------------------------------
# APPLY FUNCTION -----------------------------------------------------------
df_occupations %>%
  summarise(
    across(
      .cols = where(is.numeric)
      ,.fns = fun_capital.flex
    )) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'attribute'
    , values_to = 'capital.flex'
  ) -> df_kflex.long

df_kflex.long %>% 
  arrange(desc(capital.flex))

# ATTRIBUTE PROPORTION ----------------------------------------------------
df_occupations %>% 
  mutate(
    across(where(is.numeric)) 
    / rowSums(across(where(is.numeric)))
  ) -> df_occupations.pct

# FLEXIBLE CAPITAL PER OCCUPATION -----------------------------------------
df_occupations.pct %>% 
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'attribute'
    , values_to = 'level.pct'
  ) %>% 
  full_join(
    df_kflex.long
  ) %>% 
  group_by(
    occupation
  ) %>% 
  summarise(
    capital.flex.pct = sum(capital.flex * level.pct)
  ) %>% 
  arrange(desc(capital.flex.pct)) -> df_occupations.kflex
