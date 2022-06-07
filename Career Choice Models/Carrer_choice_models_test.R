# PACKAGES -----------------------------------------------------------------
pkg <- c(
  # 'ggthemes', 'scales' #Visualization
  'readr', 'readxl','openxlsx' #Read and write utilities
  , 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research')

# DATA --------------------------------------------------------------------
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=1060672467&single=true&output=csv') 

xl <- readxl::read_excel('C:/Users/Cao/Documents/Github/Atlas-Research/test_Df.xlsx')

xl %>% glimpse()
xl %>% View()

# sorry didn't realise the X has gone as well. "\\..\\d*" should work



# df_occupations %>% glimpse()

# df_occupations %>% view()

# Unused column
# df_occupations %>% 
  # select(-`53`) -> df_occupations

# Trim white space
# df_occupations %>%
#   mutate(
#     across(
#       .cols = everything()
#       , ~ str_trim(.x)
#     )
#   ) -> df_occupations
