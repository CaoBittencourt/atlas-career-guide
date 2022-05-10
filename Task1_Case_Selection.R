# PACKAGES -----------------------------------------------------------------
pkg <- c(
  # 'ggthemes', 'scales' #Visualization
  'readr', 'openxlsx' #Read and write utilities
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
df_askHR <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSubVYRgIbXdEIPXN-piZh33mz8xKtf69lkLFwjEmX3ppVB-v2ZaU-uMV5E9_SzcHVlD4n3Lz4TFy06/pub?gid=0&single=true&output=csv') 

df_askHR %>% glimpse()

# df_askHR %>% view()

# Unused column
df_askHR %>% 
  select(-`53`) -> df_askHR

# Trim white space
df_askHR %>%
  mutate(
    across(
      .cols = everything()
      , ~ str_trim(.x)
    )
  ) -> df_askHR

# SELECTION PARAMETERS ---------------------------------------------------
# title: max 25 words
# desc: max 100 words
# tags: max 3
# 4-5 responses

# NUMBER OF RESPONSES -----------------------------------------------------
# Responses <=> Columns
# => Min 4 columns, Max 5 columns

# Number of responses
df_askHR %>%
  mutate(
    N_Responses = apply(
      df_askHR %>% select(starts_with('Response'))
      , 1
      , function(x){
        sum(!is.na(x))
      }
    )
  ) -> df_askHR

# 4 <= Number of responses <= 5
# Remove excess responses later on
df_askHR %>%
  filter(
    N_Responses >= 4
    # , N_Responses <= 5
  ) -> df_askHR

# DESCRIPTION WORD COUNT --------------------------------------------------------------
# Word count
df_askHR %>%
  mutate(
    Desc_Word_Count = 
      
      # Apostrophes
      str_remove_all(Description, "\\'") %>%
      
      str_remove_all("\\’") %>%
      str_remove_all("\\‘") %>%
      
      str_remove_all("\\”") %>%
      str_remove_all("\\“") %>%
      
      # New lines
      str_remove_all('[\r\n]') %>%
      
      str_count('\\w+')
    
  ) -> df_askHR

# Word count <= 100
# Too limiting => Word count <= 400
df_askHR %>%
  filter(
    # Desc_Word_Count <= 300
    Desc_Word_Count <= 400
  ) -> df_askHR

# TITLE WORD COUNT --------------------------------------------------------------
# Word count
df_askHR %>%
  mutate(
    Title_Word_Count = 
      
      # Apostrophes
      str_remove_all(Title, "\\'") %>%
      
      str_remove_all("\\’") %>%
      str_remove_all("\\‘") %>%
      
      str_remove_all("\\”") %>%
      str_remove_all("\\“") %>%
      
      # New lines
      str_remove_all('[\r\n]') %>%
      
      str_count('\\w+')
    
  ) -> df_askHR

# Title word count <= 25
# Too limiting => Title word count <= 30
df_askHR %>%
  filter(
    # Title_Word_Count <= 25
    Title_Word_Count <= 30
  ) -> df_askHR


# NUMBER OF CASES -------------------------------------------------------------------------
df_askHR %>% 
  nrow(.)

# EXPORT FILTERED DATA -----------------------------------------------------------
df_askHR %>% 
  openxlsx::write.xlsx(
    file = 'Default_Mentorships_Filtered.xlsx'
    )
