# --- SETUP -----------------------------------------------------------
# DATA --------------------------------------------------------------------
# DEFAULT TEXTS FOR IMPUTATION
# setwd(
#   'C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report'
# )
map(
  excel_sheets('./career_finder_report2.xlsx')
  , ~ read_excel(
    './career_finder_report2.xlsx'
    , sheet = .x
    , trim_ws = F
  )
) -> list_df.text

names(list_df.text) <- excel_sheets('./career_finder_report2.xlsx')

# --- DATA -----------------------------------------------------------
# WRANGLING -------------------------------------------------------------------
# Remove carriage returns
list_df.text %>%
  map(function(df){
    
    df %>% 
      mutate(across(
        where(is.character)
        , ~ str_remove_all(.x, "\r") %>% 
          str_remove_all("\\\\n") %>% 
          str_replace_all("\n", "  \n")
      ))
    
  }) -> list_df.text
