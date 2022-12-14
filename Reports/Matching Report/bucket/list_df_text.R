# --- SETUP -----------------------------------------------------------
# DATA --------------------------------------------------------------------
# DEFAULT TEXTS FOR IMPUTATION
map(
  excel_sheets('./career_finder_report.xlsx')
  , ~ read_excel('./career_finder_report.xlsx', sheet = .x)
) -> list_df_text

names(list_df_text) <- excel_sheets('./career_finder_report.xlsx')

# --- DATA -----------------------------------------------------------
# WRANGLING -------------------------------------------------------------------
# Remove carriage returns
list_df_text %>%
  map(function(df){
    
    df %>% 
      mutate(across(
        where(is.character)
        , ~ str_remove_all(.x, "\r") %>% 
          str_remove_all("\\\\n") %>% 
          str_replace_all("\n", "  \n")
      ))
    
  }) -> list_df_text

# SECTION LIST ------------------------------------------------------------
# Section list
list_df_text$sections$text %>% 
  as.list() -> list_sections

names(list_sections) <- list_df_text$sections$section
