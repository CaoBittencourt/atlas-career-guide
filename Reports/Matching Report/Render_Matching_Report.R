# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'psych' #Factor Analysis
  , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
  , 'jsonify' #Work with JSON (faster than jsonlite)
  , 'ggthemes' #Data visualization
  , 'tidyverse', 'glue' #Data wrangling
  , 'tinytex' #LaTeX
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

# FUNCTIONS ---------------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/KNN_Matching.R')
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')

# PARAMETERS --------------------------------------------------------------
# Selected respondent
# chr_user <- 'Martijn'
chr_user <- 'Cao'
# chr_user <- 'Gabriel'

# KNN parameters
# dbl_threshold <- 0.33
# dbl_threshold <- 0
dbl_threshold <- 0.17

# DATA --------------------------------------------------------------------
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.EFA.R')

# EFA-REDUCED POPULATION-WEIGHTED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')

# USER INPUT DATA FRAME
df_input <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv')

# ------- DATA -----------------------------------------------------------
# EFA-REDUCED OCCUPATIONS DATA FRAME -----------------------------------------------
# Select only necessary variables
df_occupations %>% 
  select(
    occupation
    , entry_level_education
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
  # ) -> df_occupations.test2
  ) -> df_occupations.test

df_occupations.test %>%
  slice(1) %>% 
  select(all_of(
    list_factors %>%
      flatten() %>%
      flatten_chr()
  )) %>% 
  mutate(
    across(
      .cols = everything()
      ,.fns = function(x){
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
  )

# EFA-REDUCED QUERY VECTOR -----------------------------------------------
# Select user
df_input %>% 
  filter(Name == chr_user) -> df_input

# EFA-reduced data frame
df_input %>% 
  select(
    all_of(
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

# ------- RESULTS --------------------------------------------------------
# KNN MATCHING ---------------------------------------------------------------
fun_KNN.matching(
  .df_data.numeric = df_occupations.test
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = dbl_threshold
  , .dbl_decimals = 4
) -> df_KNN.output

df_KNN.output %>% 
  # df_occupations %>%
  filter(
    entry_level_education %in% c(
      "Bachelor's degree"
      , "Doctoral or professional degree"
      # , "Associate's degree"i
      , "Master's degree"
    )
  ) %>%
  View()

# FACTOR SCORES (USER) -----------------------------------------------------------
fun_factor.scores(
  .df_data.numeric = df_input
  , .list_factor.keys = list_factors
) -> list_factor.scores




lapply(
  list_factors
  , function(scales){
    
    psych::scoreVeryFast(
      keys = scales
      , items = df_input 
      , totals = F #Average scores
    ) %>% 
      as_tibble() %>% 
      colMeans()
    
  } 
) %>% 
  flatten_df() -> df_factor.scores

df_input %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'score'
  ) %>% 
  full_join(
    df_factors.names
  ) -> df_input.long

df_factor.scores %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'factor'
    , values_to = 'factor.score'
  ) %>% 
  full_join(
    df_input.long
  ) -> df_input.long

# # FACTOR SCORES (POPULATION) -----------------------------------------------------------
# lapply(
#   list_factors
#   , function(scales){
# 
#     psych::scoreVeryFast(
#       keys = scales
#       , items = df_occupations.pop
#       , totals = F #Average scores
#     ) %>%
#       as_tibble() %>%
#       colMeans()
# 
#   }
# ) %>%
#   flatten_df() -> df_occupations.scores
# 
# df_occupations.pop %>%
#   pivot_longer(
#     cols = everything()
#     , names_to = 'item'
#     , values_to = 'score'
#   ) %>%
#   full_join(
#     df_factors.names
#   ) -> df_input.long
# 
# df_factor.scores %>%
#   pivot_longer(
#     cols = everything()
#     , names_to = 'factor'
#     , values_to = 'factor.score'
#   ) %>%
#   full_join(
#     df_input.long
#   ) -> df_input.long

# DYNAMIC TEXT ------------------------------------------------------------
# Report Title
chr_report.title <- glue('Professional Profile — {chr_user}')

# Numbers for dynamic reporting with R Markdown

# Captions for dynamic reporting with R Markdown

# PLOTS -------------------------------------------------------------------


df_input.long %>% 
  filter(
    competency == 'Skills'
  ) %>% 
  ggplot(aes(
    x = item
    , y = score
    , fill = factor
  )) + 
  geom_col() + 
  facet_wrap(
    facets = vars(factor)
  ) + 
  guides(
    fill = F
  )

# RENDER R MARKDOWN REPORT --------------------------------------------------
rmarkdown::render('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Factor_Scores_Report.Rmd')
