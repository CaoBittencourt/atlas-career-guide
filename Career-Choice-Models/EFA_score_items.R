# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'tidyverse', 'glue' #Data wrangling
  , 'psych', 'GPArotation' #Factor analysis
  # , 'ctv' #Most relevant psychometrics packages
  , 'paletteer' #Palettes for visualization
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})



# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models')

# KNN MATCHING ---------------------------------------------------------------
source('./KNN_Matching.R')

# SKILLS FACTOR LIST -----------------------------------------------------------------
list_skill.factors <- list(
  'General' = c(
    # Factor 1 is composed of cognitive, non-technical, general skills (general competencies)
    'Judgment_and_Decision.L'
    , 'Complex_Problem_Solving.L'
    , 'Active_Learning.L'
    , 'Critical_Thinking.L'
  )
  , 'Technical' = c(
    # Factor 2 is composed of mechanical, hands-on, specialist skills (technical)
    'Equipment_Selection.L'
    , 'Troubleshooting.L'
    , 'Repairing.L'
    , 'Equipment_Maintenance.L'
  )
  
)

# lapply(
#   list_skill.factors
#   , function(x){paste0('+',x)}
#   ) -> list_skill.factors

# ABILITIES FACTOR LIST ---------------------------------------------------
list_ablt.factors <- list(
  'Perception' = c(
    # Factor 1 is composed of perceptual abilities (perception):
    'Night_Vision.L'
    , 'Sound_Localization.L'
    , 'Glare_Sensitivity.L'
  )
  , 'Dexterity' = c(
    # Factor 2 is composed of manual abilities (dexterity):
    'Finger_Dexterity.L'
    , 'Arm_Hand_Steadiness.L'
    , 'Manual_Dexterity.L'
  )
  , 'Robustness' = c(
    # Factor 3 is composed of bodily robustness, potency, and coordination (overall body robustness)
    'Stamina.L'
    , 'Gross_Body_Coordination.L'
    , 'Trunk_Strength.L'
  )
  , 'Intelligence' = c(
    # Factor 4 is composed of cognitive abilities (intelligence):
    'Inductive_Reasoning.L'
    , 'Problem_Sensitivity.L'
    , 'Deductive_Reasoning.L'
  ) 
)

# KNOWLEDGE FACTOR LIST ---------------------------------------------------
list_know.factors <- list(
  'Health' = c(
    # Factor 1 is composed of health-related fields of knowledge (health / help).
    'Therapy_and_Counseling.L'
    , 'Psychology.L'
    , 'Medicine_and_Dentistry.L'
  )
  , 'Building' = c(
    # Factor 2 is composed of engineering / building-related fields of knowledge (build).
    'Physics.L'
    , 'Engineering_and_Technology.L'
    , 'Building_and_Construction.L'
  ) 
  , 'Business' = c(
    # Factor 3 is composed of financial and enterprising fields of knowledge (FGV).
    'Economics_and_Accounting.L'
    , 'Sales_and_Marketing.L'
    , 'Administration_and_Management.L'
  )
  , 'Arts_Humanities' = c(
    # Factor 4 is composed of arts and humanities (communists).
    'History_and_Archeology.L'
    , 'Geography.L'
    , 'Fine_Arts.L'
  ) 
)


# ALL FACTORS LIST -------------------------------------------------------------
list_factors <- list(
  'Skills' = list_skill.factors
  # , 'Abilities' = list_ablt.factors
  , 'Knowledge' = list_know.factors
)

# EFA-REDUCED OCCUPATIONS DATA FRAME -------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Matching data frame
df_occupations %>%
  select(
    where(
      negate(is.numeric)
    )
    , all_of(
      list_factors %>%
        flatten() %>% 
        flatten_chr()
      #   c(
      #   flatten_chr(list_skill.factors)
      #   , flatten_chr(list_ablt.factors)
      #   , flatten_chr(list_know.factors)
      # )
    )
  ) %>%
  mutate(
    across(
      .cols = all_of(
        list_factors %>%
          flatten() %>% 
          flatten_chr()
        #   c(
        #   flatten_chr(list_skill.factors)
        #   , flatten_chr(list_ablt.factors)
        #   , flatten_chr(list_know.factors)
        # )
      )
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

df_occupations %>%
  # Select only highly qualified professions
  filter(
    Entry_level_Education %in% c(
      "Bachelor's degree"
      , "Doctoral or professional degree"
      , "Associate's degree"
      , "Master's degree"
    )
  ) -> df_occupations



# EFA-REDUCED QUERY VECTOR (USER INPUT) -----------------------------------------------
# User questionnaires data frame
df_input <- read_csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv'))

df_input %>% 
  select(
    Name
    , all_of(
      list_factors %>%
        flatten() %>% 
        flatten_chr()
      #   c(
      #   flatten_chr(list_skill.factors)
      #   , flatten_chr(list_ablt.factors)
      #   , flatten_chr(list_know.factors)
      # )
    )
  ) %>%  
  mutate(
    across(
      .cols = all_of(
        list_factors %>%
          flatten() %>% 
          flatten_chr()
        #   c(
        #   flatten_chr(list_skill.factors)
        #   , flatten_chr(list_ablt.factors)
        #   , flatten_chr(list_know.factors)
        # )
      )
      , .fns = function(x){
        recode(x
               , '1' = .0
               , '2' = .25
               , '3' = .5
               , '4' = .75
               , '5' = 1
        )}
    )
  ) -> df_input

# Keep only completed questionnaires
df_input %>% 
  drop_na() -> df_input


# Vector of names
chr_names <- df_input$Name
names(chr_names) <- df_input$Name

# SCORE ITEMS (OCCUPATIONS) -----------------------------------------------
psych::scoreVeryFast(
  keys = list_factors %>% flatten()
  
  , items = df_occupations %>%
    select(
      all_of(
        list_factors %>%
          flatten() %>% 
          flatten_chr()
      )
    )
  , totals = F #Average scores
) %>% 
  as_tibble() %>% 
  bind_cols(
    df_occupations
  ) %>% 
  select(
    where(
      negate(is.numeric)
    )
    , list_factors %>%
      flatten() %>% 
      names()
  ) -> df_occupations.scores

# SCORE ITEMS (INPUT)-------------------------------------------------------------
lapply(
  chr_names
  , function(name){
    
    lapply(
      list_factors
      , function(scales){
        
        psych::scoreVeryFast(
          keys = scales
          
          , items = df_input %>%
            filter(Name == name) %>% 
            select(flatten_chr(scales))
          
          , totals = F #Average scores
        ) %>% 
          as_tibble() %>% 
          colMeans()
        
      } 
    )
  } %>% flatten_df()
) -> list_factor.scores

# KNN MATCHING ON FACTOR SCORES ---------------------------------------------------------------
lapply(
  list_factor.scores
  , function(factor_scores){
    
    fun_KNN.matching(
      .df_data.numeric = df_occupations.scores
      , .vec_query.numeric = factor_scores
      , .int_k = nrow(df_occupations)
    ) %>% 
      return(.)
    
  }) -> list_KNN.output

list_KNN.output$Milena %>% view()
list_KNN.output$Martijn %>% 
  select(
    Occupation
    , list_factors %>%
      flatten() %>%
      names()
    , starts_with('Similarity.')
  ) %>% 
  view()

list_KNN.output$Gabriel  %>% 
  select(
    Occupation
    , list_factors %>%
      flatten() %>%
      names()
    , starts_with('Similarity.')
  ) %>% 
  view()

list_KNN.output$Acilio %>% view()
list_KNN.output$Cao %>% view()

