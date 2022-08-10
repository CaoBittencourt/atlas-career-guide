# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
  , 'jsonify' #Work with JSON (faster than jsonlite)
  , 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# KNN MATCHING FUNCTION -------------------------------------------------------------------------
fun_KNN.matching <- function(
    .df_data.numeric
    , .vec_query.numeric
    , .int_k = 1
    , .auto_select.k = F
    , .imput.over_qualification = T
    , .dbl_over_qualification.threshold = 0.1
    , .dbl_decimals = 4
){
  
  # Get numeric data only
  .df_data.numeric %>%
    select(where(is.numeric)) -> .df_data.numeric.temp
  
  if(is.data.frame(.vec_query.numeric)){
    .vec_query.numeric %>%
      select(where(is.numeric)) -> .vec_query.numeric
  }
  
  # Define k
  if(.auto_select.k){
    # RECOMMENDED
    # Typical suggested value for k is sqrt(nrow(df))
    # Looking for k nearest neighbors in all career clusters
    
    .df_data.numeric %>%
      nrow(.) %>%
      sqrt(.) %>%
      round(.) -> .int_k
    
  }
  
  if(.imput.over_qualification){
    
    .vec_query.numeric %>%
      rename_with(
        .fn = function(x){paste0(x,'.imput')}
      ) %>%
      bind_cols(
        .df_data.numeric.temp
      ) %>%
      mutate(
        across(
          .cols = c(
            !ends_with('.imput')
          )
          ,.fns = function(x){
            
            ifelse(
              # Overqualified if > cutoff and requirement <= cutoff
              x <= .dbl_over_qualification.threshold 
              & eval(sym(paste0(cur_column(),'.imput'))) > x
              , yes = x
              , no = eval(sym(paste0(cur_column(),'.imput')))
            )
            
          }
          , .names = '{col}.sub'
        )
      ) %>%
      select(
        ends_with('.sub')
      ) %>%
      rename_with(
        function(x){str_remove(x,'.sub')}
      ) -> .vec_query.numeric
    
    lapply(
      1:nrow(.vec_query.numeric)
      , function(x){
        
        FNN::get.knnx(
          data = .df_data.numeric.temp[x,]
          , query = .vec_query.numeric[x,]
          , k = 1
        ) -> KNN.output
        
      }) %>% 
      bind_rows() -> KNN.output 
    
    # # Find the k nearest neighbors
    # FNN::get.knnx(
    #   data = .df_data.numeric.temp
    #   , query = .vec_query.numeric
    #   , k = 1
    # ) -> KNN.output
    
    # KNN.output$nn.index[,1] -> KNN.output$nn.index
    # 
    # KNN.output$nn.dist[,1] -> KNN.output$nn.dist
    
    # Arrange original data frame with KNN output
    .df_data.numeric %>%
      mutate(#Add euclidean distances and convert them to similarities
        Euclidean_Distance = as.vector(KNN.output$nn.dist)
        
        , Similarity = 1 - (pmin(Euclidean_Distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
        
        , across(
          .cols = starts_with('Similarity.')
          ,.fns = function(x){round(x,.dbl_decimals)}
        )
        
      ) %>%
      arrange(Euclidean_Distance) %>% 
      return(.)
    
  } else {
    
    # Find the k nearest neighbors
    FNN::get.knnx(
      data = .df_data.numeric.temp
      , query = .vec_query.numeric
      , k = .int_k
    ) -> KNN.output
    
    # Arrange original data frame with KNN output
    .df_data.numeric %>%
      slice(as.vector(KNN.output$nn.index)) %>%
      mutate(#Add euclidean distances and convert them to similarities
        Euclidean_Distance = as.vector(KNN.output$nn.dist)
        
        , Similarity = 1 - (pmin(Euclidean_Distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
        
        , across(
          .cols = starts_with('Similarity.')
          ,.fns = function(x){round(x,.dbl_decimals)}
        )
        
      ) %>% 
      arrange(Euclidean_Distance) %>% 
      return(.)
    
  }
  
}

# SKILLS FACTOR LIST -----------------------------------------------------------------
list_skill.factors <- list(
  'General' = c(
    # Factor 1 is composed of cognitive, non-technical, general skills (general competencies)
    'Judgment_and_Decision.I'
    , 'Active_Learning.I'
    , 'Critical_Thinking.I'
  )
  , 'Technical' = c(
    # Factor 2 is composed of mechanical, hands-on, specialist skills (technical)
    'Equipment_Selection.I'
    , 'Troubleshooting.I'
    , 'Operations_Monitoring.I'
  )
)

# ABILITIES FACTOR LIST ---------------------------------------------------
list_ablt.factors <- list(
  'Perception' = c(
    # Factor 1 is composed of perceptual abilities (perception):
    'Night_Vision.I'
    , 'Sound_Localization.I'
    , 'Glare_Sensitivity.I'
  )
  , 'Dexterity' = c(
    # Factor 2 is composed of manual abilities (dexterity):
    'Finger_Dexterity.I'
    , 'Arm_Hand_Steadiness.I'
    , 'Control_Precision.I'
  )
  , 'Robustness' = c(
    # Factor 3 is composed of bodily robustness, potency, and coordination (overall body robustness)
    'Stamina.I'
    , 'Gross_Body_Coordination.I'
    , 'Trunk_Strength.I'
  )
  , 'Intelligence' = c(
    # Factor 4 is composed of cognitive abilities (intelligence):
    'Inductive_Reasoning.I'
    , 'Problem_Sensitivity.I'
    , 'Deductive_Reasoning.I'
  )
)

# KNOWLEDGE FACTOR LIST ---------------------------------------------------
list_know.factors <- list(
  'Health' = c(
    # Factor 1 is composed of health-related fields of knowledge (health / help).
    'Therapy_and_Counseling.I'
    , 'Psychology.I'
    , 'Medicine_and_Dentistry.I'
  )
  , 'Building' = c(
    # Factor 2 is composed of engineering / building-related fields of knowledge (build).
    'Physics.I'
    , 'Engineering_and_Technology.I'
    , 'Mechanical.I'
  ) 
  , 'Business' = c(
    # Factor 3 is composed of financial and enterprising fields of knowledge (FGV).
    'Economics_and_Accounting.I'
    , 'Sales_and_Marketing.I'
    , 'Administration_and_Management.I'
  )
  , 'Arts_Humanities' = c(
    # Factor 4 is composed of arts and humanities (communists).
    'History_and_Archeology.I'
    , 'Geography.I'
    , 'Fine_Arts.I'
  ) 
)

# ALL FACTORS LIST -------------------------------------------------------------
list_factors <- list(
  'Skills' = list_skill.factors
  , 'Abilities' = list_ablt.factors
  , 'Knowledge' = list_know.factors
)

# EFA-REDUCED OCCUPATIONS DATA FRAME -------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Select only necessary variables
df_occupations %>%
  select(
    Occupation
    , Entry_level_Education #Filter will be applied later on in Bubble via user input
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
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# EFA-REDUCED QUERY VECTOR (JSON) -----------------------------------------------
# User questionnaires data frame
df_input <- read_csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv'))

# df_input %>% 
#   to_json() -> dsds
# 
# from_json(dsds) %>% 
#   as_tibble() -> df_input

df_input %>% 
  filter(Name == 'Martijn') -> df_input

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
               , '1' = .0
               , '2' = .25
               , '3' = .5
               , '4' = .75
               , '5' = 1
        )}
    )
  ) -> df_input

# KNN MATCHING ---------------------------------------------------------------
fun_KNN.matching(
  .df_data.numeric = df_occupations
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = 0
  , .dbl_decimals = 4
) -> df_KNN.output

# CONVERT OUTPUT TO JSON --------------------------------------------------
df_KNN.output %>% 
  select(
    Occupation
    , Entry_level_Education
    , starts_with('Similarity')
  ) %>% 
  to_json(digits = 4) %>% 
  pretty_json() -> JSON_KNN.output

# P.S.: filtering based on "Entry_level_Education" should be applied via user input in Bubble