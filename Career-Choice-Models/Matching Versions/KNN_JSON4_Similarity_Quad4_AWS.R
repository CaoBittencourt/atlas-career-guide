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

# HANDLER FUNCTION --------------------------------------------------------
handler <- function(body, ...){
  # SKILLS FACTOR LIST -----------------------------------------------------------------
  list_skill.factors <- list(
    'General' = c(
      # Factor 1 is composed of cognitive, non-technical, general skills (general competencies)
      'judgment_and_decision.l'
      , 'complex_problem_solving.l'
      , 'active_learning.l'
      , 'critical_thinking.l'
    )
    , 'Technical' = c(
      # Factor 2 is composed of mechanical, hands-on, specialist skills (technical)
      'equipment_selection.l'
      , 'troubleshooting.l'
      , 'repairing.l'
      , 'equipment_maintenance.l'
    )
    
  )
  
  # ABILITIES FACTOR LIST ---------------------------------------------------
  list_ablt.factors <- list(
    'Perception' = c(
      # Factor 1 is composed of perceptual abilities (perception):
      'night_vision.l'
      , 'sound_localization.l'
      , 'glare_sensitivity.l'
    )
    , 'Dexterity' = c(
      # Factor 2 is composed of manual abilities (dexterity):
      'finger_dexterity.l'
      , 'arm_hand_steadiness.l'
      , 'manual_dexterity.l'
    )
    , 'Robustness' = c(
      # Factor 3 is composed of bodily robustness, potency, and coordination (overall body robustness)
      'stamina.l'
      , 'gross_body_coordination.l'
      , 'trunk_strength.l'
    )
    , 'Intelligence' = c(
      # Factor 4 is composed of cognitive abilities (intelligence):
      'inductive_reasoning.l'
      , 'problem_sensitivity.l'
      , 'deductive_reasoning.l'
    )
  )
  
  # KNOWLEDGE FACTOR LIST ---------------------------------------------------
  list_know.factors <- list(
    'Health' = c(
      # Factor 1 is composed of health-related fields of knowledge (health / help).
      'therapy_and_counseling.l'
      , 'psychology.l'
      , 'medicine_and_dentistry.l'
    )
    , 'Building' = c(
      # Factor 2 is composed of engineering / building-related fields of knowledge (build).
      'physics.l'
      , 'engineering_and_technology.l'
      , 'building_and_construction.l'
    ) 
    , 'Business' = c(
      # Factor 3 is composed of financial and enterprising fields of knowledge (FGV).
      'economics_and_accounting.l'
      , 'sales_and_marketing.l'
      , 'administration_and_management.l'
    )
    , 'Arts_Humanities' = c(
      # Factor 4 is composed of arts and humanities (communists).
      'history_and_archeology.l'
      , 'geography.l'
      , 'fine_arts.l'
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
  # df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')
  df_occupations <- readr::read_csv('./occupations.csv')
  
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
  from_json(body) %>%
    as_tibble() -> df_input
  
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
  
  
  # HANDLER FUNCTION RETURN -------------------------------------------------
  return(
    list(
      statusCode = 200,
      headers = list("Content-Type" = "application/json"),
      body = JSON_KNN.output)
  )
  
}
