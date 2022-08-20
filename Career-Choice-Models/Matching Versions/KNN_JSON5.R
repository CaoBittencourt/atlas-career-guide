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
  
  'Discernment' = c(
    # Factor 1 is composed of cognitive, non-technical, general competencies related to decision-making (discernment)
    'Judgment_and_Decision.L'
    , 'Active_Learning.L'
    , 'Complex_Problem_Solving.L'
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

# ABILITIES FACTOR LIST ---------------------------------------------------
list_ablt.factors <- list(
  
  'Perception' = c(
    # Factor 1 is composed of perceptual abilities (perception):
    'Glare_Sensitivity.L'
    , 'Peripheral_Vision.L'
    , 'Sound_Localization.L'
  )
  , 'Dexterity' = c(
    # Factor 2 is composed of manual abilities (dexterity):
    'Arm_Hand_Steadiness.L'
    , 'Finger_Dexterity.L'
    , 'Control_Precision.L'
  )
  , 'Intelligence' = c(
    # Factor 4 is composed of cognitive abilities (intelligence):
    'Inductive_Reasoning.L'
    , 'Deductive_Reasoning.L'
    , 'Information_Ordering.L'
  )
  
)

# KNOWLEDGE FACTOR LIST ---------------------------------------------------
list_know.factors <- list(
  
  'Health' = c(
    # Factor 1 is composed of health-related fields of knowledge (health).
    'Therapy_and_Counseling.L'
    , 'Medicine_and_Dentistry.L'
    , 'Psychology.L'
    , 'Biology.L'
  )
  , 'Building' = c(
    # Factor 2 is composed of engineering / building-related fields of knowledge (building).
    'Physics.L'
    , 'Engineering_and_Technology.L'
    , 'Mechanical.L'
    , 'Building_and_Construction.L'
  ) 
  , 'Business' = c(
    # Factor 3 is composed of financial and enterprising fields of knowledge (business).
    'Administration_and_Management.L'
    , 'Economics_and_Accounting.L'
    , 'Personnel_and_Human_Resources.L'
    , 'Administrative.L'
  )
  , 'Arts_Humanities' = c(
    # Factor 4 is composed of arts and humanities (arts & humanities).
    'History_and_Archeology.L'
    , 'Fine_Arts.L'
    , 'Philosophy_and_Theology.L'
    , 'Sociology_and_Anthropology.L'
  ) 
  
)

# WORK CONTEXT FACTOR LIST ---------------------------------------------------
list_context.factors <- list(
  
  'Environmental_Hazards' = c(
    # Factor 1 is composed of work contexts related to environmental job hazards (environmental hazards).
    'Outdoors_Under_Cover'
    , 'Outdoors_Exposed_to_Whether'
    , 'Indoors_Not_Environmentally_Controlled'
    , 'Exposed_to_High_Places'
  )
  , 'Physical_Exertion' = c(
    # Factor 2 is composed of work contexts related to physical activity (physical exertion).
    'Spend_Time_Standing'
    , 'Spend_Time_Making_Repetitive_Motions'
    , 'Spend_Time_Bending_or_Twisting_the_Body'
    , 'Spend_Time_Walking_and_Running'
  ) 
  , 'Interpersonal_Conflict' = c(
    # Factor 3 is composed of work contexts related to conflict management (interpersonal conflict).
    'Frequency_of_Conflict_Situations'
    , 'Deal_with_Unpleasant_or_Angry_People'
    , 'Deal_with_Physically_Aggressive_People'
    , 'Contact_With_Others'
  )
  
)

# WORK ACTIVITIES FACTOR LIST ---------------------------------------------------
list_activities.factors <- list(
  
  'Analytical' = c(
    # Factor 1 is composed of analytical work activities (analytical).
    'Processing_Information.L'
    , 'Analysing_Data_or_Information.L'
    , 'Working_with_Computers.L'
    , 'Documenting_Recording_Information.L'
    , 'Updating_and_Using_Relevant_Knowledge.L'
  )
  , 'Managerial' = c(
    # Factor 2 is composed of managerial work activities (managerial).
    'Guiding_Directing_and_Motivating_Subordinates.L'
    , 'Coordinating_the_Work_and_Activities_of_Others.L'
    , 'Developing_and_Building_Teams.L'
    , 'Staffing_Organizational_Units.L'
    , 'Coaching_and_Developing_Others.L'
  ) 
  , 'Mechanical' = c(
    # Factor 3 is composed of mechanical work activities (mechanical).
    'Repairing_and_Maintaining_Mechanical_Equipment.L'
    , 'Controlling_Machines_and_Processes.L'
    , 'Inspecting_Equipment_Structures_or_Materials.L'
    , 'Repairing_and_Maintaining_Electronic_Equipment.L'
    , 'Operating_Vehicles_Mechanized_Devices_or_Equipment.L'
  )
  
)

# ALL FACTORS LIST -------------------------------------------------------------
list_factors <- list(
  'Skills' = list_skill.factors
  , 'Abilities' = list_ablt.factors
  , 'Knowledge' = list_know.factors
  , 'Work_Context' = list_context.factors
  , 'Work_Activities' = list_activities.factors
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