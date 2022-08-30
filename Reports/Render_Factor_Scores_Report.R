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
source('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models/KNN_Matching.R')

# PARAMETERS --------------------------------------------------------------
# Selected respondent
chr_user <- 'Martijn'
# chr_user <- 'Gabriel'

# KNN parameters
dbl_threshold <- 0 

# ------- DATA -----------------------------------------------------------
# SKILLS FACTOR LIST -----------------------------------------------------------------
list_skill.factors <- list(
  
  'Discernment' = c(
    # Factor 1 is composed of cognitive, non-technical, general competencies related to decision-making (discernment)
    'judgment_and_decision.l'
    , 'active_learning.l'
    , 'complex_problem_solving.l'
    , 'critical_thinking.l'
  )
  , 'Technical Skills' = c(
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
    'glare_sensitivity.l'
    , 'peripheral_vision.l'
    , 'sound_localization.l'
  )
  , 'Dexterity' = c(
    # Factor 2 is composed of manual abilities (dexterity):
    'arm_hand_steadiness.l'
    , 'finger_dexterity.l'
    , 'control_precision.l'
  )
  , 'Intelligence' = c(
    # Factor 4 is composed of cognitive abilities (intelligence):
    'inductive_reasoning.l'
    , 'deductive_reasoning.l'
    , 'information_ordering.l'
  )
  
)

# KNOWLEDGE FACTOR LIST ---------------------------------------------------
list_know.factors <- list(
  
  'Health Science' = c(
    # Factor 1 is composed of health-related fields of knowledge (health).
    'therapy_and_counseling.l'
    , 'medicine_and_dentistry.l'
    , 'psychology.l'
    , 'biology.l'
  )
  , 'Building' = c(
    # Factor 2 is composed of engineering / building-related fields of knowledge (building).
    'physics.l'
    , 'engineering_and_technology.l'
    , 'mechanical.l'
    , 'building_and_construction.l'
  ) 
  , 'Business' = c(
    # Factor 3 is composed of financial and enterprising fields of knowledge (business).
    'administration_and_management.l'
    , 'economics_and_accounting.l'
    , 'personnel_and_human_resources.l'
    , 'administrative.l'
  )
  , 'Arts & Humanities' = c(
    # Factor 4 is composed of arts and humanities (arts & humanities).
    'history_and_archeology.l'
    , 'fine_arts.l'
    , 'philosophy_and_theology.l'
    , 'sociology_and_anthropology.l'
  ) 
  
)

# # WORK CONTEXTS FACTOR LIST ---------------------------------------------------
# list_context.factors <- list(
#   
#   'Environmental_Hazards' = c(
#     # Factor 1 is composed of work contexts related to environmental job hazards (environmental hazards).
#     'outdoors_under_cover'
#     , 'outdoors_exposed_to_weather'
#     , 'indoors_not_environmentally_controlled'
#     , 'exposed_to_high_places'
#   )
#   , 'Physical_Exertion' = c(
#     # Factor 2 is composed of work contexts related to physical activity (physical exertion).
#     'spend_time_standing'
#     , 'spend_time_making_repetitive_motions'
#     , 'spend_time_bending_or_twisting_the_body'
#     , 'spend_time_walking_and_running'
#   ) 
#   , 'Interpersonal_Conflict' = c(
#     # Factor 3 is composed of work contexts related to conflict management (interpersonal conflict).
#     'frequency_of_conflict_situations'
#     , 'deal_with_unpleasant_or_angry_people'
#     , 'deal_with_physically_aggressive_people'
#     , 'contact_with_others'
#   )
#   
# )
# 
# # WORK ACTIVITIES FACTOR LIST ---------------------------------------------------
# list_activities.factors <- list(
#   
#   'Analytical' = c(
#     # Factor 1 is composed of analytical work activities (analytical).
#     'processing_information.l'
#     , 'analysing_data_or_information.l'
#     , 'working_with_computers.l'
#     , 'documenting_recording_information.l'
#     , 'updating_and_using_relevant_knowledge.l'
#   )
#   , 'Managerial' = c(
#     # Factor 2 is composed of managerial work activities (managerial).
#     'guiding_directing_and_motivating_subordinates.l'
#     , 'coordinating_the_work_and_activities_of_others.l'
#     , 'developing_and_building_teams.l'
#     , 'staffing_organizational_units.l'
#     , 'coaching_and_developing_others.l'
#   ) 
#   , 'Mechanical' = c(
#     # Factor 3 is composed of mechanical work activities (mechanical).
#     'repairing_and_maintaining_mechanical_equipment.l'
#     , 'controlling_machines_and_processes.l'
#     , 'inspecting_equipment_structures_or_materials.l'
#     , 'repairing_and_maintaining_electronic_equipment.l'
#     , 'operating_vehicles_mechanized_devices_or_equipment.l'
#   )
#   
# )
# ALL CATEGORIES FACTOR LIST -------------------------------------------------------------
# Factors list
list_factors <- list(
  'Skills' = list_skill.factors
  , 'Abilities' = list_ablt.factors
  , 'Fields of Knowledge' = list_know.factors
  # , 'Work Contexts' = list_context.factors
  # , 'Work Activities' = list_activities.factors
)

# # Factor names data frame
# list_factors %>% 
#   bind_rows(
#     .id = 'competency'
#   ) %>% 
#   pivot_longer(
#     cols = -competency
#     , names_to = 'factor'
#     , values_to = 'item'
#   ) %>% 
#   drop_na() %>% 
#   group_by(competency) %>%
#   summarise(
#     factor = unique(factor)
#   ) %>% 
#   ungroup() -> df_factors.names

# Factor names data frame
list_factors %>%
  bind_rows(
    .id = 'competency'
  ) %>%
  pivot_longer(
    cols = -competency
    , names_to = 'factor'
    , values_to = 'item'
  ) %>%
  drop_na() %>%
  ungroup() -> df_factors.names

# EFA-REDUCED OCCUPATIONS DATA FRAME -------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Select only necessary variables
df_occupations %>%
  select(
    occupation
    , entry_level_education #Filter will be applied later on in Bubble via user input
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

# EFA-REDUCED QUERY VECTOR -----------------------------------------------
# User questionnaires data frame
df_input <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv')

df_input %>% 
  filter(Name == chr_user) -> df_input

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


# ------- RESULTS --------------------------------------------------------
# KNN MATCHING ---------------------------------------------------------------
fun_KNN.matching(
  .df_data.numeric = df_occupations
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = 0
  , .dbl_decimals = 4
) -> df_KNN.output

# FACTOR SCORES -----------------------------------------------------------
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

# DYNAMIC TEXT ------------------------------------------------------------
# Numbers for dynamic reporting with R Markdown

# Captions for dynamic reporting with R Markdown

# PLOTS -------------------------------------------------------------------


# RENDER R MARKDOWN REPORT --------------------------------------------------


setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Old')

rmarkdown::render('./dsds.Rmd')
