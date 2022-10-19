# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# DATA --------------------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.R')

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
#     'outdoors_under_cover.l'
#     , 'outdoors_exposed_to_weather.l'
#     , 'indoors_not_environmentally_controlled.l'
#     , 'exposed_to_high_places.l'
#   )
#   , 'Physical_Exertion' = c(
#     # Factor 2 is composed of work contexts related to physical activity (physical exertion).
#     'spend_time_standing.l'
#     , 'spend_time_making_repetitive_motions.l'
#     , 'spend_time_bending_or_twisting_the_body.l'
#     , 'spend_time_walking_and_running.l'
#   ) 
#   , 'Interpersonal_Conflict' = c(
#     # Factor 3 is composed of work contexts related to conflict management (interpersonal conflict).
#     'frequency_of_conflict_situations.l'
#     , 'deal_with_unpleasant_or_angry_people.l'
#     , 'deal_with_physically_aggressive_people.l'
#     , 'contact_with_others.l'
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
list(
  'Skills' = list_skill.factors
  , 'Abilities' = list_ablt.factors
  , 'Fields of Knowledge' = list_know.factors
  # , 'Work Contexts' = list_context.factors
  # , 'Work Activities' = list_activities.factors
) -> list_factors

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
  ) -> df_occupations

