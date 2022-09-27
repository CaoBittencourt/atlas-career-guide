# -------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'ggridges', 'gghighlight', 'scales' , 'viridis', 'ggalt'
  # , 'ComplexHeatmap'
  , 'circlize' #'paletteer' #Data visualization
  , 'devtools' #Dev Tools
  , 'psych' #Factor scores
  # , 'fastcluster' #Faster clusterization algorithms
  , 'numbers' #Divisors
  # , 'ggalt', 'hrbrthemes', 'extrafont' #Data visualization
  # , 'ggthemr' #Data visualization
  , 'tidyverse', 'rlang' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

install_github('jokergoo/ComplexHeatmap')
# install_github('jokergoo/circlize')

library(ComplexHeatmap)

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# # [TO DO] FONTS -------------------------------------------------------------------
# font_import(prompt = F)
# loadfonts(device = 'win')
# hrbrthemes::
# hrbrthemes::import_roboto_condensed()

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

# Variables
list_factors %>%
  flatten() %>% 
  flatten_chr() -> chr_vars

# Select only necessary variables
df_occupations %>%
  select(
    occupation
    , all_of(chr_vars)
  ) %>%
  mutate(
    across(
      .cols = chr_vars
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# SPLIT DATA FRAME BY FACTORS ---------------------------------------------
map(
  list_factors
  , function(vars){
    
    df_occupations %>% 
      select(flatten_chr(vars)) %>% 
      return()
    
  }
) -> list_df_occupations.factors

# CONVERT TO MATRIX --------------------------------------------------------------------
# Keep occupation names
df_occupations %>%
  pull(occupation) %>%
  factor() -> fct_occupations

# Convert to matrix
map(
  list_df_occupations.factors
  , as.matrix
) -> list_mtx_occupations

# Rownames
map(
  list_mtx_occupations
  , function(mtx){
    
    fct_occupations -> rownames(mtx)
    
    return(mtx)
    
  }
) -> list_mtx_occupations

# CLUSTERIZATION --------------------------------------------------------------------
# K-means clusterization algorithm
kmeans(
  bind_cols(list_mtx_occupations)
  , centers = 8
)$cluster -> int_kmeans.split

# SAMPLE OF OCCUPATIONS ---------------------------------------------------
int_kmeans.split 


# # DATA --------------------------------------------------------------------
# # df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')
# 
# # # Filter one sample to fit
# # df_occupations %>% 
# #   sample_n(50) -> df_occupations
# 
# # # # Keep occupation cluster
# # df_occupations %>%
# #   pull(career_cluster) %>%
# #   factor() -> fct_cluster
# 
# # Keep occupation names
# df_occupations %>% 
#   pull(occupation) %>% 
#   factor() -> fct_occupations
# 
# # Convert to matrix
# df_occupations %>%
#   select(where(
#     is.numeric
#   )) %>% 
#   as.matrix() -> mtx_occupations
# 
# fct_occupations -> rownames(mtx_occupations)
# 
# # Clusterization
# # fastcluster::hclust(
# #   dist(mtx_occupations)
# # ) -> hclust_clusters
# # 
# # cutree(hclust_clusters, 10) %>% view
# 
# # mtx_occupations[hclust_clusters$order,] -> mtx_occupations
# 
# kmeans(mtx_occupations, centers = 8)$cluster -> int_kmeans.split
# 
# # Split variables
# ncol(mtx_occupations)
# 
# divisors(ncol(mtx_occupations))
# 
# # split(
# #   seq(1,161)
# #   , cut(
# #     seq(1,161)
# #     , 7, labels = F
# #     )
# # ) -> list_index
# 
# list(1:ncol(mtx_occupations)) -> list_index
# 
# paste0('mtx', names(list_index)) -> names(list_index)
# 
# map(
#   list_index
#   , function(index){
#     
#     return(mtx_occupations[,index])
#     
#   }
# ) -> list_mtx_occupations
# 
# list_mtx_occupations %>% 
#   sapply(ncol)
# 
# list_mtx_occupations %>% 
#   sapply(ncol) %>% 
#   sum() == ncol(mtx_occupations)

# # DATA (SALARIES) --------------------------------------------------------------------
# df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')
# 
# # # Filter one sample to fit
# # df_occupations %>% 
# #   sample_n(50) -> df_occupations
# 
# # Keep occupation cluster
# df_occupations %>% 
#   pull(career_cluster) %>% 
#   factor() -> fct_cluster
# 
# # Keep occupation names
# df_occupations %>% 
#   pull(occupation) %>% 
#   factor() -> fct_occupations
# 
# # Convert to matrix
# df_occupations %>% 
#   select(annual_wage_2021) %>%
#   # select(1:50) %>%
#   # mutate(across(
#   #   .fns = function(x){x / 100}
#   # )) %>% 
#   as.matrix() -> mtx_occupations
# 
# rownames(mtx_occupations) <- fct_occupations
# colnames(mtx_occupations) <- 'Annual Wage (2021)'
# 
# t(mtx_occupations) -> mtx_occupations
# 
# # Sample 
# mtx_occupations[1:50,] -> mtx_occupations1
# mtx_occupations[51:100,] -> mtx_occupations2
# mtx_occupations[101:150,] -> mtx_occupations3
# mtx_occupations[151:200,] -> mtx_occupations4

# -------- PLOT ELEMENTS ---------------------------------------------------
# -------- PLOTS -----------------------------------------------------------
# CIRCULAR HEATMAP FUNCTION -----------------------------------------------
fun_plot.heatmap.circ <- function(){}

# TEST --------------------------------------------------------------------
# Track proportions 
map(
  list_mtx_occupations
  , ~ ncol(.) / ncol(mtx_occupations)
) -> list_proportions 

# Colors
# Atlas palette 1
# fun_color <- colorRamp2(c(0, 0.5, 1), plasma(3))
# colorRamp2(
#   c(0, 0.5, 1)
#   , c(
#     '#753AF9'
#     , '#D4D5D8'
#     , '#4AF7B0'
#   )
# ) -> fun_color

# Atlas palette 2
# colorRamp2(
#   c(0, 1)
#   , c(
#     '#753AF9'
#     , '#4AF7B0'
#   )
# ) -> fun_color

# Red white green
# colorRamp2(
#   c(0, 0.5, 1)
#   , c(
#     '#F22B29'
#     , '#FFFFFF'
#     , '#4AF7B0'
#   )
# ) -> fun_color

# Red white blue
colorRamp2(
  c(0, 0.5, 1)
  , c(
    '#F22B29'
    , '#FFFFFF'
    , '#182766'
  )
) -> fun_color

circos.clear()
circos.par(
  start.degree = 85
  , gap.degree = 10
)

Map(
  function(mtx, prop){
    
    circos.heatmap(
      mat = mtx
      , split = int_kmeans.split 
      , col = fun_color
      # , track.height = 0.5
      # , track.height = 0.8 * prop
      , track.height = 0.7 * prop
      # , rownames.side = 'outside'
      # , cell.border = '#FFFFFF'
      # , cell.lwd = 2.23
      # , cell.lwd = .125
      # , cell.lwd = .0625
      , show.sector.labels = T
      , cluster = F
    )
    
  }
  , mtx = list_mtx_occupations
  , prop = list_proportions
)

circos.heatmap(
  mat = list_mtx_occupations$Skills
    # , split = split
    , col = fun_color
  , track.height = .8
  # , rownames.side = 'outside'
  # , cell.border = 'white'
  # , cell.lwd = 2.23
  # , cluster = T
)

# # TEST --------------------------------------------------------------------
# # fun_color <- colorRamp2(c(0, 0.5, 1), plasma(3))
# # colorRamp2(
# #   c(0, 0.5, 1)
# #   , c(
# #     '#753AF9'
# #     , '#D4D5D8'
# #     , '#4AF7B0'
# #   )
# # ) -> fun_color
# 
# colorRamp2(
#   c(0, 1)
#   , c(
#     '#753AF9'
#     , '#4AF7B0'
#   )
# ) -> fun_color
# 
# circos.clear()
# circos.par(
#   start.degree = 85
#   , gap.degree = 10
# )
# 
# map(
#   list_mtx_occupations
#   , function(mtx){
#     
#     circos.heatmap(
#       mat = mtx
#       , split = int_kmeans.split 
#       , col = fun_color
#       , track.height = 0.5
#       # , rownames.side = 'outside'
#       , cell.border = 'white'
#       # , cell.lwd = 2.23
#       # , cell.lwd = .125
#       , cell.lwd = .0625
#       , cluster = T
#     )
#     
#   }
# )
# 
# circos.heatmap(
#   mat = 
#     # , split = split
#     , col = fun_color
#   # , rownames.side = 'outside'
#   , cell.border = 'white'
#   , cell.lwd = 2.23
#   , cluster = T
# )

# TEST --------------------------------------------------------------------
set.seed(123)
mat1 = rbind(cbind(matrix(rnorm(50*5, mean = 1), nr = 50),
                   matrix(rnorm(50*5, mean = -1), nr = 50)),
             cbind(matrix(rnorm(50*5, mean = -1), nr = 50), 
                   matrix(rnorm(50*5, mean = 1), nr = 50))
)
rownames(mat1) = paste0("R", 1:100)
colnames(mat1) = paste0("C", 1:10)
mat1 = mat1[sample(100, 100), ] # randomly permute rows
split = sample(letters[1:5], 100, replace = TRUE)
split = factor(split, levels = letters[1:5])
# col_fun1 <- colorRamp2(c(-2, 0, 2), c("red", "white", "blue"))
col_fun1 <- colorRamp2(c(-2, 0, 2), plasma(3))

circos.clear()
circos.par(
  start.degree = 85
  , gap.degree = 10
)
circos.heatmap(
  mat = mat1
  # , split = split
  , col = col_fun1
  , rownames.side = 'outside'
  , cell.border = 'white'
  , cell.lwd = 2.23
  , cluster = T
)

# TEST --------------------------------------------------------------------
# col_fun1 <- colorRamp2(c(0, 0.5, 1), plasma(3))
col_fun1 <- colorRamp2(c(0, median(mtx_occupations), max(mtx_occupations)), plasma(3))

circos.clear()
circos.par(
  # start.degree = 90
  start.degree = 85
  , gap.degree = 10
)
circos.heatmap(
  mat = mtx_occupations1 %>% view
  # , split = fct_occupations
  , col = col_fun1
  # , rownames.side = 'outside'
  , rownames.side = 'inside'
  , cell.border = 'white'
  , cell.lwd = 2.23
  , cluster = T
)
circos.heatmap(
  mat = mtx_occupations2
  # , split = fct_occupations
  , col = col_fun1
  # , rownames.side = 'outside'
  , cell.border = 'white'
  , cell.lwd = 2.23
  , cluster = T
)
circos.heatmap(
  mat = mtx_occupations3
  # , split = fct_occupations
  , col = col_fun1
  # , rownames.side = 'outside'
  , cell.border = 'white'
  , cell.lwd = 2.23
  , cluster = T
)
circos.heatmap(
  mat = mtx_occupations4
  # , split = fct_occupations
  , col = col_fun1
  # , rownames.side = 'outside'
  , cell.border = 'white'
  , cell.lwd = 2.23
  , cluster = T
)
