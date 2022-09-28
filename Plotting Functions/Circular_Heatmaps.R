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

# -------- FACTORS -----------------------------------------------------------
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

# REORDER CATEGORIES FOR AESTHETIC PURPOSES -------------------------------------------------------------
# Factors list
list_factors <- list(
  'Fields of Knowledge' = list_know.factors
  , 'Abilities' = list_ablt.factors
  , 'Skills' = list_skill.factors
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

# -------- DATA -----------------------------------------------------------


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
    annual_wage_2021
    , occupation
    , all_of(chr_vars)
  ) %>%
  mutate(
    across(
      .cols = all_of(chr_vars)
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# CONVERT TO MATRIX --------------------------------------------------------------------
# Keep occupation names
df_occupations %>%
  pull(occupation) %>%
  factor() -> fct_occupations

# Convert to matrix
df_occupations %>% 
  select(all_of(chr_vars)) %>% 
  as.matrix() -> mtx_occupations

# Rownames
fct_occupations -> rownames(mtx_occupations)

# CLUSTERIZATION --------------------------------------------------------------------
# N of clusters
int_cluster <- 8 

# K-means clusterization algorithm
kmeans(
  mtx_occupations
  , centers = int_cluster
)$cluster -> int_kmeans.split

# SAMPLE OF OCCUPATIONS ---------------------------------------------------
# Total occupations to display
int_cluster * 50

# Top n
int_top.n <- 50

# Select top n from each cluster
int_kmeans.split %>% 
  as_tibble(rownames = 'occupation') %>% 
  rename(cluster.kmeans = value) %>% 
  mutate(cluster.kmeans = factor(cluster.kmeans)) %>% 
  full_join(df_occupations) %>% 
  group_by(cluster.kmeans) %>% 
  arrange(desc(annual_wage_2021)) %>% 
  slice(1:int_top.n) %>%
  ungroup() -> df_occupations

# Adjusted matrix
df_occupations %>% 
  column_to_rownames('occupation') %>%
  select(all_of(chr_vars)) %>% 
  as.matrix() -> mtx_occupations

# Adjusted split
df_occupations %>% 
  pull(cluster.kmeans) -> int_kmeans.split

# SPLIT DATA BY FACTORS ---------------------------------------------
map(
  list_factors
  , function(vars){
    
    mtx_occupations[,flatten_chr(vars)] %>% 
      return()
    
  }
) -> list_mtx_occupations

# -------- PLOT ELEMENTS ---------------------------------------------------
# TRACK HEIGHT ------------------------------------------------------------
# Track proportions 
map(
  list_mtx_occupations
  , ~ ncol(.) / ncol(mtx_occupations)
) -> list_tracks

# Total track length
int_track.size <- 0.70

# Track sizes
map(
  list_tracks
  , ~ . * int_track.size
) -> list_tracks

# COLORS ------------------------------------------------------------------
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

# Atlas palette 3
colorRamp2(
  c(0.3, 0.5, 0.7)
  , c(
    '#753AF9'
    , '#FFFFFF'
    , '#4AF7B0'
  )
) -> fun_color

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

# Red white blue (boost colors)
colorRamp2(
  c(0.3, 0.5, 0.7)
  , c(
    '#F22B29'
    , '#FFFFFF'
    , '#182766'
  )
) -> fun_color

# OCCUPATION LABELS -------------------------------------------------------

# SECTOR LABELS -----------------------------------------------------------
c(
  rep(F, length(list_mtx_occupations) - 1)
  , T
) -> lgc_sector.labels


# -------- PLOTS -----------------------------------------------------------
# CIRCULAR HEATMAP FUNCTION -----------------------------------------------
fun_plot.heatmap.circ <- function(){}

# TEST --------------------------------------------------------------------
circos.clear()

circos.par(
  start.degree = 85
  # , gap.degree = 10
  , gap.degree = 5
)

# Circular heatmap 
Map(
  function(mtx, track, sector.labels){
    
    circos.heatmap(
      mat = mtx
      , split = int_kmeans.split 
      , col = fun_color
      , track.height = track
      # , rownames.side = 'outside'
      , cell.border = '#FFFFFF'
      , bg.border = '#212121'
      , bg.lwd = 2.23
      # , cell.border = '#212121'
      # , cell.lwd = 2.23
      # , cell.lwd = .125
      , cell.lwd = .0625
      , show.sector.labels = sector.labels
      , cluster = T
    )
    
  }
  , mtx = list_mtx_occupations
  , track = list_tracks
  , sector.labels = lgc_sector.labels
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
