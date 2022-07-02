# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'scales' #Visualization
  , 'readr', 'readxl','openxlsx' #Read and write utilities
  # , 'MASS' #Generate normal correlated data
  , 'TruncatedNormal' #Generate truncated normal correlated data
  , 'tidyverse', 'labelled', 'glue' #Data wrangling
  , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
  # , 'knn.covertree' # Calculates cosine KNN, but package is too slow
  , 'lsa' #Cosine similarity
  , 'parallelDist' #Fast distance computations (for Euclidean)
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# # MASS vs dplyr conflicts
# select <- dplyr::select

# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models')

# FUNCTIONS ---------------------------------------------------------------
source('./Simulated_Data.R')
source('./KNN_Matching.R')

# DATA --------------------------------------------------------------------
# # Auto-EFA skills
# source('./Auto_EFA_skills_output.R')
# # Auto-EFA abilities
# source('./Auto_EFA_abilities_output.R')
# # Auto-EFA knowledge
# source('./Auto_EFA_knowledge_output.R')
# Fully-auto-EFA "best models"
source('./Fully_Auto_EFA_all_output.R')

# Only numeric variables
df_occupations %>%
  select(
    Occupation
    , all_of(c(# Selected skills
      chr_Skill.Items
    ))
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .cols = where(is.numeric)
      ,.fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.skill

# Only numeric variables
df_occupations %>%
  select(
    Occupation
    , all_of(c(# Selected abilities
      chr_Ablt.Items
    ))
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .cols = where(is.numeric)
      ,.fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.ablt

# Only numeric variables
df_occupations %>%
  select(
    Occupation
    , all_of(c(# Selected fields of knowledge
      chr_Know.Items
    ))
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .cols = where(is.numeric)
      ,.fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.know

# Only numeric variables
df_occupations %>%
  select(
    Occupation
    , all_of(c(# All selected items
      chr_Skill.Items
      , chr_Ablt.Items
      , chr_Know.Items
    ))
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .cols = where(is.numeric)
      ,.fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.items


# SIMULATED QUESTIONNAIRE ---------------------------------------------------------------
# Skills only
df_simulations.skill <- fun_simulate.tmvnorm(
  df_data.numeric = df_occupations.numeric.skill
  , int_n.simulations = 100
  , chr_observations.name = 'Subject'
  , dbl_lower.bound = 0
  , dbl_upper.bound = 1
)

# Abilities only
df_simulations.ablt <- fun_simulate.tmvnorm(
  df_data.numeric = df_occupations.numeric.ablt
  , int_n.simulations = 100
  , chr_observations.name = 'Subject'
  , dbl_lower.bound = 0
  , dbl_upper.bound = 1
)

# Fields of Knowledge only
df_simulations.know <- fun_simulate.tmvnorm(
  df_data.numeric = df_occupations.numeric.know
  , int_n.simulations = 100
  , chr_observations.name = 'Subject'
  , dbl_lower.bound = 0
  , dbl_upper.bound = 1
)

# All items
df_simulations <- fun_simulate.tmvnorm(
  df_data.numeric = df_occupations.numeric.items
  , int_n.simulations = 100
  , chr_observations.name = 'Subject'
  , dbl_lower.bound = 0
  , dbl_upper.bound = 1
)

# APPLY KNN ---------------------------------------------------------------
lapply(
  df_simulations$Subject
  , function(subj){
    
    df_simulations %>% 
      filter(Subject == subj) %>%
      select(-Subject) -> vec_compare
    
    fun_KNN.matching(
      df_data.numeric = df_occupations.numeric.items
      , vec_query.numeric = vec_compare
      , int_k = nrow(df_occupations.numeric.items)
      , auto_select.k = F
    ) %>% 
      return(.)
    
  }) -> list_KNN.matching

# # APPLY KNN (skills) ---------------------------------------------------------------
# lapply(
#   df_simulations.skill$Subject
#   , function(subj){
#     
#     df_simulations.skill %>%
#       filter(Subject == subj) %>%
#       select(-Subject) -> vec_compare
#     
#     fun_KNN.matching(
#       df_data.numeric = df_occupations.numeric.skill
#       , vec_query.numeric = vec_compare
#       , int_k = nrow(df_occupations.numeric.skill)
#       , auto_select.k = F
#     ) %>%
#       rename_with(
#         .cols = starts_with(c('Similarity.','Euclidean_'))
#         , .fn = function(x){paste0(x,'.Skill')}
#       ) %>% 
#       return(.)
#     
#   }) -> list_KNN.matching.skill
# 
# # APPLY KNN (abilities) ---------------------------------------------------------------
# lapply(
#   df_simulations.ablt$Subject
#   , function(subj){
#     
#     df_simulations.ablt %>%
#       filter(Subject == subj) %>%
#       select(-Subject) -> vec_compare
#     
#     fun_KNN.matching(
#       df_data.numeric = df_occupations.numeric.ablt
#       , vec_query.numeric = vec_compare
#       , int_k = nrow(df_occupations.numeric.ablt)
#       , auto_select.k = F
#     ) %>%
#       rename_with(
#         .cols = starts_with(c('Similarity.','Euclidean_'))
#         , .fn = function(x){paste0(x,'.Ablt')}
#       ) %>% 
#       return(.)
#     
#   }) -> list_KNN.matching.ablt
# 
# 
# # APPLY KNN (knowledge) ---------------------------------------------------------------
# lapply(
#   df_simulations.know$Subject
#   , function(subj){
#     
#     df_simulations.know %>%
#       filter(Subject == subj) %>%
#       select(-Subject) -> vec_compare
#     
#     fun_KNN.matching(
#       df_data.numeric = df_occupations.numeric.know
#       , vec_query.numeric = vec_compare
#       , int_k = nrow(df_occupations.numeric.know)
#       , auto_select.k = F
#     ) %>%
#       rename_with(
#         .cols = starts_with(c('Similarity.','Euclidean_'))
#         , .fn = function(x){paste0(x,'.Know')}
#       ) %>% 
#       return(.)
#     
#   }) -> list_KNN.matching.know
# 
# 
# # AVERAGE OF MATCHINGS -----------------------------------------------------
# list_KNN.matching.skill %>%
#   bind_rows(.id = 'Subject') %>%
#   select(-ends_with('.L')) -> df_Skill.matching
# 
# list_KNN.matching.ablt %>%
#   bind_rows(.id = 'Subject') %>%
#   select(-ends_with('.L')) -> df_Ablt.matching
# 
# list_KNN.matching.know %>%
#   bind_rows(.id = 'Subject') %>%
#   select(-ends_with('.L')) -> df_Know.matching
# 
# 
# mean(
#   c(
#     df_Skill.matching[1,4] %>% pull()
#     , df_Ablt.matching[1,4] %>% pull()
#     , df_Know.matching[1,4] %>% pull()
#   )
# )
# 
# df_Skill.matching %>%
#   full_join(
#     df_Ablt.matching
#   ) %>%
#   full_join(
#     df_Know.matching
#   ) %>%
#   group_by(
#     
#   ) -> df_Items.matching
# 
# df_Items.matching %>%
#   # filter(Subject == 'Subject 26') %>%
#   pivot_longer(
#     cols = starts_with('Similarity.')
#     , names_to = 'Similarity'
#     , values_to = 'Value'
#   ) %>%
#   ggplot(aes(
#     x = Value
#     , color = Similarity
#   )) +
#   geom_density(size = 1.5)
# 
# df_Items.matching$Similarity.Common %>% summary()
# df_Items.matching$Similarity.Gaussian %>% summary()
# 

# GRAPHS ------------------------------------------------------------------
list_KNN.matching %>% 
  bind_rows() -> test

test %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) %>%
  ggplot(aes(
    x = Value
    , color = Similarity
  )) + 
  geom_density(size = 1.5)

test$Similarity.Common %>% summary()
test$Similarity.Gaussian %>% summary()
df_occupations.numeric.items %>% ncol()


list_KNN.matching.skill %>%
  bind_rows() -> test

test %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) %>%
  ggplot(aes(
    x = Value
    , color = Similarity
  )) +
  geom_density(size = 1.5)

test$Similarity.Common %>% summary()
test$Similarity.Gaussian %>% summary()
df_occupations.numeric.skill %>% ncol()

list_KNN.matching.ablt %>%
  bind_rows() -> test

test %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) %>%
  ggplot(aes(
    x = Value
    , color = Similarity
  )) +
  geom_density(size = 1.5)

test$Similarity.Common %>% summary()
test$Similarity.Gaussian %>% summary()
df_occupations.numeric.ablt %>% ncol()

list_KNN.matching.know %>%
  bind_rows() -> test

test %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) %>%
  ggplot(aes(
    x = Value
    , color = Similarity
  )) +
  geom_density(size = 1.5)

test$Similarity.Common %>% summary()
test$Similarity.Gaussian %>% summary()
df_occupations.numeric.know %>% ncol()
