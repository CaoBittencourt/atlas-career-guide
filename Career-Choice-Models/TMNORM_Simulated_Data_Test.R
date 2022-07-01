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
    all_of(c(# Selected skills, abilities, and knowledge
      # where(function(x){str_detect(attributes(x)$label, '_Skills.')}) #Skills only
      # , where(function(x){str_detect(attributes(x)$label, 'Abilities.')}) #Skills only
      # , where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Skills only
      # , -ends_with('.I') #Using recommended levels
      chr_Skill.Items
      , chr_Ablt.Items
      , chr_Know.Items
    )
    )
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.items

# SIMULATED QUESTIONNAIRE ---------------------------------------------------------------
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
