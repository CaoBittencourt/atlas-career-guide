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
      # df_skills.items$Item
      # , df_ablt.items$Item
      # where(function(x){str_detect(attributes(x)$label, '_Skills.')}) #Skills only
      # , where(function(x){str_detect(attributes(x)$label, 'Abilities.')}) #Skills only
      # , where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Skills only
      # , -ends_with('.I') #Using recommended levels
      # , df_know.items$Item
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
# Set number of individuals
n_simulations <- 1000

# Generic names for each individual
chr_individuals <- paste('Subject', 1:n_simulations)
names(chr_individuals) <- chr_individuals

# Get the first line from the correlation matrix
# In order to keep the original relationship between variables
df_occupations.numeric.items %>% 
  cov() -> cov_mat

df_occupations.numeric.items %>% 
  cor() %>% 
  as_tibble() %>%
  slice(1) -> df_correlations

# Simulate normal distributions in accordance with the correlation matrix
# Mean for each variable
df_occupations.numeric.items %>%
  summarise(across(
    .fns = mean
  )) %>% 
  as.numeric() -> dbl_mean

# Multivariate truncated normal distribution
rtmvnorm(
  n = n_simulations
  , mu = dbl_mean
  , sigma = cov_mat
  , lb = rep(0,length(dbl_mean)) #Scale between 0 and 1
  , ub = rep(1,length(dbl_mean)) #Scale between 0 and 1 
) -> mat_trunc_norm

colnames(mat_trunc_norm) <- colnames(df_occupations.numeric.items)

mat_trunc_norm %>%
  as_tibble() -> df_simulations

# Add the names of each individual
df_simulations %>% 
  mutate(
    Subject = chr_individuals
    , .before = names(.)[1]
  ) -> df_simulations

# APPLY KNN ---------------------------------------------------------------
# Define k

# RECOMMENDED
# Typical suggested value for k is sqrt(nrow(df))
# Looking for k nearest neighbors in all career clusters
# df_occupations.numeric.items %>%
#   nrow(.) %>%
#   sqrt(.) %>%
#   round(.) -> k.value

# ARBITRARY (INPUT)
# Or arbitrarily assign k (e.g. top 10 closest matches)
# May k could be given via user input
# k.value <- 10
# k.value <- 5
# k.value <- 3
k.value <- 1

# COMPLETE COMPARISON
# Or find k nearest neighbors using the whole data set
# i.e. order everything with respect to euclidean distance
k.value <- nrow(df_occupations.numeric.items)

# Find the k nearest neighbors
lapply(
  chr_individuals
  , function(subject){
    
    df_simulations %>% 
      filter(Subject == subject) -> df_subject
    
    FNN::get.knnx(
      data = df_occupations.numeric.items
      , query = df_subject %>% select(where(is.numeric))
      , k = k.value
    ) -> KNN.output
    
    # Arrange original data frame with KNN output
    df_occupations %>% 
      slice(as.vector(KNN.output$nn.index)) %>% 
      select(#For the present purposes, keep only the following columns
        Career_Cluster
        , Occupation
        , Annual_Wage_2021
        # , df_loadings.items.skills_ablts$Metric
      ) %>% 
      # plyr::rbind.fill(df_subject) %>%
      mutate(#Add euclidean distances and convert them to similarities
        Euclidean_Distance = as.vector(KNN.output$nn.dist)
        # Common similarity: 1/(1 + dist), "+1" for dist = 0
        , Similarity.Common = 1 / (1 + Euclidean_Distance)
        # Similarity via gaussian kernel: exp(-dist)
        , Similarity.Gaussian = exp(-Euclidean_Distance)
        # Normalized by max value: 1 - dist.norm = 1 - dist/max(dist)
        , Similarity.Max = 1 - (Euclidean_Distance / max(Euclidean_Distance))
        
        # [Try again] Yielding negative values for greater distances
        # Tangent similarity: 1 - arctan(dist) = 1 for dist = 0 
        # , Similarity.Tan = 1 - atan(Euclidean_Distance)
        # , Similarity.Tan = 1 - atan(Euclidean_Distance/2)
        # , Similarity.Tan = 1 - atan(Euclidean_Distance/pi)
        
        # [Try again] Equivalence between euclidean and cosine
        # [Wrong formula] Yielding negative values for greater distances
        # , Similarity.Cosine = 1 - (Euclidean_Distance/2)
        # , Similarity.Cosine = 1 - (Euclidean_Distance^2)/2
        
        # Wage difference for later on
        , Wage.Diff = Annual_Wage_2021 - first(Annual_Wage_2021)
        
        # ) %>% 
        # mutate(#Round values
        #   across(
        #     .cols = starts_with('Similarity.')
        #     ,.fns = function(x){round(x,4)}
        #   )
        # ) -> df_occupations.KNN
        
      ) -> df_occupations.KNN
    
    return(df_occupations.KNN)
    
  }
) -> list_df_occupations.KNN

list_df_occupations.KNN %>% 
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
