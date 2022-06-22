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
# EFA skills
source('./EFA_skills_only.R')
# EFA abilities
source('./EFA_abilities_only.R')

# Selected skills and abilities
df_loadings.items.skills %>%
  bind_rows(df_loadings.items.ablts) -> df_loadings.items.skills_ablts

# Only numeric variables
df_occupations %>%
  select(
    df_loadings.items.skills_ablts$Metric #Selected skills and abilities only
    # where(function(x){str_detect(attributes(x)$label, '_Skills.')}) #Skills
    # , where(function(x){str_detect(attributes(x)$label, 'Abilities.')}) #Abilities
    # , where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Knowledge
    # where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Knowledge
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance values
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric

# Simpler names, if needed
# colnames(df_occupations.numeric) <- paste0('V',seq_along(df_occupations.numeric))

# SIMULATED QUESTIONNAIRE ---------------------------------------------------------------
# Set number of individuals
n_simulations <- 500

# Generic names for each individual
chr_individuals <- paste('Subject', 1:n_simulations)
names(chr_individuals) <- chr_individuals

# Get the first line from the correlation matrix
# In order to keep the original relationships between variables
df_occupations.numeric %>% 
  cov() -> cov_mat

df_occupations.numeric %>% 
  cor() %>% 
  as_tibble() %>%
  slice(1) -> df_correlations

# Simulate normal distributions in accordance with the correlation matrix
# Mean for each variable
df_occupations.numeric %>%
  summarise(across(
    .fns = mean
  )) %>% 
  as.numeric() -> dbl_mean

# Multivariate truncated normal distribution with correlated data
rtmvnorm(
  n = n_simulations
  , mu = dbl_mean
  , sigma = cov_mat
  , lb = rep(0,length(dbl_mean))
) -> mat_trunc_norm

colnames(mat_trunc_norm) <- colnames(df_occupations.numeric)

mat_trunc_norm %>%
  as_tibble() -> df_simulations

# mvrnorm(
#   n = n_simulations
#   , mu = dbl_mean
#   , Sigma = cov_mat
#   , empirical = T
# ) %>%
#   as_tibble() -> df_simulations

# Only non-negative values
sum(df_simulations < 0)
sum(df_simulations < 0)/(ncol(df_simulations)*nrow(df_simulations))

# df_simulations[df_simulations < 0] <- 0

# Add the names of each individual
df_simulations %>% 
  mutate(
    Subject = chr_individuals
    , .before = names(.)[1]
  ) -> df_simulations

# # Verify that the original parameters (mean, sd, and correlation) are still the same
# # Mean
# sum(
#   df_simulations %>%
#     summarise(
#       across(
#         .cols = where(is.numeric)
#         ,.fns = function(x){round(mean(x),1)}
#       )
#     ) ==
#     df_occupations.numeric %>%
#     summarise(
#       across(
#         .cols = where(is.numeric)
#         ,.fns = function(x){round(mean(x),1)}
#       )
#     )
# )/ncol(df_occupations.numeric)
# 
# # Sd
# sum(
#   df_simulations %>%
#     summarise(
#       across(
#         .cols = where(is.numeric)
#         ,.fns = function(x){round(sd(x),1)}
#       )
#     ) ==
#     df_occupations.numeric %>%
#     summarise(
#       across(
#         .cols = where(is.numeric)
#         ,.fns = function(x){round(sd(x),1)}
#       )
#     )
# )/ncol(df_occupations.numeric)
# 
# # Correlations
# sum(
#   df_simulations %>%
#     select(where(
#       is.numeric
#     )) %>%
#     cor() %>%
#     as_tibble() %>%
#     slice(1) %>%
#     round(1) ==
# 
#     df_correlations %>%
#     round(1)
# )/ncol(df_simulations)


# APPLY KNN ---------------------------------------------------------------
# Define k

# RECOMMENDED
# Typical suggested value for k is sqrt(nrow(df))
# Looking for k nearest neighbors in all career clusters
# df_occupations.numeric %>%
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
# k.value <- nrow(df_occupations.numeric)

# Find the k nearest neighbors
lapply(
  chr_individuals
  , function(subject){
    
    df_simulations %>% 
      filter(Subject == subject) -> df_subject
    
    FNN::get.knnx(
      data = df_occupations.numeric
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

# test %>%
#   summarise(
#     across(
#       .cols = starts_with('Similarity.')
#       ,.fns = list(
#         'Max' = max
#         , 'Mean' = mean
#         , 'Min' = min
#         , 'SD' = sd
#       ))
#   ) %>% 
#   pivot_longer(
#     cols = everything()
#     , names_to = 'Similarity'
#     , values_to = 'Value'
#   ) %>% view()

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
