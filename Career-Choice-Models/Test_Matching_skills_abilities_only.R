# PACKAGES -----------------------------------------------------------------
pkg <- c(
  # 'ggthemes', 'scales' #Visualization
  'readr', 'readxl','openxlsx' #Read and write utilities
  , 'tidyverse', 'labelled' #Data wrangling
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
    where(is.numeric)
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance values
  ) %>%
  select(
    df_loadings.items.skills_ablts$Metric #Selected skills and abilities only
  ) -> df_occupations.numeric

# Simpler names, if needed
# colnames(df_occupations.numeric) <- paste0('V',seq_along(df_occupations.numeric))


# [NO] EUCLIDEAN DISTANCE ------------------------------------------------------
# df_occupations.numeric %>% 
#   as.matrix(.) %>% 
#   parallelDist::parDist(method = 'euclidean')

# Performs too many unnecessary calculations
# Suboptimal output
# Although faster than dist(), 
# parDist() is still not the best method in terms of speed and output

# COSINE DISTANCE (SIMILARITY) ------------------------------------------------------
# df_occupations.numeric %>%
#   as.matrix(.) %>%
#   parallelDist::parDist(method = 'cosine')

# This gives similarity between variables (not rows)
# df_occupations.numeric %>%
#   as.matrix(.) %>%
#   lsa::cosine(.)  

# df_occupations.numeric %>%
#   as.matrix(.) -> dsds
# 
# # This gives the similarity between two random careers
# lsa::cosine(
#   x = dsds[sample(1:nrow(df_occupations.numeric), 1), ]
#   , y = dsds[sample(1:nrow(df_occupations.numeric), 1), ]
# ) 
# 
# # Something must be wrong. Accountants and actors are not 88% the same
# lsa::cosine(
#   x = dsds[1, ]
#   , y = dsds[2, ]
# ) 


# "Short answer: Cosine distance is not the overall best performing distance metric out there"
# Performs too many unnecessary calculations
# Suboptimal output
# Although faster than dist(), 
# parDist() is still not the best method in terms of speed and output

# KNN ------------------------------------------------------
# Typical suggested value for k is sqrt(nrow(df))
# Looking for k nearest neighbors in all career clusters
df_occupations.numeric %>%
  nrow(.) %>%
  sqrt(.) %>%
  round(.) -> k.value

# Or arbitrarily assign k (e.g. top 10 closest matches)
# May k could be given via user input
# k.value <- 10

# If using KNN to find best matches within a given field,
# k-value must be less than the number of careers in the field
# Number of occupations in each career cluster
# i.e. subset original DF, then applyr sqrt(nrow(DF)) to subset
# df_occupations %>%
#   group_by(Career_Cluster) %>%
#   tally(.) %>%
#   arrange(desc(n)) %>% 
#   mutate(k = round(sqrt(n)))


# Subsets of data (usual guideline is to follow the 80/20 rule, i.e. Pareto Principle)
# Training set (80)

# Test set (20)


# Find the k nearest neighbors
career.sample <- sample(1:nrow(df_occupations.numeric), 1)

#Euclidean normalization
df_occupations.numeric %>% 
  norm(type = '2') -> df_occupations.numeric.norm

FNN::get.knnx(
  data = df_occupations.numeric[-career.sample, ]
  , query = df_occupations.numeric[career.sample, ]
  , k = k.value
) -> KNN.output

# Package too slow
# knn.covertree::find_knn(
#   data = df_occupations.numeric[-career.sample,]
#   , query = df_occupations.numeric[career.sample,]
#   , k = k.value
#   , distance = 'cosine'
# )

# Convert euclidean distance to measure of similarity

# Using euclidean-cosine equivalence
# cos(theta) = 1 - e.dist/2
# 1 - ((KNN.output$nn.dist/max(KNN.output$nn.dist))/2)
# 1 - (KNN.output$nn.dist^2)/2

# 1 - ((KNN.output$nn.dist / sqrt( sum ( KNN.output$nn.dist ^ 2)))^2)/2 

# 1 / (1 + (KNN.output$nn.dist)^.25)

# Using gaussian kernel (ok)
# exp(-KNN.output$nn.dist)
# round(exp(-KNN.output$nn.dist/(2*(sd(KNN.output$nn.dist)^2))),2)
round(
  exp(-
        (
          KNN.output$nn.dist
          / (2*sd(KNN.output$nn.dist)^2)
        )
  )
  , 2)

# Won't work for KNN
# round(1 - KNN.output$nn.dist/max(KNN.output$nn.dist),2)
# Using inverse of euclidean distance
# round(1/(1 + KNN.output$nn.dist), 2)
