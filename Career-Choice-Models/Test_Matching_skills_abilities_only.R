# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'scales' #Visualization
  , 'readr', 'readxl','openxlsx' #Read and write utilities
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
    , where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Knowledge
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

# [ ] EUCLIDEAN DISTANCE ------------------------------------------------------
# Too slow
# df_occupations.numeric %>%
#   as.matrix(.) %>%
#   parallelDist::parDist(method = 'euclidean')

# Performs too many unnecessary calculations
# Suboptimal output
# Although faster than dist(), 
# parDist() is still not the best method in terms of speed and output

# [ ] COSINE DISTANCE (SIMILARITY) ------------------------------------------------------
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

# [ ] COSINE KNN (SIMILARITY) ------------------------------------------------------
# Too slow
# knn.covertree::find_knn(
#   data = df_occupations.numeric[-career.sample,]
#   , query = df_occupations.numeric[career.sample,]
#   , k = k.value
#   , distance = 'cosine'
# )

# "Short answer: Cosine distance is not the overall best performing distance metric out there"
# Performs too many unnecessary calculations
# Suboptimal output
# Although faster than dist(), 
# parDist() is still not the best method in terms of speed and output

# [x] EUCLIDEAN (I.E. REGULAR) KNN ------------------------------------------------------
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

# COMPLETE COMPARISON
# Or find k nearest neighbors using the whole data set
# i.e. order everything with respect to euclidean distance
k.value <- nrow(df_occupations.numeric) - 1

# Find the k nearest neighbors

# Sample
career.sample <- sample(1:nrow(df_occupations.numeric), 1)
career.sample <- 1
FNN::get.knnx(
  data = df_occupations.numeric[-career.sample, ] #Data - sample
  , query = df_occupations.numeric[career.sample, ] #Sample
  , k = k.value
) -> KNN.output

# Arrange original data frame with KNN output
df_occupations %>% 
  slice(c(career.sample
          , as.vector(KNN.output$nn.index))
  ) %>% 
  select(#For the present purposes, keep only the following columns
    Career_Cluster
    , Occupation
    , Annual_Wage_2021
    # , df_loadings.items.skills_ablts$Metric
  ) %>% 
  mutate(#Add euclidean distances and convert them to similarities
    Euclidean_Distance = c(0, as.vector(KNN.output$nn.dist))
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
    
  ) %>% 
  mutate(#Round values
    across(
      .cols = starts_with('Similarity.')
      ,.fns = function(x){round(x,4)}
    )
  ) -> df_occupations.KNN

# view(df_occupations.KNN)

# EXAMPLES OF SIMILARITIES VISUALIZATIONS ---------------------------------
# Similarity bar plot (all professions)
df_occupations.KNN %>% 
  mutate(
    # Occupation = rev(fct_inorder(Occupation))
    Occupation = fct_inorder(Occupation)
  ) %>%
  ggplot(aes(
    # x = Similarity.Gaussian
    x = Similarity.Common
    # x = Similarity.Max
    # x = Similarity.Tan
    # x = Similarity.Cosine
    , y = Occupation
  )) + 
  geom_col() +
  scale_x_continuous(
    expand = c(0,0)
    , limits = c(0,1)
    , breaks = seq(0,1,0.25)
    , labels = percent_format()) + 
  labs(
    title = str_to_title(
      glue('Professions Most Similar to {df_occupations.KNN[1,]$Occupation}')
    )
    , x = 'Similarity (%)'
    , y = NULL
  ) + 
  ggthemes::theme_gdocs()

# EXAMPLES OF SIMILARITIES VISUALIZATIONS ---------------------------------
# Top 10 most similar professions
df_occupations.KNN %>% 
  slice(2:11) %>%
  mutate(
    # Occupation = rev(fct_inorder(Occupation))
    Occupation = fct_inorder(Occupation)
  ) %>% 
  ggplot(aes(
    # x = Similarity.Gaussian
    x = Similarity.Common
    # x = Similarity.Max
    # x = Similarity.Tan
    # x = Similarity.Cosine
    , y = Occupation
    , fill = Wage.Diff > 0 
  )) + 
  geom_col() + 
  scale_x_continuous(
    expand = c(0,0)
    , limits = c(0,1)
    , breaks = seq(0,1,0.25)
    , labels = percent_format()) + 
  labs(
    title = str_to_title(
      glue('Top 10 Professions Most Similar to {df_occupations.KNN[1,]$Occupation}')
    )
    , x = 'Similarity (%)'
    , y = NULL
    , fill = 'Wage Difference > $0.00'
  ) + 
  ggthemes::theme_gdocs()


# WAGE DIFFERENCE X SIMILARITY MATRIX ------------------------------------------------
df_occupations.KNN %>% 
  ggplot(aes(
    x = Wage.Diff
    # , y = Similarity.Gaussian
    , y = Similarity.Common
    , color = Career_Cluster
  )) + 
  geom_hline(yintercept = 0.5, size = 1.5) +
  geom_vline(xintercept = 0, size = 1.5) +
  geom_point(size = 2.2, alpha = 0.6) + 
  scale_x_continuous(
    expand = expansion(add = 25000)
    , labels = dollar_format()
  ) + 
  scale_y_continuous(
    expand = c(0,0)
    , limits = c(0,1)
    , breaks = seq(0,1,0.25)
    , labels = percent_format()) +
  labs(
    title = str_to_title(
      glue('upside vs. similarity matrix for {df_occupations.KNN[1,]$Occupation}')
    )
    , x = 'Wage Difference (USD)'
    , y = 'Similarity (%)'
    , color = 'Career Cluster'
  ) + 
  ggthemes::theme_gdocs()



# TOP 10 WAGE DIFFERENCE X SIMILARITY MATRIX ------------------------------------------------
df_occupations.KNN %>% 
  slice(1:11) %>%
  mutate(
    Career_Cluster = factor(Career_Cluster)
  ) %>% 
  ggplot(aes(
    x = Wage.Diff
    # , y = Similarity.Gaussian
    , y = Similarity.Common
    , color = Career_Cluster
  )) + 
  geom_hline(yintercept = 0.5, size = 1.5) +
  geom_vline(xintercept = 0, size = 1.5) +
  geom_point(size = 15, alpha = 0.6) + 
  scale_x_continuous(
    expand = expansion(add = 25000)
    , labels = dollar_format()
  ) + 
  scale_y_continuous(
    expand = c(0,0)
    , limits = c(0,1)
    , breaks = seq(0,1,0.25)
    , labels = percent_format()) +
  labs(
    title = str_to_title(
      glue('upside vs. similarity matrix for {df_occupations.KNN[1,]$Occupation}')
    )
    , x = 'Wage Difference (USD)'
    , y = 'Similarity (%)'
    , color = 'Career Cluster'
  ) + 
  ggthemes::theme_gdocs()


