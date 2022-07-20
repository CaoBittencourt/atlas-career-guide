# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# KNN MATCHING FUNCTION -------------------------------------------------------------------------
fun_KNN.matching <- function(
    .df_data.numeric
    , .vec_query.numeric
    , .int_k = 1
    , .auto_select.k = F
){ 
  
  # Get numeric data only
  .df_data.numeric %>%
    select(where(is.numeric)) -> .df_data.numeric.temp
  
  if(is.data.frame(.vec_query.numeric)){
    .vec_query.numeric %>% 
      select(where(is.numeric)) -> .vec_query.numeric
  }
  
  # Define k
  if(.auto_select.k){
    # RECOMMENDED
    # Typical suggested value for k is sqrt(nrow(df))
    # Looking for k nearest neighbors in all career clusters
    
    .df_data.numeric %>% 
      nrow(.) %>%
      sqrt(.) %>%
      round(.) -> .int_k
    
  }
  
  # Find the k nearest neighbors
  FNN::get.knnx(
    data = .df_data.numeric.temp
    , query = .vec_query.numeric
    , k = .int_k
  ) -> KNN.output
  
  # Arrange original data frame with KNN output
  .df_data.numeric %>% 
    slice(as.vector(KNN.output$nn.index)) %>% 
    mutate(#Add euclidean distances and convert them to similarities
      Euclidean_Distance = as.vector(KNN.output$nn.dist)
      # # CHEAP QUICK FIX
      # # , Euclidean_Distance = min(Euclidean_Distance, 2)
      # , Euclidean_Distance = min(Euclidean_Distance, 1)
      # Common similarity: 1/(1 + dist), "+1" for dist = 0
      , Similarity.Common = 1 / (1 + Euclidean_Distance)
      # Similarity via gaussian kernel: exp(-dist)
      , Similarity.Gaussian = exp(-Euclidean_Distance)
      
      # [Try again] Yielding negative values for greater distances
      # Tangent similarity: 1 - arctan(dist) = 1 for dist = 0 
      # , Similarity.Tan = 1 - atan(Euclidean_Distance)
      # , Similarity.Tan = 1 - atan(Euclidean_Distance/2)
      # , Similarity.Tan = 1 - atan(Euclidean_Distance/pi)
      
      # [Try again] Equivalence between euclidean and cosine
      # [Wrong formula] Yielding negative values for greater distances
      # , Similarity.Cosine = 1 - (Euclidean_Distance/2)
      , Similarity.Cosine = 1 - ((Euclidean_Distance^2)/2)
      
    ) %>% 
    return(.)
  
}


# KNN MATCHING FUNCTION (WITH L2 NORMALIZATION) -------------------------------------------------------------------------
fun_KNN.matching <- function(
    .df_data.numeric
    , .vec_query.numeric
    , .int_k = 1
    , .auto_select.k = F
    , .euclidean.norm = T
){ 
  
  # Get numeric data only
  .df_data.numeric %>%
    select(where(is.numeric)) -> .df_data.numeric.temp
  
  if(is.data.frame(.vec_query.numeric)){
    .vec_query.numeric %>% 
      select(where(is.numeric)) -> .vec_query.numeric
  }
  
  # Euclidean normalization (L2 norm)
  if(.euclidean.norm){
    
    .df_data.numeric.temp %>% 
      rowwise() %>% 
      mutate(
        across(
          .cols = everything()
          ,.fns = function(x){x/norm(x, type = '2')}
        )
      ) %>% 
      ungroup() -> .df_data.numeric.temp
    
    if(is.data.frame(.vec_query.numeric)){
      .vec_query.numeric %>% 
        rowwise() %>% 
        mutate(
          across(
            .cols = everything()
            ,.fns = function(x){x/norm(x, type = '2')}
          )
        ) %>% 
        ungroup() -> .vec_query.numeric
      
    } else { 
      
      .vec_query.numeric <- .vec_query.numeric / norm(.vec_query.numeric, type = '2')
      
    }
    
  }
  
  return(
    list(
      .vec_query.numeric
      , .df_data.numeric.temp
    )
  )
  
  # Define k
  if(.auto_select.k){
    # RECOMMENDED
    # Typical suggested value for k is sqrt(nrow(df))
    # Looking for k nearest neighbors in all career clusters
    
    .df_data.numeric %>% 
      nrow(.) %>%
      sqrt(.) %>%
      round(.) -> .int_k
    
  }
  
  # Find the k nearest neighbors
  FNN::get.knnx(
    data = .df_data.numeric.temp
    , query = .vec_query.numeric
    , k = .int_k
  ) -> KNN.output
  
  # Arrange original data frame with KNN output
  .df_data.numeric %>% 
    slice(as.vector(KNN.output$nn.index)) %>% 
    mutate(#Add euclidean distances and convert them to similarities
      Euclidean_Distance = as.vector(KNN.output$nn.dist)
      # # CHEAP QUICK FIX
      # # , Euclidean_Distance = min(Euclidean_Distance, 2)
      # , Euclidean_Distance = min(Euclidean_Distance, 1)
      # Common similarity: 1/(1 + dist), "+1" for dist = 0
      , Similarity.Common = 1 / (1 + Euclidean_Distance)
      # Similarity via gaussian kernel: exp(-dist)
      , Similarity.Gaussian = exp(-Euclidean_Distance)
      
      # [Try again] Yielding negative values for greater distances
      # Tangent similarity: 1 - arctan(dist) = 1 for dist = 0 
      # , Similarity.Tan = 1 - atan(Euclidean_Distance)
      # , Similarity.Tan = 1 - atan(Euclidean_Distance/2)
      # , Similarity.Tan = 1 - atan(Euclidean_Distance/pi)
      
      # [Try again] Equivalence between euclidean and cosine
      # [Wrong formula] Yielding negative values for greater distances
      # , Similarity.Cosine = 1 - (Euclidean_Distance/2)
      , Similarity.Cosine = 1 - ((Euclidean_Distance^2)/2)
      
    ) %>% 
    return(.)
  
}

