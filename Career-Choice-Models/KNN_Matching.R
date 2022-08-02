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

# # KNN MATCHING FUNCTION -------------------------------------------------------------------------
# fun_KNN.matching <- function(
#     .df_data.numeric
#     , .vec_query.numeric
#     , .int_k = 1
#     , .auto_select.k = F
# ){
# 
#   # Get numeric data only
#   .df_data.numeric %>%
#     select(where(is.numeric)) -> .df_data.numeric.temp
# 
#   if(is.data.frame(.vec_query.numeric)){
#     .vec_query.numeric %>%
#       select(where(is.numeric)) -> .vec_query.numeric
#   }
# 
#   # Define k
#   if(.auto_select.k){
#     # RECOMMENDED
#     # Typical suggested value for k is sqrt(nrow(df))
#     # Looking for k nearest neighbors in all career clusters
# 
#     .df_data.numeric %>%
#       nrow(.) %>%
#       sqrt(.) %>%
#       round(.) -> .int_k
# 
#   }
# 
#   # Find the k nearest neighbors
#   FNN::get.knnx(
#     data = .df_data.numeric.temp
#     , query = .vec_query.numeric
#     , k = .int_k
#   ) -> KNN.output
# 
#   # Arrange original data frame with KNN output
#   .df_data.numeric %>%
#     slice(as.vector(KNN.output$nn.index)) %>%
#     dplyr::mutate(#Add euclidean distances and convert them to similarities
#       Euclidean_Distance = as.vector(KNN.output$nn.dist)
#       # TRUNCATE DISTANCE AT 2
#       , Euclidean_Distance = pmin(Euclidean_Distance, 2)
# 
#       # Simple similarity, no adjustments
#       , Similarity.Simple = 1 - Euclidean_Distance
#       # Common similarity: 1/(1 + dist), "+1" for dist = 0
#       , Similarity.Common = 1 / (1 + Euclidean_Distance)
#       # Maximum-normalized similarity: 1 - dist/max(dist)
#       , Similarity.Max = 1 - (Euclidean_Distance / max(Euclidean_Distance))
#       # Similarity via gaussian kernel: exp(-dist)
#       , Similarity.Gaussian = exp(-Euclidean_Distance)
#       # Square root similarity
#       , Similarity.Sqrt1 = 1 - (sqrt(pmin(Euclidean_Distance, sqrt(2))) / 2)
#       # Square root similarity 2 ("cosine-ish" + square root)
#       # , Similarity.Sqrt2 = sqrt(1 - ((Euclidean_Distance^2)/2))
#       , Similarity.Sqrt2 = sqrt(
#         round(1 - ((pmin(Euclidean_Distance, sqrt(2)) ^ 2) / 2), 10)
#         )
#       # Quadratic similarity
#       # , Similarity.Quad = 1 - (Euclidean_Distance^2)
#       , Similarity.Quad = 1 - (pmin(Euclidean_Distance,sqrt(2)) ^ 2)
#       # "Cosine-ish" similarity
#       # Equivalence between euclidean and cosine (if euclidean E [0,2])
#       , Similarity.Cosine = 1 - ((Euclidean_Distance ^ 2) / 2)
#       # Angular similarity
#       , Similarity.Angular = 1 - (acos(Similarity.Cosine) / pi)
# 
#       # [Try again] Yielding negative values for greater distances
#       # Tangent similarity: 1 - arctan(dist) = 1 for dist = 0
#       # , Similarity.Tan = 1 - atan(Euclidean_Distance)
#       # , Similarity.Tan = 1 - atan(Euclidean_Distance/2)
#       # , Similarity.Tan = 1 - atan(Euclidean_Distance/pi)
# 
#     ) %>%
#     return(.)
# 
# }
# 

# KNN MATCHING FUNCTION -------------------------------------------------------------------------
fun_KNN.matching <- function(
    .df_data.numeric
    , .vec_query.numeric
    , .int_k = 1
    , .auto_select.k = F
    , .imput.over_qualification = T
    , .dbl_over_qualification.threshold = 0.05
    , .dbl_decimals = 4
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
  
  if(.imput.over_qualification){
    
    .vec_query.numeric %>%
      rename_with(
        .fn = function(x){paste0(x,'.imput')}
      ) %>%
      bind_cols(
        .df_data.numeric.temp
      ) %>%
      mutate(
        across(
          .cols = c(
            !ends_with('.imput')
          )
          ,.fns = function(x){
            
            ifelse(
              # Overqualified if > cutoff and requirement <= cutoff
              x <= .dbl_over_qualification.threshold 
              & eval(sym(paste0(cur_column(),'.imput'))) > x
              , yes = x
              , no = eval(sym(paste0(cur_column(),'.imput')))
            )
            
          }
          , .names = '{col}.sub'
        )
      ) %>%
      select(
        ends_with('.sub')
      ) %>%
      rename_with(
        function(x){str_remove(x,'.sub')}
      ) -> .vec_query.numeric
    
    lapply(
      1:nrow(.vec_query.numeric)
      , function(x){
        
        FNN::get.knnx(
          data = .df_data.numeric.temp[x,]
          , query = .vec_query.numeric[x,]
          , k = 1
        ) -> KNN.output
        
      }) %>% 
      bind_rows() -> KNN.output 
      
    # # Find the k nearest neighbors
    # FNN::get.knnx(
    #   data = .df_data.numeric.temp
    #   , query = .vec_query.numeric
    #   , k = 1
    # ) -> KNN.output

    # KNN.output$nn.index[,1] -> KNN.output$nn.index
    # 
    # KNN.output$nn.dist[,1] -> KNN.output$nn.dist
    
    # Arrange original data frame with KNN output
    .df_data.numeric %>%
      mutate(#Add euclidean distances and convert them to similarities
        Euclidean_Distance = as.vector(KNN.output$nn.dist)
        
        # Simple similarity, no adjustments
        , Similarity.Sub1 = 1 - pmin(Euclidean_Distance, 1) #Bound at [0,1]
        , Similarity.Sub2 = 1 - pmin(Euclidean_Distance, 2) #Bound at [-1,1]
        # Common similarity: 1/(1 + dist), "+1" for dist = 0
        , Similarity.Div = 1 / (1 + Euclidean_Distance) #Bound at [0,1]
        # Maximum-normalized similarity: 1 - dist/max(dist)
        , Similarity.Max = 1 - (Euclidean_Distance / max(Euclidean_Distance)) #Bound at [0,1]
        # Similarity via gaussian kernel: exp(-dist)
        , Similarity.Gauss1 = exp(-Euclidean_Distance) #Bound at [0,1]
        , Similarity.Gauss2 = exp(-0.5*Euclidean_Distance) #Bound at [0,1]
        , Similarity.Gauss3 = exp(-0.25*Euclidean_Distance) #Bound at [0,1]
        , Similarity.Gauss4 = exp(-0.125*Euclidean_Distance) #Bound at [0,1]
        # Square root similarity
        , Similarity.Sqrt1 = 1 - (sqrt(pmin(Euclidean_Distance, 4)) / 2) #Bound at [0,1]
        , Similarity.Sqrt2 = 1 - (sqrt(pmin(Euclidean_Distance, 16)) / 2) #Bound at [-1,1]
        # Square root similarity 2 ("cosine-ish" + square root)
        , Similarity.Sqrt3 = sqrt(
          round(1 - ((pmin(Euclidean_Distance, sqrt(2)) ^ 2) / 2), 10)
        ) #Bound at [0,1]
        , Similarity.Sqrt4 = sqrt(
          round(1 - ((pmin(Euclidean_Distance, 2) ^ 2) / 4), 10)
        ) #Bound at [0,1]
        , Similarity.Sqrt5 = sqrt(
          round(1 - ((pmin(Euclidean_Distance, 3) ^ 2) / 9), 10)
        ) #Bound at [0,1]
        # Quadratic similarity
        # , Similarity.Quad = 1 - (Euclidean_Distance^2)
        , Similarity.Quad1 = 1 - (pmin(Euclidean_Distance,1) ^ 2) #Bound at [0,1]
        , Similarity.Quad2 = 1 - (pmin(Euclidean_Distance,sqrt(2)) ^ 2) / 2 #Bound at [0,1]
        , Similarity.Quad3 = 1 - (pmin(Euclidean_Distance,sqrt(2)) ^ 2) #Bound at [-1,1]
        # "Cosine-ish" similarity
        # Equivalence between euclidean and cosine (if euclidean E [0,2])
        , Similarity.Cos = 1 - ((pmin(Euclidean_Distance, 2) ^ 2) / 2) #Bound at [-1,1]
        # Angular similarity
        , Similarity.Ang = 1 - (acos(Similarity.Cos) / pi) #Bound at [0,1]
        
        , across(
          .cols = starts_with('Similarity.')
          ,.fns = function(x){round(x,.dbl_decimals)}
        )
        
      ) %>%
      arrange(Euclidean_Distance) %>% 
      return(.)
    
  } else {
    
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
        
        # Simple similarity, no adjustments
        , Similarity.Sub1 = 1 - pmin(Euclidean_Distance, 1) #Bound at [0,1]
        , Similarity.Sub2 = 1 - pmin(Euclidean_Distance, 2) #Bound at [-1,1]
        # Common similarity: 1/(1 + dist), "+1" for dist = 0
        , Similarity.Div = 1 / (1 + Euclidean_Distance) #Bound at [0,1]
        # Maximum-normalized similarity: 1 - dist/max(dist)
        , Similarity.Max = 1 - (Euclidean_Distance / max(Euclidean_Distance)) #Bound at [0,1]
        # Similarity via gaussian kernel: exp(-dist)
        , Similarity.Gauss1 = exp(-Euclidean_Distance) #Bound at [0,1]
        , Similarity.Gauss2 = exp(-0.5*Euclidean_Distance) #Bound at [0,1]
        , Similarity.Gauss3 = exp(-0.25*Euclidean_Distance) #Bound at [0,1]
        , Similarity.Gauss4 = exp(-0.125*Euclidean_Distance) #Bound at [0,1]
        # Square root similarity
        , Similarity.Sqrt1 = 1 - (sqrt(pmin(Euclidean_Distance, 4)) / 2) #Bound at [0,1]
        , Similarity.Sqrt2 = 1 - (sqrt(pmin(Euclidean_Distance, 16)) / 2) #Bound at [-1,1]
        # Square root similarity 2 ("cosine-ish" + square root)
        , Similarity.Sqrt3 = sqrt(
          round(1 - ((pmin(Euclidean_Distance, sqrt(2)) ^ 2) / 2), 10)
        ) #Bound at [0,1]
        , Similarity.Sqrt4 = sqrt(
          round(1 - ((pmin(Euclidean_Distance, 2) ^ 2) / 4), 10)
        ) #Bound at [0,1]
        , Similarity.Sqrt5 = sqrt(
          round(1 - ((pmin(Euclidean_Distance, 3) ^ 2) / 9), 10)
        ) #Bound at [0,1]
        # Quadratic similarity
        # , Similarity.Quad = 1 - (Euclidean_Distance^2)
        , Similarity.Quad1 = 1 - (pmin(Euclidean_Distance,1) ^ 2) #Bound at [0,1]
        , Similarity.Quad2 = 1 - (pmin(Euclidean_Distance,sqrt(2)) ^ 2) / 2 #Bound at [0,1]
        , Similarity.Quad3 = 1 - (pmin(Euclidean_Distance,sqrt(2)) ^ 2) #Bound at [-1,1]
        # "Cosine-ish" similarity
        # Equivalence between euclidean and cosine (if euclidean E [0,2])
        , Similarity.Cos = 1 - ((pmin(Euclidean_Distance, 2) ^ 2) / 2) #Bound at [-1,1]
        # Angular similarity
        , Similarity.Ang = 1 - (acos(Similarity.Cos) / pi) #Bound at [0,1]
        
        , across(
          .cols = starts_with('Similarity.')
          ,.fns = function(x){round(x,.dbl_decimals)}
        )
        
      ) %>%
      return(.)
    
  }
  
}

