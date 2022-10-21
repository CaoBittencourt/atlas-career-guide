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
#       euclidean_distance = as.vector(KNN.output$nn.dist)
#       # TRUNCATE DISTANCE AT 2
#       , euclidean_distance = pmin(euclidean_distance, 2)
# 
#       # Simple similarity, no adjustments
#       , similarity.Simple = 1 - euclidean_distance
#       # Common similarity: 1/(1 + dist), "+1" for dist = 0
#       , similarity.Common = 1 / (1 + euclidean_distance)
#       # Maximum-normalized similarity: 1 - dist/max(dist)
#       , similarity.Max = 1 - (euclidean_distance / max(euclidean_distance))
#       # similarity via gaussian kernel: exp(-dist)
#       , similarity.Gaussian = exp(-euclidean_distance)
#       # Square root similarity
#       , similarity.Sqrt1 = 1 - (sqrt(pmin(euclidean_distance, sqrt(2))) / 2)
#       # Square root similarity 2 ("cosine-ish" + square root)
#       # , similarity.Sqrt2 = sqrt(1 - ((euclidean_distance^2)/2))
#       , similarity.Sqrt2 = sqrt(
#         round(1 - ((pmin(euclidean_distance, sqrt(2)) ^ 2) / 2), 10)
#         )
#       # Quadratic similarity
#       # , similarity.Quad = 1 - (euclidean_distance^2)
#       , similarity.Quad = 1 - (pmin(euclidean_distance,sqrt(2)) ^ 2)
#       # "Cosine-ish" similarity
#       # Equivalence between euclidean and cosine (if euclidean E [0,2])
#       , similarity.Cosine = 1 - ((euclidean_distance ^ 2) / 2)
#       # Angular similarity
#       , similarity.Angular = 1 - (acos(similarity.Cosine) / pi)
# 
#       # [Try again] Yielding negative values for greater distances
#       # Tangent similarity: 1 - arctan(dist) = 1 for dist = 0
#       # , similarity.Tan = 1 - atan(euclidean_distance)
#       # , similarity.Tan = 1 - atan(euclidean_distance/2)
#       # , similarity.Tan = 1 - atan(euclidean_distance/pi)
# 
#     ) %>%
#     return(.)
# 
# }
# 

# # KNN MATCHING FUNCTION -------------------------------------------------------------------------
# fun_KNN.matching <- function(
    #     .df_data.numeric
#     , .vec_query.numeric
#     , .int_k = 1
#     , .auto_select.k = F
#     , .imput.over_qualification = T
#     , .dbl_over_qualification.threshold = 0.05
#     , .dbl_decimals = 4
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
#   if(.imput.over_qualification){
#     
#     .vec_query.numeric %>%
#       rename_with(
#         .fn = function(x){paste0(x,'.imput')}
#       ) %>%
#       bind_cols(
#         .df_data.numeric.temp
#       ) %>%
#       mutate(
#         across(
#           .cols = c(
#             !ends_with('.imput')
#           )
#           ,.fns = function(x){
#             
#             ifelse(
#               # Overqualified if > cutoff and requirement <= cutoff
#               x <= .dbl_over_qualification.threshold 
#               & eval(sym(paste0(cur_column(),'.imput'))) > x
#               , yes = x
#               , no = eval(sym(paste0(cur_column(),'.imput')))
#             )
#             
#           }
#           , .names = '{col}.sub'
#         )
#       ) %>%
#       select(
#         ends_with('.sub')
#       ) %>%
#       rename_with(
#         function(x){str_remove(x,'.sub')}
#       ) -> .vec_query.numeric
#     
#     lapply(
#       1:nrow(.vec_query.numeric)
#       , function(x){
#         
#         FNN::get.knnx(
#           data = .df_data.numeric.temp[x,]
#           , query = .vec_query.numeric[x,]
#           , k = 1
#         ) -> KNN.output
#         
#       }) %>% 
#       bind_rows() -> KNN.output 
#       
#     # # Find the k nearest neighbors
#     # FNN::get.knnx(
#     #   data = .df_data.numeric.temp
#     #   , query = .vec_query.numeric
#     #   , k = 1
#     # ) -> KNN.output
# 
#     # KNN.output$nn.index[,1] -> KNN.output$nn.index
#     # 
#     # KNN.output$nn.dist[,1] -> KNN.output$nn.dist
#     
#     # Arrange original data frame with KNN output
#     .df_data.numeric %>%
#       mutate(#Add euclidean distances and convert them to similarities
#         euclidean_distance = as.vector(KNN.output$nn.dist)
#         
#         # Simple similarity, no adjustments
#         , similarity.Sub1 = 1 - pmin(euclidean_distance, 1) #Bound at [0,1]
#         , similarity.Sub2 = 1 - pmin(euclidean_distance, 2) #Bound at [-1,1]
#         # Common similarity: 1/(1 + dist), "+1" for dist = 0
#         , similarity.Div = 1 / (1 + euclidean_distance) #Bound at [0,1]
#         # Maximum-normalized similarity: 1 - dist/max(dist)
#         , similarity.Max = 1 - (euclidean_distance / max(euclidean_distance)) #Bound at [0,1]
#         # similarity via gaussian kernel: exp(-dist)
#         , similarity.Gauss1 = exp(-euclidean_distance) #Bound at [0,1]
#         , similarity.Gauss2 = exp(-0.5*euclidean_distance) #Bound at [0,1]
#         , similarity.Gauss3 = exp(-0.25*euclidean_distance) #Bound at [0,1]
#         , similarity.Gauss4 = exp(-0.125*euclidean_distance) #Bound at [0,1]
#         # Square root similarity
#         , similarity.Sqrt1 = 1 - (sqrt(pmin(euclidean_distance, 4)) / 2) #Bound at [0,1]
#         , similarity.Sqrt2 = 1 - (sqrt(pmin(euclidean_distance, 16)) / 2) #Bound at [-1,1]
#         # Square root similarity 2 ("cosine-ish" + square root)
#         , similarity.Sqrt3 = sqrt(
#           round(1 - ((pmin(euclidean_distance, sqrt(2)) ^ 2) / 2), 10)
#         ) #Bound at [0,1]
#         , similarity.Sqrt4 = sqrt(
#           round(1 - ((pmin(euclidean_distance, 2) ^ 2) / 4), 10)
#         ) #Bound at [0,1]
#         , similarity.Sqrt5 = sqrt(
#           round(1 - ((pmin(euclidean_distance, 3) ^ 2) / 9), 10)
#         ) #Bound at [0,1]
#         # Quadratic similarity
#         # , similarity.Quad = 1 - (euclidean_distance^2)
#         , similarity.Quad1 = 1 - (pmin(euclidean_distance,1) ^ 2) #Bound at [0,1]
#         , similarity.Quad2 = 1 - (pmin(euclidean_distance,sqrt(2)) ^ 2) / 2 #Bound at [0,1]
#         , similarity.Quad3 = 1 - (pmin(euclidean_distance,sqrt(2)) ^ 2) #Bound at [-1,1]
#         , similarity.Quad4 = 1 - (pmin(euclidean_distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
#         , similarity.Quad5 = 1 - (pmin(euclidean_distance,3) ^ 2) / 4.5 #Bound at [-1,1]
#         # "Cosine-ish" similarity
#         # Equivalence between euclidean and cosine (if euclidean E [0,2])
#         , similarity.Cos = 1 - ((pmin(euclidean_distance, 2) ^ 2) / 2) #Bound at [-1,1]
#         # Angular similarity
#         , similarity.Ang = 1 - (acos(similarity.Cos) / pi) #Bound at [0,1]
#         
#         , across(
#           .cols = starts_with('similarity.')
#           ,.fns = function(x){round(x,.dbl_decimals)}
#         )
#         
#       ) %>%
#       arrange(euclidean_distance) %>% 
#       return(.)
#     
#   } else {
#     
#     # Find the k nearest neighbors
#     FNN::get.knnx(
#       data = .df_data.numeric.temp
#       , query = .vec_query.numeric
#       , k = .int_k
#     ) -> KNN.output
#     
#     # Arrange original data frame with KNN output
#     .df_data.numeric %>%
#       slice(as.vector(KNN.output$nn.index)) %>%
#       mutate(#Add euclidean distances and convert them to similarities
#         euclidean_distance = as.vector(KNN.output$nn.dist)
#         
#         # Simple similarity, no adjustments
#         , similarity.Sub1 = 1 - pmin(euclidean_distance, 1) #Bound at [0,1]
#         , similarity.Sub2 = 1 - pmin(euclidean_distance, 2) #Bound at [-1,1]
#         # Common similarity: 1/(1 + dist), "+1" for dist = 0
#         , similarity.Div = 1 / (1 + euclidean_distance) #Bound at [0,1]
#         # Maximum-normalized similarity: 1 - dist/max(dist)
#         , similarity.Max = 1 - (euclidean_distance / max(euclidean_distance)) #Bound at [0,1]
#         # similarity via gaussian kernel: exp(-dist)
#         , similarity.Gauss1 = exp(-euclidean_distance) #Bound at [0,1]
#         , similarity.Gauss2 = exp(-0.5*euclidean_distance) #Bound at [0,1]
#         , similarity.Gauss3 = exp(-0.25*euclidean_distance) #Bound at [0,1]
#         , similarity.Gauss4 = exp(-0.125*euclidean_distance) #Bound at [0,1]
#         # Square root similarity
#         , similarity.Sqrt1 = 1 - (sqrt(pmin(euclidean_distance, 4)) / 2) #Bound at [0,1]
#         , similarity.Sqrt2 = 1 - (sqrt(pmin(euclidean_distance, 16)) / 2) #Bound at [-1,1]
#         # Square root similarity 2 ("cosine-ish" + square root)
#         , similarity.Sqrt3 = sqrt(
#           round(1 - ((pmin(euclidean_distance, sqrt(2)) ^ 2) / 2), 10)
#         ) #Bound at [0,1]
#         , similarity.Sqrt4 = sqrt(
#           round(1 - ((pmin(euclidean_distance, 2) ^ 2) / 4), 10)
#         ) #Bound at [0,1]
#         , similarity.Sqrt5 = sqrt(
#           round(1 - ((pmin(euclidean_distance, 3) ^ 2) / 9), 10)
#         ) #Bound at [0,1]
#         # Quadratic similarity
#         # , similarity.Quad = 1 - (euclidean_distance^2)
#         , similarity.Quad1 = 1 - (pmin(euclidean_distance,1) ^ 2) #Bound at [0,1]
#         , similarity.Quad2 = 1 - (pmin(euclidean_distance,sqrt(2)) ^ 2) / 2 #Bound at [0,1]
#         , similarity.Quad3 = 1 - (pmin(euclidean_distance,sqrt(2)) ^ 2) #Bound at [-1,1]
#         , similarity.Quad4 = 1 - (pmin(euclidean_distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
#         , similarity.Quad5 = 1 - (pmin(euclidean_distance,3) ^ 2) / 4.5 #Bound at [-1,1]
#         # "Cosine-ish" similarity
#         # Equivalence between euclidean and cosine (if euclidean E [0,2])
#         , similarity.Cos = 1 - ((pmin(euclidean_distance, 2) ^ 2) / 2) #Bound at [-1,1]
#         # Angular similarity
#         , similarity.Ang = 1 - (acos(similarity.Cos) / pi) #Bound at [0,1]
#         
#         , across(
#           .cols = starts_with('similarity.')
#           ,.fns = function(x){round(x,.dbl_decimals)}
#         )
#         
#       ) %>%
#       arrange(euclidean_distance) %>%
#       return(.)
#     
#   }
#   
# }
# 

# DISTANCE TO SIMILARITY FUNCTION ------------------------------
fun_similarity <- function(.dbl_distance){
  
  # # Already considered formulae
  # similarity.sub1 <- 1 - pmin(.dbl_distance, 1) #Bound at [0,1]
  # similarity.sub2 <- 1 - pmin(.dbl_distance, 2) #Bound at [-1,1]
  # # Common similarity: 1/(1 + dist), "+1" for dist <- 0
  # similarity.div <- 1 / (1 + .dbl_distance) #Bound at [0,1]
  # # maximum-normalized similarity: 1 - dist/max(dist)
  # similarity.max <- 1 - (.dbl_distance / max(.dbl_distance)) #Bound at [0,1]
  # # similarity via gaussian kernel: exp(-dist)
  # similarity.gauss1 <- exp(-.dbl_distance) #Bound at [0,1]
  # similarity.gauss2 <- exp(-0.5*.dbl_distance) #Bound at [0,1]
  # similarity.gauss3 <- exp(-0.25*.dbl_distance) #Bound at [0,1]
  # similarity.gauss4 <- exp(-0.125*.dbl_distance) #Bound at [0,1]
  # # Square root similarity
  # similarity.sqrt1 <- 1 - (sqrt(pmin(.dbl_distance, 4)) / 2) #Bound at [0,1]
  # similarity.sqrt2 <- 1 - (sqrt(pmin(.dbl_distance, 16)) / 2) #Bound at [-1,1]
  # # Square root similarity 2 ("cosine-ish" + square root)
  # similarity.sqrt3 <- sqrt(
  #   round(1 - ((pmin(.dbl_distance, sqrt(2)) ^ 2) / 2), 10)
  # ) #Bound at [0,1]
  # similarity.sqrt4 <- sqrt(
  #   round(1 - ((pmin(.dbl_distance, 2) ^ 2) / 4), 10)
  # ) #Bound at [0,1]
  # similarity.sqrt5 <- sqrt(
  #   round(1 - ((pmin(.dbl_distance, 3) ^ 2) / 9), 10)
  # ) #Bound at [0,1]
  # # quadratic similarity
  # # , similarity.quad <- 1 - (.dbl_distance^2)
  # similarity.quad1 <- 1 - (pmin(.dbl_distance,1) ^ 2) #Bound at [0,1]
  # similarity.quad2 <- 1 - (pmin(.dbl_distance,sqrt(2)) ^ 2) / 2 #Bound at [0,1]
  # similarity.quad3 <- 1 - (pmin(.dbl_distance,sqrt(2)) ^ 2) #Bound at [-1,1]
  # similarity.quad4 <- 1 - (pmin(.dbl_distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
  # similarity.quad5 <- 1 - (pmin(.dbl_distance,3) ^ 2) / 4.5 #Bound at [-1,1]
  # # "cosine-ish" similarity
  # # Equivalence between euclidean and cosine (if euclidean E [0,2])
  # similarity.cos <- 1 - ((pmin(.dbl_distance, 2) ^ 2) / 2) #Bound at [-1,1]
  # # angular similarity
  # similarity.ang <- 1 - (acos(similarity.cos) / pi) #Bound at [0,1]
  
  similarity.quad <- 1 - (pmin(.dbl_distance, 2.5) ^ 2) / 6.25 #Bound at [0,1]
  
  return(similarity.quad)
  
}

# # KNN MATCHING FUNCTION -------------------------------------------------------------------------
# fun_KNN.matching <- function(
#     .df_data.numeric
#     , .vec_query.numeric
#     , .int_k = 1
#     , .auto_select.k = F
#     , .imput.over_qualification = T
#     , .dbl_over_qualification.threshold = 0.1
#     , .dbl_decimals = 4
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
#   if(.imput.over_qualification){
#     
#     .vec_query.numeric %>%
#       rename_with(
#         .fn = function(x){paste0(x,'.imput')}
#       ) %>%
#       bind_cols(
#         .df_data.numeric.temp
#       ) %>%
#       mutate(
#         across(
#           .cols = c(
#             !ends_with('.imput')
#           )
#           ,.fns = function(x){
#             
#             ifelse(
#               # Overqualified if > cutoff and requirement <= cutoff
#               x <= .dbl_over_qualification.threshold 
#               & eval(sym(paste0(cur_column(),'.imput'))) > x
#               , yes = x
#               , no = eval(sym(paste0(cur_column(),'.imput')))
#             )
#             
#           }
#           , .names = '{col}.sub'
#         )
#       ) %>%
#       select(
#         ends_with('.sub')
#       ) %>%
#       rename_with(
#         function(x){str_remove(x,'.sub')}
#       ) -> .vec_query.numeric
#     
#     lapply(
#       1:nrow(.vec_query.numeric)
#       , function(x){
#         
#         FNN::get.knnx(
#           data = .df_data.numeric.temp[x,]
#           , query = .vec_query.numeric[x,]
#           , k = 1
#         ) -> KNN.output
#         
#       }) %>% 
#       bind_rows() -> KNN.output 
#     
#     # # Find the k nearest neighbors
#     # FNN::get.knnx(
#     #   data = .df_data.numeric.temp
#     #   , query = .vec_query.numeric
#     #   , k = 1
#     # ) -> KNN.output
#     
#     # KNN.output$nn.index[,1] -> KNN.output$nn.index
#     # 
#     # KNN.output$nn.dist[,1] -> KNN.output$nn.dist
#     
#     # Arrange original data frame with KNN output
#     .df_data.numeric %>%
#       mutate(#Add euclidean distances and convert them to similarities
#         euclidean_distance = as.vector(KNN.output$nn.dist)
#         
#         , similarity = 1 - (pmin(euclidean_distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
#         , similarity = (1 + (1 - (pmin(euclidean_distance,2.5) ^ 2) / 3.125)) / 2 #Bound at [0,1]
#         
#         , across(
#           .cols = starts_with('similarity')
#           ,.fns = function(x){round(x,.dbl_decimals)}
#         )
#         
#       ) %>%
#       arrange(euclidean_distance) %>% 
#       return(.)
#     
#   } else {
#     
#     # Find the k nearest neighbors
#     FNN::get.knnx(
#       data = .df_data.numeric.temp
#       , query = .vec_query.numeric
#       , k = .int_k
#     ) -> KNN.output
#     
#     # Arrange original data frame with KNN output
#     .df_data.numeric %>%
#       slice(as.vector(KNN.output$nn.index)) %>%
#       mutate(#Add euclidean distances and convert them to similarities
#         euclidean_distance = as.vector(KNN.output$nn.dist)
#         
#         # , similarity = 1 - (pmin(euclidean_distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
#         , similarity = (1 + (1 - (pmin(euclidean_distance,2.5) ^ 2) / 3.125)) / 2 #Bound at [0,1]
#         
#         , across(
#           .cols = starts_with('similarity')
#           ,.fns = function(x){round(x,.dbl_decimals)}
#         )
#         
#       ) %>% 
#       arrange(euclidean_distance) %>% 
#       return(.)
#     
#   }
#   
# }

# KNN MATCHING FUNCTION -------------------------------------------------------------------------
fun_KNN.matching <- function(
    .df_data.numeric
    , .vec_query.numeric
    , .int_k = 1
    , .auto_select.k = F
    , .imput.over_qualification = T
    , .dbl_over_qualification.threshold = 0.1
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
        euclidean_distance = as.vector(KNN.output$nn.dist)
        , similarity = fun_similarity(euclidean_distance)
        , across(
          .cols = starts_with('similarity')
          ,.fns = function(x){round(x,.dbl_decimals)}
        )
        
      ) %>%
      arrange(euclidean_distance) %>% 
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
        euclidean_distance = as.vector(KNN.output$nn.dist)
        , similarity = fun_similarity(euclidean_distance)
        , across(
          .cols = starts_with('similarity')
          ,.fns = function(x){round(x,.dbl_decimals)}
        )
        
      ) %>% 
      arrange(euclidean_distance) %>% 
      return(.)
    
  }
  
}
