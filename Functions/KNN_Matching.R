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

# # DISTANCE TO SIMILARITY FUNCTION ------------------------------
# fun_similarity <- function(.dbl_distance){
#   
#   # # Already considered formulae
#   # similarity.sub1 <- 1 - pmin(.dbl_distance, 1) #Bound at [0,1]
#   # similarity.sub2 <- 1 - pmin(.dbl_distance, 2) #Bound at [-1,1]
#   # # Common similarity: 1/(1 + dist), "+1" for dist <- 0
#   # similarity.div <- 1 / (1 + .dbl_distance) #Bound at [0,1]
#   # # maximum-normalized similarity: 1 - dist/max(dist)
#   # similarity.max <- 1 - (.dbl_distance / max(.dbl_distance)) #Bound at [0,1]
#   # # similarity via gaussian kernel: exp(-dist)
#   # similarity.gauss1 <- exp(-.dbl_distance) #Bound at [0,1]
#   # similarity.gauss2 <- exp(-0.5*.dbl_distance) #Bound at [0,1]
#   # similarity.gauss3 <- exp(-0.25*.dbl_distance) #Bound at [0,1]
#   # similarity.gauss4 <- exp(-0.125*.dbl_distance) #Bound at [0,1]
#   # # Square root similarity
#   # similarity.sqrt1 <- 1 - (sqrt(pmin(.dbl_distance, 4)) / 2) #Bound at [0,1]
#   # similarity.sqrt2 <- 1 - (sqrt(pmin(.dbl_distance, 16)) / 2) #Bound at [-1,1]
#   # # Square root similarity 2 ("cosine-ish" + square root)
#   # similarity.sqrt3 <- sqrt(
#   #   round(1 - ((pmin(.dbl_distance, sqrt(2)) ^ 2) / 2), 10)
#   # ) #Bound at [0,1]
#   # similarity.sqrt4 <- sqrt(
#   #   round(1 - ((pmin(.dbl_distance, 2) ^ 2) / 4), 10)
#   # ) #Bound at [0,1]
#   # similarity.sqrt5 <- sqrt(
#   #   round(1 - ((pmin(.dbl_distance, 3) ^ 2) / 9), 10)
#   # ) #Bound at [0,1]
#   # # quadratic similarity
#   # # , similarity.quad <- 1 - (.dbl_distance^2)
#   # similarity.quad1 <- 1 - (pmin(.dbl_distance,1) ^ 2) #Bound at [0,1]
#   # similarity.quad2 <- 1 - (pmin(.dbl_distance,sqrt(2)) ^ 2) / 2 #Bound at [0,1]
#   # similarity.quad3 <- 1 - (pmin(.dbl_distance,sqrt(2)) ^ 2) #Bound at [-1,1]
#   # similarity.quad4 <- 1 - (pmin(.dbl_distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
#   # similarity.quad5 <- 1 - (pmin(.dbl_distance,3) ^ 2) / 4.5 #Bound at [-1,1]
#   # # "cosine-ish" similarity
#   # # Equivalence between euclidean and cosine (if euclidean E [0,2])
#   # similarity.cos <- 1 - ((pmin(.dbl_distance, 2) ^ 2) / 2) #Bound at [-1,1]
#   # # angular similarity
#   # similarity.ang <- 1 - (acos(similarity.cos) / pi) #Bound at [0,1]
#   
#   # similarity.quad <- 1 - (pmin(.dbl_distance, 2.5) ^ 2) / 6.25 #Bound at [0,1]
#   # similarity.quad <- 1 - ((pmin(.dbl_distance, 2.5) / 2.5) ^ 2) #Bound at [0,1]
#   # similarity.quad <- 1 - (pmin(.dbl_distance, 2.5 * (120/33)) / (2.5 * (120 / 33)))^2 #Bound at [0,1]
#   # similarity.quad <- 1 - .dbl_distance / 120 #Bound at [0,1]
#   # similarity.quad <- 1 - sqrt(.dbl_distance / 120) #Bound at [0,1]
#   # similarity.quad <- 1 - sqrt(.dbl_distance / 120) #Bound at [0,1]
#   # similarity.quad <-  (1 - (.dbl_distance / 120))^2 #Bound at [0,1]
#   # similarity.quad <-  (1 - sqrt(.dbl_distance / 120))^2 #Bound at [0,1]
#   # similarity.exp <- 1 - .dbl_distance * exp(1 - .dbl_distance)
#   # similarity.exp <- 1 - .dbl_distance * exp(1 - .dbl_distance)
#   # similarity.exp <- 1 - .dbl_distance^2 * exp(1 - .dbl_distance^2)
#   # similarity.exp <- 1 - (.dbl_distance / 120)^2 * exp(1 - (.dbl_distance / 120)^2)
#   # similarity.exp <- 1 - (.dbl_distance / 120) * exp(1 - (.dbl_distance / 120))
#   # similarity.exp <- 1 - sqrt(.dbl_distance / 120) * exp(1 - sqrt(.dbl_distance / 120))
#   
#   df_occupations %>% 
#     select(
#       ends_with('.l')
#     ) %>% 
#     select(1:120) %>%
#     rowSums() %>%
#     max() -> dist.max
#   
#   # similarity.exp <- 1 - sqrt(.dbl_distance / dist.max) * exp(1 - sqrt(.dbl_distance / dist.max))
#   # similarity.exp <- 1 - (.dbl_distance / dist.max) * exp(1 - sqrt(.dbl_distance / dist.max))
#   # similarity.exp <- 1 - ((.dbl_distance / dist.max) * exp(1 - (.dbl_distance / dist.max)))^2
#   # similarity.exp <- 1 - sqrt(sqrt(.dbl_distance / dist.max) * exp(1 - sqrt(.dbl_distance / dist.max)))
#   # similarity.sqrt.quad <- (1 - sqrt(.dbl_distance / dist.max))^2
#   # similarity.sqrt.quad <- (1 - (1/2) * sqrt(.dbl_distance / dist.max))^2
#   
#   
#   similarity.exp <- 1 - (sqrt(.dbl_distance / dist.max) * exp(1 - sqrt(.dbl_distance / dist.max)))^2
#   # similarity.exp <- sqrt(1 - sqrt(.dbl_distance / dist.max) * exp(1 - sqrt(.dbl_distance / dist.max)))
#   
#   # return(similarity.quad)
#   return(similarity.exp)
#   # return(similarity.sqrt.quad)
#   
# }
# 
# tibble(
#   euclidean_distance = 
#     seq(dbl_dist.max, 0, length.out = 50)
#   , euclidean_dist.max = 
#     euclidean_distance / dbl_dist.max
#   , similarity = 
#     seq(dbl_dist.max, 0, length.out = 50) %>% 
#     fun_similarity()
#   , similarity.lag = 
#     lag(similarity)
#   , monotonic = 
#     similarity >= similarity.lag
# ) %>% 
#   slice(-1) %>%
#   reframe(all(monotonic))
# 
# tibble(
#   euclidean_distance = 
#     seq(dbl_dist.max, 0, length.out = 50)
#   , euclidean_dist.max = 
#     euclidean_distance / dbl_dist.max
#   , similarity = 
#     seq(dbl_dist.max, 0, length.out = 50) %>% 
#     fun_similarity()
#   , similarity.lag = 
#     lag(similarity)
#   , monotonic = 
#     similarity >= similarity.lag
# ) %>% 
#   filter(
#     similarity >= 0.5
#   ) 
# 
# map_df(
#   seq(
#     dbl_dist.max
#     , 0
#     , length.out = 10
#   )
#   , ~ fun_similarity(.x)
# )
# 
# fun_similarity(dbl_dist.max)
# fun_similarity(0.5 * dbl_dist.max)
# fun_similarity(0.25 * dbl_dist.max)
# fun_similarity(0.125 * dbl_dist.max)
# fun_similarity(0.05 * dbl_dist.max)
# fun_similarity(0.025 * dbl_dist.max)
# fun_similarity(0.01 * dbl_dist.max)
# fun_similarity(0.005 * dbl_dist.max)
# fun_similarity(0)
# 
# df_occupations %>% 
#   select(
#     ends_with('.l')
#   ) %>% 
#   select(1:120) %>%
#   rowSums() %>%
#   max() -> dbl_dist.max 
# 
# plot(
#   function(x){
#     # 1 - exp(x/dbl_dist.max)
#     # 1 - (x/dbl_dist.max) * exp(1-(x/dbl_dist.max))
#     # 1 - (x/dbl_dist.max) ^ exp(1-(x/dbl_dist.max))
#     # 1 - (x/dbl_dist.max) * exp((1-(x/dbl_dist.max))^2)
#     # 1 - (x/dbl_dist.max) * exp(1-(x/dbl_dist.max)^2)
#     # 1 - ((x/dbl_dist.max)^2) * exp(1 - (x/dbl_dist.max)^2)
#     # 1 - ((x/dbl_dist.max)) * exp(1 - (x/dbl_dist.max))
#     # 1 - (sqrt(x/dbl_dist.max)) * exp(1 - sqrt(x/dbl_dist.max))
#     # 1 - (x/dbl_dist.max) * exp(1 - sqrt(x/dbl_dist.max))
#     # 1 - ((x/dbl_dist.max)^2) * exp(1 - sqrt(x/dbl_dist.max))
#     # 1 - sqrt(x/dbl_dist.max) * exp(1 - (x/dbl_dist.max))
#     1 - ((sqrt(x/dbl_dist.max)) * exp(1 - sqrt(x/dbl_dist.max)))^2
#     # sqrt(1 - (sqrt(x/dbl_dist.max)) * exp(1 - sqrt(x/dbl_dist.max)))
#     # 1 - sqrt(x / dbl_dist.max)
#     # sqrt(x / dbl_dist.max)
#     # 1 - x / dbl_dist.max
#     # (1 - sqrt(x / dbl_dist.max))^2
#     # (1 - (x / dbl_dist.max) * sqrt(x / dbl_dist.max))^2
#     # (1 - (x / dbl_dist.max))^2
#     # (1 - sqrt(x / dbl_dist.max)/2)^2
#     # 1 - sqrt((sqrt(x/dbl_dist.max)) * exp(1 - sqrt(x/dbl_dist.max)))
#     # (1 - (sqrt(x/dbl_dist.max)) * exp(1 - sqrt(x/dbl_dist.max)))^2
#     # (1 - ((x/dbl_dist.max)) * exp(1 - (x/dbl_dist.max)))^2
#     # 1 - (sqrt(x/dbl_dist.max)) * exp(1 - sqrt(x/dbl_dist.max))
#     # 1 - (sqrt(x/dbl_dist.max)) * exp(1 - sqrt(x/dbl_dist.max))
#     # 1 - sqrt(sqrt(x/dbl_dist.max) * exp(1 - sqrt(x/dbl_dist.max)))
#     # (1 - (sqrt(x/dbl_dist.max) * exp(1 - sqrt(x/dbl_dist.max))))^2
#     # 1 - sqrt(x/dbl_dist.max) * exp(1 - sqrt(x/dbl_dist.max))
#     
#   }
#   , from = range(0,dbl_dist.max)
#   , xlim = c(0,dbl_dist.max)
#   # , xlim = c(0,10)
#   # , xlim = c(0,5)
#   # , xlim = c(0,2.5)
# )
# 
# qplot(
#   ((((1 - df_KNN.output$euclidean_distance) - mean(1 - df_KNN.output$euclidean_distance)) / sd(1 - df_KNN.output$euclidean_distance)))
#   , geom = 'density'
# )
# # DISTANCE TO SIMILARITY FUNCTION 2 ------------------------------
# fun_similarity <- function(
#     .df_data.numeric
#     , .dbl_distance
#     , .dbl_scale.lb = 0
#     , .dbl_scale.ub = 1
# ){
#   
#   .df_data.numeric %>%
#     select(where(
#       is.numeric
#     )) -> .df_data.numeric
#   
#   .df_data.numeric %>%
#     slice(
#       .df_data.numeric %>% 
#         mutate(across(
#           .cols = everything()
#           ,.fns = 
#             ~ if_else(
#               .x >= mean(c(
#                 .dbl_scale.lb
#                 , .dbl_scale.ub
#               ))
#               , abs(.x - .dbl_scale.lb)
#               , abs(.x - .dbl_scale.ub)
#             )
#         )) %>% 
#         rowSums() %>%
#         which.max()
#     ) -> df_dist.max
#   
#   df_dist.max -> df_dist.max.query
#   
#   df_dist.max.query[
#     df_dist.max.query >= 
#       mean(c(
#         .dbl_scale.lb
#         , .dbl_scale.ub
#       ))
#   ] <- .dbl_scale.lb
#   
#   df_dist.max.query[
#     df_dist.max.query < 
#       mean(c(
#         .dbl_scale.lb
#         , .dbl_scale.ub
#       ))
#   ] <- .dbl_scale.ub
#   
#   FNN::get.knnx(
#     data = df_dist.max
#     , query = df_dist.max.query
#     , k = 1
#   )$nn.dist %>% 
#     as.numeric() -> dbl_dist.max
#   
#   # Normalize by the maximum euclidean distance
#   .dbl_distance <- .dbl_distance / dbl_dist.max
#   
#   # # Already considered formulae
#   similarity.sub <- 1 - .dbl_distance #Bound at [0,1]
#   # # Common similarity: 1/(1 + dist), "+1" for dist <- 0
#   # similarity.div <- 1 / (1 + .dbl_distance) #Bound at [0,1]
#   # # maximum-normalized similarity: 1 - dist/max(dist)
#   # similarity.max <- 1 - (.dbl_distance / max(.dbl_distance)) #Bound at [0,1]
#   # # similarity via gaussian kernel: exp(-dist)
#   # similarity.gauss1 <- exp(-.dbl_distance) #Bound at [0,1]
#   # similarity.gauss2 <- exp(-0.5*.dbl_distance) #Bound at [0,1]
#   # similarity.gauss3 <- exp(-0.25*.dbl_distance) #Bound at [0,1]
#   # similarity.gauss4 <- exp(-0.125*.dbl_distance) #Bound at [0,1]
#   # # Square root similarity
#   # similarity.sqrt1 <- 1 - (sqrt(pmin(.dbl_distance, 4)) / 2) #Bound at [0,1]
#   # similarity.sqrt2 <- 1 - (sqrt(pmin(.dbl_distance, 16)) / 2) #Bound at [-1,1]
#   # # Square root similarity 2 ("cosine-ish" + square root)
#   # similarity.sqrt3 <- sqrt(
#   #   round(1 - ((pmin(.dbl_distance, sqrt(2)) ^ 2) / 2), 10)
#   # ) #Bound at [0,1]
#   # similarity.sqrt4 <- sqrt(
#   #   round(1 - ((pmin(.dbl_distance, 2) ^ 2) / 4), 10)
#   # ) #Bound at [0,1]
#   # similarity.sqrt5 <- sqrt(
#   #   round(1 - ((pmin(.dbl_distance, 3) ^ 2) / 9), 10)
#   # ) #Bound at [0,1]
#   # # quadratic similarity
#   # # , similarity.quad <- 1 - (.dbl_distance^2)
#   # similarity.quad1 <- 1 - (pmin(.dbl_distance,1) ^ 2) #Bound at [0,1]
#   # similarity.quad2 <- 1 - (pmin(.dbl_distance,sqrt(2)) ^ 2) / 2 #Bound at [0,1]
#   # similarity.quad3 <- 1 - (pmin(.dbl_distance,sqrt(2)) ^ 2) #Bound at [-1,1]
#   # similarity.quad4 <- 1 - (pmin(.dbl_distance,2.5) ^ 2) / 3.125 #Bound at [-1,1]
#   # similarity.quad5 <- 1 - (pmin(.dbl_distance,3) ^ 2) / 4.5 #Bound at [-1,1]
#   # # "cosine-ish" similarity
#   # # Equivalence between euclidean and cosine (if euclidean E [0,2])
#   # similarity.cos <- 1 - ((pmin(.dbl_distance, 2) ^ 2) / 2) #Bound at [-1,1]
#   # # angular similarity
#   # similarity.ang <- 1 - (acos(similarity.cos) / pi) #Bound at [0,1]
#   
#   # similarity.quad <- 1 - (pmin(.dbl_distance, 2.5) ^ 2) / 6.25 #Bound at [0,1]
#   # similarity.quad <- 1 - ((pmin(.dbl_distance, 2.5) / 2.5) ^ 2) #Bound at [0,1]
#   # similarity.quad <- 1 - (pmin(.dbl_distance, 2.5 * (120/33)) / (2.5 * (120 / 33)))^2 #Bound at [0,1]
#   # similarity.quad <- 1 - .dbl_distance #Bound at [0,1]
#   # similarity.quad <- 1 - sqrt(.dbl_distance) #Bound at [0,1]
#   # similarity.quad <- 1 - sqrt(.dbl_distance) #Bound at [0,1]
#   # similarity.quad <-  (1 - (.dbl_distance))^2 #Bound at [0,1]
#   # similarity.quad <-  (1 - sqrt(.dbl_distance))^2 #Bound at [0,1]
#   # similarity.exp <- 1 - .dbl_distance * exp(1 - .dbl_distance)
#   # similarity.exp <- 1 - .dbl_distance * exp(1 - .dbl_distance)
#   # similarity.exp <- 1 - .dbl_distance^2 * exp(1 - .dbl_distance^2)
#   # similarity.exp <- 1 - (.dbl_distance)^2 * exp(1 - (.dbl_distance)^2)
#   # similarity.exp <- 1 - (.dbl_distance) * exp(1 - (.dbl_distance))
#   # similarity.exp <- 1 - sqrt(.dbl_distance) * exp(1 - sqrt(.dbl_distance))
#   
#   # similarity.exp <- 1 - sqrt(.dbl_distance) * exp(1 - sqrt(.dbl_distance))
#   # similarity.exp <- 1 - (.dbl_distance) * exp(1 - sqrt(.dbl_distance))
#   # similarity.exp <- 1 - ((.dbl_distance) * exp(1 - (.dbl_distance)))^2
#   # similarity.exp <- 1 - sqrt(sqrt(.dbl_distance) * exp(1 - sqrt(.dbl_distance)))
#   # similarity.sqrt.quad <- (1 - sqrt(.dbl_distance))^2
#   # similarity.sqrt.quad <- (1 - (1/2) * sqrt(.dbl_distance))^2
#   
#   
#   # similarity.exp <- 1 - (sqrt(.dbl_distance) * exp(1 - sqrt(.dbl_distance)))^2
#   # similarity.exp <- sqrt(1 - sqrt(.dbl_distance) * exp(1 - sqrt(.dbl_distance)))
#   
#   # # return(similarity.quad)
#   # return(similarity.exp)
#   # # return(similarity.sqrt.quad)
#   return(similarity.sub)
#   
# }
# \
# DISTANCE TO SIMILARITY FUNCTION 2 ------------------------------
fun_similarity <- function(
    .df_data.numeric
    , .dbl_distance
    , .dbl_scale.lb = 0
    , .dbl_scale.ub = 1
){
  
  .df_data.numeric %>%
    select(where(
      is.numeric
    )) -> .df_data.numeric
  
  .df_data.numeric %>%
    slice(
      .df_data.numeric %>% 
        mutate(across(
          .cols = everything()
          ,.fns = 
            ~ if_else(
              .x >= mean(c(
                .dbl_scale.lb
                , .dbl_scale.ub
              ))
              , abs(.x - .dbl_scale.lb)
              , abs(.x - .dbl_scale.ub)
            )
        )) %>% 
        rowSums() %>%
        which.max()
    ) -> df_dist.max
  
  df_dist.max -> df_dist.max.query
  
  df_dist.max.query[
    df_dist.max.query >= 
      mean(c(
        .dbl_scale.lb
        , .dbl_scale.ub
      ))
  ] <- .dbl_scale.lb
  
  df_dist.max.query[
    df_dist.max.query < 
      mean(c(
        .dbl_scale.lb
        , .dbl_scale.ub
      ))
  ] <- .dbl_scale.ub
  
  FNN::get.knnx(
    data = df_dist.max
    , query = df_dist.max.query
    , k = 1
  )$nn.dist %>% 
    as.numeric() -> dbl_dist.max
  
  # Normalize by the maximum euclidean distance
  .dbl_distance <- .dbl_distance / dbl_dist.max
  
  # # Already considered formulae
  dbl_similarity <- 1 - .dbl_distance #Bound at [0,1]
  
  return(dbl_similarity)
  
}

# # KNN MATCHING FUNCTION -------------------------------------------------------------------------
# fun_KNN.matching <- function(
    #     .df_data.numeric
#     , .vec_query.numeric
#     , .int_k = 1
#     , .auto_select.k = F
#     , .imput.over_qualification = T
#     , .dbl_over_qualification.threshold = 0
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
#         , similarity = fun_similarity(euclidean_distance)
#         , similarity = round(similarity, .dbl_decimals)
#       ) %>%
#       # arrange(desc(similarity)) %>%
#       arrange(euclidean_distance) %>%
#       mutate(
#         rank = row_number()
#         , rank.norm = seq(1, 0, - 1 / (n() - 1))
#         , .before = everything()
#       ) %>% 
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
#         , similarity = fun_similarity(euclidean_distance)
#         , similarity = round(similarity, .dbl_decimals)
#       ) %>% 
#       # arrange(desc(similarity)) %>%
#       arrange(euclidean_distance) %>%
#       mutate(
#         rank = row_number()
#         , rank.norm = seq(1, 0, - 1 / (n() - 1))
#         , .before = everything()
#       ) %>% 
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
    , .dbl_over_qualification.threshold = 0
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
        , similarity = 
          fun_similarity(
            .df_data.numeric =
              .df_data.numeric.temp
            , .dbl_distance = 
              euclidean_distance
            , .dbl_scale.lb = 0
            , .dbl_scale.ub = 1
          )
        , similarity = round(similarity, .dbl_decimals)
      ) %>%
      # arrange(desc(similarity)) %>%
      arrange(euclidean_distance) %>%
      mutate(
        rank = row_number()
        , rank.norm = seq(1, 0, - 1 / (n() - 1))
        , .before = everything()
      ) %>% 
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
        , similarity = 
          fun_similarity(
            .df_data.numeric =
              .df_data.numeric.temp
            , .dbl_distance = 
              euclidean_distance
            , .dbl_scale.lb = 0
            , .dbl_scale.ub = 1
          )
        , similarity = round(similarity, .dbl_decimals)
      ) %>% 
      # arrange(desc(similarity)) %>%
      arrange(euclidean_distance) %>%
      mutate(
        rank = row_number()
        , rank.norm = seq(1, 0, - 1 / (n() - 1))
        , .before = everything()
      ) %>% 
      return(.)
    
  }
  
}

# # DISTANCE TO SIMILARITY FUNCTION 2 ------------------------------
# fun_similarity2 <- function(.dbl_distance, .dbl_scale.max = NULL){
# 
#   # Numeric
#   stopifnot(
#     "'.dbl_distance' must be a numeric vector of distances between numeric vectors." =
#         is.numeric(.dbl_distance)
#   )
# 
#   stopifnot(
#     "'.dbl_scale.max' must be a single number indicating the maximum value of the scale, if there is any." =
#         !length(.dbl_scale.max)
#         | is.numeric(.dbl_scale.max)
#         & length(.dbl_scale.max) == 1
#         & .dbl_scale.max > 0
#   )
# 
#   # Scale
#   if(length(.dbl_scale.max)){
# 
#     .dbl_distance / .dbl_scale.max -> .dbl_distance
# 
#   }
# 
#   # Similarity
#   1 - .dbl_distance -> dbl_similarity
# 
#   # Output
#   return(dbl_similarity)
# 
# }

# # KNN MATCHING FUNCTION 2 -------------------------------------------------------------------------
# fun_KNN.matching2 <- function(
    #     .df_data.numeric
#     , .vec_query.numeric
#     , .int_k = 1
#     , .auto_select.k = F
# ){
# 
#   # Data frame
# 
#   # Numeric
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
#     # Typical suggested value for k is sqrt(nrow(df))
# 
#     .df_data.numeric %>%
#       nrow() %>%
#       sqrt() %>%
#       round() -> .int_k
# 
#   }
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
#         , similarity = fun_similarity2(euclidean_distance)
#       ) %>%
#       arrange(euclidean_distance) %>%
#       mutate(
#         rank = row_number()
#         , rank.norm = seq(1, 0, - 1 / (n() - 1))
#         , .before = 1
#       ) -> df_data.knn
# 
#     # Output
#     return(df_data.knn)
# 
# }

# # KNN MATCHING FUNCTION 3 -------------------------------------------------------------------------
# fun_KNN.matching3 <- function(
    #     .df_data.numeric
#     , .vec_query.numeric
#     , .int_k = NULL
# ){
# 
#   # Data frame
# 
#   # Numeric
#   stopifnot(
#     "'.int_k' must be a single integer." =
#       !length(.int_k) |
#       is.integer(.int_k)
#   )
# 
#   # Get numeric data only
#   .df_data.numeric %>%
#     select(where(is.numeric)) %>%
#     as.matrix() -> mtx_data
# 
#   if(is.data.frame(.vec_query.numeric)){
#     .vec_query.numeric %>%
#       select(where(is.numeric)) %>%
#       slice(1) %>%
#       as.numeric() -> .vec_query.numeric
#   }
# 
#   # Square root of data for normalization
#   mtx_data %>%
#     sqrt() -> mtx_data.sqrt
# 
#   # Replicate query vector n times
#   rbind(.vec_query.numeric) %x%
#     rep(1, nrow(mtx_data)) -> mtx_query
# 
#   # Define k
#   if(!length(.int_k)){
# 
#     .df_data.numeric %>%
#       nrow() -> .int_k
# 
#   }
# 
#   # Find the k nearest neighbors
#   FNN::get.knnx(
#     data = mtx_data * mtx_data.sqrt
#     , query = mtx_query * mtx_data.sqrt
#     , k = .int_k
#   ) -> list_KNN.output
# 
#   # Get diagonal
#   # map(list_KNN.output, diag) -> list_KNN.output
#   # return(list_KNN.output)
#   # stop()
# 
#   # Arrange original data frame with KNN output
#   .df_data.numeric %>%
#     # slice(list_KNN.output$nn.index[1,]) %>%
#     mutate(#Add euclidean distances and convert them to similarities
#       euclidean_distance = diag(list_KNN.output$nn.dist)
#       , similarity = fun_similarity(euclidean_distance)
#       , similarity2 = 1 - (euclidean_distance / sqrt(ncol(mtx_query)))
#     ) %>%
#     arrange(euclidean_distance) %>%
#     mutate(
#       rank = row_number()
#       , rank.norm = seq(1, 0, - 1 / (n() - 1))
#       , .before = 1
#     ) -> df_data.knn
# 
#   # Output
#   return(df_data.knn)
# 
# }

# # KNN MATCHING FUNCTION 3 -------------------------------------------------------------------------
# fun_KNN.matching3 <- function(
#     .df_data.numeric
#     , .vec_query.numeric
#     , .int_k = NULL
# ){
#   
#   # Data frame
#   
#   # Numeric
#   stopifnot(
#     "'.int_k' must be a single integer." =
#       !length(.int_k) |
#       is.integer(.int_k)
#   )
#   
#   # Get numeric data only
#   .df_data.numeric %>%
#     select(where(is.numeric)) %>%
#     as.matrix() -> mtx_data
#   
#   if(is.data.frame(.vec_query.numeric)){
#     .vec_query.numeric %>%
#       select(where(is.numeric)) %>%
#       slice(1) %>%
#       as.numeric() -> .vec_query.numeric
#   }
#   
#   # Replicate query vector n times
#   rbind(.vec_query.numeric) %x%
#     rep(1, nrow(mtx_data)) -> mtx_query
#   
#   # Square root of data for normalization
#   # Coefficients for normalization
#   # mtx_data %>%
#   #   sqrt() -> mtx_data.sqrt
#   
#   sqrt(
#     # Ok
#     # mtx_data ^ (mtx_query / 2)
#     # mtx_data ^ (mtx_query / 2 * (1 - mtx_data))
#     # mtx_data ^ (mtx_query / 2 * (1 - mtx_data) ^ 2)
#     # mtx_data ^ (mtx_query / 2 * sqrt(1 - mtx_data))
#     
#     mtx_data ^ (mtx_query / mtx_data)
#     
#     # mtx_data ^ (mtx_query / ((9 * mtx_data) ^ 4))
#     
#     # mtx_data ^ mtx_query
#     # ((1 - mtx_query) ^ (1 - mtx_data)) * (mtx_data ^ mtx_query)
#     # Lixo
#     # mtx_data ^ (mtx_query * (1 - mtx_data))
#     # mtx_data ^ (mtx_query / mtx_data)
#     # mtx_data ^ ((mtx_query / mtx_data) * (1 - mtx_data))
#     # sqrt(mtx_data ^ (mtx_query / mtx_data))
#     # (mtx_data ^ (mtx_query / mtx_data))^2
#     # mtx_data ^ (mtx_query / 20 * sqrt(mtx_data))
#     
#     # mtx_data ^ (2 * mtx_query)
#     # mtx_data ^ (2 * mtx_query * (1 - mtx_data))
#     # mtx_data ^ (4 * mtx_query * (1 - mtx_data)^2)
#     # mtx_data ^ (mtx_query * (1 - mtx_data)^2)
#     # mtx_data ^ ((mtx_query / (2 * mtx_data)) ^ 2)
#     # mtx_data ^ (((1 - mtx_data) ^ 2) * (mtx_query / (2 * mtx_data)))
#     # mtx_data ^ (((1 - mtx_data) ^ 2) * (mtx_query / (2 * mtx_data)))
#     # sqrt(mtx_data ^ (mtx_query / (2 * mtx_data)))
#     # sqrt(mtx_data ^ (((1 - mtx_data) ^ 2) * mtx_query / (2 * mtx_data)))
#     # sqrt(mtx_data ^ (mtx_query * (1 - mtx_data) / (20*mtx_data)))
#     # sqrt(mtx_data ^ (mtx_query * ((1 - mtx_data) / (4*mtx_data))^4))
#     # sqrt(mtx_data ^ (mtx_query * ((1 - mtx_data) / (2 * mtx_data)) ^ (2 * mtx_query)))
#     # sqrt(mtx_data ^ ((mtx_query * ((1 - mtx_data) / (2 * mtx_data)) ^ (2 * mtx_query))^2))
#     # mtx_data ^ ((mtx_query * ((1 - mtx_data) / (2 * mtx_data)) ^ (2 * mtx_query))^2)
#     # mtx_data ^ (mtx_query / (mtx_data ^ mtx_query))
#     # mtx_data ^ ((mtx_query / mtx_data)^2)
#     # mtx_data ^ ((mtx_query / mtx_data)^2)
#     # mtx_data ^ (mtx_query / (mtx_data ^ (1 - mtx_query)))
#   ) -> mtx_coef.sqrt
#   
#   # Define k
#   if(!length(.int_k)){
#     
#     .df_data.numeric %>%
#       nrow() -> .int_k
#     
#   }
#   
#   # Find the k nearest neighbors
#   FNN::get.knnx(
#     data = mtx_data * mtx_coef.sqrt
#     , query = mtx_query * mtx_coef.sqrt
#     , k = .int_k
#   ) -> list_KNN.output
#   
#   # Get diagonal
#   # map(list_KNN.output, diag) -> list_KNN.output
#   # return(list_KNN.output)
#   # stop()
#   
#   # Arrange original data frame with KNN output
#   .df_data.numeric %>%
#     # slice(list_KNN.output$nn.index[1,]) %>%
#     mutate(#Add euclidean distances and convert them to similarities
#       euclidean_distance = diag(list_KNN.output$nn.dist)
#       , similarity = 
#         fun_similarity(
#           .df_data.numeric = 
#             as_tibble(
#               mtx_data * mtx_coef.sqrt
#             )
#           , .dbl_distance =
#             euclidean_distance
#           , .dbl_scale.lb = 0
#           , .dbl_scale.ub = 1
#         )
#     ) %>%
#     arrange(euclidean_distance) %>%
#     mutate(
#       rank = row_number()
#       , rank.norm = seq(1, 0, - 1 / (n() - 1))
#       , .before = 1
#     ) -> df_data.knn
#   
#   # Output
#   return(df_data.knn)
#   
# }
