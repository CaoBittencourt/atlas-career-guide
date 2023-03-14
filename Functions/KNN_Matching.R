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
        , similarity = fun_similarity(euclidean_distance)
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
        , similarity = fun_similarity(euclidean_distance)
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
# 
# # test --------------------------------------------------------------------
# fun_KNN.matching3(
#   .df_data.numeric =
#     df_occupations %>%
#     select(
#       occupation
#       , names(df_input)
#     )
#   , .vec_query.numeric = df_input
# ) -> lala
# 
# lala %>% view
# 
# 
# length(unique(diag(lala$nn.index)))
# length(unique(lala$nn.index[1,]))
# length(unique(lala$nn.index[,1]))
# 
# length(unique(diag(lala$nn.dist)))
# length(unique(lala$nn.dist[1,]))
# length(unique(lala$nn.dist[,1]))
#   
# df_occupations %>%
#   slice() %>% view
# 
# FNN::get.knnx(
#   data = lala$data
#   , query = lala$query
#   , k = nrow(lala$query)
# ) -> lalala
# 
# lala$nn.index %>% view
# lala$nn.dist %>% dim
# 
# fun_KNN.matching(
#   .df_data.numeric =
#     df_occupations %>%
#     select(
#       occupation
#       , names(df_input)
#     )
#   , .vec_query.numeric = df_input
#   , .auto_select.k = F
#   , .imput.over_qualification = T
#   , .dbl_over_qualification.threshold = 0
# ) -> lalala
# 
# 
# df_occupations %>% 
#   slice_sample(n = 1) -> dsds
# 
# df_occupations %>% 
#   filter(occupation == 'Chief Executives') -> dsds
# 
# df_occupations -> dsds
# 
# FNN::get.knnx(
#   data = 
#     dsds %>% 
#     select(ends_with('.l'))
#   , query = df_input
#   , k = nrow(dsds)
# ) %>% 
#   last() %>%
#   as.numeric() -> dist1
# 
# 
# list(
#   occupation = rbind(rep(1,3), c(1,1,0), c(1,0,0), rep(0,3)) %>% 
#     `rownames<-`(sapply(1:4, function(x){paste0('occ',x)}))
#   , user = 
#     rbind(c(1,1,1)) %x% 
#     rep(1,2) %>%
#     rbind(c(3,3,3)) %>% 
#     rbind(c(2,2,2)) %>%
#     `rownames<-`(sapply(1:4, function(x){paste0('vec',x)}))
#   , knn = FNN::get.knnx(
#     data = rbind(rep(1,3), c(1,1,0), c(1,0,0), rep(0,3))
#     , query = 
#       rbind(c(1,1,1)) %x% 
#       rep(1,2) %>%
#       rbind(c(3,3,3)) %>% 
#       rbind(c(2,2,2))
#     , k = 4
#   )
# ) -> lala
# 
# list(
#   occupation = dsds %>% slice(1:3) %>% select(names(df_input))
#   , user = df_input
#   , dist = FNN::get.knnx(
#     data = 
#       ((dsds %>% 
#           slice(1:3) %>% 
#           select(names(df_input))) *
#          (dsds %>% 
#             slice(1:3) %>% 
#             select(names(df_input))
#           %>% sqrt())) %>% 
#       as_tibble()
#     , query = 
#       (((df_input %>%
#           as.matrix() %x%
#           rep(1, 3)) * 
#          (dsds %>% 
#             slice(1:3) %>% 
#             select(names(df_input))
#           %>% sqrt())) %>% 
#       as_tibble())
#     , k = 3
#   )$nn.dist
# ) -> lala
# 
# diag(lala$dist)
# 
# ((df_input %>%
#     as.matrix() %x%
#     rep(1, 3)) *
#   (dsds %>% 
#   slice(1:3) %>% 
#   select(names(df_input)) %>%
#   sqrt() %>%
#   as.matrix() %>% 
#   `colnames<-`(NULL)))[,33] == 
#   (df_input %>%
#      as.matrix() %x%
#      rep(1, 3))[,33] * (dsds %>% 
#     slice(1:3) %>% 
#     select(names(df_input)) %>%
#     sqrt() %>%
#     as.matrix())[,33]
#   
# 
# FNN::get.knnx(
#   data = 
#     (dsds %>% 
#        select(ends_with('.l')) %>% 
#        sqrt()) * 
#     (dsds %>% 
#        select(ends_with('.l')))
#   , query = 
#     # df_input *
#     df_input %>%
#     as.numeric() %>%
#     rbind() %x%
#     rep(1, nrow(dsds)) *
#     (dsds %>% 
#        select(ends_with('.l')) %>% 
#        sqrt()) 
#   , k = nrow(dsds)
# ) %>% 
#   last() %>% 
#   diag() -> dist2
# 
# 
# dsds$occupation
# 
# 1 - dist1
# 1 - dist1 / sqrt(ncol(df_input))
# lalala %>% 
#   filter(
#     occupation == dsds$occupation
#   ) %>% 
#   select(
#     euclidean_distance
#     , similarity
#   )
# 
# 1 - (min(dist2, 2.5) / 2.5)^2
# qplot(1 - dist2 / sqrt(ncol(df_input)))
# 
# fun_similarity(dist1)
# 
# lala %>% 
#   select(occupation, euclidean_distance) -> dsds
# 
# dsds %>% view
# nrow(dsdsds)
# 
# lalala %>% 
#   select(occupation, euclidean_distance) -> dsdsds
# 
# dsds %>% 
#   mutate(
#     method = 'euclidean.adjusted'
#   ) %>% 
#   bind_rows(
#     dsdsds %>% 
#       mutate(
#         method = 'euclidean'
#       )
#   ) %>% 
#   fun_plot.density(aes(
#     x = euclidean_distance
#     , fill = method
#   )
#   , .dbl_limits.x = c(-2,2)
#   )
# 
# summary(dsds$euclidean_distance)
# summary(dsdsds$euclidean_distance)
# 
# lalala$data %>% nrow
# lalala$user %>% nrow
# 
# lalala$knn$nn.dist
# sort(fun_similarity(lalala$dist), decreasing = T) %>% view
