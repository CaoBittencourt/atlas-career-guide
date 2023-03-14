fun_euclidean.adjusted <- function(.dbl_x, .dbl_y){
  
  # Numeric vectors of same length
  stopifnot(
    "'.dbl_x' must be a numeric vector the same length as '.dbl_y'." =
      is.numeric(.dbl_x)
  )
  
  stopifnot(
    "'.dbl_y' must be a numeric vector the same length as '.dbl_x'." =
      is.numeric(.dbl_y)
  )
  
  stopifnot(
    "'.dbl_x' must be a numeric vector the same length as '.dbl_y'." = c(
      pluck_depth(.dbl_x) == 1
      , pluck_depth(.dbl_y) == 1
    )
  )
  
  stopifnot(
    "'.dbl_x' must be a numeric vector the same length as '.dbl_y'." =
      length(.dbl_x) == length(.dbl_y)
  )
  
  # Adjusted squared euclidean distance
  # sum(.dbl_y*(.dbl_x-.dbl_y)^2, na.rm = T) / 
  #   sum(.dbl_y, na.rm = T) -> dbl_distance.squared
  # 
  sum(.dbl_y*(.dbl_x-.dbl_y)^2, na.rm = T) -> dbl_distance.squared
  
  # dbl_distance.squared = NA <=> sum(.dbl_y) = 0
  if(!is.na(dbl_distance.squared)){
    
    sqrt(dbl_distance.squared) -> dbl_distance
    
  } else {
    
    0 -> dbl_distance
    
  }
  
  # Output
  return(dbl_distance)
  
} 

fun_euclidean.adjusted2 <- function(.dbl_x, .dbl_y){
  
  # Numeric vectors of same length
  stopifnot(
    "'.dbl_x' must be a numeric vector the same length as '.dbl_y'." =
      is.numeric(.dbl_x)
  )
  
  stopifnot(
    "'.dbl_y' must be a numeric vector the same length as '.dbl_x'." =
      is.numeric(.dbl_y)
  )
  
  stopifnot(
    "'.dbl_x' must not contain missing values." =
      !any(is.na(.dbl_x))
  )
  
  stopifnot(
    "'.dbl_y' must not contain missing values." =
      !any(is.na(.dbl_y))
  )
  
  stopifnot(
    "'.dbl_x' must be a numeric vector the same length as '.dbl_y'." = c(
      pluck_depth(.dbl_x) == 1
      , pluck_depth(.dbl_y) == 1
    )
  )
  
  stopifnot(
    "'.dbl_x' must be a numeric vector the same length as '.dbl_y'." =
      length(.dbl_x) == length(.dbl_y)
  )
  
  # Sum of weights
  sum(.dbl_y) -> sum_y
  
  # Normalization by the square root of .dbl_y
  sqrt(.dbl_y) * .dbl_x -> .dbl_x
  
  sqrt(.dbl_y) * .dbl_y -> .dbl_y
  
  # Adjusted squared euclidean distance
  # sum((.dbl_x - .dbl_y)^2) / sum_y -> dbl_distance.squared
  sum((.dbl_x - .dbl_y)^2) -> dbl_distance.squared
  
  # dbl_distance.squared = NA <=> sum(.dbl_y) = 0
  if(!is.na(dbl_distance.squared)){
    
    sqrt(dbl_distance.squared) -> dbl_distance
    
  } else {
    
    0 -> dbl_distance
    
  }
  
  # Output
  return(dbl_distance)
  
} 

map(
  1:nrow(df_occupations)
  , function(occ){
    
    # fun_euclidean.adjusted(
    fun_euclidean.adjusted2(
      .dbl_x = 
        # as.numeric(df_input)
        rep(0, ncol(df_input))
      , .dbl_y = 
        df_occupations %>% 
        select(
          names(df_input)
        ) %>% 
        slice(occ) %>% 
        as.numeric()
    )
    
  }
) %>% 
  flatten_dbl() -> dsdsds

qplot(
  fun_similarity(
  # fun_similarity2(
    dsdsds
  # , .dbl_scale.max = 1
))

qplot(dsdsds)
qplot(1 - dsdsds)
qplot(1 - sqrt(dsdsds))
qplot(1 - dsdsds^2)


vapply(
  df_occupations %>%
    select(
      df_input %>% 
        select(
          where(is.numeric)
        ) %>% 
        names
    ) %>% 
    t()
  , FUN.VALUE = numeric(1)
  , function(y){
    
    fun_euclidean.adjusted(
      .dbl_x = 
        df_input %>%
        select(where(is.numeric)) %>%
        as.numeric()
      , .dbl_y = y
    )
    
  }
)

t1 <- Sys.time()
df_occupations %>%
  select(
    occupation
    , df_input %>% 
      select(
        where(is.numeric)
      ) %>% 
      names
  ) %>%
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'y'
    , values_to = 'y.val'
  ) %>% 
  group_by(occupation) %>%
  summarise(
    distance.adjusted = 
      fun_euclidean.adjusted(
        .dbl_x = 
          df_input %>%
          select(where(is.numeric)) %>%
          as.numeric()
        , y.val
      )
  )
t2 <- Sys.time()

t2 - t1

t3 <- Sys.time()
df_occupations %>%
  select(
    occupation
    , df_input %>% 
      select(
        where(is.numeric)
      ) %>% 
      names
  ) %>%
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'y'
    , values_to = 'y.val'
  ) %>% 
  group_by(occupation) %>%
  summarise(
    distance.adjusted = 
      fun_euclidean.adjusted2(
        .dbl_x = 
          df_input %>%
          select(where(is.numeric)) %>%
          as.numeric()
        , y.val
      )
  )

t4 <- Sys.time()

t5 <- Sys.time()
FNN::get.knnx(
  data = df_input %>%
    select(where(is.numeric)) %>%
    as.numeric()
  , query = .vec_query.numeric
  , k = .int_k
) -> KNN.output

df_occupations %>%
  select(
    occupation
    , df_input %>% 
      select(
        where(is.numeric)
      ) %>% 
      names
  ) %>%
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'y'
    , values_to = 'y.val'
  ) %>% 
  group_by(occupation) %>%
  summarise(
    distance.adjusted = 
      fun_euclidean.adjusted2(
        .dbl_x = 
          df_input %>%
          select(where(is.numeric)) %>%
          as.numeric()
        , y.val
      )
  )

t6 <- Sys.time()

t2 - t1
t4 - t3
t6 - t5


sapply(
  df_occupations %>%
    select(
      df_input %>% 
        select(
          where(is.numeric)
        ) %>% 
        names
    ) %>% 
    t() %>% 
    data.frame
  , function(y){
    
    fun_euclidean.adjusted(
      .dbl_x = 
        df_input %>%
        select(where(is.numeric)) %>%
        as.numeric()
      , .dbl_y = y
    )
    
  }
)

df_occupations %>%
  select(
    df_input %>% 
      select(
        where(is.numeric)
      ) %>% 
      names
  ) %>% 
  rowwise() %>%
  # rowwise()
  mutate(
    euclidean_distance = 
      list(
        vapply(
          .
          # , MARGIN = 1
          , FUN.VALUE = numeric(1)
          , FUN = 
            function(.y){
              
              fun_euclidean.adjusted(
                .dbl_x = 
                  df_input %>%
                  select(where(is.numeric)) %>%
                  as.numeric()
                , .dbl_y = .y
              )
              
            }
        )
      )
    
    , .before = 1
  )


vapply(
  df_occupations %>%
    select(
      df_input %>% 
        select(
          where(is.numeric)
        ) %>% 
        names
    ) %>% 
    slice(1) %>% 
    as.numeric()
  , FUN.VALUE = numeric(1)
  , FUN = 
    function(.y){
      
      fun_euclidean.adjusted(
        .dbl_x = 
          df_input %>%
          select(where(is.numeric)) %>%
          as.numeric()
        , .dbl_y = .y
      )
      
    }
)

fun_euclidean.adjusted(
  .dbl_x = 
    df_input %>%
    select(where(is.numeric)) %>%
    as.numeric()
  , .dbl_y = 
    df_occupations %>%
    select(
      df_input %>% 
        select(
          where(is.numeric)
        ) %>% 
        names
    ) %>% 
    slice(1) %>% 
    as.numeric()
)

0.3860378

apply(
  df_occupations %>%
    select(
      df_input %>% 
        select(
          where(is.numeric)
        ) %>% 
        names
    )
  , MARGIN = 1
  , FUN = 
    function(.y){
      fun_euclidean.adjusted(
        .dbl_x =
          df_input %>%
          select(where(is.numeric)) %>%
          as.numeric()
        , .dbl_y = .y
      )
    }
)

fun_similarity2 <- function(.dbl_distance, .dbl_scale.max = NULL){
  
  # Numeric
  stopifnot(
    "'.dbl_distance' must be a sigle number indicating the distance between two numeric vectors." = 
      c(
        is.numeric(.dbl_distance)
        , length(.dbl_distance) == 1
      )
  )
  
  # stopifnot(
  #   "'.dbl_scale.max' must be a single number indicating the maximum value of the scale, if any." = 
  #     c(
  #       is.numeric(.dbl_scale.max)
  #       , length(.dbl_scale.max) == 1
  #       , .dbl_scale.max > 0 |
  #         !length(.dbl_scale.max)
  #     )
  # )
  
  # Scale
  if(length(.dbl_scale.max)){
    
    .dbl_distance / .dbl_scale.max -> .dbl_distance
    
  }
  
  # Similarity
  1 - .dbl_distance -> dbl_similarity
  
  # Output 
  return(dbl_similarity)
  
}

source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/KNN_Matching.R')
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.EFA.R')

fun_factor.scores2(
  .df_data = df_occupations
  , .list_factor.keys = list_factors
  , .lgc_pivot.long = T
) -> dsds

fun_factor.scores2(
  .df_data = df_input
  , .list_factor.keys = list_factors
  , .lgc_pivot.long = T
) -> lalala

lalala$scores.long -> df1

dsds$scores.long %>%
  filter(
    occupation == 'Chief Executives'
  ) -> df2

dsds$scores.long %>% 
  filter(
    occupation == 'Maids and Housekeeping Cleaners'
    # occupation == 'Actuaries'
  ) -> df2


Vectorize(
  FUN = fun_euclidean.adjusted
  # , vectorize.args = c('x','y')
  , vectorize.args = 'y'
) -> fun_euclidean.adjusted2

fun_euclidean.adjusted2(
  x = 
    df_input %>% 
    select(where(is.numeric)) %>%
    as.numeric()
  , y = 
    df_occupations %>%
    select(
      df_input %>% 
        select(
          where(is.numeric)
        ) %>% 
        names
    ) %>% 
    matrix()
) %>% view
fun_similarity2(
  .dbl_scale.max = 1
)


fun_similarity(.dbl_distance = as.numeric(dist(rbind(df1$item.score,df2$item.score))))
fun_similarity(.dbl_distance = fun_euclidean.adjusted(df1$item.score,df2$item.score))

fun_similarity(.dbl_distance = as.numeric(dist(rbind(df2$item.score,df1$item.score))))
fun_similarity(.dbl_distance = fun_euclidean.adjusted(df2$item.score,df1$item.score))

fun_similarity2(.dbl_distance = as.numeric(dist(rbind(df1$item.score,df2$item.score))))
fun_similarity2(.dbl_distance = fun_euclidean.adjusted(df1$item.score,df2$item.score))

fun_similarity2(.dbl_distance = as.numeric(dist(rbind(df2$item.score,df1$item.score))))
fun_similarity2(.dbl_distance = fun_euclidean.adjusted(df2$item.score,df1$item.score))


sum(df1$item.score * abs(df2$item.score - df1$item.score))
sqrt(sum(df1$item.score * (df2$item.score - df1$item.score)^2))

sum(df1$item.score * abs(df2$item.score - df1$item.score)) / sum(df1$item.score)
sqrt(sum(df1$item.score * (df2$item.score - df1$item.score)^2) / sum(df1$item.score))

sqrt(sum(rep(2,10) * (rep(0,10) - rep(2,10))^2) / sum(rep(2,10)))
1 - sqrt(sum(rep(3,10) * (rep(0,10) - rep(3,10))^2) / sum(rep(3,10)))

dist(rbind(rep(3,10), rep(0,10)))

# dist(rbind(x1,x2))
# sqrt((1-0)^2 + (0-1)^2)
# dist(rbind(c(1,0),c(0,1)))
# dist(rbind(df1$item.score,df2$item.score))
# 1 - fun_euclidean.adjusted(df1$item.score,df2$item.score)
# 

fun_euclidean.adjusted(0,1)
fun_euclidean.adjusted(0.5,1)
fun_euclidean.adjusted(0,0.9)


ymax <- 0

while(
  fun_euclidean.adjusted(1, ymax) < 
  fun_euclidean.adjusted(1, ymax + 0.0000001)
){
  
  ymax <- ymax + 0.0000001
  
}

ymax
