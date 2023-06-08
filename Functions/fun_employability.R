# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Functions -------------------------------------------------------------


# [FUNCTIONS] -------------------------------------------------------------
# - Interchangeability ----------------------------------------------------
fun_interchangeability <- function(.dbl_similarity){
  
  # Arguments validation
  stopifnot(
    "'.dbl_similarity' must be numeric." =
      is.numeric(.dbl_similarity)
  )
  
  .dbl_similarity -> s
  
  # Interchangeability coefficients
  # Coefficient 1
  # dbl_interchangeability <- s
  
  # Coefficient 2
  # dbl_interchangeability <- s ^ 2
  
  # Coefficient 3
  # dbl_interchangeability <- s * s + (1 - s) * s ^ 2
  
  # Coefficient 4
  # dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^(1/s))
  
  # Coefficient 4
  dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^4)
  
  # Output
  return(dbl_interchangeability)
  
}


# DISTANCE TO SIMILARITY FUNCTION ------------------------------
fun_employability <- function(
    .df_data
    , .df_query = NULL
    , .int_employment
    , .dbl_scale.lb = 0
    , .dbl_scale.ub = 100
    , .dbl_over_qualification.threshold = NULL
){
  
  # Arguments validation
  stopifnot(
    "'.df_data' must be a data frame." =
      is.data.frame(.df_data)
  )
  
  stopifnot(
    "'.int_employment' must be a numeric vector the same length as '.df_data'." = 
      all(
        is.numeric(.int_employment)
        , length(.int_employment) ==
          nrow(.df_data)
      )
  )
  
  # Similarity
  if(length(.df_query)){
    
    fun_knn.matching(
      .df_data =
        .df_data
      , .df_query = 
        .df_query
      , .dbl_scale.lb =
        .dbl_scale.lb 
      , .dbl_scale.ub =
        .dbl_scale.ub
      , .int_k = 
        nrow(.df_data)
      , .dbl_over_qualification.threshold =
        .dbl_over_qualification.threshold
    ) -> df_knn
    
    # Interchangeability
    df_knn %>%
      mutate(
        interchangeability = 
          fun_interchangeability(
            similarity
          )
      ) -> df_knn
    
    # Probability of finding a job
    df_knn %>% 
      mutate(
        employment = 
          .int_employment
      ) %>% 
      reframe(
        employability = 
          sum(
            interchangeability * 
              employment / 
              sum(employment)
          )
      ) -> df_knn
    
  } else {
    
    .df_data %>% 
      group_nest(
        row_number()
      ) %>% 
      pull(data) %>% 
      map(
        ~ fun_knn.matching(
          .df_data =
            .df_data
          , .df_query = .x
          , .dbl_scale.lb =
            .dbl_scale.lb 
          , .dbl_scale.ub =
            .dbl_scale.ub
          , .int_k = 
            nrow(.df_data)
          , .dbl_over_qualification.threshold =
            .dbl_over_qualification.threshold
        )
      ) -> list_knn
    
    # Interchangeability
    map(
      list_knn
      , ~ .x %>% 
        mutate(
          interchangeability = 
            fun_interchangeability(
              similarity
            )
        )
    ) -> list_knn
    
    # Probability of finding a job
    map(
      list_knn
      , ~ .x %>% 
        mutate(
          employment = 
            .int_employment
        ) %>% 
        reframe(
          employability = 
            sum(
              interchangeability * 
                employment / 
                sum(employment)
            )
        )
    ) -> list_knn
    
    list_knn -> df_knn
    
  }
  
  # Output
  return(df_knn)
  
}

# df_sample %>%
#   select(ends_with('.l')) %>% 
#   mutate(across(
#     .cols = everything()
#     ,.fns = ~ 100 * .x
#   )) -> lalala
# 
# lalala %>% 
#   rename_with(
#     .fn = function(x){
#       paste0(x,'.imput')
#     }
#   ) %>%
#   bind_cols(
#     df_occupations.ai %>% 
#       select(names(df_sample)) %>% 
#       select(ends_with('.l'))
#   ) %>%
#   mutate(
#     across(
#       .cols = c(
#         !ends_with('.imput')
#       )
#       ,.fns = function(x){
#         
#         ifelse(
#           # Overqualified if > cutoff and requirement <= cutoff
#           x <= 0 
#           & eval(sym(paste0(cur_column(),'.imput'))) > x
#           , yes = x
#           , no = eval(sym(paste0(cur_column(),'.imput')))
#         )
#         
#       }
#       , .names = '{col}.sub'
#     )
#   ) %>%
#   select(
#     ends_with('.sub')
#   ) %>%
#   rename_with(
#     function(x){
#       str_remove(x,'.sub')
#     }
#   ) -> dsds
# 
# FNN::get.knnx(
#   data = 
#     df_occupations.ai %>% 
#     select(names(df_sample)) %>% 
#     select(ends_with('.l'))
#   , query = dsds
#   , k = 873
# ) -> lala
# 
# dsdsds$nn.index %>% view
# lala$nn.index %>% view
# lala$nn.dist %>% view
# 
# fun_KNN.matching(
#   .df_data.numeric = 
#     df_occupations.ai %>% 
#     select(names(df_sample))
#   , .vec_query.numeric = 
#     lalala
#   , .int_k = 873
#   , .imput.over_qualification = T
#   , .dbl_over_qualification.threshold = 0
#   , .dbl_decimals = 1000
# ) -> lalalala
# 
# lala$nn.index %>% view
# lala$nn.dist[,1]

# lala$
#   nn.dist %>% 
#   as_tibble() %>% 
#   mutate(across(
#     .cols = everything()
#     ,.fns = ~ 
#       round(.x, 4) ==
#       round(lalalala$euclidean_distance, 4)
#   )) %>% 
#   colSums()

# lala$nn.dist %>% 
#   as_tibble() %>% 
#   group_nest(
#     row_number()
#   ) %>% 
#   pull(data) %>%
#   map(
#     ~ all(
#       round(.x, 10) ==
#         round(
#           lalalala$
#             euclidean_distance
#           , 10
#         )
#     ))

fun_employability(
  .df_data = 
    df_occupations.ai %>% 
    select(names(df_sample)) %>% 
    select(ends_with('.l'))
  , .df_query = lalala
  , .int_employment = 
    df_occupations$employment2
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
  , .dbl_over_qualification.threshold = 0
)


dsdsds$nn.index %>% view
dsdsds$nn.dist %>% view
dsdsds$nn.dist[,1]

FNN::get.knnx(
  data = 
    df_occupations.ai %>% 
    select(ends_with('.l'))
  , query = 
    df_occupations.ai %>% 
    select(ends_with('.l'))
  , k = 873
) -> dsdsds

df_occupations.ai %>% 
  filter(str_detect(
    str_to_lower(occupation)
    , 'data|anal|econo|physicist'
  )) %>%
  group_nest(
    row_number()
  ) %>% 
  pull(data) %>% 
  # sample(10) %>%
  map(
    ~ fun_knn.matching(
      .df_data =
        df_occupations.ai
      , .df_query = .x
      , .dbl_scale.lb = 0 
      , .dbl_scale.ub = 100
      , .int_k = nrow(df_occupations.ai)
      , .dbl_over_qualification.threshold = 0
    )
    , .progress = T
  ) -> list_knn

set_names(
  list_knn
  , list_knn %>% 
    map_chr(
      ~ .x %>%
        slice(1) %>% 
        pull(occupation)
    )
  ) -> list_knn

list_knn %>% 
  bind_rows(
    .id = 'oqqupation'
  ) %>% 
  full_join(
    df_occupations %>% 
      select(
        occupation
        , employment2
      )
  ) %>% 
  # filter(
  #   oqqupation != 
  #     occupation
  # ) %>% 
  mutate(
    interchangeability = 
      fun_interchangeability(
        similarity
      )
  ) %>% 
  group_by(oqqupation) %>% 
  reframe(
    employability = 
      sum(
        interchangeability *
          employment2 /
          sum(employment2)
      )
  ) %>% 
  arrange(desc(
    employability
  )) %>% 
  print(n = nrow(.))

list_knn %>% 
  bind_rows(
    .id = 'oqqupation'
  ) %>% 
  filter(
    oqqupation != 
      occupation
  ) %>% 
  mutate(
    interchangeability = 
      fun_interchangeability(
        similarity
      )
  ) %>% 
  pivot_longer(
    cols = c(
      interchangeability
      , similarity
    )
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  fun_plot.density(aes(
    x = value
    , fill = metric
  )
  , .list_axis.x.args = list(
    limits = c(-0.1,1.1)
    , breaks = seq(0,1, length.out = 5)
  )
  , .sym_facets = oqqupation
  , .int_facets = 5
  , .fun_format.x = percent
  )
  

list_knn %>% 
  map(
    ~ .x %>% 
      select(
        rank
        , rank.norm
        , occupation
        , similarity
      ) %>% 
      mutate(
        interchangeability = 
          fun_interchangeability(
            similarity
          )
      ) %>% 
      full_join(
        df_occupations %>% 
          select(
            occupation
            , employment2
          )
      ) %>% 
      mutate(
        occupation = 
          first(
            occupation
          )
      ) %>% 
      group_by(occupation) %>%
      reframe(
        employability = 
          sum(
            interchangeability * 
              employment2 / 
              sum(employment2)
          )
      )
  ) %>% 
  bind_rows() %>% 
  arrange(desc(
    employability
  )) %>% 
  print(n = 100)

fun_employability(
  .df_data = 
    df_occupations.ai
  , .df_query =
    df_sample %>%
    mutate(across(
      .cols = ends_with('.l')
      ,.fns = ~ .x * 100
    ))
  , .int_employment = 
    df_occupations %>% 
    select(
      occupation
      , employment2
    ) %>% 
    full_join(
      df_occupations.ai
    ) %>% 
    pull(employment2)
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
  , .dbl_over_qualification.threshold = 0
) -> dsds

FNN::get.knnx(
  data = 
    df_occupations.ai %>% 
    select(ends_with('.l'))
  , query = 
    df_occupations.ai %>% 
    select(ends_with('.l'))
  , k = 873
) -> dsdsds


dsdsds$
  nn.index %>% 
  as_tibble() %>% 
  group_nest(
    row_number()
  ) %>%
  rename(index = 2) %>% 
  full_join(
    dsdsds$
      nn.dist %>% 
      as_tibble() %>% 
      group_nest(
        row_number()
      ) %>% 
      rename(dist = 2)
  ) %>%
  bind_cols(
    df_occupations.ai
  ) %>%
  mutate(
    similarity = 
      map(
        dist
        , ~ fun_similarity(
          df_occupations.ai
          , .dbl_distance = .x
          , .dbl_scale.lb = 0
          , .dbl_scale.ub = 100
        )
        , .progress = T
      )
  )

dsds %>% 
  full_join(
    df_occupations %>% 
      select(
        occupation
        , employment2
      )
  ) %>% 
  select(
    rank
    , occupation
    , similarity
    , employment2
  ) %>% 
  mutate(
    .after = occupation
    , interchangeability = 
      fun_interchangeability(similarity)
  ) -> dsds

dsds %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    )
    , 'physician|doctor|medic|surgeon|clinic'
  ))

dsds %>% 
  pivot_longer(
    cols = c(
      interchangeability
      , similarity
    )
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  fun_plot.histogram(aes(
    x = value
    , fill = metric
  ))

dsds %>% 
  reframe(
    employability = 
      sum(
        interchangeability * 
          employment2 / 
          sum(employment2)
      )
  )


fun_knn.matching(
  .df_data =
    df_occupations.ai
  , .df_query =
    df_sample %>%
    mutate(across(
      .cols = ends_with('.l')
      ,.fns = ~ .x * 100
    ))
  , .int_k =
    df_occupations.ai %>%
    nrow()
  , .dbl_over_qualification.threshold = 0
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
)

df_occupations.ai %>%
  fun_employability(
    .df_data.query =
      df_sample
  )

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
