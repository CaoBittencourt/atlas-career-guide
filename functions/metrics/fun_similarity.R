# [SETUP] -------------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'bvls'
  , 'fastglm'
  , 'weights'
  , 'tidyverse' #Data wrangling
  # , 'modeest' #Mode
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTIONS] -------------------------------------------------------------
# # - Interchangeability function -------------------------------------------
# fun_interchangeability <- function(
    #     
#   
#   
# ){
#   
#   
#   
# }

# - Auxiliary weights function --------------------------------------------
fun_w <- function(df_data_cols, dbl_scaling = 1){
  
  # Get maximum item scores for each column
  vapply(
    as_tibble(df_data_cols)
    , function(x){max(x, na.rm = T)}
    , FUN.VALUE = numeric(1)
  ) -> mtx_weights
  
  # Prevent division by zero
  mtx_weights[
    mtx_weights == 0
  ] <- 1
  
  # Importance of each item compared to most important item
  # df_data_cols / 
  #   mtx_weights -> 
  #   mtx_weights
  
  # Calculate matching regression weights
  # fun_interchangeability(
  #   .mtx_similarity = mtx_weights
  #   , .dbl_scaling = dbl_scaling
  # ) -> mtx_weights
  
  # Output
  return(mtx_weights)
  
}

# - Auxiliary similarity function ---------------------------------------------------
fun_s <- function(
    
  df_data_cols
  , df_query_cols
  , chr_method
  , dbl_scale_ub
  , dbl_scale_lb
  , mtx_weights
  
){
  
  # Arguments validation handled by fun_similarity
  # Data wrangling handled by fun_similarity
  # Regression weights handled by fun_similarity
  
  # Apply method
  if(
    chr_method == 'bvls'
  ){
    
    sqrt(mtx_weights) ->
      mtx_weights
    
    df_data_cols *
      mtx_weights ->
      df_data_cols
    
    map2_dbl(
      .x = as_tibble(df_data_cols)
      , .y = as_tibble(mtx_weights)
      , ~
        coef(bvls(
          as.matrix(.x)
          , df_query_cols[,] * .y
          , bl = 0
          , bu = 1
        ))
      
    ) -> dbl_similarity
    
  } else if(
    chr_method == 'pearson'
  ){
    
    map2_dbl(
      .x = as_tibble(df_data_cols)
      , .y = as_tibble(mtx_weights)
      , ~ (
        1 +
          weights::wtd.cors(
            df_query_cols[,]
            , .x
            , weight = .y
          )[,]
      ) / 2
      
    ) -> dbl_similarity
    
    
  } else {
    
    list_c(map(
      .x = as.integer(df_query_cols[,])
      , ~ rep(
        c(1,0), times = c(
          .x, (dbl_scale_ub - dbl_scale_lb) - .x
        )
      )
    )) -> int_bernoulli
    
    map2_dbl(
      .x =
        map(
          .x = as_tibble(df_data_cols)
          , ~
            as.matrix(list_c(map(
              .x = as.integer(.x)
              , ~ rep(
                c(1,0), times = c(
                  .x, (dbl_scale_ub - dbl_scale_lb) - .x
                )
              )
            )))
        )
      , .y =
        as_tibble(mtx_weights)[rep(
          1:nrow(mtx_weights)
          , each = 
            dbl_scale_ub - 
            dbl_scale_lb
        ),]
      , ~
        coef(fastglmPure(
          x = .x
          , y = int_bernoulli
          , family = binomial(
            link = chr_method
          ), weights = .y
        ))
    ) -> dbl_similarity
    
    exp(dbl_similarity) /
      (1 + exp(dbl_similarity)) ->
      dbl_similarity
    
  }
  
  # # Add similarities to original data frame
  # df_data_rows$similarity <- dbl_similarity
  # 
  # # Sort data frame
  # if(lgc_sort){
  #   
  #   df_data_rows %>%
  #     arrange(desc(
  #       similarity
  #     )) -> df_data_rows
  #   
  # }
  
  # Output
  # return(df_data_rows)
  return(dbl_similarity)
  
}

# - Generic Similarity Matrix function ---------------------------------------------------
fun_similarity <- function(
    
  df_data_rows
  , df_query_rows
  , chr_method = c('bvls', 'logit', 'probit', 'pearson')
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , mtx_weights = NULL
  , lgc_sort = F
  , id_col = NULL
  
){
  
  # Argument validation
  # Arguments validation
  stopifnot(
    "'df_data_rows' must be a data frame." =
      is.data.frame(df_data_rows)
  )
  
  stopifnot(
    "'df_query_rows' must be a data frame." =
      is.data.frame(df_query_rows)
  )
  
  stopifnot(
    "'chr_method' must be one of the following methods: 'bvls', 'logit', 'probit', or 'pearson'." =
      chr_method %in% c('bvls', 'logit', 'probit', 'pearson')
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'mtx_weights' must be either NULL or numeric." =
      any(
        is.numeric(mtx_weights)
        , is.null(mtx_weights)
      )
  )
  
  stopifnot(
    "'lgc_sort' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_sort)
        , !is.na(lgc_sort)
      )
  )
  
  stopifnot(
    "'id_col' must be a character indicating the ID column of 'df_data_rows'." =
      any(
        id_col %in% names(df_data_rows)
        , is.null(id_col)
      )
  )
  
  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  chr_method[[1]] -> chr_method
  
  str_to_lower(chr_method) -> chr_method
  
  Filter(
    function(x){
      
      all(
        is.numeric(x)
        , x <= dbl_scale_ub
        , x >= dbl_scale_lb
      )
      
    }
    , df_query_rows
  ) -> df_query_cols
  
  df_data_rows[names(
    df_query_cols
  )] -> df_data_cols
  
  # Pivot data
  t(df_query_cols) -> df_query_cols
  t(df_data_cols) -> df_data_cols
  
  # Weights
  if(!length(mtx_weights)){
    
    fun_w(
      df_data_cols = 
        df_data_cols
    ) -> mtx_weights
    
  }
  
  return(list(
    'w' = mtx_weights
    , 'dfc' = df_data_cols
    , 'dfr' = df_data_rows
  ))
  stop()
  
  # Apply similarity function
  if(nrow(df_query_rows) == 1){
    
    fun_s(
      df_data_cols = df_data_cols
      , df_query_cols = df_query_cols
      , chr_method = chr_method
      , dbl_scale_ub = dbl_scale_ub
      , dbl_scale_lb = dbl_scale_lb
      , mtx_weights = mtx_weights
    ) -> df_data_rows$similarity
    
    # Sort data frame
    if(lgc_sort){
      
      df_data_rows %>%
        arrange(desc(
          similarity
        )) -> df_data_rows
      
    }
    
    list_similarity <- NULL
    mtx_similarity <- NULL
    
  } else {
    
    df_query_cols %>%
      as_tibble() %>% 
      map(
        ~ fun_s(
          df_data_cols = df_data_cols
          , df_query_cols = as.matrix(.x)
          , chr_method = chr_method
          , dbl_scale_ub = dbl_scale_ub
          , dbl_scale_lb = dbl_scale_lb
          , mtx_weights = mtx_weights
        )
      ) -> list_similarity
    
    # Similarity matrix
    if(
      all(
        df_data_cols ==
        df_query_cols
      )
    ){
      
      list_similarity %>%
        bind_cols() %>%
        as.matrix() ->
        mtx_similarity
      
      if(length(id_col)){
        
        id_col[[1]] -> id_col
        
        df_data_rows %>%
          pull(!!sym(id_col)) ->
          colnames(
            mtx_similarity
          )
        
        colnames(
          mtx_similarity
        ) ->
          rownames(
            mtx_similarity
          )
        
        colnames(
          mtx_similarity
        ) ->
          names(
            list_similarity
          )
        
      }
      
    }
    
    df_data_rows <- NULL
    
  }
  
  # Output
  return(compact(list(
    'df_similarity' = df_data_rows
    , 'list_similarity' = list_similarity
    , 'mtx_similarity' = mtx_similarity
  )))
  
}

# dsds --------------------------------------------------------------------
# fun_similarity(
#   df_data_rows = 
#     df_occupations %>% 
#     select(
#       occupation,
#       ends_with('.l')
#     ) %>%
#     slice_head(n = 10)
#   , df_query_rows = 
#     df_occupations %>% 
#     select(
#       occupation,
#       ends_with('.l')
#     ) %>%
#     slice_head(n = 10)
#   , chr_method = 'bvls'
#   # , chr_method = 'logit'
#   # , chr_method = 'pearson'
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
#   , lgc_sort = F
#   , id_col = 'occupation'
# ) -> dsds

fun_similarity(
  df_data_rows = df_occupations
  # df_data_rows = df_occupations[names(df_input)]
  , df_query_rows = df_input
  # , chr_method = 'bvls'
  , chr_method = 'logit'
  # , chr_method = 'pearson'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_sort = F
) -> dsds

view(round(dsds,4))

dsds$
  df_similarity %>%
  select(
    occupation
    , education_years
    , similarity
  ) %>%
  arrange(desc(
    similarity
  )) %>% 
  mutate(
    I = fun_interchangeability(
      similarity
      , .dbl_scaling = 1
      , .dbl_years_education = 21
      , .dbl_years_education_min =
        education_years
    ) %>% as.numeric()
    , I = round(I, 4)
    # ) %>% view
  ) %>%
  filter(str_detect(
    str_to_lower(
      occupation
    ), 'hosp|exec'
  ))


# fun_w(
#   df_occupations %>% 
#     select(
#       ends_with('.l')
#     ) %>% 
#     as.matrix() %>% 
#     t() %>% 
#     `colnames<-`(
#       df_occupations$
#         occupation
#     ) / 100
#   , scale = 0.5
# ) %>% 
#   round(4) %>% 
#   as_tibble(
#     rownames = 'item'
#   ) %>% 
#   select(
#     item
#     , starts_with('Hospital')
#   ) %>% 
#   view

(
  fun_interchangeability(
    dsds$df_similarity$similarity %>% 
      set_names(df_occupations$occupation)
    , .dbl_scaling = 1
    , .dbl_years_education = 21
    , .dbl_years_education_min = 
      df_occupations$
      education_years
  ) * 
    fun_interchangeability(
      dsds$df_similarity$similarity %>% 
        set_names(df_occupations$occupation)
      , .dbl_scaling = 1
    )
) %>% 
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  rename(
    I = 2
  ) %>% 
  mutate(
    I = round(I, 4)
  ) %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    ), 'hosp|engineer'
  )) %>% 
  arrange(desc(I)) %>%
  print(n = nrow(.))









