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
# - Generic Similarity function ---------------------------------------------------
fun_s <- function(
    
  df_data_rows
  , df_query_rows
  , chr_method = c('bvls', 'logit', 'probit', 'pearson')
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_sort = F
  
){
  
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
    "'lgc_sort' must be either TRUE or FALSE." =
      all(
        is.logical(lgc_sort)
        , !is.na(lgc_sort)
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
        is.numeric(x),
        all(x <= dbl_scale_ub)
      )
      
    }
    , df_query_rows
  ) -> df_query_rows
  
  df_data_rows[names(
    df_query_rows
  )] -> df_data_cols
  
  df_query_rows[
    df_query_rows > 
      dbl_scale_ub
  ] <- dbl_scale_ub
  
  df_query_rows[
    df_query_rows < 
      dbl_scale_lb
  ] <- dbl_scale_lb
  
  df_data_cols[
    df_data_cols > 
      dbl_scale_ub
  ] <- dbl_scale_ub
  
  df_data_cols[
    df_data_cols < 
      dbl_scale_lb
  ] <- dbl_scale_lb
  
  # Pivot data
  t(df_query_rows) -> df_query_cols
  t(df_data_cols) -> df_data_cols
  
  rm(df_query_rows)
  
  # Weights
  fun_interchangeability(
    as.matrix(
      df_data_cols
    ) / dbl_scale_ub
    , .dbl_scaling = 0.125
    # , .dbl_scaling =
    #   map_dbl(
    #     df_data_cols
    #     , ~
    #       # 2 - .x %>%
    #       # 1 - .x %>%
    #       # 1 / .x %>%
    #       .x %>%
    #       fun_kflex(
    #         .dbl_scale.lb =
    #           dbl_scale_lb
    #         , .dbl_scale.ub =
    #           dbl_scale_ub
    #       )
    #   )
  ) -> mtx_weights
  
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
        bvls(
          as.matrix(.x)
          , df_query_cols[,] * .y
          , bl = 0
          , bu = 1
        )[[1]]
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
            df_input.t[,]
            , .x
            , weight = .y
          )[,]
      ) / 2
      
    ) -> dbl_similarity
    
    
  } else {
    
    list_c(map(
      .x = as.integer(df_query_cols)
      , ~ rep(
        c(1,0), times =
          c(.x, dbl_scale_ub - .x)
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
                c(1,0), times =
                  c(.x, dbl_scale_ub - .x)
              )
            )))
        )
      , .y =
        as_tibble(mtx_weights)[rep(
          1:nrow(mtx_weights)
          , each = dbl_scale_ub
        ),]
      , ~
        coef(fastglmPure(
          x = .x
          , y = int_bernoulli
          , family = binomial(
            link = chr_method
          )
          , weights = .y
        ))
    ) -> dbl_similarity
    
    exp(dbl_similarity) /
      (1 + exp(dbl_similarity)) ->
      dbl_similarity
    
  }
  
  # Add similarities to original data frame
  df_data_rows$similarity <- dbl_similarity
  
  # Sort data frame
  if(lgc_sort){
    
    df_data_rows %>%
      arrange(desc(
        similarity
      )) -> df_data_rows
    
  }
  
  # Output
  return(df_data_rows)
  
}

# - Generic Similarity Matrix function ---------------------------------------------------
fun_similarity <- function(
    
  df_data_rows
  , df_query_rows
  , chr_method = c('bvls', 'logit', 'probit', 'pearson')
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_sort = F
  , id_col = NULL
  
){
  
  # Argument validation
  stopifnot(
    "'df_query_rows' must be a data frame." =
      is.data.frame(df_query_rows)
  )
  
  stopifnot(
    "'id_col' must be a character indicating the ID column of 'df_data_rows'." =
      any(
        id_col %in% names(df_data_rows)
        , is.null(id_col)
      )
  )
  
  # Apply similarity function
  if(nrow(df_query_rows) == 1){
    
    match.call(expand.dots = T) ->
      call_similarity
    
    call_similarity$
      id_col <- NULL
    
    as.name('fun_s') -> 
      call_similarity[[1]]
    
    eval.parent(call_similarity) -> 
      df_similarity
    
    list_similarity <- NULL
    mtx_similarity <- NULL
    
  } else {
    
    df_query_rows %>%
      group_nest(
        row_number()
      ) %>%
      pull(data) %>% 
      map( 
        ~ fun_s(
          df_data_rows = df_data_rows
          , df_query_rows = .x
          , chr_method = chr_method
          , dbl_scale_ub = dbl_scale_ub
          , dbl_scale_lb = dbl_scale_lb
          , lgc_sort = F
        )
      ) -> list_similarity
    
    df_similarity <- NULL
    
    # Similarity matrix
    if(
      all(
        df_data_rows ==
        df_query_rows
      )
    ){
      
      list_similarity %>% 
        set_names(paste0(
          'V', 1:length(list_similarity)
        )) %>% 
        map(~ .x$similarity) %>%
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
        
      }
      
    }
    
  }
  
  # Output
  return(compact(list(
    'df_similarity' = 
      df_similarity
    , 'list_similarity' = 
      list_similarity
    , 'mtx_similarity' = 
      mtx_similarity
  )))
  
}

fun_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_input
  , chr_method = 'bvls'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_sort = T
) -> dsds

# - Interchangeability function -------------------------------------------
fun_interchangeability <- function(
    
  
  
){
  
  
  
}



