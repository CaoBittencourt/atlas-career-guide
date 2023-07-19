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

# - Professional type -----------------------------------------------------
fun_professional_type <- function(df_data, efa_model){
  
  # Factor scores items
  # Factor scores factor scores
  # Numbers -> description
    # Highly / poorly qualified (score levels)
    # Generalist / specialist (score dispersion)
    # Areas of expertise / focus (relative score importance)
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(df_data)
        , any(
          loadings(efa_model)[,] %>%
            rownames() %in%
            names(df_data)
        )))
  
  stopifnot(
    "'efa_model' must be a factor analysis object." =
      any(
        str_to_lower(class(
          efa_model
        )) == 'factanal'
        , str_to_lower(class(
          efa_model
        )) == 'fa'
      )
  )
  
  # Data wrangling
  # df_data %>% 
  #   mutate(
  #     id = row_number()
  #     , id = factor(id)
  #   ) -> df_data
  
  # Calculate factor scores
  fun_factor_scores(
    df_data = df_data
    , efa_model = efa_model
    , lgc_pivot = T
  ) -> df_factor_scores_long
  
  # rm(df_data)
  
  df_factor_scores_long %>% 
    # group_by(id) %>%
    group_by(across(c(
      -where(is.numeric)
      , -factor
    ) , .fns = fct_inorder
    )) %>%
    # group_by(not numeric not factor) %>% 
    mutate(
      factor_score = 
        factor_score /
        max(factor_score)
    ) -> df_factor_scores_long
  
  # Calculate item standard deviation
  # df_data %>% 
  #   pivot_longer(
  #     cols = ...
  #     # cols = items
  #     , names_to = 'item'
  #     , values_to = 'item_score'
  #   ) %>% 
  #   group_by(id) %>%  
  #   # group_by(not numeric not item) %>%
  #   reframe(
  #     sd = sd(item_score)
  #   ) %>% 
  #   left_join(
  #     df_factor_scores_long
  #   ) -> df_factor_scores_long
  
  # Estimate professional profile
  df_factor_scores_long %>% 
    group_by(across(c(
      -where(is.numeric)
      , -factor
    ) , .fns = fct_inorder
    )) %>%
    # group_by(id) %>% 
    # group_by(across(
    #   !where(is.numeric)
    # )) %>% 
    mutate(
      professional_profile = 
        fun_interchangeability(
          factor_score
          , sd(factor_score)
          # , sd(df_input[-1])
          # , first(sd)
        ) %>% as.numeric()
      , professional_profile = 
        round(professional_profile, 4)
      , factor = 
        factor(
          factor
          , levels = 
            str_sort(
              factor
              , numeric = T
            )
        )
    ) -> df_factor_scores_long
  
  return(df_factor_scores_long)
  stop()
  
  # group_by(numeric not factor) %>% 
  
  # dbl_factor_scores / 
  #   max(dbl_comparison) -> 
  #   dbl_comparison_norm
  # 
  # fun_interchangeability(
  #   dbl_comparison_norm
  #   , .dbl_scaling =
  #     sd(dbl_comparison_norm)
  # ) -> dbl_comparison_norm
  
  # Output
  return(df_factor_scores_long)
  
}

fun_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_input
  , chr_method = 'logit'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , mtx_weights = NULL
  , lgc_sort = T
  , id_col = 'occupation'
) -> dsds

# - Auxiliary factor score matching (field of expertise) -------------------
fun_similarity_factor_scores <- function(
    dbl_factor_scores
    , dbl_comparison
){
  
  # Arguments validation
  stopifnot(
    "'dbl_factor_scores' must be a vector of factor scores." = 
      is.numeric(dbl_factor_scores)
  )
  
  stopifnot(
    "'dbl_comparison' must be a vector of factor scores." = 
      is.numeric(dbl_comparison)
  )
  
  # Coerce arguments
  dbl_comparison[1:length(
    dbl_factor_scores
  )] -> dbl_comparison
  
  # Most important factors
  dbl_comparison / 
    max(dbl_comparison) -> 
    dbl_comparison_norm
  
  fun_interchangeability(
    dbl_comparison_norm
    , .dbl_scaling =
      sd(dbl_comparison_norm)
  ) -> dbl_comparison_norm
  
  as.numeric(
    dbl_comparison_norm
  ) -> dbl_comparison_norm
  
  # Mean factor similarity
  weighted.mean(pmin(
    dbl_factor_scores / dbl_comparison
    , 1), dbl_comparison_norm
  ) -> dbl_factor_scores_similarity
  
  # Output
  return(dbl_factor_scores_similarity)
  
}

# - Auxiliary weights function --------------------------------------------
fun_w <- function(df_data_cols, dbl_scaling = 0.25){
  
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
  
  # Relative importance compared to top item
  t(
    t(df_data_cols) / 
      mtx_weights
  ) -> mtx_weights
  
  # Calculate matching regression weights
  fun_interchangeability(
    .mtx_similarity = mtx_weights
    , .dbl_scaling = dbl_scaling
  ) -> mtx_weights
  
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
fun_professional_type(
  df_data = 
    df_input
    # df_occupations %>%
    # slice_sample(
    #   n = 5
    # ) %>%
    # select(
    #   occupation
    #   , ends_with('.l')
    # )
  , efa_model = 
    efa_model
) %>%
  group_by(occupation) %>% 
  mutate(
    rank = min_rank(
      professional_profile
    )
    , professional_profile_desc = 
      cut(professional_profile, breaks = 5, labels = F)
  ) %>%
  fun_plot.heatmap(aes(
    x = factor
    , y = occupation
    , label = rank
    , fill = professional_profile_desc
    , alpha = professional_profile
  )
  , .list_legend = guides(
    alpha = 'none'
  )
  , .reorder_fct = F
  , .fun_format.y = function(y){
    str_wrap(y,40)
  }
  )

df_factors %>% 
  group_by(factor, factor.name) %>% 
  slice(1) %>% 
  select(factor, factor.name) %>% 
  view

df_occupations %>% 
  slice_sample(
    n = 1
  ) %>% 
  select(
    occupation
    , ends_with('.l')
    , education_years
  ) -> df_sample

df_sample$occupation

fun_similarity(
  df_data_rows = df_occupations
  # df_data_rows = df_occupations[names(df_input)]
  # , df_query_rows = df_input
  , df_query_rows = df_sample %>% select(!education_years)
  # , chr_method = 'bvls'
  , chr_method = 'logit'
  # , chr_method = 'pearson'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_sort = F
) -> dsds

dsds

dsds$
  df_similarity %>%
  select(
    occupation
    , entry_level_education
    # , education_years
    , similarity
  ) %>%
  arrange(desc(
    similarity
  )) %>% 
  left_join(
    df_occupations %>% 
      select(
        occupation
        , ends_with('.l')
      ) %>% 
      fun_factor_scores(
        efa_model = efa_model
        # , lgc_pivot = T
        , lgc_pivot = T
      )
  ) -> dsdsds

dsdsds %>% 
  filter(
    occupation == first(occupation)
  ) %>% 
  pull(factor_score) -> 
  lalala

dsdsds %>% 
  filter(
    occupation == last(occupation)
  ) %>% 
  pull(factor_score) -> 
  lala

fun_similarity_factor_scores(
  dbl_factor_scores = lalala,
  dbl_comparison = lala
)


dsdsds %>% 
  filter(
    occupation %in% 
      c(
        first(occupation)
        , sample(df_occupations$occupation, 1)
      )
  ) %>% 
  reframe(
    similarity_factor = 
      fun_factor_scores_similarity(
        dbl_factor_scores = 
          filter(.,occupation == first(occupation)) %>% 
          pull(factor_score)
        , dbl_comparison = 
          filter(.,occupation != first(occupation)) %>% 
          pull(factor_score)
      )
  )

mutate(
  I = fun_interchangeability(
    similarity
    , .dbl_scaling = 2
    # , .dbl_years_education = 21 + 2.5
    , .dbl_years_education =
      df_sample$education_years
    , .dbl_years_education_min =
      education_years
  ) %>% as.numeric()
  # , I = 
  #   I * fun_similarity_factor_scores(
  #     dbl_factor_scores = dsds
  #     , dbl_comparison = lalala
  #   )
  # , I = I *
  #   fun_interchangeability(
  #     similarity
  #     , .dbl_scaling = 1
  #   ) %>% as.numeric()
  # , I = round(I, 4)
  # ) %>% view
) %>% 
  full_join(
    df_occupations %>% 
      select(
        occupation
        , employment2
      )
  ) %>% 
  reframe(
    employability = 
      sum(I * employment2) /
      sum(employment2)
  )

# ) %>%
# filter(str_detect(
#   str_to_lower(
#     occupation
#   ), 'hosp|exec'
# ))


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









