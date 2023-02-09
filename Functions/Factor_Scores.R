# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'psych' #Factor Analysis
  , 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# ------- FUNCTIONS -----------------------------------------------------------
# FACTOR SCORES FUNCTION -----------------------------------------------------------
fun_factor.scores <- function(
    
  .df_data.numeric
  , .list_factor.keys
  , .lgc_totals = F
  , .lgc_pivot.long = F
  
){
  
  # Data frame
  if(!is.data.frame(.df_data.numeric)){
    
    stop("'.df_data.numeric' must be a data frame.")
    
  }
  
  # List
  if(!is.list(.list_factor.keys)){
    
    stop("'.list_factor.keys' must be a data frame.")
    
  }
  
  # Logical
  if(!(isTRUE(.lgc_totals) | !isTRUE(.lgc_totals))){
    
    stop("'.lgc_totals' must be a either TRUE or FALSE.")
    
  }
  
  if(!(isTRUE(.lgc_pivot.long) | !isTRUE(.lgc_pivot.long))){
    
    stop("'.lgc_pivot.long' must be a either TRUE or FALSE.")
    
  }
  
  
  # Select only numeric data
  .df_data.numeric %>%
    select(where(is.numeric)) -> .df_data.numeric
  
  # Select only variables in the list of factor keys
  .df_data.numeric %>%
    select(
      .list_factor.keys %>%
        flatten_chr() %>%
        all_of()
    ) -> .df_data.numeric
  
  # Factor scores
  map(
    .list_factor.keys
    , function(scales){
      
      psych::scoreVeryFast(
        keys = scales
        , items = .df_data.numeric
        , totals = .lgc_totals #Average scores
      ) %>%
        as_tibble() -> df_temp
      
      if(nrow(.df_data.numeric) == 1){
        
        df_temp %>%
          colMeans() -> df_temp
        
      }
      
      return(df_temp)
      
    }
  ) %>%
    flatten_df() -> df_factor.scores
  
  
  # Pivot data
  if(.lgc_pivot.long){
    
    # Factor names data frame
    map_df(
      .list_factor.keys
      , ~ enframe(
        .x
        , name = 'factor'
        , value = 'item'
      )
    ) %>% 
      unnest(
        cols = 'item'
      ) %>% 
      full_join(
        map_df(
          list(.list_factor.keys)
          , ~ enframe(
            .x
            , name = 'category'
            , value = 'item'
          )
        ) %>% 
          unnest(
            cols = 'item'
          ) %>% 
          unnest(
            cols = 'item'
          )
      ) %>% 
      relocate(
        category
        , factor
        , item
      ) -> df_factor.names
    
    # # Factor names data frame
    # list_factors %>%
    #   bind_rows(
    #     .id = 'category'
    #   ) %>%
    #   pivot_longer(
    #     cols = -category
    #     , names_to = 'factor'
    #     , values_to = 'item'
    #   ) %>%
    #   drop_na() %>%
    #   ungroup() -> df_factor.names
    
    # Pivot and join to factor names
    .df_data.numeric %>%
      pivot_longer(
        cols = everything()
        , names_to = 'item'
        , values_to = 'score'
      ) %>%
      full_join(
        df_factor.names
      ) -> df_factor.scores.long
    
    # Add factor scores
    df_factor.scores %>%
      pivot_longer(
        cols = everything()
        , names_to = 'factor'
        , values_to = 'factor.score'
      ) %>%
      full_join(
        df_factor.scores.long
      ) -> df_factor.scores.long
    
  } else {
    
    df_factor.scores.long <- NULL
    
  }
  
  
  # Output
  return(compact(list(
    'factor.scores' = df_factor.scores
    , 'factor.scores.long' = df_factor.scores.long
  )))
  
}

# # FACTOR SCORES FUNCTION 2 -----------------------------------------------------------
# fun_factor.scores2 <- function(
    # 
#   .df_data
#   , .list_factor.keys
#   , .lgc_totals = F
#   , .lgc_pivot.long = F
#   , .lgc_sample.averages = F
# 
# ){
# 
#   # Data frame
#   stopifnot(
#     "'.df_data' must be a data frame." =
#       is.data.frame(.df_data)
#   )
# 
#   # Named 3-level list
#   stopifnot(
#     "'.list_factor.keys' must be a named three-level list of factor keys." = c(
#       is.list(.list_factor.keys)
#       , !is.data.frame(.list_factor.keys)
#       , vec_depth(.list_factor.keys) <= 3
#     )
#   )
# 
#   # Logical
#   stopifnot(
#     "'.lgc_totals' must be either TRUE or FALSE." =
#       isTRUE(.lgc_totals) |
#       !isTRUE(.lgc_totals)
#   )
# 
#   stopifnot(
#     "'.lgc_pivot.long' must be either TRUE or FALSE." =
#       isTRUE(.lgc_pivot.long) |
#       !isTRUE(.lgc_pivot.long)
#   )
# 
#   stopifnot(
#     "'.lgc_sample.averages' must be either TRUE or FALSE." =
#       isTRUE(.lgc_pivot.long) |
#       !isTRUE(.lgc_pivot.long)
#   )
# 
#   # Factor list provided
#   as.logical(length(.list_factor.keys)) -> lgc_factor.list
# 
#   # Fill in empty levels of 3-level factor list
#   if(vec_depth(.list_factor.keys) == 1){
# 
#     # Generic category and factor
#     list(
#       Category1 = list(
#         Factor1 =
#           .df_data %>%
#           select(
#             where(is.numeric)
#           ) %>%
#           names()
#       )
#     ) -> .list_factor.keys
# 
#   }
# 
#   if(vec_depth(.list_factor.keys) == 2){
# 
#     # If unnamed factors, apply generic names
#     if(!length(names(.list_factor.keys))){
# 
#       setNames(
#         .list_factor.keys
#         , paste0(
#           'Factor'
#           , 1:length(.list_factor.keys)
#         )
#       ) -> .list_factor.keys
# 
#     }
# 
#     # Generic category
#     list(
#       Category1 = .list_factor.keys
#     ) -> .list_factor.keys
# 
#   }
# 
#   if(vec_depth(.list_factor.keys) == 3){
# 
#     # If unnamed factors, apply generic name
#     if(!length(names(flatten(.list_factor.keys)))){
# 
#       map2(
#         .x = .list_factor.keys
#         , .y = 1:length(.list_factor.keys)
#         , .f = function(.x, .y){
# 
#           setNames(
#             .x
#             , paste0(.y, 'Factor', 1:length(.x))
#           )
# 
#         }
#       ) -> .list_factor.keys
# 
#       # If unnamed categories, apply generic names
#       if(!length(names(.list_factor.keys))){
# 
#         setNames(
#           .list_factor.keys
#           , paste0(
#             'Category'
#             , seq(1, length(.list_factor.keys))
#           )
#         ) -> .list_factor.keys
# 
#       }
# 
#     }
# 
#   }
# 
#   # Select id and only variables in the list of factor keys
#   .df_data %>%
#     select(
#       !where(is.numeric)
#       , .list_factor.keys %>%
#         flatten_chr() %>%
#         all_of()
#     ) -> .df_data
# 
#   # Category scores
#   psych::scoreVeryFast(
#     keys = .list_factor.keys
#     , items = .df_data %>% select(where(is.numeric))
#     , totals = .lgc_totals #Average scores
#   ) %>%
#     as_tibble() -> df_category.scores
# 
#   if(nrow(.df_data) == 1){
# 
#     df_category.scores %>%
#       colMeans() -> df_category.scores
# 
#   }
# 
#   .df_data %>%
#     select(
#       setdiff(
#         names(.df_data)
#         , names(df_category.scores)
#       )
#     ) %>%
#     bind_cols(
#       df_category.scores
#     ) %>%
#     relocate(
#       !where(is.numeric)
#       , .list_factor.keys %>%
#         names()
#       , where(is.numeric)
#     ) -> df_category.scores
# 
#   df_category.scores -> df_factor.scores
# 
#   # Factor scores
#   if(lgc_factor.list){
# 
#     map(
#       .list_factor.keys
#       , function(scales){
# 
#         psych::scoreVeryFast(
#           keys = scales
#           , items = .df_data %>% select(where(is.numeric))
#           , totals = .lgc_totals #Average scores
#         ) %>%
#           as_tibble() -> df_temp
# 
#         if(nrow(.df_data) == 1){
# 
#           df_temp %>%
#             colMeans() -> df_temp
# 
#         }
# 
#         return(df_temp)
# 
#       }
#     ) %>%
#       flatten_df() -> df_factor.scores
# 
#     .df_data %>%
#       select(
#         setdiff(
#           names(.df_data)
#           , names(df_factor.scores)
#         )
#       ) %>%
#       bind_cols(
#         df_factor.scores
#       ) %>%
#       relocate(
#         !where(is.numeric)
#         , .list_factor.keys %>%
#           flatten() %>%
#           names()
#         , where(is.numeric)
#       ) -> df_factor.scores
# 
#     df_category.scores %>%
#       select(
#         setdiff(
#           names(df_category.scores)
#           , names(df_factor.scores)
#         )
#       ) %>%
#       bind_cols(
#         df_factor.scores
#       ) %>%
#       relocate(
#         !where(is.numeric)
#         , .list_factor.keys %>%
#           flatten() %>%
#           names()
#         , where(is.numeric)
#       ) -> df_factor.scores
# 
#   }
# 
#   # Factor names data frame
#   .list_factor.keys %>%
#     bind_rows(
#       .id = 'category'
#     ) -> df_factor.names
# 
#   if(!lgc_factor.list){
# 
#     df_factor.names %>%
#       pivot_longer(
#         cols = everything()
#         , names_to = 'factor'
#         , values_to = 'item'
#       ) %>%
#       drop_na() %>%
#       ungroup() %>%
#       mutate(
#         category = factor
#         , .before = factor
#       ) -> df_factor.names
# 
#   } else {
# 
#     df_factor.names %>%
#       pivot_longer(
#         cols = -category
#         , names_to = 'factor'
#         , values_to = 'item'
#       ) %>%
#       drop_na() %>%
#       ungroup() %>%
#       arrange(
#         factor(
#           category
#           , levels =
#             .list_factor.keys %>%
#             names()
#         )
#         , factor(
#           factor
#           , levels =
#             .list_factor.keys %>%
#             flatten() %>%
#             names()
#         )
#         , factor(
#           item
#           , levels =
#             .list_factor.keys %>%
#             flatten_chr()
#         )
#       ) -> df_factor.names
# 
#   }
# 
#   # Long data frame
#   NULL -> df_factor.scores.long
# 
#   # Pivot data
#   if(.lgc_pivot.long){
# 
#     df_category.scores %>%
#       pivot_longer(
#         cols = names(.list_factor.keys)
#         , names_to = 'category'
#         , values_to = 'category.score'
#       ) %>%
#       select(
#         !where(is.numeric)
#         , category
#         , category.score
#       ) -> df_category.scores.long
# 
#     # Pivot and join to factor names
#     .df_data %>%
#       pivot_longer(
#         cols = where(is.numeric)
#         , names_to = 'item'
#         , values_to = 'item.score'
#       ) %>%
#       full_join(
#         df_factor.names
#       ) %>%
#       full_join(
#         df_category.scores.long
#       ) -> df_factor.scores.long
# 
#     # Add factor scores
#     df_factor.scores %>%
#       pivot_longer(
#         cols = where(is.numeric)
#         , names_to = 'factor'
#         , values_to = 'factor.score'
#       ) %>%
#       inner_join(
#         df_factor.scores.long
#       ) %>%
#       select(
#         !where(is.numeric)
#         , category
#         , factor
#         , item
#         , item.score
#         , factor.score
#         , category.score
#       ) -> df_factor.scores.long
# 
#   }
# 
#   # Aggregate sample averages
#   NULL -> df_factor.scores.average
# 
#   NULL -> df_factor.scores.average.long
# 
#   if(.lgc_sample.averages){
# 
#     df_factor.scores %>%
#       summarise(across(
#         .cols = where(is.numeric)
#         ,.fns = ~ mean(.x, na.rm = T)
#       )) -> df_factor.scores.average
# 
#     if(.lgc_pivot.long){
# 
#       df_factor.scores.long %>%
#         group_by(category, factor, item) %>%
#         summarise(across(
#           .cols = where(is.numeric)
#           ,.fns = ~ mean(.x, na.rm = T)
#         )) -> df_factor.scores.average.long
# 
#     }
# 
#   }
# 
#   # Output
#   return(compact(list(
#     'factor.names' = df_factor.names
#     , 'factor.scores' = df_factor.scores
#     , 'factor.scores.long' = df_factor.scores.long
#     , 'factor.scores.average' = df_factor.scores.average
#     , 'factor.scores.average.long' = df_factor.scores.average.long
# 
#   )))
# 
# }
# 

# FACTOR SCORES FUNCTION 2 -----------------------------------------------------------
fun_factor.scores2 <- function(
    
  .df_data
  , .list_factor.keys = list()
  , .lgc_sample.averages = F
  , .lgc_pivot.long = F
  , .lgc_totals = F
  
){
  
  # Data frame
  stopifnot(
    "'.df_data' must be a data frame." = 
      is.data.frame(.df_data)
  )
  
  # Named 3-level list
  stopifnot(
    "'.list_factor.keys' must be a named three-level list of factor keys." = c(
      is.list(.list_factor.keys)
      , !is.data.frame(.list_factor.keys)
      , vec_depth(.list_factor.keys) <= 3
    )
  )
  
  # Factor keys contained in data frame
  stopifnot(
    "All items in '.list_factor.keys' must be contained in the '.df_data' data frame." = 
      !length(.list_factor.keys) |
      all(
        flatten_chr(.list_factor.keys) %in% 
          names(.df_data)
      )
  )
  
  # Logical
  stopifnot(
    "'.lgc_totals' must be either TRUE or FALSE." = 
      isTRUE(.lgc_totals) |
      !isTRUE(.lgc_totals)
  )
  
  stopifnot(
    "'.lgc_pivot.long' must be either TRUE or FALSE." = 
      isTRUE(.lgc_pivot.long) |
      !isTRUE(.lgc_pivot.long)
  )
  
  stopifnot(
    "'.lgc_sample.averages' must be either TRUE or FALSE." = 
      isTRUE(.lgc_pivot.long) |
      !isTRUE(.lgc_pivot.long)
  )
  
  # Fill in empty levels of 3-level factor list
  if(vec_depth(.list_factor.keys) == 1){
    
    # Generic category and factor
    list(
      Category1 = list(
        Factor1 = 
          .df_data %>% 
          select(
            where(is.numeric)
          ) %>%  
          names()
      )
    ) -> .list_factor.keys
    
  } 
  
  if(vec_depth(.list_factor.keys) == 2){
    
    # If unnamed factors, apply generic names
    if(!length(names(.list_factor.keys))){
      
      setNames(
        .list_factor.keys
        , paste0(
          'Factor'
          , 1:length(.list_factor.keys)
        )
      ) -> .list_factor.keys
      
    }
    
    # Generic category
    list(
      Category1 = .list_factor.keys
    ) -> .list_factor.keys
    
  } 
  
  if(vec_depth(.list_factor.keys) == 3){
    
    # If unnamed factors, apply generic name
    if(!length(names(flatten(.list_factor.keys)))){
      
      map2(
        .x = .list_factor.keys
        , .y = 1:length(.list_factor.keys)
        , .f = function(.x, .y){
          
          setNames(
            .x
            , paste0(.y, 'Factor', 1:length(.x))
          )
          
        }
      ) -> .list_factor.keys
      
      # If unnamed categories, apply generic names
      if(!length(names(.list_factor.keys))){
        
        setNames(
          .list_factor.keys
          , paste0(
            'Category'
            , 1:length(.list_factor.keys)
          )
        ) -> .list_factor.keys
        
      }
      
    }
    
  }
  
  # Select id and variables in the list of factor keys
  .df_data %>% 
    select(
      !where(is.numeric)
      , .list_factor.keys %>% 
        flatten_chr() %>% 
        all_of()
    ) -> .df_data
  
  # Category scores
  psych::scoreVeryFast(
    keys = .list_factor.keys
    , items = .df_data %>% select(where(is.numeric))
    , totals = .lgc_totals #Average scores
  ) %>% 
    as_tibble() -> df_category.scores
  
  if(nrow(.df_data) == 1){
    
    df_category.scores %>% 
      colMeans() -> df_category.scores
    
  }
  
  # Category scores
  .df_data %>%
    select(
      setdiff(
        names(.df_data)
        , names(df_category.scores)
      )
    ) %>%
    bind_cols(
      df_category.scores
    ) %>%
    relocate(
      !where(is.numeric)
      , .list_factor.keys %>%
        names()
      , where(is.numeric)
    ) -> df_category.scores
  
  # Factor scores
  map(
    .list_factor.keys
    , function(scales){
      
      psych::scoreVeryFast(
        keys = scales
        , items = .df_data %>% select(where(is.numeric))
        , totals = .lgc_totals #Average scores
      ) %>% 
        as_tibble() -> df_temp
      
      if(nrow(.df_data) == 1){
        
        df_temp %>% 
          colMeans() -> df_temp
        
      }
      
      return(df_temp)
      
    } 
  ) %>% 
    flatten_df() -> df_factor.scores
  
  # Add id and items
  .df_data %>%
    select(
      setdiff(
        names(.df_data)
        , names(df_factor.scores)
      )
    ) %>%
    bind_cols(
      df_factor.scores
    ) %>%
    relocate(
      !where(is.numeric)
      , .list_factor.keys %>%
        flatten() %>%
        names()
      , where(is.numeric)
    ) -> df_factor.scores
  
  # Add categories
  df_category.scores %>%
    select(
      setdiff(
        names(df_category.scores)
        , names(df_factor.scores)
      )
    ) %>%
    bind_cols(
      df_factor.scores
    ) %>%
    rowwise() %>%
    mutate(
      Overall = mean(
        c_across(all_of(
          flatten_chr(.list_factor.keys))
        )
        , na.rm = T
      )) %>% 
    ungroup() %>% 
    relocate(
      !where(is.numeric)
      , Overall
      , .list_factor.keys %>%
        names()
      , .list_factor.keys %>%
        flatten() %>%
        names()
      , where(is.numeric)
    ) -> df_factor.scores
  
  # Overall scores
  # df_factor.scores %>% 
  #   rowwise() %>%
  #   mutate(
  #     Overall = mean(
  #       c_across(all_of(
  #         flatten_chr(.list_factor.keys))
  #       )
  #       , na.rm = T
  #     )) %>% 
  #   ungroup() -> df_factor.scores
  
  # Factor names data frame
  map_df(
    .list_factor.keys
    , ~ enframe(
      .x
      , name = 'factor'
      , value = 'item'
    )
  ) %>% 
    unnest(
      cols = 'item'
    ) %>% 
    full_join(
      map_df(
        list(.list_factor.keys)
        , ~ enframe(
          .x
          , name = 'category'
          , value = 'item'
        )
      ) %>% 
        unnest(
          cols = 'item'
        ) %>% 
        unnest(
          cols = 'item'
        )
    ) %>% 
    relocate(
      category
      , factor
      , item
    ) -> df_factor.names
  
  # Long data frame
  NULL -> df_factor.scores.long
  
  # Pivot data
  if(.lgc_pivot.long){
    
    # Long category scores 
    df_category.scores %>% 
      pivot_longer(
        cols = names(.list_factor.keys)
        , names_to = 'category'
        , values_to = 'category.score'
      ) %>% 
      select(
        !where(is.numeric)
        , category
        , category.score
      ) -> df_category.scores.long
    
    # Pivot and join to factor names
    .df_data %>%
      pivot_longer(
        cols = where(is.numeric)
        , names_to = 'item'
        , values_to = 'item.score'
      ) %>%
      full_join(
        df_factor.names
      ) %>% 
      full_join(
        df_category.scores.long
      ) -> df_factor.scores.long
    
    # Add factor scores
    df_factor.scores %>%
      pivot_longer(
        cols = where(is.numeric)
        , names_to = 'factor'
        , values_to = 'factor.score'
      ) %>%
      inner_join(
        df_factor.scores.long
      ) %>% 
      select(
        !where(is.numeric)
        , category
        , factor
        , item
        , item.score
        , factor.score
        , category.score
      ) -> df_factor.scores.long
    
    # Overall scores
    df_factor.scores.long %>% 
      group_by(across(c(
        !where(is.numeric)
        , -category
        , -factor
        , -item
      ))) %>% 
      mutate(
        overall.score = mean(
          item.score
          , na.rm = T
        )) %>% 
      ungroup() -> df_factor.scores.long
    
  }
  
  # Aggregate sample averages
  NULL -> df_factor.scores.average
  
  NULL -> df_factor.scores.average.long
  
  if(.lgc_sample.averages){
    
    # Average scores data frame
    df_factor.scores %>% 
      summarise(across(
        .cols = where(is.numeric)
        ,.fns = ~ mean(.x, na.rm = T)
      )) -> df_factor.scores.average
    
    # Average scores long data frame
    if(.lgc_pivot.long){
      
      df_factor.scores.long %>% 
        group_by(category, factor, item) %>% 
        summarise(across(
          .cols = where(is.numeric)
          ,.fns = ~ mean(.x, na.rm = T)
        )) -> df_factor.scores.average.long
      
    }
    
  }
  
  # Output
  return(compact(list(
    'factor.names' = df_factor.names
    , 'factor.scores' = df_factor.scores
    , 'factor.scores.long' = df_factor.scores.long
    , 'factor.scores.average' = df_factor.scores.average
    , 'factor.scores.average.long' = df_factor.scores.average.long
    
  )))
  
}
