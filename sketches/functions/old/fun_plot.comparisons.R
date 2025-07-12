# [SETUP] -----------------------------------------------------------------
# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')

library(Hmisc)

# [DATA] EFA-REDUCED QUERY VECTOR -----------------------------------------------
# USER INPUT DATA FRAME
df_input <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv')

# Select user
df_input %>% 
  filter(Name == 'Cao') -> df_input

# EFA-reduced data frame
df_input %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){
        recode((x + 2)
               # recode(x
               , '1' = 0.00
               , '2' = 0.17
               , '3' = 0.33
               , '4' = 0.50
               , '5' = 0.67
               , '6' = 0.83
               , '7' = 1.00
        )}
    )
  ) -> df_input

# [FUNCTION] PLOT COMPARISON ----------------------------------------------
fun_plot.comparisons <- function(
    
  # Data
  .df_data.user = NULL
  , .df_data.comparison
  , .chr_weights.var = NULL
  # Terms of comparison (character vectors)
  , ...
  # Factor list
  , .list_factors = list()
  # Comparisons
  , .lgc_compare.items = T
  , .lgc_compare.items.average = T
  , .lgc_compare.factors = T
  , .lgc_compare.factors.average = T
  
){
  
  # List of terms
  list2(...) -> list_terms
  
  # Unique terms
  list_terms %>% 
    map(unique) -> list_terms
  
  # Arguments validation
  # Data frames
  stopifnot(
    "'.df_data.user' must be a data frame containing the user's item scores." = 
      is.data.frame(.df_data.user) |
      !length(.df_data.user)
  )
  
  stopifnot(
    "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." = 
      is.data.frame(.df_data.comparison)
  )
  
  # Numeric
  stopifnot(
    "'.chr_weights.var' must be the name of a numeric column from the comparison data frame." =
      !length(.chr_weights.var) | 
      is.character(.chr_weights.var) &
      length(.chr_weights.var) == 1 &
      .chr_weights.var %in% names(.df_data.comparison)
  )
  
  # List
  stopifnot(
    "'.list_factors' must be a list of factor keys." = c(
      is.list(.list_factors)
      | !length(.list_factors)
      , !is.data.frame(.list_factors)
    ))
  
  # Terms
  list_terms %>% 
    map(
      ~ stopifnot(
        # Character
        'The comparison terms must be a character vector.' = 
          is.character(.x) |
          is.factor(.x)
        # Valid terms (i.e. terms in a column of .df_data.comparison)
        , 'Invalid comparison terms. Please, provide comparison terms present in the Atlas database.' = 
          .x %in% 
          (
            .df_data.comparison %>% 
              select(!where(is.numeric)) %>% 
              flatten_chr()
          )
      ))
  
  # Logical
  stopifnot(
    "'.lgc_compare.items' must be either TRUE or FALSE." = 
      isTRUE(.lgc_compare.items) |
      !isTRUE(.lgc_compare.items)
  )
  
  stopifnot(
    "'.lgc_compare.items.average' must be either TRUE or FALSE." = 
      isTRUE(.lgc_compare.items.average) |
      !isTRUE(.lgc_compare.items.average)
  )
  
  stopifnot(
    "'.lgc_compare.factors' must be either TRUE or FALSE." = 
      isTRUE(.lgc_compare.factors) |
      !isTRUE(.lgc_compare.factors)
  )
  
  stopifnot(
    "'.lgc_compare.factors.average' must be either TRUE or FALSE." = 
      isTRUE(.lgc_compare.factors.average) |
      !isTRUE(.lgc_compare.factors.average)
  )
  
  
  # Weights
  # if(length(.dbl_weights)){
  #   
  #   .dbl_weights[is.na(.dbl_weights)] <- 1
  #   
  # }
  
  # if(length(.dbl_weights) & length(.df_data.user)){
  #   
  #   c(
  #     .dbl_weights
  #     , rep(1, nrow(.df_data.user))
  #   ) -> .dbl_weights
  #   
  # }
  
  # User term
  if(length(.df_data.user)){
    
    .df_data.user %>% 
      mutate(
        term = 'user'
        , n = n()
        , .before = everything()
      ) -> .df_data.user
    
  }
  
  # Term names
  if(length(names(list_terms))){
    
    Map(
      function(name, term){
        
        if(name == ''){
          
          term %>% 
            fun_text.commas(
              .chr_sep = '; '
              , .chr_last.sep = '; and '
              , .lgc_quote = F
            ) %>%
            return()
          
        } else {
          
          name %>%
            return()
        }
        
      }
      , name = names(list_terms)
      , term = list_terms
    ) -> names(list_terms)
    
  } else { 
    
    list_terms %>%
      map(
        ~ fun_text.commas(
          .x
          , .chr_sep = '; '
          , .chr_last.sep = '; and '
          , .lgc_quote = F
        )) -> names(list_terms)
    
  }
  
  # Actual individual item and factor scores
  list_terms %>%
    map(
      function(terms){
        
        .df_data.comparison %>%
          filter(if_any(
            .cols = everything()
            ,.fns = 
              ~ .x %in% all_of(terms)
          )) %>% 
          select(
            where(
              ~ is.numeric(.x)
              | any(.x %in% terms)
            )) %>%
          mutate(n = n()) %>% 
          return()
        
      }
    ) %>% 
    bind_rows(.id = 'term') -> df_scores.individual
  
  if(length(.df_data.user)){
    
    df_scores.individual %>% 
      bind_rows(
        .df_data.user %>% 
          select(
            intersect(
              names(.df_data.user)
              , names(df_scores.individual)
            ))
      ) %>% 
      mutate(across(
        .cols = -where(is.numeric)
        ,.fns = ~ ifelse(term == 'user', 'user', .x)
      )) -> df_scores.individual
    
  }
  
  if(length(.df_data.user) & length(.chr_weights.var)){
    
    df_scores.individual %>% 
      mutate(across(
        .cols = .chr_weights.var
        ,.fns = ~ ifelse(
          is.na(.x), 1, .x
        )
      )) -> df_scores.individual
    
  }
  
  df_scores.individual %>% 
    mutate(
      term = fct_inorder(term)
    ) -> df_scores.individual
  
  if(length(.list_factors)){
    
    df_scores.individual %>% 
      bind_cols(
        fun_factor.scores(
          .df_data.numeric = .
          , .list_factor.keys = .list_factors
          , .lgc_pivot.long = F
          , .lgc_totals = F
        )) -> df_scores.individual
    
  }
  
  # Average item and factor scores
  df_scores.individual %>% 
    group_by(term) %>% 
    summarise(across(
      .cols = where(is.numeric)
      ,.fns = ~ wtd.mean(
        .x
        , weights = !!sym(.chr_weights.var)
        , na.rm = T
      ))
      , n = n
    ) %>%
    ungroup() -> df_scores.average
  
  
  # Comparison plots
  NULL -> plt_items
  NULL -> plt_items.average
  NULL -> plt_factors
  NULL -> plt_factors.average
  
  # Comparison variables
  if(length(.list_factors)){
    
    .list_factors %>%
      flatten_chr() -> chr_vars
    
  } else {
    
    .df_data.comparison %>%
      select(
        where(is.numeric)
        , - .chr_weights.var
        , -n
      ) %>% 
      names() -> chr_vars
    
  }
  
  # Item by item comparison
  if(.lgc_compare.items){
    
    df_scores.individual %>%
      select(
        -where(is.numeric)
        , -term
        , all_of(c(
          chr_vars
        ))
      ) %>%
      pivot_longer(
        cols = where(is.numeric)
      # ) %>% return()
      ) -> plt_items
      # fun_plot.bar(aes(
      #   x = name
      #   , y = value
      #   , fill = term
      # )
      # , .reorder_desc = F
      # # , .reorder_fun = max
      # # , .reorder_fun = first
      # , .coord_flip = T
      # , .list_geom.param = list(
      #   position = c(
      #     position_dodge(width = 0.8)
      #   )
      #   , width = 0.7
      #   , fill = '#3854FB'
      # )
      # , .theme = ggridges::theme_ridges(
      #   grid = F
      #   # line_size = 0
      #   , center_axis_labels = T
      # )
      # , .fun_format.y = percent_format(accuracy = 1)
      # ) -> plt_items
    
  }
  
  # Average item by item comparison
  if(.lgc_compare.items.average){
    
    
    
  }
  
  # Factor scores comparison
  if(.lgc_compare.factors){
    
    
    
  }
  
  # Average factor scores comparison
  if(.lgc_compare.factors.average){
    
    
    
  }
  
  
  # Output
  return(compact(list(
    'actual' = df_scores.individual
    , 'wtg' = .chr_weights.var
    , 'vars' = chr_vars
    , 'average' = df_scores.average
    , 'items' = plt_items
    , 'items.average' = plt_items.average
    , 'factors' = plt_factors
    , 'factors.average' = plt_factors.average
  )))
  
}

# [TEST] ------------------------------------------------------------------
fun_plot.comparisons(
  .df_data.user = df_input
  , .df_data.comparison = 
    df_occupations.efa.comp %>% 
    mutate(
      wgt = employment2 / sum(employment2)
    )
  , .chr_weights.var = 'wgt'
  # , sample(df_occupations$occupation, 1)
  , 'Sample.10' = sample(df_occupations$occupation, 10)
  # , sample(df_occupations$occupation, 2)
  # , sample(df_occupations$occupation, 1)
  # , 'Finance.High_Edu' = df_occupations %>%
  #   filter(
  #     career_cluster %in% "Finance"
  #     , entry_level_education %in% c(
  #       "Bachelor's degree"
  #       , "Postsecondary nondegree award"
  #       , "Master's degree"
  #       , "Doctoral or professional degree"
  #     )) %>% 
  #   pull(occupation)
  # ,'Finance'
  # , 'Financial Managers'
  , .list_factors = list_factors.competencies
  # , lgc_factors.heatmap = F
  # ) %>% view
) -> dsds

dsds

# [TEST] PLOT -------------------------------------------------------------
dsds$actual %>% 
  select(
    term
    # , ends_with('.l')
    , all_of(c(
      list_factors.competencies %>%
        flatten() %>%
        flatten_chr()
    ))) %>%
  pivot_longer(
    cols = -term
  ) %>%
  fun_plot.bar(aes(
    x = name
    , y = value
    , fill = term
  )
  , .reorder_desc = F
  , .reorder_fun = max
  # , .reorder_fun = first
  , .coord_flip = T
  , .list_geom.param = list(
    position = c(
      position_dodge(width = 0.8)
    )
    , width = 0.7
    , fill = '#3854FB'
  )
  , .theme = ggridges::theme_ridges(
    grid = F
    # line_size = 0
    , center_axis_labels = T
  )
  , .fun_format.y = percent_format(accuracy = 1)
  # ) -> lalala
  )

dsds$average %>%
  select(
    term
    # , ends_with('.l')
    , all_of(c(
      list_factors.competencies %>% 
        flatten_chr()
    ))) %>% 
  mutate(across(
    .cols = where(is.numeric)
    ,.fns = ~ 1
  )) %>% 
  pivot_longer(
    cols = -term
  )

# # [TEST] ------------------------------------------------------------------
# fun_plot.comparisons(
#   .df_data.user = df_input
#   , .df_data.comparison = df_occupations
#   , sample(df_occupations$occupation, 1)
#   , sample(df_occupations$occupation, 10)
#   , sample(df_occupations$occupation, 2)
#   , sample(df_occupations$occupation, 1)
#   , df_occupations %>%
#     filter(
#       career_cluster %in% "Finance"
#       , entry_level_education %in% c(
#         "Bachelor's degree"
#         , "Postsecondary nondegree award"
#         , "Master's degree"
#         , "Doctoral or professional degree"
#       )) %>% 
#     pull(occupation)
#   , 'Finance'
#   , 'Financial Managers'
#   # , lgc_factors.heatmap = F
#   # ) %>% view
# ) -> dsds
# 
# dsds$actual %>% view
# 
# dsds$actual %>% view
# dsds$average %>% view
# 
# dsds$user
# dsds$data
# dsds$scores %>% view
# 
# dsds$average.scores %>% view
# 
# dsds$individual.scores %>% view
# dsds$average.scores %>% view
# 
# dsds$items.average
# dsds$factor_scores
# dsds$factor_scores.average
# 
# dsds$factor.scores %>% view
# 
# dsds$factor.scores$`Financial Managers`
# df_occupations %>% 
#   filter(occupation == 'Financial Managers') %>% 
#   pull(annual_wage_2021)
# 
# dsds$factor.scores$Finance
# dsds$factor.scores
# 
# dsds$factor.scores$Finance %>% view
# 
# intersect(
#   'Financial'
#   , df_occupations$occupation
# )
# 
# intersect(
#   df_occupations$career_cluster
#   , df_occupations$occupation
# )
# 
# 
# 
# sample(df_occupations$occupation, 10) %>% 
#   fun_text.commas(.lgc_quote = F) %>% 
#   
#   
#   str_trunc(
#     width = 40
#     , ellipsis = ' ...'
#   )
# 
# # fun_factor.scores()
# # scoreVeryFast()
# fun_plot.comparisons(
#   .df_data = tibble()
#   , c(
#     # df_occupations$occupation %>%
#     #   unique() %>%
#     #   sample(2),
#     df_occupations$entry_level_education %>% 
#       unique()# %>%
#     # sample(2)
#   )
#   , df_occupations$occupation %>%
#     unique() %>%
#     sample(2)
# )
# 
# 
# 
# df_occupations %>% 
#   group_by(across(where(
#     ~ !is.numeric(.x)
#   ))) %>% 
#   summarise(across(
#     .cols = where(is.numeric)
#     ,.fns = mean
#   )) -> lalala
# 
# 
# bind_cols(
#   lalala
#   , fun_factor.scores(
#     .df_data.numeric = lalala
#     , .list_factor.keys = list_factors
#   ) %>% 
#     first()
# ) -> lala
# 
# 
# fun_factor.scores(
#   .list_factor.keys = list_factors
# )
# 
# 
# fun_plot.comparisons(
#   .df_data = tibble()
#   , c(
#     # df_occupations$occupation %>% 
#     #   unique() %>%
#     #   sample(2), 
#     df_occupations$entry_level_education %>% 
#       unique() %>%
#       sample(2)
#   )
# ) %>% 
#   map(
#     function(x){
#       x %>%
#         pull(1) %>% 
#         unique()
#       
#     }
#   )
# 
# 
# # ) %>%
# first() %>% 
#   fun_factor.scores(
#     .list_factor.keys = list_factors
#     , .lgc_pivot.long = T
#   )
# 
# df_occupations %>% 
#   filter(if_any(
#     .cols = everything()
#     ,.fns = 
#       ~ .x %in% all_of(
#         c(
#           df_occupations$occupation %>%
#             unique() %>%
#             sample(2)
#           , df_occupations$entry_level_education %>% 
#             unique() %>%
#             sample(2)
#         )
#       ))) %>% view()
# 
# 
