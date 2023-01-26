# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')
# Commas
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_commas.R')

library(Hmisc)

df_occupations

fun_plot.comparisons <- function(
    
  # Data
  .df_data.user = NULL
  , .df_data.comparison
  # Terms of comparison (character vectors)
  , ...
  # Comparisons
  , lgc_compare.items = T
  , lgc_compare.items.average = T
  , lgc_compare.factors = T
  , lgc_compare.factors.average = T
  
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
      is.data.frame(.df_data.user) | !length(.df_data.user)
  )
  
  stopifnot(
    "'.df_data.comparison' must be a data frame containing the item scores for all comparison terms." = 
      is.data.frame(.df_data.comparison)
  )
  
  # Terms
  list_terms %>% 
    map(
      ~ stopifnot(
        # Character
        'The comparison terms must be a character vector.' = 
          is.character(.x) | is.factor(.x)
        # Valid terms (i.e. terms in a column of df_occupations)
        , 'Invalid comparison terms. Please, provide comparison terms present in the Atlas database.' = 
          .x %in% 
          (
            df_occupations %>% 
              select(!where(is.numeric)) %>% 
              flatten_chr()
          )
      ))
  
  # Logical
  stopifnot(
    "'lgc_compare.items' must be either TRUE or FALSE." = 
      isTRUE(lgc_compare.items) | !isTRUE(lgc_compare.items)
  )
  
  stopifnot(
    "'lgc_compare.items.average' must be either TRUE or FALSE." = 
      isTRUE(lgc_compare.items.average) | !isTRUE(lgc_compare.items.average)
  )
  
  stopifnot(
    "'lgc_compare.factors' must be either TRUE or FALSE." = 
      isTRUE(lgc_compare.factors) | !isTRUE(lgc_compare.factors)
  )
  
  stopifnot(
    "'lgc_compare.factors.average' must be either TRUE or FALSE." = 
      isTRUE(lgc_compare.factors.average) | !isTRUE(lgc_compare.factors.average)
  )
  
  
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
  list_terms %>%
    map(
      ~ fun_text.commas(
        .x
        , .chr_sep = '; '
        , .chr_last.sep = '; and '
        , .lgc_quote = F
      )) -> names(list_terms)
  
  # Actual individual item and factor scores
  list_terms %>%
    map(
      function(terms){
        
        df_occupations %>%
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
    bind_rows(.id = 'term') %>%
    mutate(
      term = factor(term, levels = names(list_terms))
    ) -> df_scores.individual
  
  if(length(.df_data.user)){
    
    df_scores.individual %>% 
      bind_rows(
        .df_data.user
      ) -> df_scores.individual
    
  }
  
  df_scores.individual %>% 
    bind_cols(
      fun_factor.scores(
        .df_data.numeric = .
        , .list_factor.keys = list_factors
        , .lgc_pivot.long = F
        , .lgc_totals = F
      )) %>% 
    mutate(
      employment2 = ifelse(
        is.na(employment2)
        , 1
        , employment2
      )
    ) -> df_scores.individual
  
  # Average item and factor scores
  df_scores.individual %>% 
    group_by(term) %>% 
    summarise(across(
      .cols = where(is.numeric)
      ,.fns = ~ wtd.mean(
        .x
        , weights = 
          employment2 / sum(employment2)
        , na.rm = T
      ))
      , n = n
    ) %>%
    ungroup() -> df_scores.average
  
  # Comparison plots
  # 
  if(lgc_compare.items){
    
    
    
  } else {
    
    NULL -> plt_factor.scores
    
  }
  
  return(compact(list(
    'actual' = df_scores.individual
    , 'average' = df_scores.average
  )))
  
}


fun_plot.comparisons(
  .df_data.user = df_input
  , .df_data.comparison = df_occupations
  , sample(df_occupations$occupation, 1)
  , sample(df_occupations$occupation, 10)
  , sample(df_occupations$occupation, 2)
  , sample(df_occupations$occupation, 1)
  , 'Finance'
  , 'Financial Managers'
  , lgc_factors.heatmap = F
  # ) %>% view
) -> dsds

dsds$actual %>% view
dsds$average %>% view

dsds$user
dsds$data
dsds$scores %>% view

dsds$average.scores %>% view

dsds$individual.scores %>% view
dsds$average.scores %>% view

dsds$items.average
dsds$factor_scores
dsds$factor_scores.average

dsds$factor.scores %>% view

dsds$factor.scores$`Financial Managers`
df_occupations %>% 
  filter(occupation == 'Financial Managers') %>% 
  pull(annual_wage_2021)

dsds$factor.scores$Finance
dsds$factor.scores

dsds$factor.scores$Finance %>% view

intersect(
  'Financial'
  , df_occupations$occupation
)

intersect(
  df_occupations$career_cluster
  , df_occupations$occupation
)



sample(df_occupations$occupation, 10) %>% 
  fun_text.commas(.lgc_quote = F) %>% 
  
  
  str_trunc(
    width = 40
    , ellipsis = ' ...'
  )

# fun_factor.scores()
# scoreVeryFast()
fun_plot.comparisons(
  .df_data = tibble()
  , c(
    # df_occupations$occupation %>%
    #   unique() %>%
    #   sample(2),
    df_occupations$entry_level_education %>% 
      unique()# %>%
    # sample(2)
  )
  , df_occupations$occupation %>%
    unique() %>%
    sample(2)
)



df_occupations %>% 
  group_by(across(where(
    ~ !is.numeric(.x)
  ))) %>% 
  summarise(across(
    .cols = where(is.numeric)
    ,.fns = mean
  )) -> lalala


bind_cols(
  lalala
  , fun_factor.scores(
    .df_data.numeric = lalala
    , .list_factor.keys = list_factors
  ) %>% 
    first()
) -> lala


fun_factor.scores(
  .list_factor.keys = list_factors
)


fun_plot.comparisons(
  .df_data = tibble()
  , c(
    # df_occupations$occupation %>% 
    #   unique() %>%
    #   sample(2), 
    df_occupations$entry_level_education %>% 
      unique() %>%
      sample(2)
  )
) %>% 
  map(
    function(x){
      x %>%
        pull(1) %>% 
        unique()
      
    }
  )


# ) %>%
first() %>% 
  fun_factor.scores(
    .list_factor.keys = list_factors
    , .lgc_pivot.long = T
  )

df_occupations %>% 
  filter(if_any(
    .cols = everything()
    ,.fns = 
      ~ .x %in% all_of(
        c(
          df_occupations$occupation %>%
            unique() %>%
            sample(2)
          , df_occupations$entry_level_education %>% 
            unique() %>%
            sample(2)
        )
      ))) %>% view()


