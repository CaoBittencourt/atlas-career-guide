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
  .df_data
  # Terms of comparison (character vectors)
  , ...
  # # Comparisons
  # , lgc_factors.dumbbell = T
  , lgc_factors.heatmap = T
  # , lgc_items.heatmap = T
  # , lgc_items.circular.bar = T
  # , lgc_items.circular.heatmap = T
  # , lgc_etc = T
  
){
  
  # List of terms
  list2(...) -> list_terms
  
  # Unique terms
  list_terms %>% 
    map(unique) -> list_terms
  
  # Arguments validation
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
  
  
  # Term names
  list_terms %>%
    map(
      ~ fun_text.commas(
        .x
        , .chr_sep = '; '
        , .chr_last.sep = '; and '
        , .lgc_quote = F
      )) -> names(list_terms)
  
  # Comparison columns
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
          return()
        
      }
    ) -> list_df
  
  # Factor scores (list)
  list_df %>%
    map(
      ~ bind_cols(
        .x
        , fun_factor.scores(
          .df_data.numeric = .x
          , .list_factor.keys = list_factors
          , .lgc_pivot.long = F
          , .lgc_totals = F
        ))
    ) -> list_df_factor.scores
  
  # Factor scores by terms (data frame)
  list_df_factor.scores %>%
    map(
      ~ .x %>%
        # group_by(across(
        #   where(
        #     ~ !is.numeric(.x)
        #   ))) %>%
        summarise(across(
          .cols = where(is.numeric)
          ,.fns = ~ wtd.mean(
            .x
            , weights = 
              employment2 / sum(employment2)
          ))
          , n = n()
        ) #%>%
      # ungroup()
    ) %>% 
    bind_rows(.id = 'term') -> df_factor.scores
  
  # # Comparison terms data frames
  #   Map(
  #     function(chr_cols, chr_terms){
  # 
  #       df_occupations %>%
  #         select(
  #           chr_cols
  #           , where(is.numeric)
  #           ) %>%
  #       filter(
  #         !!sym(all_of(chr_cols)) %in% chr_terms
  #       )
  #     }
  #     , chr_cols = list_cols
  #     , chr_terms = list_terms
  #   ) %>%
  #   return()
  
  # # Comparison columns
  # list_terms %>% 
  #   map(
  #     function(x){
  #       
  #       df_occupations %>% 
  #         select(
  #           where(
  #             ~ any(x %in% .x)
  #           # )) %>%
  #           )) %>%
  #         # select(1) %>% 
  #         names()
  #     }
  #   ) -> list_cols
  # 
  # 
  # # Comparison terms data frames
  #   Map(
  #     function(chr_cols, chr_terms){
  # 
  #       df_occupations %>%
  #         select(
  #           chr_cols
  #           , where(is.numeric)
  #           ) %>%
  #       filter(
  #         !!sym(all_of(chr_cols)) %in% chr_terms
  #       )
  #     }
  #     , chr_cols = list_cols
  #     , chr_terms = list_terms
  #   ) %>%
  #   return()
  
  # FACTORS HEATMAP
  if(lgc_factors.heatmap){
    
    df_factor.scores %>%
      select(
        term
        , list_factors %>% 
          flatten() %>% 
          names()
      ) %>% 
      pivot_longer(
        cols = -term
        , names_to = 'factor'
        , values_to = 'score'
      ) %>% 
      fun_plot.heatmap(aes(
        x = term
        , y = factor
        , fill = score
      )) -> plt_factor.scores
    
  } else {
    
    NULL -> plt_factor.scores
    
  }
  
  return(compact(list(
    # 'factor.scores' = plt_factor.scores
    'factor.scores' = df_factor.scores
  )))
  
}


fun_plot.comparisons(
  .df_data = tibble()
  , sample(df_occupations$occupation, 1)
  , sample(df_occupations$occupation, 10)
  , sample(df_occupations$occupation, 2)
  , sample(df_occupations$occupation, 1)
  , 'Finance'
  , 'Financial Managers'
  , lgc_factors.heatmap = F
) -> dsds

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


