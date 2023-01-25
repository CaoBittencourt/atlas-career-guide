# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.EFA.R')

fun_plot.comparisons <- function(
    
  # Data
  .df_data
  # Terms of comparison (character vectors)
  , ...
  # # Comparisons
  # , lgc_factors.dumbbell = T
  # , lgc_factors.heatmap = T
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
  
  # Terms validation
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
  
  # Factor scores
  list_df %>%
    map(
      ~ bind_cols(
        .x
        , fun_factor.scores(
          .df_data.numeric = .x
          , .list_factor.keys = list_factors
          , .lgc_pivot.long = F
          , .lgc_totals = F
        )) %>% 
        # first() %>% 
        # group_by(across(
        #   where(
        #     ~ !is.numeric(.x)
        #   ))) %>% 
        summarise(across(
          .cols = where(is.numeric)
          ,.fns = mean
        )) %>% 
        ungroup()
    ) -> list_df_factors.scores
  
  return(list_df_factors.scores)
  
  
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
  
  # Factor scores
  # list_terms %>% 
  #   map(
  #     fun_factor.scores
  #   )
  
  
}

fun_plot.comparisons(
  .df_data = tibble()
  , sample(df_occupations$occupation, 1)
  , sample(df_occupations$occupation, 10)
  , sample(df_occupations$occupation, 2)
  , sample(df_occupations$occupation, 1)
) %>% bind_rows() %>% view()


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


