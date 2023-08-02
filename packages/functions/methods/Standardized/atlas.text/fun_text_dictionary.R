# [SETUP] --------------------------------------------------------------------
# # - Packages --------------------------------------------------------------
# library(dplyr)
# library(purrr)

# [FUNCTIONS] --------------------------------------------------------------------
# - Dictionary evaluation -------------------------------------------------
fun_text_dictionary <- function(
    
  # Dictionary data frame
  df_dictionary_long
  # Score to be evaluated
  , dbl_score_eval
  # Dictionary entry
  , chr_dictionary_key
  
){
  
  # Arguments validation
  stopifnot(
    "'df_dictionary_long' must be a data frame containing 'key', 'interval', 'interval_lb', and 'text' columns." =
      all(
        is.data.frame(
          df_dictionary_long
        )
        , all(c(
          'key'
          , 'interval'
          , 'interval_lb'
          , 'text'
        ) %in%
          names(
            df_dictionary_long
          )
        ))
  )
  
  stopifnot(
    "'dbl_score_eval' must be numeric." =
      is.numeric(dbl_score_eval)
  )
  
  stopifnot(
    "'chr_dictionary_key' must be a character." =
      all(
        is.character(chr_dictionary_key)
        , chr_dictionary_key %in%
          df_dictionary_long$key
      )
  )
  
  # Find interval
  df_dictionary_long %>% 
    filter(
      key == chr_dictionary_key
    ) %>% 
    mutate(
      score_interval = 
        findInterval(
          dbl_score_eval
          , interval_lb
        )
    ) %>% 
    filter(
      interval ==
        score_interval
    ) %>% 
    pull(
      text
    ) -> chr_text
  
  # Output
  return(chr_text)
  
}

# - Vectorized dictionary evaluation -------------------------------------------------
fun_text_dictionary_list <- function(
    
  # Dictionary data frame
  df_dictionary_long
  # Named list of scores to be evaluated
  , list_dbl_score_eval
  
){
  
  # Arguments validation
  stopifnot(
    "'df_dictionary_long' must be a data frame containing 'key', 'interval', 'interval_lb', and 'text' columns." =
      all(
        is.data.frame(
          df_dictionary_long
        )
        , all(c(
          'key'
          , 'interval'
          , 'interval_lb'
          , 'text'
        ) %in%
          names(
            df_dictionary_long
          )
        ))
  )
  
  stopifnot(
    "'list_dbl_score_eval' must be a named list of numeric elements." = 
      all(
        is.list(list_dbl_score_eval)
        , !is.null(names(
          list_dbl_score_eval
        ))
        , all(names(
          list_dbl_score_eval %in% 
            df_dictionary_long$key
        ))
        , map_lgl(
          list_dbl_score_eval
          , is.numeric
        ) %>% 
          all()
      )
  )
  
  # Find interval
  map2(
    .x = list_dbl_score_eval
    , .y = names(list_dbl_score_eval)
    , ~ 
      df_dictionary_long %>% 
      filter(key == .y) %>% 
      mutate(
        score_interval = 
          .x %>% 
          findInterval(
            interval_lb
          )
      ) %>% 
      filter(
        interval ==
          score_interval
      ) %>% 
      pull(
        text
      )
  ) -> list_text_eval
  
  # Output
  return(list_text_eval)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Test ------------------------------------------------------------------
# library(dplyr)
# library(purrr)
# 
# fun_text_dictionary(
#   df_dictionary_long =
#     tibble(
#       key = rep(c('dsds', 'lalala'), each = 2),
#       interval = rep(1:2, 2),
#       interval_lb = rep(1:2, 2),
#       text = as.character(rep(1:2, 2))
#     )
#   , dbl_score_eval = 2
#   , chr_dictionary_key = 'lalala'
# )
# 
