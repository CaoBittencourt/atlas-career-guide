# [SETUP] --------------------------------------------------------------------
# - Packages --------------------------------------------------------------
# [FUNCTIONS] --------------------------------------------------------------------
# - Dictionary evaluation -------------------------------------------------
fun_dictionary <- function(
    
  # Dictionary data frame
  .df_dictionary.long
  # Score to be evaluated
  , .dbl_score.eval
  # Dictionary entry
  , .chr_dictionary.key
  
){
  
  # Arguments validation
  stopifnot(
    "'.df_dictionary.long' must be a data frame containing 'key', 'interval', 'interval.lb', and 'text' columns." =
      all(
        is.data.frame(
          .df_dictionary.long
        )
        , all(c(
          'key'
          , 'interval'
          , 'interval.lb'
          , 'text'
        ) %in%
          names(
            .df_dictionary.long
          )
        ))
  )
  
  stopifnot(
    "'.dbl_score.eval' must be numeric." =
      is.numeric(.dbl_score.eval)
  )
  
  stopifnot(
    "'.chr_dictionary.key' must be a character." =
      all(
        is.character(.chr_dictionary.key)
        , .chr_dictionary.key %in%
          .df_dictionary.long$key
      )
  )
  
  # Find interval
  .df_dictionary.long %>% 
    filter(
      key == .chr_dictionary.key
    ) %>% 
    mutate(
      score.interval = 
        findInterval(
          .dbl_score.eval
          , interval.lb
        )
    ) %>% 
    filter(
      interval ==
        score.interval
    ) %>% 
    pull(
      text
    ) -> chr_text
  
  # Output
  return(chr_text)
  
}

# - Vectorized dictionary evaluation -------------------------------------------------
fun_dictionary.list <- function(
    
  # Dictionary data frame
  .df_dictionary.long
  # Named list of scores to be evaluated
  , .list_dbl_score.eval
  
){
  
  # Arguments validation
  stopifnot(
    "'.df_dictionary.long' must be a data frame containing 'key', 'interval', 'interval.lb', and 'text' columns." =
      all(
        is.data.frame(
          .df_dictionary.long
        )
        , all(c(
          'key'
          , 'interval'
          , 'interval.lb'
          , 'text'
        ) %in%
          names(
            .df_dictionary.long
          )
        ))
  )
  
  stopifnot(
    "'.list_dbl_score.eval' must be a named list of numeric elements." = 
      all(
        is.list(.list_dbl_score.eval)
        , !is.null(names(
          .list_dbl_score.eval
        ))
        , all(names(
          .list_dbl_score.eval %in% 
            .df_dictionary.long$key
        ))
        , map_lgl(
          .list_dbl_score.eval
          , is.numeric
        ) %>% 
          all()
      )
  )
  
  # Find interval
  map2(
    .x = .list_dbl_score.eval
    , .y = names(.list_dbl_score.eval)
    , ~ 
      .df_dictionary.long %>% 
      filter(key == .y) %>% 
      mutate(
        score.interval = 
          .x %>% 
          findInterval(
            interval.lb
          )
      ) %>% 
      filter(
        interval ==
          score.interval
      ) %>% 
      pull(
        text
      )
  ) -> list_text.eval
  
  # Output
  return(list_text.eval)
  
}
