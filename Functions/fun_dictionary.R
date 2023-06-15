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
        .dbl_score.eval %>% 
        findInterval(
          score.interval
          , interval.lb
        )
    ) %>% 
    filter(
      inteval ==
        score.interval
    ) %>% 
    pull(
      text
    ) -> chr_text
  
  # Output
  return(chr_text)
  
}
