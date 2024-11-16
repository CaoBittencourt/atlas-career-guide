fun_qualification <- function(.df_data, .dbl_scale.ub){
  
  .df_data %>% 
    select(where(
      is.numeric
    )) -> df_data.numeric
  
  rowSums(df_data.numeric) /
    (
      ncol(df_data.numeric) * 
        .dbl_scale.ub
    ) -> dbl_qualification
  
  .df_data %>% 
    mutate(
      qualification = 
        dbl_qualification
    ) %>% 
    return()
  
}

fun_qualification(
  df_occupations.ai
  , 100
  ) %>% 
  select(
    occupation
    , qualification
  ) %>% 
  arrange(desc(
    qualification
  )) %>% 
  mutate(
    similarity = 
      runif(n())
    , coef = 
      (1 / qualification) %>% 
      round(2)
  )

seq(0,1,length.out = 5) ^ (1 / 0.536)
0.7 ^ 0.49
