# PACKAGES ----------------------------------------------------------------
library(moments)
library(TruncatedNormal)

# DATA --------------------------------------------------------------------
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Select only necessary variables
df_occupations %>%
  select(
    # occupation,
    ends_with('.l')
  ) %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# Sample for testing
df_occupations[,sample.int(n = ncol(df_occupations), size = 30)] -> df_occupations

# # SAMPLE DENSITIES ---------------------------------------------------------------
# df_occupations$dynamic_flexibility.l %>% qplot(geom = 'density')
# df_occupations$biology.l %>% qplot(geom = 'density')
# df_occupations$basic.mathematics.l %>% qplot(geom = 'density')
# df_occupations$active_learning.l %>% qplot(geom = 'density')
# df_occupations$getting_information.l %>% qplot(geom = 'density')
# df_occupations$thinking_creatively.l %>% qplot(geom = 'density')
# df_occupations$organizing_planning_and_prioritizing_work.l %>% qplot(geom = 'density')
# df_occupations$updating_and_using_relevant_knowledge.l %>% qplot(geom = 'density')

# SKEWNESS AND VARIANCE ---------------------------------------------------
# Asymmetry
map(
  df_occupations %>%
    select(where(is.numeric))
  , skewness
) -> list_skew

# Variance
map(
  df_occupations %>%
    select(where(is.numeric))
  , var
) -> list_var

# LINEAR ---------------------------------------------------------
# 1 - (vr * sk)
Map(
  function(sk, vr){
    
    1 - (vr * sk)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value
  ))

# (1 - sk) * (1 - vr)
Map(
  function(sk, vr){
    
    (1 - sk) * (1 - vr)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value
  ))

# (-sk) * (1 - vr)
Map(
  function(sk, vr){
    
    (-sk) * (1 - vr)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value
  ))

# (sk / abs(sk)) * (0.5 - abs(sk)) * (1 - vr)
Map(
  function(sk, vr){
    
    (sk / abs(sk)) * (0.5 - abs(sk)) * (1 - vr)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value
  ))


# EXPONENTIAL -------------------------------------------------------------
# 1 - (vr ^ sk)
Map(
  function(sk, vr){
    
    1 - (vr ^ sk)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value
  ))

# vr ^ sk
Map(
  function(sk, vr){
    
    vr ^ sk
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value
  ))

# LINEAR ---------------------------------------------------------
# 1 - (vr * sk)
Map(
  function(sk, vr){
    
    1 - (vr * sk)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.density(aes(
    x = value
  ))

# (1 - sk) * (1 - vr)
Map(
  function(sk, vr){
    
    (1 - sk) * (1 - vr)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.density(aes(
    x = value
  ))

# (-sk) * (1 - vr)
Map(
  function(sk, vr){
    
    (-sk) * (1 - vr)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.density(aes(
    x = value
  ))

# (sk / abs(sk)) * (0.5 - abs(sk)) * (1 - vr)
Map(
  function(sk, vr){
    
    (sk / abs(sk)) * (0.5 - abs(sk)) * (1 - vr)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.density(aes(
    x = value
  ))

# EXPONENTIAL -------------------------------------------------------------
# 1 - (vr ^ sk)
Map(
  function(sk, vr){
    
    1 - (vr ^ sk)
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.density(aes(
    x = value
  ))

# vr ^ sk
Map(
  function(sk, vr){
    
    vr ^ sk
    
  }
  , sk = list_skew
  , vr = list_var
) %>% 
  flatten_df() %>% 
  pivot_longer(cols = everything()) %>% 
  fun_plot.density(aes(
    x = value
  ))
