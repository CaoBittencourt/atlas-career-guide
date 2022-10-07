# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'moments' #Skewness
  , 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# PLOTTING FUNCTIONS ------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Plotting Functions/Auto_plots.R')

# DATA --------------------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# # Employed workers data frame
# df_workers <- readr::read_csv('C:/Users/Cao/Documents/Github/Atlas-Research/Atlas_database.csv')
# 
# Select only necessary variables
df_occupations %>%
  select(
    occupation
    , ends_with('.l')
  ) %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){x/100}
    )
  ) -> df_occupations
# 
# df_workers %>% view()
#   select(
#     title
#     , quick_facts.qf_number_of_jobs.value
#   ) %>% 
#   rename(
#     occupation = title
#     , workers = quick_facts.qf_number_of_jobs.value
#   ) -> df_workers
#   
#   
# df_workers %>% 
#   filter(str_detect(tolower(title), 'agricult'))
# 
# df_workers %>% 
#   group_by(occupation) %>% 
#   tally() %>% 
#   arrange(desc(n)) -> dsds
# 
# df_occupations %>% 
#   group_by(occupation) %>% 
#   tally() -> dsds
# 
# all(dsds$n == 1)

# # POPULATION-WEIGHTED DATA FRAME ------------------------------------------
# df_occupations %>% 
#   left_join(df_workers)
# 
#   mutate(workers = work.force / pmin(workers, na.rm = T)) %>% 
#   group_by(occupation) %>% 
#   slice(1:workers)

# -------- INTRODUCTION TO CAPITAL FLEXIBILITY ------------------------------
# QUIZ: WHICH ARE THE MOST FLEXIBLE ATTRIBUTES? --------------------------
# Take 8 random attributes
df_occupations[,sample.int(ncol(df_occupations), 8)] -> df_sample

# Plot densities
df_sample %>% 
  pivot_longer(
    cols = everything()
  ) %>% 
  fun_plot.density(aes(
    x = value
  )
  , .sym_facets = name
  , .int_facets = 2
  , .reorder_fct = T
  , .reorder_fun = median
  , .dbl_limits.x = c(0,1)
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.y = element_blank())
  , .list_labs = list(
    title = str_to_title('guessing game: which of these attributes is the most widely applicable?')
    , x = str_to_title('attribute level')
    , y = str_to_title('density')
  ))

# -------- EXPLORATORY ANALYSIS -------------------------------------------
# SKEWNESS AND VARIANCE ---------------------------------------------------
# Asymmetry
map(
  df_occupations %>%
    select(where(is.numeric))
  , skewness
) %>% flatten_df() %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'skewness'
  ) -> df_skew

# Variance
map(
  df_occupations %>%
    select(where(is.numeric))
  , var
) %>% flatten_df() %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'variance'
  ) -> df_var

# Skewness and variance data frame
full_join(
  df_var
  , df_skew
) -> df_skew_var

# VARIANCE PLOTS -------------------------------------------------------------------
# Density
df_skew_var %>% 
  fun_plot.density(aes(
    x = variance
  )
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.y = element_blank())
  , .list_labs = list(
    title = str_to_title('variance distribution of all attributes')
    , x = str_to_title('variance')
    , y = str_to_title('density')
  ))

# Line chart
df_skew_var %>% 
  arrange(variance) %>% 
  mutate(n = row_number()) %>% 
  fun_plot.line(aes(
    x = n
    , y = variance
  )
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.x = element_blank())
  , .list_labs = list(
    title = str_to_title('variance distribution of all attributes')
    , x = str_to_title('attribute')
    , y = str_to_title('variance')
  ))

# Line chart
df_skew_var %>% 
  arrange(variance) %>% 
  mutate(n = row_number()) %>% 
  fun_plot.line(aes(
    x = n
    , y = variance
    , group = 1
  )
  , .dbl_limits.y = c(0,1)
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.x = element_blank())
  , .list_labs = list(
    title = str_to_title('variance distribution of all attributes')
    , x = str_to_title('attribute')
    , y = str_to_title('variance')
  ))

# SKEWNESS PLOTS -------------------------------------------------------------------
# Density
df_skew_var %>%
  fun_plot.density(aes(
    x = skewness
  )
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.y = element_blank())
  , .list_labs = list(
    title = str_to_title('skewness distribution of all attributes')
    , x = str_to_title('skewness')
    , y = str_to_title('density')
  )) + 
  geom_vline(
    xintercept = 1
    , linetype = 'solid'
    , size = 1.2
    , color = '#E53124'
  ) + 
  geom_vline(
    xintercept = c(0.5,-0.5)
    , linetype = 'dashed'
    , color = '#212121'
  ) + 
  geom_vline(
    xintercept = -1
    , linetype = 'solid'
    , size = 1.2
    , color = '#09D781'
  )

# Linechart
df_skew_var %>% 
  arrange(desc(skewness)) %>% 
  mutate(n = row_number()) %>% 
  fun_plot.line(aes(
    x = n
    , y = skewness
    , group = 1
  )
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.x = element_blank())
  , .list_labs = list(
    title = str_to_title('skewness distribution of all attributes')
    , x = str_to_title('attribute')
    , y = str_to_title('skewness')
  )) + 
  geom_hline(
    yintercept = 1
    , linetype = 'solid'
    , size = 1.2
    , color = '#E53124'
  ) + 
  geom_hline(
    yintercept = c(0.5,-0.5)
    , linetype = 'dashed'
    , color = '#212121'
  ) + 
  geom_hline(
    yintercept = -1
    , linetype = 'solid'
    , size = 1.2
    , color = '#09D781'
  )

# VARIANCE VS SKEWNESS PLOT ---------------------
df_skew_var %>% 
  pivot_longer(
    cols = -item
  ) %>% 
  fun_plot.density(aes(
    x = value
  )
  , .sym_facets = name
  , .int_facets = 2
  , .reorder_fct = T
  , .reorder_fun = median
  # , .dbl_limits.x = c(0,1)
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.y = element_blank())
  , .list_labs = list(
    title = str_to_title('variance and skewness distribution')
    , x = str_to_title('measure')
    , y = str_to_title('density')
  ))

# -------- CAPITAL FLEXIBLITY ---------------------------------------------
# TEST FUNCTIONS --------------------------------------------------------------------
tibble(
  vr = seq(0,1,.0005)
  , sk = seq(-2,2,.002)
) -> df_random

df_random %>% 
  mutate(
    # kflex = -sk * (1 - vr)
    # kflex = exp(abs(sk))*(1-vr)
    # kflex = exp(-sk) * (1 - vr)
    # kflex = ((1 - sk)^2) * (1 - vr)
    # kflex = ((1 - sk)^2) ^ (1 - vr)
    # kflex = ((1 - sk)^2) ^ vr
    # kflex = (- sk) ^ (1 - vr)
    # kflex = ((- sk) / abs(sk)) * ((sk ^ 2)^(1 - vr))
    # kflex = (1 - sk) ^ (1 - vr)
    # kflex = exp((- sk) * (1 - vr))
    # kflex = exp((- sk) * (vr^2))
    # kflex = - (1 / sk)
  ) %>% 
  ggplot(aes(
    x = sk
    , y = vr
  )) +
  geom_line(size = 1.5) + 
  geom_line(aes(
    x = sk
    , y = kflex
  )
  , color = 'red'
  , size = 1.5
  ) +
  scale_x_reverse() 

# DEFINE FUNCTIONS ---------------------------------------------------------------
fun_capital.flex <- function(sk, vr){
  
  list(
    'capital.flex1' = - sk * (1 - vr)
    # , 'capital.flex2' = exp(abs(sk)) * (1 - vr)
    , 'capital.flex2' = - (sk / abs(sk)) * (1 - abs(sk)) * (1 - vr)
    , 'capital.flex3' = - (sk / abs(sk)) * (0.5 - abs(sk)) * (1 - vr)
    , 'capital.flex4' = - (sk / 0.5) * (1 - vr)
    # , 'exp1' = 1 - (vr ^ sk) #too high and too low capital flexibility scores
    # , 'exp2' = vr ^ sk #too high and too low capital flexibility scores
  ) %>% 
    bind_cols() %>%
    return()
  
}

# APPLY FUNCTIONS -----------------------------------------------------------
df_skew_var %>%
  mutate(
    fun_capital.flex(sk = skewness, vr = variance)
  ) -> df_skew_var

df_skew_var %>% 
  pivot_longer(
    cols = -c(item, variance, skewness)
    , names_to = 'funct'
    , values_to = 'capital.flex'
  ) %>% 
  arrange(funct) -> df_skew_var.long

# -------- RESULTS --------------------------------------------------------
# LINE CHARTS -------------------------------------------------------------------
df_skew_var.long %>%
  group_by(funct) %>% 
  arrange(capital.flex) %>% 
  mutate(n = row_number()) %>% 
  ungroup() %>% 
  fun_plot.line(aes(
    x = n
    , y = capital.flex
  )
  , .sym_facets = funct
  , .reorder_fct = T
  , .reorder_desc = F
  , .reorder_fun = max
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.x = element_blank())
  , .list_labs = list(
    title = str_to_title('comparison of capital flexibility formulae')
    , x = str_to_title('attribute')
    , y = str_to_title('capital flexiblity score')
  ))

# DENSITIES -------------------------------------------------------------------
df_skew_var.long %>% 
  fun_plot.density(aes(
    x = capital.flex
  )
  , .sym_facets = funct
  , .reorder_fct = F
  , .reorder_desc = F
  , .theme = ggridges::theme_ridges(center_axis_labels = T)
  , .list_labs = list(
    title = str_to_title('comparison of capital flexibility formulae')
    , x = str_to_title('capital flexiblity score')
    , y = str_to_title('density')
  ))

# DESCRIPTIVE STATISTICS FOR CAPITAL FLEXIBLITY ---------------------
df_skew_var %>% 
  select(-c(item, variance, skewness)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(across(
    .fns = list(sd = sd, mean = mean)
  ))
