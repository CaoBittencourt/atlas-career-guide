# -------- SETUP ----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'moments' #Skewness
  , 'patchwork' #Data visualization
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
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Select only necessary variables
df_occupations %>%
  select(
    ends_with('.l')
  ) %>%
  mutate(
    across(
      .cols = ends_with('.l')
      , .fns = function(x){x/100}
    )
  ) -> df_occupations

# POPULATION-WEIGHTED DATA FRAME ------------------------------------------
df_occupations %>% 
  mutate(work.force = work.force / pmin(work.force, na.rm = T)) %>% 
  group_by(occupation) %>% 
  slice(1:work.force)

# -------- INTRODUCTION TO CAPITAL FLEXIBILITY ------------------------------
# QUIZ: WHICH ARE THE MOST FLEXIBLE ATTRIBUTES? --------------------------
# Take 5 random attributes
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
  , .reorder_fun = mode
  , .dbl_limits.x = c(0,1)
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.y = element_blank())
  , .list_labs = list(
    title = str_to_title('guessing game: which of these attributes is the most applicable?')
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
  fun_plot.line(aes(
    x = item
    , y = variance
    , group = 1
  )
  , .reorder_fct = T
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
  fun_plot.line(aes(
    x = item
    , y = skewness
    , group = 1
  )
  , .reorder_fct = T
  , .reorder_desc = T
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

# DESCRIPTIVE STATISTICS FOR VARIANCE AND SKEWNESS ---------------------
df_skew_var %>% 
  select(variance, skewness) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(across(
    .fns = list(sd = sd, mean = mean)
  )) 

# -------- CAPITAL FLEXIBLITY ---------------------------------------------
# FUNCTIONS ---------------------------------------------------------------
fun_capital.flex <- function(sk,vr){
  
  list(
    'linear1' = 1 - (vr * sk)
    , 'linear2' = (1 - sk) * (1 - vr)
    , 'linear3' = (-sk) * (1 - vr)
    , 'linear4' = (sk / abs(sk)) * (0.5 - abs(sk)) * (1 - vr)
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
  select(-c(variance, skewness)) %>% 
  pivot_longer(
    cols = -item
    , names_to = 'funct'
    , values_to = 'capital.flex'
  ) -> df_skew_var.long

# -------- RESULTS --------------------------------------------------------
# LINE CHARTS -------------------------------------------------------------------
map(
  unique(df_skew_var.long$funct)
  , function(chr_funct){
    
    df_skew_var.long %>% 
      filter(funct == chr_funct) %>% 
      fun_plot.line(aes(
        x = item
        , y = capital.flex
        , group = 1
      )
      , .sym_facets = funct
      , .reorder_fct = T
      , .reorder_desc = F
      , .reorder_fun = max
      , .theme = ggridges::theme_ridges(center_axis_labels = T) +
        theme(axis.text.x = element_blank())
      )
  }
)  %>% 
  wrap_plots()


# DENSITIES -------------------------------------------------------------------
df_skew_var.long %>% 
  fun_plot.density(aes(
    x = capital.flex
  )
  , .sym_facets = funct
  , .reorder_fct = F
  , .reorder_desc = F
  , .theme = ggridges::theme_ridges(center_axis_labels = T)
  )

map(
  unique(df_skew_var.long$funct)
  , function(chr_funct){
    
    df_skew_var.long %>% 
      filter(funct == chr_funct) %>% 
      fun_plot.density(aes(
        x = capital.flex
      )
      , .sym_facets = funct
      , .reorder_fct = F
      , .reorder_desc = F
      , .theme = ggridges::theme_ridges(center_axis_labels = T)
      )
  }
)  %>% 
  wrap_plots()


# DESCRIPTIVE STATISTICS FOR CAPITAL FLEXIBLITY ---------------------
df_skew_var %>% 
  select(-c(item, variance, skewness)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(across(
    .fns = list(sd = sd, mean = mean)
  )) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value_sd
    , label = number(value_sd)
  ))

df_skew_var %>% 
  select(-c(item, variance, skewness)) %>% 
  pivot_longer(cols = everything()) %>% 
  group_by(name) %>% 
  summarise(across(
    .fns = list(sd = sd, mean = mean)
  )) %>% 
  fun_plot.lollipop(aes(
    x = name
    , y = value_mean
    , label = number(value_sd)
  ))
