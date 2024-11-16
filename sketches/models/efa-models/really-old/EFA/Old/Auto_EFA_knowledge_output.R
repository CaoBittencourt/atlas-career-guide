# PACKAGES -----------------------------------------------------------------
pkg <- c(
  # 'paletteer', 'ggthemes', 'scales' #Visualization
  'readr', 'readxl','openxlsx' #Read and write utilities
  , 'tidyverse', 'labelled' #Data wrangling
  # , 'caret' #Variance filter
  , 'psych' #Factor analysis
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models')

# AUTO-EFA FUNCTIONS ----------------------------------------------------------------
source('./Auto_EFA.R')

# DATA --------------------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Labels character vector
chr_labels <- scan(
  url('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=1223197022&single=true&output=csv')
  , sep=','
  , what = ''
  , quiet = T
)

# Exploratory analyses
df_occupations %>% glimpse()
df_occupations %>% class()
df_occupations %>% head()

chr_labels %>% glimpse()
chr_labels %>% class()
chr_labels %>% head()

ncol(df_occupations) == length(chr_labels) 

# Apply labels
df_occupations %>%
  labelled::set_variable_labels(
    .labels = chr_labels
  ) -> df_occupations

# Only numeric variables
df_occupations %>%
  select(
    where(function(x){str_detect(attributes(x)$label, 'Knowledge.')}) #Knowledge only
    , -ends_with('.I') #Using recommended levels
    # , -ends_with('.L') #Using importance levels
  ) %>% 
  mutate(#0 to 100 => 0 to 1 (helps calculate similarity later on)
    across(
      .fns = function(x){x/100}
    )
  ) -> df_occupations.numeric.know


# EFA 1: 4 factors ---------------------------------------------------------------------
n.facts <- 4

# AUTO-EFA
df_occupations.numeric.know %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
    ) -> EFA_know.4

# TOP N ITEMS: EFA4 Model ---------------------------------------------------------------------
# TOP 3 ITEMS
top.n <- 4

EFA_know.4$loadings.long %>%
  fun_top.items(n.items = top.n) -> df_know.items

# EFA 2: RERUN EFA WITH TOP ITEMS ONLY ------------------------------------
n.facts <- 4

df_occupations.numeric.know %>% 
  select(df_know.items$Item) -> df_occupations.numeric.know.items

# AUTO-EFA
df_occupations.numeric.know.items %>%
  fun_factor.analysis(
    # Basic
    int_nfactors = n.facts
    , chr_rotation = 'promax'
    # Underloadings and crossloadings
    , remove_under_loading.items = T
    , remove_cross_loading.items = T
    , dbl_under_loading.threshold = 0.4 #Lesser than 0.4 loading = under loading
    , dbl_cross_loading.threshold = 0.05 #Lesser than 0.05 loading difference = cross loading
    # Diagrams and tests
    , show_diagrams = T
    , show_results = F
  ) -> EFA_know.4.items
