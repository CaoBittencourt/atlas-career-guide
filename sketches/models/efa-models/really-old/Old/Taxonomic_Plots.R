# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'scales' #Visualization
  , 'readr', 'readxl','openxlsx' #Read and write utilities
  , 'tidyverse', 'labelled' #Data wrangling
  # , 'ggtree' #Taxonomic plots
  , 'ggraph', 'igraph', 'collapsibleTree' #Taxonomic plots
)

# Activate / inst all packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models')

# DATA --------------------------------------------------------------------
df_tree <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=1576859482&single=true&output=csv')

# TAXONOMIC PLOTS ---------------------------------------------------------
# Everything
df_tree %>% 
  collapsibleTree(
    names(df_tree)
  )

# Skills

# Abilities

# Knowledge

