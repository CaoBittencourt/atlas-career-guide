# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
pkg <- c(
  'dplyr', 'tidyr', 'readr' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Data ------------------------------------------------------------------
# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/atlas-research/data/df_occupations_2023.csv'
) -> df_occupations

# Factor model
read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa/efa_equamax_14factors.rds'
) -> efa_model

# [DATA] ------------------------------------------------------------------
# - EFA data --------------------------------------------------------------
df_occupations %>%
  select(!(
    setdiff(
      df_occupations %>% 
        select(starts_with(
          'item'
        )) %>% 
        names()
      , efa_model$
        model %>% 
        colnames()
    ))
  ) -> df_occupations

# [EXPORT] ----------------------------------------------------------------
# - Set working directory -------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Write csv -------------------------------------------------------------------
write_csv(
  x = df_occupations
  , file = './df_occupations_2023_efa.csv'
)