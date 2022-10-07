# -------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'ggthemes', 'ggridges', 'gghighlight', 'scales' , 'viridis', 'ggalt'
  # , 'ComplexHeatmap'
  , 'circlize' #'paletteer' #Data visualization
  , 'devtools' #Dev Tools
  # , 'ggalt', 'hrbrthemes', 'extrafont' #Data visualization
  # , 'ggthemr' #Data visualization
  , 'tidyverse', 'rlang' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

install_github('jokergoo/ComplexHeatmap')
# install_github('jokergoo/circlize')

library(ComplexHeatmap)

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# # [TO DO] FONTS -------------------------------------------------------------------
# font_import(prompt = F)
# loadfonts(device = 'win')
# hrbrthemes::
# hrbrthemes::import_roboto_condensed()

# -------- DATA -----------------------------------------------------------
# OCCUPATIONS DATA FRAME -------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

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

# REORDER ROWS AND COLS TO LOOK LIKE ATLAS LOGO ---------------------------
# Cols
df_occupations %>%
  select(-occupation) %>% 
  colSums() %>% 
  sort(decreasing = T) %>% 
  names() -> chr_cols.sort

# # Rows
# df_occupations %>%
#   rowSums() %>%
#   as_tibble() %>%
#   mutate(index = row_number()) %>%
#   arrange(desc(value)) %>%
#   pull(index) -> int_rows.index

# Reorder
# df_occupations[int_rows.index, chr_cols.sort] -> df_occupations
# df_occupations[, chr_cols.sort] -> df_occupations
df_occupations[, c('occupation', chr_cols.sort)] %>% 
  # arrange(across(everything(), desc)) -> df_occupations
  # arrange(across(everything())) -> df_occupations
  arrange(across(
    head(
      chr_cols.sort
      , round(ncol(.) / 3)
    ))) -> df_occupations


# -------- PLOT ELEMENTS ---------------------------------------------------
# TRACK HEIGHT ------------------------------------------------------------
# int_track.size <- 0.125
# int_track.size <- 0.25
int_track.size <- 0.2

# LIST OF COLORS ------------------------------------------------------------------
colorRamp2(
  # c(0, 1)
  # c(0.3, 1)
  c(0.3, 0.7)
  # c(0, 0.7)
  , c(
    '#753AF9'
    , '#4AF7B0'
  )
) -> fun_colors

# -------- PLOTS -----------------------------------------------------------
# ATLAS LOGO HEATMAP --------------------------------------------------------------------
df_occupations %>%
  pivot_longer(cols = - occupation) %>% 
  fun_plot.heatmap(aes(
    x = occupation
    , y = name
    , fill = value
  )
  , .reorder_fct = F
  , .coord_polar = T
  )
