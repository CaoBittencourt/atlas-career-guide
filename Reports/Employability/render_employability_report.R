# [SETUP] -----------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'tinytex' #LaTeX
  , 'knitr' #Knitr
  , 'openxlsx' #Import/Export excel
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Install TinyTex
if(!tinytex::is_tinytex()){
  tinytex::install_tinytex()
}

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Functions ---------------------------------------------------------------
# KNN matching
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/KNN_Matching.R')
# Employability
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_employability.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# Regressions
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_regressions.R')
# # Factor scores
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')

# - Data --------------------------------------------------------------------
# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas.complete_equamax_15_factors.csv'
) -> df_occupations

# User input data frame
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
) -> df_input

# - Parameters --------------------------------------------------------------
# Colors
list(
  'green' = '#4AF7B0'
  , 'purple1' = '#753AF9'
  , 'purple2' = '#301866'
  , 'purple3' = '#3854FB'
  , 'blue1' = '#56D0F5'
  , 'blue2' = '#ABF4D4'
  , 'blue3' = '#43DED1'
  , 'blue4' = '#182766'
  , 'red' = '#CE3527'
  , 'black' = '#212121'
  , 'grey' = '#D4D5D8'
) -> list_atlas.pal

# Employment levels
df_occupations$
  employment2 -> int_employment

# Market wages
df_occupations$
  annual_wage_2021 -> dbl_wages

# User wage
1000 * 5 * 12 -> dbl_wage.user

# Scale upper bound
.dbl_scale.ub <- 100

# [RESULTS] --------------------------------------------------------
# - Estimate occupations's employability ---------------------------------------------------------------
fun_employability.workflow.m(
  .df_data = 
    df_occupations
  , .int_employment = 
    int_employment
  , .dbl_wages = 
    dbl_wages
  , .dbl_scale.ub = 
    .dbl_scale.ub
) -> list_employability

list_employability$
  employability -> df_employability

# - Estimate user's employability -----------------------------------------------------------
fun_employability.workflow(
  .df_data = 
    df_occupations
  , .df_query = 
    df_input
  , .int_employment = 
    int_employment
  , .dbl_wages.market = 
    dbl_wages
  , .dbl_wage.current = 
    dbl_wage.user
  , .dbl_scale.ub = 
    .dbl_scale.ub
) -> list_employability.user

# - Estimate attributes's employability -----------------------------------------------------
# Run weighted NNLS on occupation's employability coefficients
fun_lm(
  .df_data = 
    df_employability
  , .sym_vars.dependent = 
    'employability'
  , .sym_vars.independent = 
    df_employability %>%
    select(ends_with('.l')) %>% 
    names()
  , .dbl_weights = 
    int_employment
  , .lgc_intercept = F
  , .dbl_lower.bounds = 0
) -> list_employability.nnls

list_employability.nnls$
  model.tidy

df_input %>% 
  pivot_longer(
    cols = ends_with('.l')
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  left_join(
    list_employability.nnls$
      model.tidy %>% 
      rename(
        item = term
        , item.employability = estimate
      )
  ) %>%
  reframe(
    employability = sum(
      item.employability * 
        item.score
    )
  )

# [TABLES] ---------------------------------------------------------
# - N most employable occupations ----------------------------------------------------------
df_employability %>% 
  arrange(-employability) %>% 
  select(!ends_with('.l')) %>% 
  slice_head(n = 15) %>% 
  mutate(across(
    .cols = where(is.numeric)
    ,.fns = ~ percent(.x, accuracy = .01)
  )) %>% 
  rename(
    Occupation = occupation 
    , Employability = employability
    , `Optimal Employability` = employability.optimal
  ) -> df_employability.top10

# - N least employable occupations ----------------------------------------------------------
df_employability %>% 
  arrange(-employability) %>% 
  select(!ends_with('.l')) %>% 
  slice_tail(n = 15) %>% 
  mutate(across(
    .cols = where(is.numeric)
    ,.fns = ~ percent(.x, accuracy = .01)
  )) %>% 
  rename(
    Occupation = occupation 
    , Employability = employability
    , `Optimal Employability` = employability.optimal
  ) -> df_employability.bot10

# - User's N most interchangeable occupations -----------------------------
list_employability.user$
  interchangeability %>% 
  select(
    -ends_with('.l')
    , -distance
  ) %>% 
  mutate(
    employment = 
      int_employment
  ) %>% 
  arrange(-interchangeability) %>% 
  slice_head(n = 15) %>% 
  mutate(across(
    .cols = c(
      similarity
      , interchangeability
    )
    ,.fns = ~ percent(.x, accuracy = .01)
  )
  , employment = 
    dollar(
      employment
      , prefix = ''
      , accuracy = 1
    )
  ) %>% 
  rename(
    `job posts` = employment
  ) %>%
  rename_with(
    .cols = everything()
    ,.fn = str_to_title
  ) -> df_user.top10

# - User's N least interchangeable occupations -----------------------------
list_employability.user$
  interchangeability %>% 
  select(
    -ends_with('.l')
    , -distance
  ) %>% 
  mutate(
    employment = 
      int_employment
  ) %>% 
  arrange(-interchangeability) %>% 
  slice_tail(n = 15) %>% 
  mutate(across(
    .cols = c(
      similarity
      , interchangeability
    )
    ,.fns = ~ percent(.x, accuracy = .01)
  )
  , employment = 
    dollar(
      employment
      , prefix = ''
      , accuracy = 1
    )
  ) %>% 
  rename(
    `job posts` = employment
  ) %>%
  rename_with(
    .cols = everything()
    ,.fn = str_to_title
  ) -> df_user.bot10

# [PLOTS] ----------------------------------------------------------
# - Circular bar chart of occupations's employability -----------------------------------------------------
df_employability %>%
  select(
    !ends_with('.l')
  ) %>%
  mutate(
    employment = 
      int_employment / 
      min(int_employment)
    , employability.desc = 
      findInterval(
        employability
        , seq(0, 1, length.out = 5)
      ) %>% 
      recode(
        '1' = 'Very Low Employability'
        , '2' = 'Low Employability'
        , '3' = 'Medium Employability'
        , '4' = 'High Employability'
        , '5' = 'Very High Employability'
      ) %>% 
      factor(
        levels = 
          c(
            'Very Low Employability'
            , 'Low Employability'
            , 'Medium Employability'
            , 'High Employability'
            , 'Very High Employability'
          )
      )
  ) %>%
  fun_plot.bar(aes(
    x = occupation
    , y = employability
    , fill = employability.desc
  )
  , .chr_manual.pal =
    set_names(
      viridis::viridis(5)
      , c(
        'Very Low Employability'
        , 'Low Employability'
        , 'Medium Employability'
        , 'High Employability'
        , 'Very High Employability'
      )
    )
  , .list_labs = 
    list(
      fill = NULL
      , y = 'Employability Coefficient'
    )
  , .coord_polar = T
  , .list_axis.y.args = 
    list(
      breaks = seq(0, 1, length.out = 5)
    )
  , .fun_format.y = percent
  , .theme = 
    theme_void() + 
    theme(
      legend.position = 'right'
      , legend.direction = 'vertical'
      , panel.grid = element_blank()
      , panel.border = element_blank()
      , plot.margin = margin(0, 0, 0, 0)
      , plot.title = element_blank()
      , plot.subtitle = element_blank()
      , axis.title = element_blank()
      , axis.text = element_blank()
      , axis.ticks = element_blank()
      , axis.line = element_blank()
    ) 
  ) -> plt_employability.polar

# - Distribution of occupations's employability (don't forget to weigh) --------
df_employability %>%
  select(
    !ends_with('.l')
  ) %>%
  mutate(
    employment = 
      int_employment / 
      min(int_employment)
    , employability.desc = 
      findInterval(
        employability
        , seq(0, 1, length.out = 5)
      ) %>% 
      recode(
        '1' = 'Very Low Employability'
        , '2' = 'Low Employability'
        , '3' = 'Medium Employability'
        , '4' = 'High Employability'
        , '5' = 'Very High Employability'
      ) %>% 
      factor(
        levels = 
          rev(c(
            'Very Low Employability'
            , 'Low Employability'
            , 'Medium Employability'
            , 'High Employability'
            , 'Very High Employability'
          ))
      )
  ) %>% 
  group_by(
    occupation
  ) %>% 
  slice(rep(
    1, employment
  )) %>% 
  ungroup() %>%
  mutate(
    unique_id = row_number()
  ) %>% 
  fun_plot.histogram(aes(
    x = employability
    , fill = employability.desc
  )
  , .chr_manual.pal = 
    set_names(
      viridis::viridis(5)
      , c(
        'Very Low Employability'
        , 'Low Employability'
        , 'Medium Employability'
        , 'High Employability'
        , 'Very High Employability'
      )
    )
  , .list_labs = 
    list(
      fill = NULL
      , x = 'Employability Coefficient'
      , y = 'Frequency'
    )
  , .list_axis.x.args = 
    list(
      breaks = seq(0, 1, length.out = 5)
      , limits = c(-.1, 1.1)
    )
  , .fun_format.x = percent
  , .theme = 
    ggridges::theme_ridges(
      center_axis_labels = T
      , font_size = 12
    ) + 
    theme(
      # legend.position = c(0,0)
      legend.position = 'bottom'
      , legend.direction = 'horizontal'
      # , plot.margin = margin(0, 0, 0, 0)
    ) 
  ) -> plt_employability.hist

# - Distribution of user's interchangeability and similarity (don't forget to weigh) --------
list_employability.user$
  interchangeability %>%
  select(
    !ends_with('.l')
  ) %>%
  mutate(
    employment = 
      int_employment / 
      min(int_employment)
  ) %>% 
  group_by(
    occupation
  ) %>% 
  slice(rep(
    1, employment
  )) %>% 
  ungroup() %>%
  rename_with(
    .cols = c(
      interchangeability
      , similarity
    )
    , .fn = str_to_title
  ) %>% 
  pivot_longer(
    cols = c(
      Interchangeability
      , Similarity
    )
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  fun_plot.histogram(aes(
    x = value
    , fill = metric
  )
  , .list_labs = 
    list(
      fill = NULL
      , x = 'Interchangeability Coefficient'
      , y = 'Frequency'
    )
  , .list_axis.x.args = 
    list(
      breaks = seq(0, 1, length.out = 5)
      , limits = c(-.1, 1.1)
    )
  , .fun_format.x = percent
  , .theme = 
    ggridges::theme_ridges(
      center_axis_labels = T
    ) + 
    theme(
      legend.position = 'bottom'
      , legend.direction = 'horizontal'
    ) 
  ) -> plt_interchangeability.hist

# - Lollipop / Circular bar chart of items's marginal employability -------
list_employability.nnls$
  model.tidy %>% 
  mutate(
    estimate = 
      estimate * 100
    , estimate.desc = 
      cut(
        estimate
        , breaks = 5
        , labels = F) %>% 
      recode(
        '1' = 'Very Low Employability'
        , '2' = 'Low Employability'
        , '3' = 'Medium Employability'
        , '4' = 'High Employability'
        , '5' = 'Very High Employability'
      ) %>% 
      factor(
        levels = 
          c(
            'Very Low Employability'
            , 'Low Employability'
            , 'Medium Employability'
            , 'High Employability'
            , 'Very High Employability'
          )
      )
  ) %>%
  filter(
    estimate > 0
  ) %>% 
  fun_plot.lollipop(aes(
    x = term
    , y = estimate
    , color = estimate.desc
  )
  , .chr_manual.pal = 
    set_names(
      viridis::viridis(5)
      , c(
        'Very Low Employability'
        , 'Low Employability'
        , 'Medium Employability'
        , 'High Employability'
        , 'Very High Employability'
      )
    )
  , .list_labs = 
    list(
      y = 'Marginal Employability (0-100 scale)'
      , x = NULL
      , color = NULL
    )
  , .theme = 
    ggridges::theme_ridges(
      center_axis_labels = T
    ) + 
    theme(
      legend.position = 'bottom'
      , legend.direction = 'vertical'
      # , plot.margin = margin(0, 0, 0, 0)
    ) 
  ) -> plt_employability.lollipop

# [RENDER] -----------------------------------------------------------
# - Render R Markdown report --------------------------------------------------
rmarkdown::render(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Employability/employability_coefficient_notes3.Rmd'
)