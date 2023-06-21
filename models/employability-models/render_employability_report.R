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
source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_knn.R')
# Employability
source('C:/Users/Cao/Documents/Github/atlas-research/functions/metrics/fun_employability.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/functions/methods/fun_plots.R')
# Regressions
source('C:/Users/Cao/Documents/Github/Atlas-Research/functions/methods/fun_regressions.R')
# # Factor scores
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')

# - Data --------------------------------------------------------------------
# Occupations data frame
read_csv(
  'C:/Users/Cao/Documents/Github/atlas-research/data/df_atlas.complete_equamax_15_factors.csv'
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
# - Estimate similarity ---------------------------------------------------
df_occupations %>% 
  slice_sample(n = 1) %>% 
  select(
    occupation
    , ends_with('.l')
  ) -> df_sample

df_input -> df_sample

df_occupations %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    ), 'hosp'
  )
  | occupation == 
    df_sample$
    occupation
  ) %>% 
  select(
    ends_with('.l')
  ) -> lalala

alpha = a / sum(a)
alpha = alpha / average(a)

alpha = a / average(a)
alpha = alpha / sum(a)

tibble(
  a = c(0.5,0.5)
  , d = c(50,0)
) %>% 
  reframe(
    d = sum(d * a)
    # , s = 1 - d / 200
    , s = 200 * prod(exp(-a*d))
  )

tibble(
  # a = c(0.00941466,0.005740528)
  a = c(0.5,0.5)
  , d = c(83, 50)
) %>%
  mutate(
    s = exp(-a*d)
  ) %>% 
  reframe(
    s = round(prod(s), 4)
  )

as_tibble(lalala / rowSums(lalala)) %>% view


plot(function(x){100 ^ ((x - 1)/x)})
plot(function(x){(100 ^ (1 - 1/x))/100})


df_occupations %>% 
  slice_sample(n=1) -> 
  df_sample

df_input[-1] -> a_u

df_sample %>% 
  select(
    ends_with('.l')
  ) -> a_u

t(a_u)

df_occupations %>% 
  select(names(a_u)) %>% 
  t() -> mtx_A

df_occupations$
  occupation -> colnames(mtx_A)

as_tibble(mtx_A) %>% 
  mutate(
    .before = 1
    , user = 
      a_u %>% 
      t() %>%
      as.numeric()
  ) -> lalala

make.names(names(
  lalala
)) -> names(lalala)


lm(
  formula =
    user ~ 0 + .
  , data =
    lalala[c(1,2)]
)

map_df(
  .x = lalala[-1]
  , ~ 
    bvls::bvls(
      as.matrix(.x)
      , lalala$user
      , bl = 0
      , bu = 1
    )[[1]]
) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'occupation'
    , values_to = 'similarity.bvls'
  ) -> dsds

# map_df(
#   .x = 2:874
#   , ~
#     nls(
#       paste0(
#         'user ~'
#         , 'b*'
#         , names(lalala)[.x]
#       ) %>% as.formula()
#       , data = lalala[c(1,.x)]
#       , algorithm = 'port'
#       , lower = 0
#       , upper = 1
#     ) %>% coefficients()
# ) %>%
#   pivot_longer(
#     cols = everything()
#     , names_to = 'occupation'
#     , values_to = 'similarity.bvls'
#   ) -> dsds
# 
# dsds$occupation <- df_occupations$occupation

map_df(
  .x = lalala[-1]
  , ~ 
    (weights::wtd.cors(
      lalala[1]
      , .x
      , weight =
        .x
    ) + 1) / 2
) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'occupation'
    , values_to = 'similarity.bvls'
  ) -> dsds

dsds %>% 
  arrange(desc(
    similarity.bvls
  ))

lm(
  formula =  V1 ~ 0 + .
  , data =
    as_tibble(diag(20))
) %>%
  coefficients()

c(
  rep(50,15)
  , runif(5, min = 50, max = 100)
) -> dsds

sample(dsds, size = length(dsds)) -> lalala

rep(25,20) > dsds
fun_knn.alpha(
  .df_data = 
    as_tibble(t(dsds))
  , .df_query = 
    as_tibble(
      rbind(
        t(rep(25,20))
        , t(rep(100,20))
      ))
) %>% 
  select(similarity)

lm(
  formula =  V1 ~ 0 + .
  , data =
    as_tibble(cbind(
      rep(25,20)
      , dsds
    ))
  , weights = dsds
) %>%
  coefficients()

# no
# lm(
#   formula =  V1 ~ 0 + .
#   , data =
#     as_tibble(cbind(
#       rep(25,20) / (25 * 20)
#       , dsds / sum(dsds)
#     ))
#   , weights = dsds
# ) %>%
#   coefficients()

dsds %>%
  mutate(
    similarity.pearson = 
      as.numeric(
        weights::wtd.cors(
          x = lalala[1]
          , y = lalala[-1]
        ))
    , similarity.pearson = 
      (1 + similarity.pearson) / 2
  ) %>% 
  arrange(desc(
    similarity.pearson
  )) -> dsds

dsds %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    ), 'hosp|physician'
  ))

dsds %>%
  slice(-1) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'name'
    , values_to = 'value'
  ) %>% 
  fun_plot.density(aes(
    x = value
    , fill = name
  )
  , .list_axis.x.args = 
    list(
      limits = c(0,1)
      , breaks = seq(0,1,length.out = 7)
    )
  )


(cor(
  lalala[1]
  , lalala[2]
) + 1) / 2 

dsds %>% 
  arrange(desc(
    similarity.bvls
  )) %>% 
  filter(
    similarity.bvls == 1
  )

bvls::bvls(
  as.matrix(lalala[777])
  , lalala$user
  , bl = 0
  , bu = 1
) -> dsds



nls(
  formula = 
    as.formula(
      paste(
        names(dsds)[1]
        , '~'
        , paste0(
          'b'
          ,1:20
          ,'*'
          ,names(dsds)[-1]
          , collapse = '+'
        )
      )
    )
  , data = dsds
  , algorithm = 'port'
  , lower = rep(0,49)
  , upper = rep(1,49)
) -> dsdsds


fun_knn.alpha(
  .df_data = 
    df_occupations %>% 
    select(names(
      df_sample
    ))
  , .df_query = df_sample
  , .dbl_scale.ub = 100
  , .lgc_sort = T
) %>% 
  select(
    occupation
    , distance
    , similarity
  ) %>% 
  rename(
    s = similarity
    , d = distance
  ) %>% 
  mutate(
    s2 = 
      s * s +
      (1 - s) *
      s ^ Inf
    # s^(1/s^2)
    # exp(-s^2)
    # 1 / exp(-(1/s)*(s-(1/s)))
  ) %>%
  arrange(d) %>%
  select(
    occupation
    , d, s, s2
  ) %>%
  print(n = 25)

# - Estimate occupations's employability ---------------------------------------------------------------
fun_employability.workflow.m(
  .df_data = 
    df_occupations %>% 
    select(
      occupation
      , ends_with('.l')
    )
  , .int_employment = 
    df_occupations$
    employment2
  , .dbl_wages = 
    df_occupations$
    annual_wage_2021
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
) -> list_employability.nnls

df_employability %>% 
  mutate(
    fitted = 
      list_employability.nnls$
      fitted
  ) %>% 
  select(
    occupation
    , employability
    , fitted
  ) %>% 
  mutate(
    diff = 
      fitted - 
      employability
  ) %>% 
  filter(str_detect(
    str_to_lower(occupation)
    , 'stat|econ'
  )) %>% 
  arrange(-diff) %>% 
  print(n = nrow(.))

list_employability.nnls$
  model.tidy %>% 
  mutate(
    estimate = 
      round(estimate, 4)
  ) %>% 
  arrange(estimate) %>% 
  print(n = nrow(.))


df_employability %>% 
  mutate(
    employability.fitted =
      list_employability.nnls$
      fitted
  ) %>% 
  ggplot(aes(
    x = employability.fitted
    , y = employability
  )) + 
  geom_point(
    color = 'blue'
  ) + 
  geom_abline(
    intercept = 0
    , slope = 1
  ) + 
  geom_smooth(
    method = 'lm'
    , color = '#212121'
  ) + 
  xlim(0,1) +
  ylim(0,1) 



list_employability.nnls$
  model.tidy

list_employability.nnls$r2

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