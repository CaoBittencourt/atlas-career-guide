# [SETUP] -----------------------------------------------------------
# - Workspace -------------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

tryCatch(
  
  load('./sketch_not_iq_image.RData')
  
  , error = function(e){
    
    source('./sketch_not_iq_setup.R')
    
  }
  
)

# - Packages --------------------------------------------------------------
lapply(
  c(chr_profile, pkg)
  , function(x){
    if(!require(
      x, character.only = T
    )){
      install.packages(x)
      require(x)
    }}
)

select <- dplyr::select

# - Data ------------------------------------------------------------------
# User data
read_csv(
  # 'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
) -> df_input

# # Maids
# df_occupations %>%
#   select(
#     occupation
#     , ends_with('.l')
#   ) %>%
#   filter(
#     occupation ==
#       'Maids and Housekeeping Cleaners'
#   ) -> df_input

# df_occupations %>%
#   slice_sample(
#     n = 1
#   ) %>%
#   select(
#     occupation
#     , ends_with('.l')
#   ) -> df_input
# 
# df_input$occupation

# - Parameters ------------------------------------------------------------
# Mean IQ 
dbl_iq_mean <- 100

# IQ Standard Deviation
dbl_iq_sd <- 15

# IQ Proxy
df_factors %>% 
  filter(str_detect(
    str_to_lower(
      factor.name
    ), 'intellig|discern'
    # ), 'intellig'
  )) %>% 
  pull(item) ->
  chr_iq_proxy

df_occupations %>% 
  select(
    employment2
    , any_of(
      chr_iq_proxy
    )
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  reframe(
    iq_proxy_mean = 
      weighted.mean(
        item.score
        , employment2
      )
    , iq_proxy_sd = 
      wtd.var(
        item.score
        , employment2
      )
    , iq_proxy_sd =
      sqrt(iq_proxy_sd)
  ) -> df_iq_proxy

# WAIS IQ Labels
seq(
  dbl_iq_mean - 3 * 10
  , dbl_iq_mean + 3 * 10
  , 10
) -> dbl_iq_seq

c(
  'Retarded',
  'Borderline Retarded',
  'Low Average',
  'Average',
  'High Average',
  'Superior',
  'Very Superior'
) -> chr_iq_wais

chr_iq_wais -> 
  names(dbl_iq_seq)

# [NOT IQ] ------------------------------------------------------------------
# - Estimate Occupation's NOT IQ -----------------------------------------------------
df_occupations %>% 
  select(
    occupation
    , employment2
    , annual_wage_2021
    , any_of(
      chr_iq_proxy
    )) %>% 
  pivot_longer(
    cols = any_of(chr_iq_proxy)
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  group_by(occupation) %>%
  reframe(
    NOT_IQ = 
      fun_not_iq(
        dbl_scores = item.score
        , dbl_proxy_mean = 
          df_iq_proxy$
          iq_proxy_mean
        , dbl_proxy_sd = 
          df_iq_proxy$
          iq_proxy_sd
        , dbl_iq_mean = 
          dbl_iq_mean
        , dbl_iq_sd = 
          dbl_iq_sd
      )
    , employment2 =
      first(employment2)
    , annual_wage_2021 =
      first(annual_wage_2021)
  ) %>% 
  mutate(
    NOT_IQ_class = 
      findInterval(
        NOT_IQ
        , dbl_iq_seq
      ),
    NOT_IQ_class = 
      recode(
        NOT_IQ_class
        , '0' = chr_iq_wais[[1]]
        , '1' = chr_iq_wais[[1]]
        , '2' = chr_iq_wais[[2]]
        , '3' = chr_iq_wais[[3]]
        , '4' = chr_iq_wais[[4]]
        , '5' = chr_iq_wais[[5]]
        , '6' = chr_iq_wais[[6]]
        , '7' = chr_iq_wais[[7]]
      ),
    NOT_IQ_class = 
      factor(
        NOT_IQ_class
        , levels = 
          chr_iq_wais
      )
  ) -> df_occupations_not_iq

# - Simulate Normally Distributed Population --------------------------------------------
df_occupations_not_iq %>% 
  mutate(
    employment2 = 
      employment2 / 
      min(employment2)
    , employment2 = 
      ceiling(employment2)
  ) %>%
  group_by(
    occupation
  ) %>%
  reframe(
    NOT_IQ = pmax(
      rnorm(
        n = employment2
        , mean = NOT_IQ
        , sd = dbl_iq_sd
      ), 0)
  ) -> df_population_not_iq

# - Descriptive Statistics ------------------------------------------------
summary(df_population_not_iq$NOT_IQ)

# - NOT IQ Scores vs. Wage ---------------------------------
lm(
  annual_wage_2021 ~ 0 + NOT_IQ
  , weights = df_occupations_not_iq$employment2
  , data = df_occupations_not_iq
) -> model_not_iq

coef(model_not_iq) %>% 
  dollar()

# - Most and Least Intelligent Occupations ---------------------------------
df_occupations_not_iq %>% 
  arrange(desc(NOT_IQ)) %>% 
  mutate(
    rank = row_number()
  ) %>% 
  slice(1,n()) %>%
  select(
    rank
    , occupation
    , NOT_IQ
    , NOT_IQ_class
  )

# - Estimate User's NOT IQ -----------------------------------------------------
df_input %>% 
  select(
    occupation
    , any_of(
      chr_iq_proxy
    )) %>% 
  pivot_longer(
    cols = any_of(chr_iq_proxy)
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  reframe(
    NOT_IQ = 
      fun_not_iq(
        dbl_scores = item.score
        , dbl_proxy_mean = 
          df_iq_proxy$
          iq_proxy_mean
        , dbl_proxy_sd = 
          df_iq_proxy$
          iq_proxy_sd
        , dbl_iq_mean = 
          dbl_iq_mean
        , dbl_iq_sd = 
          dbl_iq_sd
      )
  ) %>% 
  mutate(
    IQ = 128
    , error = NOT_IQ - IQ
    , pct_error = percent(error / IQ, accuracy = .01)
    , NOT_IQ_class = 
      findInterval(
        NOT_IQ
        , dbl_iq_seq
      ),
    NOT_IQ_class = 
      recode(
        NOT_IQ_class
        , '0' = chr_iq_wais[[1]]
        , '1' = chr_iq_wais[[1]]
        , '2' = chr_iq_wais[[2]]
        , '3' = chr_iq_wais[[3]]
        , '4' = chr_iq_wais[[4]]
        , '5' = chr_iq_wais[[5]]
        , '6' = chr_iq_wais[[6]]
        , '7' = chr_iq_wais[[7]]
      )
    , IQ_class = 
      findInterval(
        IQ
        , dbl_iq_seq
      ),
    IQ_class = 
      recode(
        IQ_class
        , '0' = chr_iq_wais[[1]]
        , '1' = chr_iq_wais[[1]]
        , '2' = chr_iq_wais[[2]]
        , '3' = chr_iq_wais[[3]]
        , '4' = chr_iq_wais[[4]]
        , '5' = chr_iq_wais[[5]]
        , '6' = chr_iq_wais[[6]]
        , '7' = chr_iq_wais[[7]]
      )
  )

# [PLOT] ------------------------------------------------------------------
# - Distribution of NOT IQ ------------------------------------------------
df_population_not_iq %>% 
  fun_plot.density(aes(
    x = NOT_IQ
  )
  , .list_labs = list(
    title = 'The NOT IQ Bell Curve',
    subtitle = 'Estimating the distribution of NOT IQ scores',
    x = 'NOT IQ',
    y = NULL
  )
  , .list_axis.x.args = list(
    breaks = pretty_breaks(13)
  )
  , .fun_format.x = number
  ) + 
  annotate(
    geom = 'text',
    x = 155,
    y = 0.015,
    label = str_wrap(
      '
      Strangely enough, the NOT IQ distribution is exactly like an IQ distribution!
      This is convenient because technically speaking, in the US, it is illegal for HR Recruiters to apply IQ tests on job applicants.
      Therefore, we could sell a NOT IQ Assessment as an alternative to regular IQ tests.
      '
      , width = 40
    ) 
  )

# - Most and Least Intelligent Occupations -----------------------------------------------
df_occupations_not_iq %>% 
  arrange(desc(NOT_IQ)) %>% 
  mutate(
    rank = row_number()
  ) %>% 
  slice(
    1:5, 
    seq(
      round(n()/2) - 2,
      round(n()/2) + 2
    ),
    (n() - 5 + 1):n()
  ) %>% 
  group_by(occupation) %>% 
  reframe(
    NOT_IQ = pmax(
      rnorm(
        n = 25000
        , mean = NOT_IQ
        , sd = dbl_iq_sd
      )
      , 0)
    , NOT_IQ_class = 
      first(NOT_IQ_class)
  ) %>% 
  fun_plot.ridges(aes(
    x = NOT_IQ
    , y = occupation
    , fill = NOT_IQ_class
  )
  , .list_labs = list(
    title = 'Most and Least Intelligent Occupations',
    subtitle = 'NOT IQ Scores for a sample of 15 occupations',
    x = 'NOT IQ',
    y = NULL,
    fill = NULL
  )
  , .list_axis.x.args = list(
    breaks = pretty_breaks(13)
  )
  , .fun_format.x = number
  , .fun_format.y = function(y){y}
  , .chr_manual.pal = 
    set_names(
      viridis(length(chr_iq_wais))
      , chr_iq_wais
    )
  )

# - NOT IQ vs Wage --------------------------------------------------------
df_occupations_not_iq %>% 
  fun_plot.scatter(aes(
    x = annual_wage_2021
    , y = NOT_IQ
    , color = NOT_IQ_class
  )
  , .list_labs = list(
    title = 'NOT IQ vs. Wages',
    subtitle = 'Assessing the impact of intelligence on annual earnings',
    x = 'Annual Compensation (USD)',
    y = 'NOT IQ',
    color = NULL
  )
  , .list_axis.y.args = list(
    breaks = pretty_breaks(13)
  )
  , .dbl_limits.y = c(70,NA)
  , .fun_format.y = number
  , .fun_format.x = dollar
  , .chr_manual.pal = 
    set_names(
      viridis(length(chr_iq_wais))
      , chr_iq_wais
    )
  ) + 
  annotate(
    geom = 'text',
    x = mean(c(150,200)*1000),
    y = 90,
    label = str_wrap(
      '
      We can clearly see there is a strong positive association between NOT IQ scores and wages.
      This means more intelligent individuals tend to make more money.
      However, the benefit of NOT IQ on wages is diminishing,
      so that each additional point is less profitable than the previous point.
      '
      , width = 50
    ) 
  )

# [EXPORT] ----------------------------------------------------------------
# - Export NOT IQ Scores to Excel -----------------------------------------
df_occupations_not_iq %>%
  arrange(desc(NOT_IQ)) %>%
  write.xlsx(
    file = './df_occupations_not_iq.xlsx'
  )

