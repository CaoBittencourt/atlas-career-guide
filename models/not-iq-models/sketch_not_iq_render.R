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

# Color
seq(
  dbl_iq_mean - 3 * dbl_iq_sd
  , dbl_iq_mean + 3 * dbl_iq_sd
  , dbl_iq_sd
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
# - Estimate NOT IQ -----------------------------------------------------
df_occupations %>% 
  select(
    occupation
    , employment2
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
  mutate(
    NOT_IQ_class = 
      findInterval(
        NOT_IQ
        , dbl_iq_seq
      ),
    NOT_IQ_class = 
      recode(
        NOT_IQ_class
        , .default = chr_iq_wais[[1]]
        , '1' = chr_iq_wais[[2]]
        , '2' = chr_iq_wais[[3]]
        , '3' = chr_iq_wais[[4]]
        , '4' = chr_iq_wais[[5]]
        , '5' = chr_iq_wais[[6]]
        , '6' = chr_iq_wais[[7]]
      ),
    NOT_IQ_class = 
      factor(
        NOT_IQ_class
        , levels = 
          chr_iq_wais
        )
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
  # , .reorder_fct = F
  # , .reorder_desc = F
  , .fun_format.y = function(y){y}
  # , .chr_manual.pal =
  )

df_population_not_iq %>% 
  filter(
    occupation %in% (
      df_occupations_not_iq %>% 
        arrange(desc(NOT_IQ)) %>% 
        slice(1:10, (n() - 10 + 1):n()) %>% 
        pull(occupation)
    )
  ) %>% 
  group_by(
    occupation
  ) %>% 
  mutate(
    NOT_IQ_class = 
      findInterval(
        mean(NOT_IQ)
        , dbl_iq_seq
      ),
    NOT_IQ_class = 
      recode(
        NOT_IQ_class
        , .default = chr_iq_wais[[1]]
        , '1' = chr_iq_wais[[2]]
        , '2' = chr_iq_wais[[3]]
        , '3' = chr_iq_wais[[4]]
        , '4' = chr_iq_wais[[5]]
        , '5' = chr_iq_wais[[6]]
        , '6' = chr_iq_wais[[7]]
      ),
    NOT_IQ_class = 
      factor(NOT_IQ_class)
  ) %>%
  ungroup() %>% 
  fun_plot.ridges(aes(
    x = NOT_IQ
    , y = occupation
    , fill = NOT_IQ_class
  )
  # , .reorder_fct = F
  # , .reorder_desc = F
  , .fun_format.y = function(y){y}
  # , .chr_manual.pal =
  )

fun_plot.lollipop(aes(
  x = occupation
  , y = NOT_IQ
)
)

# - Least Intelligent Occupations -----------------------------------------------
df_models.long %>%
  filter(
    occupation !=
      df_input$
      occupation
  ) %>% 
  group_by(model) %>% 
  reframe(
    similarity = 
      range(similarity)
  ) %>% 
  mutate(
    occupation = 
      rep(c(
        'Worst Match'
        , 'Top Match'
      )
      , times = 
        length(unique(
          model
        ))
      )
  ) %>% 
  fun_plot.dumbbell2(aes(
    x = similarity
    , y = model
    , color = occupation
  )
  , .list_labs = 
    list(
      title = paste('Similarity Range for Each Model:', df_input$occupation)
      , subtitle = 'What are the maximum and minimum similarity scores?'
      , x = 'Professional Compatibility Scores (%)'
      , y = NULL
      , color = NULL 
    )
  
  , .reorder_fct = T
  , .reorder_desc = T
  , .chr_manual.pal = 
    c('blue','red')
  , .list_axis.x.args =
    list(
      limits = c(0,1)
      , breaks = seq(0,1,length.out = 7)
    )
  , .fun_format.x = percent
  )

# [EXPORT] ----------------------------------------------------------------
# - Export NOT IQ Scores to Excel -----------------------------------------



