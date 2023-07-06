df_occupations %>%
  select(
    occupation
    , any_of(
      df_factors %>%
        filter(
          str_detect(
            str_to_lower(
              factor.name
            ), 'intelligence'
          )) %>%
        pull(item)
    )) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  group_by(occupation) %>%
  reframe(
    iq = fun_iq(
      dbl_scores = item.score
      , dbl_scale_ub = 100
      , dbl_scale_lb = 0
      , int_interval = 7
      , dbl_proxy_mean = 36.1
      , dbl_proxy_sd = 10.6
      , dbl_iq_mean = 100
      , dbl_iq_sd = 15
    )
    , iq2 = 
      fun_iq2(
        dbl_scores = item.score
        , dbl_proxy_mean = 36.1
        , dbl_proxy_sd = 10.6
        , dbl_iq_mean = 100
        , dbl_iq_sd = 15
      )
  ) %>%  
  # full_join(
  #   df_occupations %>% 
  #     select(
  #       occupation
  #       , employment2
  #       )
  # ) %>% 
  # ggplot(aes(
  #   x = iq
  #   , weight = employment2
  # )) + 
# geom_density()
arrange(desc(iq)) %>%
  slice(1:100,(n()-1):n()) %>% 
  print(n = nrow(.))

df_occupations %>%
  select(
    occupation
    , any_of(
      df_factors %>% 
        filter(
          str_detect(
            str_to_lower(
              factor.name
            ), 'intelligence|discernment'
          )) %>% 
        pull(item)
    )
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  group_by(occupation) %>%
  reframe(
    iq = fun_iq(
      dbl_scores = item.score
      , dbl_scale_ub = 100
      , dbl_scale_lb = 0
      , int_interval = 7
      , dbl_proxy_mean = 41.1
      , dbl_proxy_sd = 14.8
      , dbl_iq_mean = 100
      , dbl_iq_sd = 15
    )
    , iq2 = 
      fun_iq2(
        dbl_scores = item.score
        , dbl_proxy_mean = 41.1
        , dbl_proxy_sd = 14.8
        , dbl_iq_mean = 100
        , dbl_iq_sd = 15
      )
  ) %>% 
  arrange(desc(iq)) %>%
  slice(1:100,(n()-1):n()) %>% 
  print(n = nrow(.))

df_input %>%
  select(
    occupation
    , any_of(
      df_factors %>% 
        filter(
          str_detect(
            str_to_lower(
              factor.name
            ), 'intelligence|discernment'
          )) %>% 
        pull(item)
    )
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  group_by(occupation) %>%
  reframe(
    iq = fun_iq(
      dbl_scores = item.score
      , dbl_scale_ub = 100
      , dbl_scale_lb = 0
      , int_interval = 7
      , dbl_proxy_mean = 41.1
      , dbl_proxy_sd = 14.8
      , dbl_iq_mean = 100
      , dbl_iq_sd = 15
    )
    , iq2 = 
      fun_iq2(
        dbl_scores = item.score
        , dbl_proxy_mean = 41.1
        , dbl_proxy_sd = 14.8
        , dbl_iq_mean = 100
        , dbl_iq_sd = 15
      )
  ) %>% 
  arrange(desc(iq)) %>%
  slice(1:100,(n()-1):n()) %>% 
  print(n = nrow(.))

df_input %>%
  select(
    occupation
    , any_of(
      df_factors %>% 
        filter(
          str_detect(
            str_to_lower(
              factor.name
            ), 'intelligence'
          )) %>% 
        pull(item)
    )
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  group_by(occupation) %>%
  reframe(
    iq = fun_iq(
      dbl_scores = item.score
      , dbl_scale_ub = 100
      , dbl_scale_lb = 0
      , int_interval = 7
      , dbl_proxy_mean = 36.1
      , dbl_proxy_sd = 10.6
      , dbl_iq_mean = 100
      , dbl_iq_sd = 15
    )
    , iq2 = 
      fun_iq2(
        dbl_scores = item.score
        , dbl_proxy_mean = 36.1
        , dbl_proxy_sd = 10.6
        , dbl_iq_mean = 100
        , dbl_iq_sd = 15
      )
  ) %>% 
  arrange(desc(iq)) %>%
  slice(1:100,(n()-1):n()) %>% 
  print(n = nrow(.))

# df_occupations %>% 
#   ggplot(aes(
#     # x = basic.mathematics.l
#     x = mathematics.l
#     # x = english_language.l
#     , weight = employment2
#   )) + 
#   geom_density()
# 
# df_occupations %>% 
#   filter(str_detect(
#     str_to_lower(
#       occupation
#     ), 'account'
#   )) %>% 
#   select(
#     occupation
#     , economics_and_accounting.l
#     , entry_level_education
#     , annual_wage_2021
#   ) %>% view
# 
# df_occupations %>% 
#   select(
#     employment2
#     , ends_with('.l')
#   ) %>% 
#   names() %>% 
#   sample(1)
# 
# # scheduling_work_and_activities
# # resolving_conflicts_and_negotiating_with_others
# # deal_with_unpleasant_or_angry_people
# # deductive_reasoning
# # biology
# df_occupations %>% 
#   select(
#     employment2
#     , ends_with('.l')
#   ) %>% 
#   pivot_longer(
#     cols = -1
#   ) %>% 
#   filter(
#     # value != 0
#     str_detect(
#       name,
#       # 'mathematics'
#       # 'english'
#       'english|mathematics'
#     )
#   ) %>% 
#   reframe(
#     mean = 
#       weighted.mean(
#         value, employment2
#       )
#     , sd = 
#       sqrt(wtd.var(
#         value, employment2
#       ))
#   )
# ggplot(aes(
#   x = value
#   , weight = employment2
# )) + 
#   geom_density()


# read_rds(
#   "C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds"
# ) -> efa_model

# df_occupations %>%
#   select(
#     occupation
#     , employment2
#     , any_of(
#       df_factors %>%
#         filter(
#           str_detect(
#             str_to_lower(
#               factor.name
#             ), 'intelligence'
#           )) %>%
#         pull(item)
#     )
#   ) %>% 
#   pivot_longer(
#     cols = ends_with('.l')
#     , names_to = 'item'
#     , values_to = 'item.score'
#   ) %>% 
#   mutate(
#     iq.mean = 
#       weighted.mean(
#         item.score
#         , employment2
#       )
#   ) %>% 
#   group_by(occupation) %>% 
#   reframe(
#     iq = 
#       100 * 
#       mean(item.score) / 
#       first(iq.mean)
#   ) %>% 
#   arrange(desc(iq))

df_occupations %>%
  select(
    occupation
    , any_of(
      df_factors %>% 
        filter(
          str_detect(
            str_to_lower(
              factor.name
            ), 'intelligence|discernment'
          )) %>% 
        pull(item)
    )
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  group_by(occupation) %>%
  reframe(
    iq2 = 
      fun_iq2(
        dbl_scores = item.score
        , dbl_proxy_mean = 41.1
        , dbl_proxy_sd = 14.8
        , dbl_iq_mean = 100
        , dbl_iq_sd = 15
      )
  ) %>%
  full_join(
    df_occupations %>%
      select(
        occupation
        , employment2
      )
  ) -> dsds

dsds %>% 
  ggplot(aes(
    x = iq2
    , weight = employment2
  )) +
  geom_density()

dsds %>% 
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
    iq2 = pmax(
      rnorm(
        n = employment2
        , mean = iq2
        , sd = 15
      ), 0)
  ) %>% 
  # reframe(
  #   iq = mean(iq2)
  #   , sd = sd(iq2)
  # ) %>% 
  ggplot(aes(
    x = iq2
  )) +
  geom_density()
