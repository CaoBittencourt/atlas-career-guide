# [SETUP] -----------------------------------------------------------
# - Workspace -------------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

tryCatch(
  
  load('./sketch_matching_assessment_image.RData')
  
  , error = function(e){
    
    source('./sketch_matching_assessment_setup.R')
    
  }
  
)

# - Packages --------------------------------------------------------------
pkg <- c('bvls', 'weights', 'fastglm', 'censReg', 'betareg', 'gamlss')

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
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
) -> df_input

# Maids
df_occupations %>% 
  select(
    occupation
    , ends_with('.l')
  ) %>% 
  filter(
    occupation ==
      'Maids and Housekeeping Cleaners' 
  ) -> df_input

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
dbl_similarity.ub <- 1
dbl_similarity.lb <- 0

dbl_scale.ub <- 100
dbl_scale.lb <- 0

# [DATA] ------------------------------------------------------------------
# - Column vectors -----------------------------------------------------
# User as a column vector
t(df_input[-1]) -> df_input.t

# Occupations as column vectors
df_occupations %>% 
  select(names(
    df_input[-1]
  )) %>% 
  t() -> df_occupations.t

df_occupations$
  occupation -> 
  colnames(
    df_occupations.t
  )

# - Regressions data frame --------------------------------------------------------
cbind(
  df_input.t
  , df_occupations.t
) %>% 
  as_tibble() -> 
  df_occupations.t

# [CURRENT MODEL] ---------------------------------------------------------
# - Weighted Knn matching ---------------------------------------------------------------
date_start <- Sys.time()

fun_knn.alpha(
  .df_data = 
    df_occupations
  , .df_query = df_input
  , .dbl_scale.ub = 
    dbl_scale.ub
) %>% 
  select(
    occupation
    , distance
    , similarity
  ) -> df_models

date_end <- Sys.time()

tibble(
  knn.wgt = date_end - date_start
) -> df_models.time

df_models %>% 
  rename(
    knn.wgt = similarity
  ) %>% 
  select(!distance) -> 
  df_models

# [STANDARD MODELS] --------------------------------------------------------
# - Knn with over-qualification threshold = 0 --------------------------
date_start <- Sys.time()

fun_knn.matching(
  .df_data = df_occupations
  , .df_query = df_input
  , .dbl_scale.lb = dbl_scale.lb
  , .dbl_scale.ub = dbl_scale.ub
  , .dbl_over_qualification.threshold = 0
) %>% 
  select(
    occupation
    , similarity
  ) %>% 
  full_join(
    df_models
  ) -> df_models

date_end <- Sys.time()

df_models %>% 
  rename(
    knn.0 = similarity
  ) -> df_models

df_models.time$knn.0 <- date_end - date_start

# - Knn with over-qualification threshold = 17 --------------------------
date_start <- Sys.time()

fun_knn.matching(
  .df_data = df_occupations
  , .df_query = df_input
  , .dbl_scale.lb = dbl_scale.lb
  , .dbl_scale.ub = dbl_scale.ub
  , .dbl_over_qualification.threshold = 17
) %>% 
  select(
    occupation
    , similarity
  ) %>% 
  full_join(
    df_models
  ) -> df_models

date_end <- Sys.time()

df_models %>% 
  rename(
    knn.17 = similarity
  ) -> df_models

df_models.time$knn.17 <- date_end - date_start

# - Knn with over-qualification threshold = 100 --------------------------
date_start <- Sys.time()

fun_knn.matching(
  .df_data = df_occupations
  , .df_query = df_input
  , .dbl_scale.lb = dbl_scale.lb
  , .dbl_scale.ub = dbl_scale.ub
  , .dbl_over_qualification.threshold = 100
) %>% 
  select(
    occupation
    , similarity
  ) %>% 
  full_join(
    df_models
  ) -> df_models

date_end <- Sys.time()

df_models %>% 
  rename(
    knn.100 = similarity
  ) -> df_models

df_models.time$knn.100 <- date_end - date_start

# - Normalized OLS matching -----------------------------------------------------------
date_start <- Sys.time()

# map_dbl(
map(
  .x = df_occupations.t[-1]
  , ~ 
    lm.fit(
      as.matrix(.x)
      , df_occupations.t[[1]]
    ) %>% 
    coef() %>% 
    pmin(1) %>% 
    pmax(0)
  # ) -> df_models$ols
) -> list_models

date_end <- Sys.time()

df_models.time$ols <- date_end - date_start

list_models %>% 
  bind_rows(
    .id = 'occupation'
  ) %>% 
  rename(
    ols = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

# - Normalized Pearson matching ---------------------------------------------------------------
date_start <- Sys.time()

wtd.cors(
  x = df_occupations.t[1]
  , y = df_occupations.t[-1]
)/2 + 1/2 -> list_models

# (
#   1 + wtd.cors(
#     x = df_occupations.t[1]
#     , y = df_occupations.t[-1]
#   )
# ) / 2 -> list_models

# as.numeric(
#   1 + wtd.cors(
#     x = df_occupations.t[1]
#     , y = df_occupations.t[-1]
#   )) / 2 -> df_models$pearson

date_end <- Sys.time()

list_models %>%
  as_tibble() %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'occupation'
    , values_to = 'pearson'
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$pearson <- date_end - date_start

# - BVLS matching -----------------------------------------------------------
date_start <- Sys.time()

# map_dbl(
map_dbl(
  .x = df_occupations.t[-1]
  , ~ 
    bvls(
      as.matrix(.x)
      , df_occupations.t[[1]]
      , bl = dbl_similarity.lb
      , bu = dbl_similarity.ub
    )[[1]]
  # ) -> df_models$bvls
) -> list_models

date_end <- Sys.time()

df_models.time$bvls <- date_end - date_start

list_models %>% 
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  rename(
    bvls = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

# - Tobit matching -----------------------------------------------------------
date_start <- Sys.time()

map_dbl(
  .x = 2:ncol(df_occupations.t)
  , ~
    censReg::censReg(
      V1 ~ 0 + .
      , left = dbl_scale.lb
      , right = dbl_scale.ub
      , data = df_occupations.t[c(1,.x)]
    ) %>% 
    margEff() %>% 
    as.numeric() %>% 
    pmin(1) %>% 
    pmax(0)
  # ) -> df_models$tobit
) -> list_models

date_end <- Sys.time()

list_models %>% 
  set_names(names(
    df_occupations.t[-1]
  )) %>%
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  rename(
    tobit = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$tobit <- date_end - date_start

# -- Note on Logit and Probit regressions ----------------------------------
# Generally speaking, logit and probit regression
# are suited only to binary data (0,1), following a Bernoulli distribution,
# and, thus, are utilized to estimate the probability
# of success (1) or failure (0) given certain parameters.
# This said, these models can also be applied when
# the dependent variable represents the relative count of
# successes (i.e. % of 1s in N trials)
# This is somewhat of a mid-point between Poisson Regression, 
# Logit Regression, and Tobit Regression: https://www.theanalysisfactor.com/when-to-use-logistic-regression-for-percentages-and-counts/
# In our career matching problem, then, we could think of 
# attribute scores as percentages of successful trials,
# that is: say an individual has 100 of a certain attribute;
# if we normalize this score as a percentage, dividing by the scale's upper limit,
# 100 / 100 = 1, then we could think of this normalized score
# as the ratio of successful trials in tasks related to this attribute.
# In other words: if this individual performed N tasks
# related to this attribute, they would be 100% successful.
# This way, we think of attribute scores as certainty of 
# success: if somebody has a score of 0%, then that means
# if they were to try to perform any tasks associated with
# this attribute, they would be sure to fail every time.
# Or, say, somebody has a score of 70% in an attribute.
# We could think of such a score as a track-record of 70%
# successful trials - whether hypothetical or not doesn't matter,
# so long as we can reframe this problem in terms of a Bernoulli-distributed variable.
# Thus, even though logit and probit regressions are suited
# for binary (Bernoulli) data, by reframing the attribute
# scores in this way, we could apply these methods to
# estimate career matching coefficients.

# # - Logistic regression matching -----------------------------------------------------------
# date_start <- Sys.time()
# 
# map_dbl(
#   .x = df_occupations.t[-1] / 100
#   , ~
#     fastglm(
#       as.matrix(.x)
#       , df_occupations.t[[1]] / 100
#       , family = binomial(
#         link = 'logit'
#       )
#     ) %>%
#     coef()
#   # ) -> df_models$logit
# ) -> list_models
# 
# exp(list_models) /
#   (1 + exp(list_models)) ->
#   list_models
# 
# # exp(df_models$logit) /
# #   (1 + exp(df_models$logit)) ->
# #   df_models$logit
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   rename(
#     logit = 2
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$logit <- date_end - date_start

# - Logistic regression matching 2 ----------------------------------------
date_start <- Sys.time()

list_c(map(
  .x = as.integer(df_input.t)
  , ~ rep(
    c(1,0)
    , times = c(.x, 100 - .x)
  )
)) -> int_input.logit

map(
  .x = df_occupations.t[-1]
  , ~ 
    as.matrix(list_c(map(
      .x = as.integer(.x)
      , ~ rep(
        c(1,0)
        , times = c(.x, 100 - .x)
      )
    )))
) %>% 
  map(
    ~ 
      coef(fastglmPure(
        x = .x
        , y = int_input.logit
        , family = binomial(
          link = 'logit'
        )
      ))
  ) -> list_models

date_end <- Sys.time()

list_models %>% 
  bind_rows(
    .id = 'occupation'
  ) %>% 
  rename(
    logit = 2
  ) %>% 
  mutate(
    logit = 
      exp(logit) /
      (1 + exp(logit))
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$logit <- date_end - date_start

# - Probit regression matching 2 ----------------------------------------
date_start <- Sys.time()

list_c(map(
  .x = as.integer(df_input.t)
  , ~ rep(
    c(1,0)
    , times = c(.x, 100 - .x)
  )
)) -> int_input.probit

map(
  .x = df_occupations.t[-1]
  , ~ 
    as.matrix(list_c(map(
      .x = as.integer(.x)
      , ~ rep(
        c(1,0)
        , times = c(.x, 100 - .x)
      )
    )))
) %>% 
  map(
    ~ 
      coef(fastglmPure(
        x = .x
        , y = int_input.probit
        , family = binomial(
          link = 'probit'
        )
      ))
  ) -> list_models

date_end <- Sys.time()

list_models %>% 
  bind_rows(
    .id = 'occupation'
  ) %>% 
  rename(
    probit = 2
  ) %>% 
  mutate(
    probit = 
      exp(probit) /
      (1 + exp(probit))
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$probit <- date_end - date_start

# # - Probit regression matching -----------------------------------------------------------
# date_start <- Sys.time()
# 
# map_dbl(
#   .x = df_occupations.t[-1] / 100
#   , ~
#     fastglm(
#       as.matrix(.x)
#       , df_occupations.t[[1]] / 100
#       , family = binomial(
#         link = 'probit'
#       )
#     ) %>%
#     coef()
#   # ) -> df_models$probit
# ) -> list_models
# 
# exp(list_models) /
#   (1 + exp(list_models)) ->
#   list_models
# 
# # exp(df_models$probit) /
# #   (1 + exp(df_models$probit)) ->
# #   df_models$probit
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   rename(
#     probit = 2
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$probit <- date_end - date_start

# - Beta regression matching ----------------------------------------------
date_start <- Sys.time()

map_dbl(
  .x = 2:ncol(df_occupations.t)
  , ~
    gamlss::gamlss(
      V1 ~ 0 + .
      , sigma.formula = ~ 0 + .
      , nu.formula = ~ 0 + .
      , tau.formula = ~ 0 + .
      , family = BEINF
      , data = df_occupations.t[c(1,.x)] / 100
    ) %>% 
    coef() %>% 
    pmin(1) %>% 
    pmax(0)
  # ) -> df_models$beta
) -> list_models

date_end <- Sys.time()

list_models %>% 
  set_names(names(
    df_occupations.t[-1]
  )) %>%
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  rename(
    beta = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$beta <- date_end - date_start

# betareg::betareg.fit(
#   x =
#     as.matrix((
#       df_occupations.t[[10]] /
#         dbl_scale.ub) %>%
#         pmax(0.0000000001) %>%
#         pmin(99.9999999999)
#     )
#   , y =
#     (df_occupations.t[[1]] /
#        dbl_scale.ub) %>%
#     pmax(0.0000000001) %>%
#     pmin(99.9999999999)
#   # , z =
#     # as.matrix((
#     #   df_occupations.t[[10]] /
#     #     dbl_scale.ub) %>%
#     #     pmax(0.0000000001) %>%
#     #     pmin(99.9999999999)
#     # )
# )

# betareg::betareg(
#   formula = V1 ~ 0 + .
#   , data =
#     df_occupations.t %>%
#     select(1,10) %>%
#     mutate(across(
#       .cols = everything()
#       ,.fns = ~
#         pmin(.x, 99.9999999999) %>%
#         pmax(0.0000000001) / 100
#     ))
# ) %>% coef()

# map(
#   .x = 2:ncol(df_occupations.t)
#   , ~ 
#     betareg(
#       formula = V1 ~ 0 + .
#       , data =
#         df_occupations.t[c(1,.x)] %>%
#         mutate(across(
#           .cols = everything()
#           ,.fns = ~ .x %>% 
#             pmin(99.9999999999) %>%
#             pmax(0.0000000001) / 100
#         )
#         ))
# ) -> list_models

# # - Beta regression matching 2 ----------------------------------------------
# date_start <- Sys.time()
# 
# map_dbl(
#   .x = 2:ncol(df_occupations.t)
#   , ~
#     gamlss::gamlss(
#       V1 ~ 0 + .
#       , sigma.formula = ~ 0 + .
#       , nu.formula = ~ 0 + .
#       , tau.formula = ~ 0 + .
#       , family = BEINF
#       , data = df_occupations.t[c(1,.x)] / 100
#     ) %>% 
#     coef() %>% 
#     pmin(1) %>% 
#     pmax(0)
# ) -> df_models$beta
# 
# date_end <- Sys.time()
# 
# df_models.time$beta <- date_end - date_start
# 
# # betareg::betareg.fit(
# #   x = 
# #     as.matrix((
# #       df_occupations.t[[10]] / 
# #         dbl_scale.ub) %>% 
# #         pmax(0.0000000001) %>% 
# #         pmin(99.9999999999)
# #     )
# #   , y = 
# #     (df_occupations.t[[1]] / 
# #        dbl_scale.ub) %>% 
# #     pmax(0.0000000001) %>% 
# #     pmin(99.9999999999)
# #   # , z = 
# #     # as.matrix((
# #     #   df_occupations.t[[10]] / 
# #     #     dbl_scale.ub) %>% 
# #     #     pmax(0.0000000001) %>% 
# #     #     pmin(99.9999999999)
# #     # )
# # )
# 
# # betareg::betareg(
# #   formula = V1 ~ 0 + .
# #   , data =
# #     df_occupations.t %>%
# #     select(1,10) %>%
# #     mutate(across(
# #       .cols = everything()
# #       ,.fns = ~
# #         pmin(.x, 99.9999999999) %>%
# #         pmax(0.0000000001) / 100
# #     ))
# # ) %>% coef()

# [WEIGHTED MODELS] --------------------------------------------------------
# --
# - Weighted Normalized ols matching -----------------------------------------------------------
date_start <- Sys.time()

# map_dbl(
map(
  .x = df_occupations.t[-1]
  , ~ 
    lm.wfit(
      as.matrix(.x)
      , df_occupations.t[[1]]
      , w = as.numeric(.x)
    ) %>% 
    coef() %>% 
    pmin(1) %>% 
    pmax(0)
  # ) -> df_models$ols.wgt
) -> list_models

date_end <- Sys.time()

df_models.time$ols.wgt <- date_end - date_start

list_models %>% 
  bind_rows(
    .id = 'occupation'
  ) %>% 
  rename(
    ols.wgt = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

# - Weighted Normalized Pearson matching ---------------------------------------------------------------
date_start <- Sys.time()

map_dbl(
  .x = df_occupations.t[-1]
  , ~
    as.numeric(
      1 +
        weights::wtd.cors(
          df_occupations.t[1]
          , .x
          , weight = .x
        )
    ) / 2
) -> list_models

date_end <- Sys.time()

list_models %>%
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  rename(
    pearson.wgt = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$pearson.wgt <- date_end - date_start

# - Weighted BVLS matching -----------------------------------------------------------
date_start <- Sys.time()

# map_dbl(
map_dbl(
  .x = df_occupations.t[-1]
  , ~ 
    bvls(
      as.matrix(.x * sqrt(.x))
      , df_occupations.t[[1]] *
        sqrt(.x)
      , bl = dbl_similarity.lb
      , bu = dbl_similarity.ub
    )[[1]]
  # ) -> df_models$bvls
) -> list_models

date_end <- Sys.time()

df_models.time$bvls.wgt <- date_end - date_start

list_models %>% 
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  rename(
    bvls.wgt = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

# - Weighted Logistic regression matching 2 ----------------------------------------
date_start <- Sys.time()

list_c(map(
  .x = as.integer(df_input.t)
  , ~ rep(
    c(1,0)
    , times = c(.x, 100 - .x)
  )
)) -> int_input.logit

map(
  .x = df_occupations.t[-1]
  , ~ 
    as.matrix(list_c(map(
      .x = as.integer(.x)
      , ~ rep(
        c(1,0)
        , times = c(.x, 100 - .x)
      )
    )))
) %>% 
  map(
    ~ 
      coef(fastglmPure(
        x = .x
        , y = int_input.logit
        , family = binomial(
          link = 'logit'
        )
        , weights = .x
      ))
  ) -> list_models

date_end <- Sys.time()

list_models %>% 
  bind_rows(
    .id = 'occupation'
  ) %>% 
  rename(
    logit.wgt = 2
  ) %>% 
  mutate(
    logit.wgt = 
      exp(logit.wgt) /
      (1 + exp(logit.wgt))
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$logit.wgt <- date_end - date_start

# - Weighted Probit regression matching 2 ----------------------------------------
date_start <- Sys.time()

list_c(map(
  .x = as.integer(df_input.t)
  , ~ rep(
    c(1,0)
    , times = c(.x, 100 - .x)
  )
)) -> int_input.probit

map(
  .x = df_occupations.t[-1]
  , ~ 
    as.matrix(list_c(map(
      .x = as.integer(.x)
      , ~ rep(
        c(1,0)
        , times = c(.x, 100 - .x)
      )
    )))
) %>% 
  map(
    ~ 
      coef(fastglmPure(
        x = .x
        , y = int_input.probit
        , family = binomial(
          link = 'probit'
        )
        , weights = .x
      ))
  ) -> list_models

date_end <- Sys.time()

list_models %>% 
  bind_rows(
    .id = 'occupation'
  ) %>% 
  rename(
    probit.wgt = 2
  ) %>% 
  mutate(
    probit.wgt = 
      exp(probit.wgt) /
      (1 + exp(probit.wgt))
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$probit.wgt <- date_end - date_start

# - Weighted Beta regression matching ----------------------------------------------
date_start <- Sys.time()

map_dbl(
  .x = 2:ncol(df_occupations.t)
  , ~
    gamlss::gamlss(
      V1 ~ 0 + .
      , sigma.formula = ~ 0 + .
      , nu.formula = ~ 0 + .
      , tau.formula = ~ 0 + .
      , family = BEINF
      , weights = df_occupations.t[[.x]]
      , data = df_occupations.t[c(1,.x)] / 100
    ) %>% 
    coef() %>% 
    pmin(1) %>% 
    pmax(0)
  # ) -> df_models$beta
) -> list_models

date_end <- Sys.time()

list_models %>% 
  set_names(names(
    df_occupations.t[-1]
  )) %>%
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  rename(
    beta.wgt = 2
  ) %>% 
  full_join(
    df_models
  ) -> df_models

df_models.time$beta.wgt <- date_end - date_start

# betareg::betareg.fit(
#   x =
#     as.matrix((
#       df_occupations.t[[10]] /
#         dbl_scale.ub) %>%
#         pmax(0.0000000001) %>%
#         pmin(99.9999999999)
#     )
#   , y =
#     (df_occupations.t[[1]] /
#        dbl_scale.ub) %>%
#     pmax(0.0000000001) %>%
#     pmin(99.9999999999)
#   # , z =
#     # as.matrix((
#     #   df_occupations.t[[10]] /
#     #     dbl_scale.ub) %>%
#     #     pmax(0.0000000001) %>%
#     #     pmin(99.9999999999)
#     # )
# )

# betareg::betareg(
#   formula = V1 ~ 0 + .
#   , data =
#     df_occupations.t %>%
#     select(1,10) %>%
#     mutate(across(
#       .cols = everything()
#       ,.fns = ~
#         pmin(.x, 99.9999999999) %>%
#         pmax(0.0000000001) / 100
#     ))
# ) %>% coef()

# map(
#   .x = 2:ncol(df_occupations.t)
#   , ~ 
#     betareg(
#       formula = V1 ~ 0 + .
#       , data =
#         df_occupations.t[c(1,.x)] %>%
#         mutate(across(
#           .cols = everything()
#           ,.fns = ~ .x %>% 
#             pmin(99.9999999999) %>%
#             pmax(0.0000000001) / 100
#         )
#         ))
# ) -> list_models

# --
# # - Weighted Normalized OLS matching -----------------------------------------------------------
# date_start <- Sys.time()
# 
# map_dbl(
#   .x = df_occupations.t[-1]
#   , ~ 
#     lm.wfit(
#       as.matrix(.x)
#       , df_occupations.t[[1]]
#       , w = as.numeric(.x)
#     ) %>% 
#     coef() %>% 
#     pmin(1) %>% 
#     pmax(0)
# ) -> df_models$ols.wgt
# 
# date_end <- Sys.time()
# 
# df_models.time$ols.wgt <- date_end - date_start
# 
# # - Weighted Normalized Pearson matching ---------------------------------------------------------------
# date_start <- Sys.time()
# 
# map_dbl(
#   .x = df_occupations.t[-1]
#   , ~
#     as.numeric(
#       1 +
#         weights::wtd.cors(
#           df_occupations.t[1]
#           , .x
#           , weight = .x
#         )
#     ) / 2
# ) -> df_models$pearson.wgt
# 
# date_end <- Sys.time()
# 
# df_models.time$pearson.wgt <- date_end - date_start

# # - Weighted BVLS matching -----------------------------------------------------------
# date_start <- Sys.time()
# 
# map_dbl(
#   .x = df_occupations.t[-1]
#   , ~
#     bvls(
#       as.matrix(.x * sqrt(.x))
#       , df_occupations.t[[1]] *
#         sqrt(.x)
#       , bl = dbl_similarity.lb
#       , bu = dbl_similarity.ub
#     )[[1]]
# ) -> df_models$bvls.wgt
# 
# date_end <- Sys.time()
# 
# df_models.time$bvls.wgt <- date_end - date_start

# # - Weighted Logistic regression matching 2 ----------------------------------------
# date_start <- Sys.time()
# 
# list_c(map(
#   .x = as.integer(df_input.t)
#   , ~ rep(
#     c(1,0)
#     , times = c(.x, 100 - .x)
#   )
# )) -> int_input.logit
# 
# map(
#   .x = df_occupations.t[-1]
#   , ~ 
#     as.matrix(list_c(map(
#       .x = as.integer(.x)
#       , ~ rep(
#         c(1,0)
#         , times = c(.x, 100 - .x)
#       )
#     )))
# ) %>% 
#   map(
#     ~ 
#       coef(fastglmPure(
#         x = .x
#         , y = int_input.logit
#         , family = binomial(
#           link = 'logit'
#         )
#         , weights = .x
#       ))
#   ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   bind_rows(
#     .id = 'occupation'
#   ) %>% 
#   rename(
#     logit = 2
#   ) %>% 
#   mutate(
#     logit = 
#       exp(logit) /
#       (1 + exp(logit))
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$logit <- date_end - date_start
# 
# # - Weighted Probit regression matching 2 ----------------------------------------
# date_start <- Sys.time()
# 
# list_c(map(
#   .x = as.integer(df_input.t)
#   , ~ rep(
#     c(1,0)
#     , times = c(.x, 100 - .x)
#   )
# )) -> int_input.probit
# 
# map(
#   .x = df_occupations.t[-1]
#   , ~ 
#     as.matrix(list_c(map(
#       .x = as.integer(.x)
#       , ~ rep(
#         c(1,0)
#         , times = c(.x, 100 - .x)
#       )
#     )))
# ) %>% 
#   map(
#     ~ 
#       coef(fastglmPure(
#         x = .x
#         , y = int_input.probit
#         , family = binomial(
#           link = 'probit'
#         )
#         , weights = .x
#       ))
#   ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   bind_rows(
#     .id = 'occupation'
#   ) %>% 
#   rename(
#     probit = 2
#   ) %>% 
#   mutate(
#     probit = 
#       exp(probit) /
#       (1 + exp(probit))
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$probit <- date_end - date_start
# 
# # # - Weighted Logistic regression matching -----------------------------------------------------------
# # date_start <- Sys.time()
# # 
# # map_dbl(
# #   .x = df_occupations.t[-1] / 100
# #   , ~
# #     fastglm(
# #       as.matrix(.x)
# #       , df_occupations.t[[1]] / 100
# #       , family = binomial(
# #         link = 'logit'
# #       )
# #       , weights = as.matrix(.x)
# #     ) %>%
# #     coef()
# # ) -> df_models$logit.wgt
# # 
# # exp(df_models$logit.wgt) /
# #   (1 + exp(df_models$logit.wgt)) ->
# #   df_models$logit.wgt
# # 
# # date_end <- Sys.time()
# # 
# # df_models.time$logit.wgt <- date_end - date_start
# # 
# # # - Weighted Probit regression matching -----------------------------------------------------------
# # date_start <- Sys.time()
# # 
# # map_dbl(
# #   .x = df_occupations.t[-1] / 100
# #   , ~
# #     fastglm(
# #       as.matrix(.x)
# #       , df_occupations.t[[1]] / 100
# #       , family = binomial(
# #         link = 'probit'
# #       )
# #       , weights = as.matrix(.x)
# #     ) %>%
# #     coef()
# # ) -> df_models$probit.wgt
# # 
# # exp(df_models$probit.wgt) /
# #   (1 + exp(df_models$probit.wgt)) ->
# #   df_models$probit.wgt
# # 
# # date_end <- Sys.time()
# # 
# # df_models.time$probit.wgt <- date_end - date_start
# 
# # - Weighted Beta regression matching ----------------------------------------------
# date_start <- Sys.time()
# 
# lm.fit(
#   x = as.matrix(df_occupations[1,] %>% select(ends_with('.l')))
#   , y = as.matrix(df_input %>% select(ends_with('.l')))
# ) %>% coef() -> lalala
# 
# map_dbl(
#   .x = 2:ncol(df_occupations.t)
#   , ~
#     gamlss::gamlss(
#       V1 ~ 0 + .
#       , sigma.formula = ~ 0 + .
#       , nu.formula = ~ 0 + .
#       , tau.formula = ~ 0 + .
#       , family = BEINF
#       , data = df_occupations.t[c(1,.x)] / 100
#       , weights = df_occupations.t[[.x]]
#     ) %>% 
#     coef() %>% 
#     pmin(1) %>% 
#     pmax(0)
# ) -> df_models$beta.wgt
# 
# date_end <- Sys.time()
# 
# df_models.time$beta.wgt <- date_end - date_start
# 
# # betareg::betareg.fit(
# #   x = as.matrix(df_occupations.t[[10]] / sum(df_occupations.t[[10]]))
# #   , y = df_occupations.t[[1]] / sum(df_occupations.t[[1]])
# #   # , z = as.matrix(df_occupations.t[[10]] / 100)
# # )
# 
# # betareg::betareg(
# #   formula = V1 ~ 0 + .
# #   , data =
# #     df_occupations.t %>%
# #     select(1,10) %>%
# #     mutate(across(
# #       .cols = everything()
# #       ,.fns = ~
# #         pmin(.x, 99.9999999999) %>%
# #         pmax(0.0000000001) / 100
# #     ))
# # ) %>% coef()
# 
# 
# [TEST] ------------------------------------------------------------------
# - Pivot models ------------------------------------------------------
df_models %>% 
  pivot_longer(
    cols = -1
    , names_to = 'model'
    , values_to = 'similarity'
  ) -> df_models.long

# - Running time ----------------------------------------------------------
df_models.time %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'model'
    , values_to = 'time'
  ) %>%
  mutate(
    time = as.duration(time)
    , time = as.numeric(time)
    , time.fct =
      findInterval(
        time
        , c(0,1/10,1,5,30)
      ) %>% 
      recode(
        '5' = 'Very Slow'
        , '4' = 'Slow'
        , '3' = 'Medium'
        , '2' = 'Fast'
        , '1' = 'Very Fast'
      ) %>%
      factor(
        levels =
          c(
            'Very Slow'
            , 'Slow'
            , 'Medium'
            , 'Fast'
            , 'Very Fast'
          )
      )
  ) -> df_models.time.long

df_models.time.long %>% 
  fun_plot.lollipop(aes(
    x = model
    , y = time
    , color = time.fct
  )
  , .list_labs = 
    list(
      x = NULL
      , y = 'Running Time (s)'
      , title = 'Comparison of Different Matching Models'
      , subtitle = 'Running Time for Estimating Each Model'
      , color = NULL
    )
  , .chr_manual.pal = 
    set_names(
      viridis::viridis(5)
      , c(
        'Very Slow'
        , 'Slow'
        , 'Medium'
        , 'Fast'
        , 'Very Fast'
      )
    )
  , .list_axis.y.args = 
    list(
      limits = c(0,1.2*max(df_models.time.long$time))
      , breaks = seq(0,1.2*max(df_models.time.long$time), length.out = 10)
    )
  ) + 
  annotate(
    x = ceiling(nrow(df_models.time.long) / 3)
    , y = 0.9 * max(as.numeric(df_models.time.long$time))
    , geom = 'text'
    , label = 
      str_wrap(
        'The old models, which are still in use, are much slower compared to the weighted knn revised algorithm, as well as some regression approches.
        Concerning these newer matching models, we can see they generally run in less than 1s.
        However, Beta, Logit, Probit, and Tobit regressions are slow.'
        , width = 50
      )
  )

# - Similarity distribution -----------------------------------------------
df_models.long %>%
  group_by(model) %>%
  mutate(
    similarity.kflex =
      fun_kflex(similarity)
  ) %>%
  ungroup() %>%
  mutate(
    similarity.kflex =
      cut(
        similarity.kflex
        , breaks = 5
        , labels = F
      ) %>%
      recode(
        '5' = 'Very Generalist'
        , '4' = 'Generalist'
        , '3' = 'Normal'
        , '2' = 'Specialist'
        , '1' = 'Very Specialist'
      ) %>%
      factor(
        levels =
          c(
            'Very Generalist'
            , 'Generalist'
            , 'Normal'
            , 'Specialist'
            , 'Very Specialist'
          )
      )
  ) %>%
  ungroup() %>%
  fun_plot.ridges(aes(
    x = similarity
    , y = model
    , fill = similarity.kflex
  )
  , .list_labs =
    list(
      title = paste('Similarity Distribution for Each Model:', df_input$occupation)
      , subtitle = 'Note: These similarity coefficients are not representative of the whole population.'
      , x = 'Professional Compatibility Scores (%)'
      , y = NULL
      , fill = NULL
    )
  , .list_legend = list(fill = guide_legend(nrow = 1))
  , .chr_manual.pal =
    set_names(
      viridis::viridis(5)
      ,c(
        'Very Generalist'
        , 'Generalist'
        , 'Normal'
        , 'Specialist'
        , 'Very Specialist'
      )
    )
  , .list_axis.x.args =
    list(
      limits = c(0,1.1)
      , breaks = seq(0,1,length.out = 7)
    )
  , .list_axis.y.args =
    list(
      expand = c(0.25,0)
    )
  , .fun_format.x = percent
  )

# - Similarity range -----------------------------------------------
df_models.long %>%
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

# - Similarity variance ----------------------------------------------------------
df_models.long %>% 
  full_join(
    df_occupations %>% 
      select(
        occupation
        , employment2
      )
  ) %>% 
  group_by(model) %>% 
  reframe(
    stdev = 
      sd(similarity)
    , stdev.wgt = 
      Hmisc::wtd.var(
        similarity
        , weights = 
          employment2
      ) %>% 
      sqrt()
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'metric'
    , values_to = 'value'
  ) %>%
  mutate(
    metric =
      recode(
        metric
        , 'stdev' = 'Standard Deviation'
        , 'stdev.wgt' = 'Sample-Weighted Standard Deviation'
      )
  ) %>% 
  fun_plot.dumbbell2(aes(
    x = value
    , y = model
    , color = metric
  )
  , .list_labs = 
    list(
      title = paste('Similarity Standard Deviation for Each Model:', df_input$occupation)
      , subtitle = 'Which models yield more dispersed similarity scores?'
      , x = 'Standard Deviation of Professional Compatibility Scores (%)'
      , y = NULL
      , color = NULL 
    )
  
  , .reorder_fct = T
  , .reorder_desc = T
  , .chr_manual.pal = 
    c('orange','purple')
  , .list_axis.x.args =
    list(
      limits = c(0,1)
      , breaks = seq(0,1,length.out = 7)
    )
  , .fun_format.x = percent
  )

# # - Similarity distribution (sample-weighted) -----------------------------------------------
# df_models %>%
#   full_join(
#     df_occupations %>%
#       mutate(
#         employment2 =
#           employment2 /
#           min(employment2)
#         , employment2 =
#           ceiling(employment2)
#       ) %>%
#       select(
#         occupation
#         , employment2
#       )
#   ) %>%
#   group_by(occupation) %>%
#   slice(rep(
#     1:n()
#     , first(employment2)
#   )
#   ) %>%
#   select(!employment2) %>%
#   pivot_longer(
#     cols = -1
#     , names_to = 'model'
#     , values_to = 'similarity'
#   ) %>%
#   ungroup() %>%
#   fun_plot.ridges(aes(
#     x = similarity
#     , y = model
#     , fill = model
#   )
#   , .list_labs =
#     list(
#       title = 'Sample-Weighted Similarity Distribution for Each Model'
#       , subtitle = 'Note: These similarity coefficients are representative of the whole population.'
#       , x = 'Professional Compatibility Scores (%)'
#       , y = NULL
#     )
#   , .list_axis.x.args =
#     list(
#       limits = c(0,1.1)
#       , breaks = seq(0,1,length.out = 7)
#     )
#   , .list_axis.y.args =
#     list(
#       expand = c(0.25,0)
#     )
#   , .fun_format.x = percent
#   )

# # - Similarity distribution (sample-weighted) -----------------------------------------------
# df_models.long %>% 
#   full_join(
#     df_occupations %>% 
#       mutate(
#         employment2 = 
#           employment2 / 
#           min(employment2)
#         , employment2 = 
#           ceiling(employment2)
#       ) %>% 
#       select(
#         occupation
#         , employment2
#       ) 
#   ) %>% 
#   group_by(model) %>% 
#   mutate(
#     similarity.kflex = 
#       fun_kflex(
#         similarity
#         , .dbl_weights =
#           employment2
#       )
#   ) %>%
#   ungroup() %>% 
#   mutate(
#     similarity.kflex = 
#       cut(
#         similarity.kflex
#         , breaks = 5
#         , labels = F
#       ) %>% 
#       recode(
#         '5' = 'Very Generalist'
#         , '4' = 'Generalist'
#         , '3' = 'Normal'
#         , '2' = 'Specialist'
#         , '1' = 'Very Specialist'
#       ) %>% 
#       factor(
#         levels = 
#           c(
#             'Very Generalist'
#             , 'Generalist'
#             , 'Normal'
#             , 'Specialist'
#             , 'Very Specialist'
#           )
#       )
#   ) %>% 
#   ungroup() %>%
#   fun_plot.ridges(aes(
#     x = similarity
#     , y = model
#     , fill = similarity.kflex
#   )
#   , .list_labs = 
#     list(
#       title = 'Similarity Distribution for Each Model'
#       , subtitle = 'Note: These similarity coefficients are not representative of the whole population.'
#       , x = 'Professional Compatibility Scores (%)'
#       , y = NULL
#       , fill = NULL
#     )
#   , .list_legend = list(fill = guide_legend(nrow = 1))
#   , .chr_manual.pal = 
#     set_names(
#       viridis::viridis(5)
#       ,c(
#         'Very Generalist'
#         , 'Generalist'
#         , 'Normal'
#         , 'Specialist'
#         , 'Very Specialist'
#       )
#     )
#   , .list_axis.x.args = 
#     list(
#       limits = c(0,1.1)
#       , breaks = seq(0,1,length.out = 7)
#     )
#   , .list_axis.y.args =
#     list(
#       expand = c(0.25,0)
#     )
#   , .fun_format.x = percent
#   )

# - Interpretability ------------------------------------------------------
df_models.long %>% 
  filter(
    occupation ==
      df_input$
      occupation
  ) %>% 
  arrange(desc(
    similarity
  ))

df_models.long %>%
  group_by(model) %>% 
  arrange(desc(
    similarity
  )) %>%
  slice(1) %>% 
  ungroup() %>% 
  arrange(desc(
    similarity
  ))

df_models.long %>%
  group_by(model) %>% 
  arrange(
    similarity
  ) %>%
  slice(1) %>% 
  ungroup() %>% 
  arrange(desc(
    similarity
  ))

df_models.long %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    ), '^statistician'
  )) %>% 
  arrange(desc(
    similarity
  )) %>% 
  print(n = nrow(.))

df_models.long %>% 
  filter(
    occupation ==
      sample(
        occupation, 1
      )
  ) %>% 
  arrange(desc(
    similarity
  )) %>% 
  print(n = nrow(.))

df_models.long %>%
  split(.$model) %>% 
  map(
    ~ arrange(.x, desc(2))
  ) -> list_models

list_models %>% 
  map(
    ~ .x %>%
      filter(str_detect(
        str_to_lower(
          occupation
        ), 'statisticians'
      )) %>% 
      arrange(desc(2))
  )

list_models %>% 
  map(
    ~ .x %>%
      filter(str_detect(
        str_to_lower(
          occupation
        ), 'statisticians'
      )) %>% 
      arrange(desc(2))
  )

list_models$
  bvls.wgt %>% 
  # slice_tail(n = 10)
  print(n = 400)

list_models$
  bvls %>% 
  slice_tail(n = 10)

list_models$
  bvls.wgt %>% 
  slice_tail(n = 10)

list_models$
  bvls %>% 
  slice_tail(n = 10)

source('C:/Users/Cao/Documents/Github/atlas-research/functions/metrics/fun_employability.R')

df_models %>% 
  full_join(
    df_occupations %>% 
      mutate(
        employment2 = 
          ceiling(employment2)
      ) %>% 
      select(
        occupation
        , employment2
      ) 
  ) %>% 
  mutate(across(
    .cols = -c(occupation, employment2)
    ,.fns = ~ 
      fun_interchangeability(.x)
  )) %>% 
  reframe(across(
    .cols = -c(occupation, employment2)
    ,.fns = ~ 
      fun_employability(employment2, .x)
  )) %>%
  round(4) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'model'
    , values_to = 'employability'
  ) %>% 
  arrange(desc(
    employability
  ))

list_models$
  # logit %>% 
  probit.wgt %>% 
  mutate(
    I = 
      fun_interchangeability(
        similarity
      ) %>% 
      round(2)
  ) %>% 
  filter(str_detect(
    str_to_lower(occupation)
    , 'data|econ|math|stat'
  )) %>% 
  print(n = nrow(.))

list_models$
  # logit %>%
  probit %>%
  # print(n = 100)
  slice_head(n = 10)

list(
  good = c(
    'knn.wgt'
    , 'knn.0'
    , 'knn.100'
  ) 
  , ok = c(
    'bvls.wgt'
    , 'ols.wgt'
    # , 'bvls'
    , 'ols'
    , 'bvls.wgt'
    , 'ols.wgt'
    , 'tobit'
    , 'bvls'
    , 'ols'
    , 'knn.17'
    , 'knn.100'
  ) 
  , shit = c(
    'probit.wgt'
    , 'logit.wgt'
    , 'pearson.wgt'
    , 'probit'
    , 'logit'
    , 'tobit'
    , 'pearson'
    , 'knn.17'
    , 'beta'
    , 'beta.wgt'
    , 'bvls.wgt'
    , 'ols.wgt'
    , 'bvls'
    , 'ols'
    , 'bvls.wgt'
    , 'ols.wgt'
  )
)
