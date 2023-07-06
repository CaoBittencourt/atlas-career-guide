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
source('C:/Users/Cao/Documents/Github/atlas-research/functions/metrics/fun_employability.R')
source('C:/Users/Cao/Documents/Github/atlas-research/functions/metrics/fun_logistic.R')

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

# - Level of education recode ---------------------------------------------
df_occupations %>% 
  mutate(
    .after = entry_level_education
    , education_years = recode(
      entry_level_education
      , "Bachelor's degree" = 17 + 4
      , "Postsecondary nondegree award" = 17 + 4 + 2
      , "High school diploma or equivalent" = 17
      , "Master's degree" = 17 + 5
      , "Associate's degree" = 17 + 2
      , "No formal educational credential" = 14
      , "Some college, no degree" = 17 + 2
      , "Doctoral or professional degree" = 17 + 7
    )
  ) -> df_occupations

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

# # - Tobit matching -----------------------------------------------------------
# date_start <- Sys.time()
# 
# map_dbl(
#   .x = 2:ncol(df_occupations.t)
#   , ~
#     censReg::censReg(
#       V1 ~ 0 + .
#       , left = dbl_scale.lb
#       , right = dbl_scale.ub
#       , data = df_occupations.t[c(1,.x)]
#     ) %>% 
#     margEff() %>%
#     as.numeric() %>%
#     pmin(1) %>%
#     pmax(0)
#   # ) -> df_models$tobit
# ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   set_names(names(
#     df_occupations.t[-1]
#   )) %>%
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   rename(
#     tobit = 2
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$tobit <- date_end - date_start

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
  )
  , ~
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
  )
  , ~ 
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

# # - Beta regression matching ----------------------------------------------
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
#   # ) -> df_models$beta
# ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   set_names(names(
#     df_occupations.t[-1]
#   )) %>%
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   rename(
#     beta = 2
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
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
# 
# # map(
# #   .x = 2:ncol(df_occupations.t)
# #   , ~ 
# #     betareg(
# #       formula = V1 ~ 0 + .
# #       , data =
# #         df_occupations.t[c(1,.x)] %>%
# #         mutate(across(
# #           .cols = everything()
# #           ,.fns = ~ .x %>% 
# #             pmin(99.9999999999) %>%
# #             pmax(0.0000000001) / 100
# #         )
# #         ))
# # ) -> list_models
# 
# # # - Beta regression matching 2 ----------------------------------------------
# # date_start <- Sys.time()
# # 
# # map_dbl(
# #   .x = 2:ncol(df_occupations.t)
# #   , ~
# #     gamlss::gamlss(
# #       V1 ~ 0 + .
# #       , sigma.formula = ~ 0 + .
# #       , nu.formula = ~ 0 + .
# #       , tau.formula = ~ 0 + .
# #       , family = BEINF
# #       , data = df_occupations.t[c(1,.x)] / 100
# #     ) %>% 
# #     coef() %>% 
# #     pmin(1) %>% 
# #     pmax(0)
# # ) -> df_models$beta
# # 
# # date_end <- Sys.time()
# # 
# # df_models.time$beta <- date_end - date_start
# # 
# # # betareg::betareg.fit(
# # #   x = 
# # #     as.matrix((
# # #       df_occupations.t[[10]] / 
# # #         dbl_scale.ub) %>% 
# # #         pmax(0.0000000001) %>% 
# # #         pmin(99.9999999999)
# # #     )
# # #   , y = 
# # #     (df_occupations.t[[1]] / 
# # #        dbl_scale.ub) %>% 
# # #     pmax(0.0000000001) %>% 
# # #     pmin(99.9999999999)
# # #   # , z = 
# # #     # as.matrix((
# # #     #   df_occupations.t[[10]] / 
# # #     #     dbl_scale.ub) %>% 
# # #     #     pmax(0.0000000001) %>% 
# # #     #     pmin(99.9999999999)
# # #     # )
# # # )
# # 
# # # betareg::betareg(
# # #   formula = V1 ~ 0 + .
# # #   , data =
# # #     df_occupations.t %>%
# # #     select(1,10) %>%
# # #     mutate(across(
# # #       .cols = everything()
# # #       ,.fns = ~
# # #         pmin(.x, 99.9999999999) %>%
# # #         pmax(0.0000000001) / 100
# # #     ))
# # # ) %>% coef()
# 
# # # - Beta regression matching 3 --------------------------------------------
# # rstanarm::stan_betareg.fit(
# #   x = 
# #     pmax(
# #       pmin(as.matrix(df_occupations.t[492]) / 100
# #            , 0.99999)
# #       , 0.00001
# #     )
# #   
# #   , y = pmax(
# #     pmin(df_occupations.t[[1]] / 100
# #          , 0.99999)
# #     , 0.00001
# #   )
# #   # , weights = df_occupations.t[[492]]
# # )
# 
# # # - Bayesian regression matching --------------------------------------------------------------------
# # install.packages('rstanarm')
# # library(rstanarm) -
# #   
# #   stan_glm(
# #     V1 ~ 0 + .
# #     , data = df_occupations.t[c(1,10)]
# #   ) %>% coef()
# # 
# # stan_glm.fit(
# #   x = as.matrix(df_occupations.t[492])
# #   , y = df_occupations.t[[1]]
# #   , weights = df_occupations.t[[492]]
# # ) -> dsds
# # 

# [WEIGHTED MODELS] --------------------------------------------------------
# --
# - Weights function ------------------------------------------------------
# fun_logistic(
#   .mtx_x = seq(0,1,0.1)
#   , .dbl_mid.point = 0.5
#   , .dbl_scaling.factor = 8
#   , .lgc_logistic.approx = T
# ) %>% round(4)
# fun_logistic(
#   .mtx_x =
#     as.matrix(
#       df_occupations.t
#     )
#   , .dbl_mid.point = 67
#   # , .dbl_mid.point = 67
#   # , .dbl_mid.point = 50
#   # , .dbl_mid.point = 33
#   , .dbl_logistic.ub = 1
#   # , .dbl_scaling.factor = 0.5
#   , .dbl_scaling.factor = 0.25
#   # , .dbl_scaling.factor = 0.125
#   , .dbl_displacement.factor = 1
#   , .dbl_scale.ub = dbl_scale.ub
#   , .dbl_scale.lb = dbl_scale.lb
#   , .lgc_normalize = T
# ) -> mtx_weights

# fun_interchangeability(
#   as.matrix(
#     df_occupations.t
#   ) / dbl_scale.ub
#   # , .dbl_scaling = 0.125
#   , .dbl_scaling = 0.25
# ) -> mtx_weights

# Variable scaling
fun_interchangeability(
  as.matrix(
    df_occupations.t
  ) / dbl_scale.ub
  # , .dbl_scaling = 0.125
  , .dbl_scaling = 
    map_dbl(
      df_occupations.t
      , ~ 
        # 2 - .x %>%
        # 1 - .x %>%
        # 1 / .x %>%
        .x %>%
        fun_kflex(
          .dbl_scale.lb = 
            dbl_scale.lb
          , .dbl_scale.ub = 
            dbl_scale.ub
        )
    )
) -> mtx_weights

# plot(
#   fun_logistic(
#     seq(0,100,1)
#     , .dbl_mid.point = 67
#     , .dbl_scaling.factor = 0.25
#     
#     , .dbl_displacement.factor = 1
#     , .dbl_logistic.ub = 1
#     , .dbl_scale.ub = 1
#     , .dbl_scale.lb = 0
#     , .lgc_normalize = T
#   )
# )
# 
# plot(
#   fun_interchangeability(
#     seq(0,1,0.01)
#     # , .dbl_scaling = 0.125
#     , .dbl_scaling = 0.25
#   )
# )
# 
# max(mtx_weights)
# min(mtx_weights)
# qplot(mtx_weights)
# view(round(mtx_weights,4))

# - Weighted Normalized Pearson matching ---------------------------------------------------------------
date_start <- Sys.time()

map2_dbl(
  .x = df_occupations.t[-1]
  , .y = as_tibble(mtx_weights[,-1])
  , ~
    as.numeric(
      1 +
        weights::wtd.cors(
          df_occupations.t[1]
          , .x
          , weight = .y
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
map2_dbl(
  .x = df_occupations.t[-1]
  , .y = as_tibble(sqrt(
    mtx_weights[,-1]
  ))
  , ~ 
    bvls(
      as.matrix(.x * .y)
      , df_occupations.t[[1]] * .y
      , bl = dbl_similarity.lb
      , bu = dbl_similarity.ub
    )[[1]]
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

df_models %>% 
  select(
    occupation,
    bvls.wgt
  ) %>% 
  filter(str_detect(
    str_to_lower(occupation),
    'hospital'
  )) %>% 
  mutate(
    I = 
      fun_interchangeability(
        bvls.wgt
      )
  )

# list_models %>% 
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   rename(
#     bvls.wgt = 2
#   ) %>% 
#   filter(str_detect(
#     str_to_lower(
#       occupation
#     ), 'medicine|physician|hospital|econo|statis'
#     # ), 'medicine|physician|hospital'
#     # ), 'econ|data|statis'
#   )) %>%
#   mutate(
#     I = 
#       fun_interchangeability(
#         bvls.wgt
#         , .dbl_scaling = 4
#       ) %>% round(4)
#   ) %>% 
#   arrange(desc(I)) %>% 
#   print(n = nrow(.))

# - Weighted Logistic regression matching 2 ----------------------------------------
date_start <- Sys.time()

list_c(map(
  .x = as.integer(df_input.t)
  , ~ rep(
    c(1,0)
    , times = c(.x, 100 - .x)
  )
)) -> int_input.logit

map2(
  .x = 
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
    )
  , .y = 
    as_tibble(mtx_weights)[-1][rep(
      1:nrow(mtx_weights)
      , each = 100
    ),]
  
  # df_occupations.t[-1][rep(
  #   1:nrow(df_occupations.t)
  #   , each = 100
  # ),]
  
  # fun_logistic(
  #   .dbl_x = 
  #     df_occupations.t[-1][rep(
  #       1:nrow(df_occupations.t)
  #       , each = 100
  #     ),] / 
  #     dbl_scale.ub
  #   , .dbl_mid.point = 
  #     (dbl_scale.ub / 2 ) / 
  #     dbl_scale.ub
  #   , .dbl_scaling.factor = 8
  #   , .lgc_logistic.approx = T
  # )
  , ~ 
    coef(fastglmPure(
      x = .x
      , y = int_input.logit
      , family = binomial(
        link = 'logit'
      )
      , weights = .y
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

map2(
  .x =
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
    )
  , .y = 
    as_tibble(mtx_weights)[-1][rep(
      1:nrow(mtx_weights)
      , each = 100
    ),]
  
  # df_occupations.t[-1][rep(
  #   1:nrow(df_occupations.t)
  #   , each = 100
  # ),]
  
  # fun_logistic(
  #   .dbl_x = 
  #     df_occupations.t[-1][rep(
  #       1:nrow(df_occupations.t)
  #       , each = 100
  #     ),] / 
  #     dbl_scale.ub
  #   , .dbl_mid.point = 
  #     (dbl_scale.ub / 2 ) / 
  #     dbl_scale.ub
  #   , .dbl_scaling.factor = 8
  #   , .lgc_logistic.approx = T
  # )
  , ~ 
    coef(fastglmPure(
      x = .x
      , y = int_input.probit
      , family = binomial(
        link = 'probit'
      )
      , weights = .y
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

# # - Weighted Beta regression matching ----------------------------------------------
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
#       , weights = df_occupations.t[[.x]]
#       , data = df_occupations.t[c(1,.x)] / 100
#     ) %>% 
#     coef() %>% 
#     pmin(1) %>% 
#     pmax(0)
#   # ) -> df_models$beta
# ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   set_names(names(
#     df_occupations.t[-1]
#   )) %>%
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   rename(
#     beta.wgt = 2
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$beta.wgt <- date_end - date_start
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
# 
# # map(
# #   .x = 2:ncol(df_occupations.t)
# #   , ~ 
# #     betareg(
# #       formula = V1 ~ 0 + .
# #       , data =
# #         df_occupations.t[c(1,.x)] %>%
# #         mutate(across(
# #           .cols = everything()
# #           ,.fns = ~ .x %>% 
# #             pmin(99.9999999999) %>%
# #             pmax(0.0000000001) / 100
# #         )
# #         ))
# # ) -> list_models
# 
# # --
# 
# # # - Weighted Normalized Pearson matching ---------------------------------------------------------------
# # date_start <- Sys.time()
# # 
# # map_dbl(
# #   .x = df_occupations.t[-1]
# #   , ~
# #     as.numeric(
# #       1 +
# #         weights::wtd.cors(
# #           df_occupations.t[1]
# #           , .x
# #           , weight = .x
# #         )
# #     ) / 2
# # ) -> list_models
# # 
# # date_end <- Sys.time()
# # 
# # list_models %>%
# #   as_tibble(
# #     rownames = 'occupation'
# #   ) %>% 
# #   rename(
# #     pearson.wgt = 2
# #   ) %>% 
# #   full_join(
# #     df_models
# #   ) -> df_models
# # 
# # df_models.time$pearson.wgt <- date_end - date_start
# # 
# # - Weighted BVLS matching -----------------------------------------------------------
# date_start <- Sys.time()
# 
# # map_dbl(
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
#   # ) -> df_models$bvls
# ) -> list_models
# 
# date_end <- Sys.time()
# 
# df_models.time$bvls.wgt <- date_end - date_start
# 
# list_models %>%
#   as_tibble(
#     rownames = 'occupation'
#   ) %>%
#   rename(
#     bvls.wgt = 2
#   ) %>%
#   full_join(
#     df_models
#   ) -> df_models
# 
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
# map2(
#   .x = 
#     map(
#       .x = df_occupations.t[-1]
#       , ~ 
#         as.matrix(list_c(map(
#           .x = as.integer(.x)
#           , ~ rep(
#             c(1,0)
#             , times = c(.x, 100 - .x)
#           )
#         )))
#     )
#   , .y = 
#     df_occupations.t[-1][rep(
#       1:nrow(df_occupations.t)
#       , each = 100
#     ),]
#   , ~ 
#     coef(fastglmPure(
#       x = .x
#       , y = int_input.logit
#       , family = binomial(
#         link = 'logit'
#       )
#       , weights = .y
#     ))
# ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   bind_rows(
#     .id = 'occupation'
#   ) %>% 
#   rename(
#     logit.wgt = 2
#   ) %>% 
#   mutate(
#     logit.wgt = 
#       exp(logit.wgt) /
#       (1 + exp(logit.wgt))
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$logit.wgt <- date_end - date_start
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
# map2(
#   .x =
#     map(
#       .x = df_occupations.t[-1]
#       , ~ 
#         as.matrix(list_c(map(
#           .x = as.integer(.x)
#           , ~ rep(
#             c(1,0)
#             , times = c(.x, 100 - .x)
#           )
#         )))
#     )
#   , .y = 
#     df_occupations.t[-1][rep(
#       1:nrow(df_occupations.t)
#       , each = 100
#     ),]
#   , ~ 
#     coef(fastglmPure(
#       x = .x
#       , y = int_input.probit
#       , family = binomial(
#         link = 'probit'
#       )
#       , weights = .y
#     ))
# ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   bind_rows(
#     .id = 'occupation'
#   ) %>% 
#   rename(
#     probit.wgt = 2
#   ) %>% 
#   mutate(
#     probit.wgt = 
#       exp(probit.wgt) /
#       (1 + exp(probit.wgt))
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$probit.wgt <- date_end - date_start
# 
# # - Weighted Beta regression matching ----------------------------------------------
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
#       , weights = df_occupations.t[[.x]]
#       , data = df_occupations.t[c(1,.x)] / 100
#     ) %>% 
#     coef() %>% 
#     pmin(1) %>% 
#     pmax(0)
#   # ) -> df_models$beta
# ) -> list_models
# 
# date_end <- Sys.time()
# 
# list_models %>% 
#   set_names(names(
#     df_occupations.t[-1]
#   )) %>%
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   rename(
#     beta.wgt = 2
#   ) %>% 
#   full_join(
#     df_models
#   ) -> df_models
# 
# df_models.time$beta.wgt <- date_end - date_start
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
# 
# # map(
# #   .x = 2:ncol(df_occupations.t)
# #   , ~ 
# #     betareg(
# #       formula = V1 ~ 0 + .
# #       , data =
# #         df_occupations.t[c(1,.x)] %>%
# #         mutate(across(
# #           .cols = everything()
# #           ,.fns = ~ .x %>% 
# #             pmin(99.9999999999) %>%
# #             pmax(0.0000000001) / 100
# #         )
# #         ))
# # ) -> list_models
# 
# # --

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

# - Similarity range -----------------------------------------------
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

# - Similarity distribution -----------------------------------------------
df_models.long %>%
  filter(
    occupation !=
      df_input$
      occupation
  ) %>% 
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

# - Interchangeability metrics --------------------------------------------
seq(0,1,0.001) %>% 
  as_tibble() %>%
  rename(
    s = 1
  ) %>%
  mutate(
    Coef.I = s
    #, Coef.II = s ^ 2
    , Coef.II = s ^ 4
    #, Coef.III = s * s + (1 - s) * s ^ 2
    # , Coef.III = s * s + (1 - s) * s ^ 4
    , Coef.III = s * s + (1 - s) * s ^ ((1/s)^(1/s))
    # , Coef.IV = s ^ ((1/s)^((1/s)))
    # , Coef.IV = s ^ ((1/s)^(4*(1/s)))
    , Coef.IV = s ^ ((1/s)^(1/s))
    #, Coef.V = s ^ ((1/s)^(4/s))
    #, Coef.IV = s ^ ((1/s)^(1/s))
    #, Coef.IV = s * s + (1 - s) * s ^ ((1/s)^(1/s))
    #, Coef.IV = s * s + (1 - s) * s ^ ((1/s)^(4))
    #, Coef.V = s * s + (1 - s) * s ^ ((1/s)^(1/s)^(1/s))
  ) %>% 
  rename(
    similarity = s
  ) %>% 
  pivot_longer(
    cols = starts_with('Coef')
    , names_to = 'coefficient'
    , values_to = 'interchangeability'
  ) %>% 
  fun_plot.line(aes(
    x = similarity
    , y = interchangeability
    , color = coefficient
  )
  , .chr_manual.pal = 
    c(rep('grey',3),viridis(1, direction = -1))
  , .list_labs = list(
    title = paste('Interchangeability Metrics')
    , subtitle = 'How much a person is recognizable by the Labor Market as a particular occupation?'
    , x = 'Similarity'
    , y = 'Interchangeability'
    , color = 'Coefficient'
  )
  , .fun_format.x = percent
  , .fun_format.y = percent
  )

# - Weights --------------------------------------------
seq(0,1,0.001) %>% 
  as_tibble() %>%
  rename(
    s = 1
  ) %>%
  mutate(
    Coef.I = s
    , Coef.II = s ^ 0.25
    , Coef.III = s ^ 4
    , Coef.IV = fun_logistic(s, .dbl_mid.point = 0.5, .dbl_scaling.factor = 20, .dbl_scale.ub = 1, .dbl_scale.lb = 0, .lgc_normalize = T)
    , Coef.V = fun_interchangeability(s, .dbl_scaling = 0.25)
  ) %>% 
  rename(
    similarity = s
  ) %>% 
  pivot_longer(
    cols = starts_with('Coef')
    , names_to = 'coefficient'
    , values_to = 'interchangeability'
  ) %>% 
  fun_plot.line(aes(
    x = similarity
    , y = interchangeability
    , color = coefficient
  )
  , .chr_manual.pal =
    c(rep('grey',4),viridis(1, direction = -1))
  , .list_labs = list(
    title = paste('Regression Weights Function')
    , subtitle = 'Relative importance of each attribute.'
    , x = 'Item Score'
    , y = 'Item Weight'
    , color = 'Coefficient'
  )
  , .fun_format.x = percent
  , .fun_format.y = percent
  )

# - Interchangeability distribution -----------------------------------------------
df_models.long %>%
  filter(
    occupation !=
      df_input$
      occupation
  ) %>% 
  mutate(
    interchangeability = 
      fun_interchangeability(
        similarity
      )
  ) %>% 
  group_by(model) %>%
  mutate(
    interchangeability.kflex =
      fun_kflex(interchangeability)
  ) %>%
  ungroup() %>%
  mutate(
    interchangeability.kflex =
      cut(
        interchangeability.kflex
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
    x = interchangeability
    , y = model
    , fill = interchangeability.kflex
  )
  , .list_labs =
    list(
      title = paste('Interchangeability Distribution for Each Model:', df_input$occupation)
      , subtitle = 'Note: These similarity coefficients are not representative of the whole population.'
      , x = 'Professional Interchangeability Scores (%)'
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

# - Compatibility with Statisticians -------------------
df_models.long %>% 
  mutate(
    interchangeability = 
      fun_interchangeability(
        similarity
      )
  ) %>%
  filter(str_detect(
    str_to_lower(
      occupation
    ), '^statistician'
  )) %>%
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  mutate(
    metric = 
      str_to_title(metric)
  ) -> df_match

df_match %>%
  fun_plot.dumbbell2(aes(
    x = value
    , y = model
    , color = metric
  )
  , .list_labs = 
    list(
      title = paste('Similarity Comparison:', df_input$occupation, 'vs', unique(df_match$occupation))
      , subtitle = paste0('How compatible is ',df_input$occupation,' with ',unique(df_match$occupation),'?')
      , x = 'Professional Compatibility Scores (%)'
      , y = NULL
      , color = NULL 
    )
  
  , .reorder_fct = T
  , .reorder_desc = T
  , .chr_manual.pal = 
    c('yellow','blue')
  , .list_axis.x.args =
    list(
      limits = c(0,1)
      , breaks = seq(0,1,length.out = 7)
    )
  , .fun_format.x = percent
  )

# - Compatibility with Economists -------------------
df_models.long %>% 
  mutate(
    interchangeability = 
      fun_interchangeability(
        similarity
      )
  ) %>%
  filter(str_detect(
    str_to_lower(
      occupation
    ), '^economists$'
  )) %>%
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  mutate(
    metric = 
      str_to_title(metric)
  ) -> df_match

df_match %>%
  fun_plot.dumbbell2(aes(
    x = value
    , y = model
    , color = metric
  )
  , .list_labs = 
    list(
      title = paste('Similarity Comparison:', df_input$occupation, 'vs', unique(df_match$occupation))
      , subtitle = paste0('How compatible is ',df_input$occupation,' with ',unique(df_match$occupation),'?')
      , x = 'Professional Compatibility Scores (%)'
      , y = NULL
      , color = NULL 
    )
  
  , .reorder_fct = T
  , .reorder_desc = T
  , .chr_manual.pal = 
    c('yellow','blue')
  , .list_axis.x.args =
    list(
      limits = c(0,1)
      , breaks = seq(0,1,length.out = 7)
    )
  , .fun_format.x = percent
  )

# - Compatibility with Janitors -------------------
df_models.long %>% 
  mutate(
    interchangeability = 
      fun_interchangeability(
        similarity
      )
  ) %>%
  filter(str_detect(
    str_to_lower(
      occupation
    ), 'waitress'
  )) %>%
  pivot_longer(
    cols = where(is.numeric)
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  mutate(
    metric = 
      str_to_title(metric)
  ) -> df_match

df_match %>%
  fun_plot.dumbbell2(aes(
    x = value
    , y = model
    , color = metric
  )
  , .list_labs = 
    list(
      title = paste('Similarity Comparison:', df_input$occupation, 'vs', unique(df_match$occupation))
      , subtitle = paste0('How compatible is ',df_input$occupation,' with ',unique(df_match$occupation),'?')
      , x = 'Professional Compatibility Scores (%)'
      , y = NULL
      , color = NULL 
    )
  
  , .reorder_fct = T
  , .reorder_desc = T
  , .chr_manual.pal = 
    c('yellow','blue')
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

# - Interchangeability variance ----------------------------------------------------------
df_models.long %>%
  mutate(
    interchangeability = 
      fun_interchangeability(
        similarity
      )
  ) %>% 
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
      sd(interchangeability)
    , stdev.wgt = 
      Hmisc::wtd.var(
        interchangeability
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
      title = paste('Interchangeability Standard Deviation for Each Model:', df_input$occupation)
      , subtitle = 'Which models yield more dispersed interchangeability scores?'
      , x = 'Standard Deviation of Professional Interchangeability Scores (%)'
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

# - Employability ----------------------------------------------------------
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
  pivot_longer(
    cols = everything()
    , names_to = 'model'
    , values_to = 'employability'
  ) %>% 
  mutate(
    employability.fct =
      cut(
        employability
        , breaks = 5
        , labels = F
      ) %>%
      recode(
        '5' = 'Very High Employability'
        , '4' = 'High Employability'
        , '3' = 'Medium Employability'
        , '2' = 'Low Employability'
        , '1' = 'Very Low Employability'
      ) %>%
      factor(
        levels =
          c(
            'Very High Employability'
            , 'High Employability'
            , 'Medium Employability'
            , 'Low Employability'
            , 'Very Low Employability'
          )
      )
  ) -> df_employability

df_employability %>% 
  fun_plot.lollipop(aes(
    x = model
    , y = employability
    , label = percent(employability, .01)
    , color = employability.fct
  )
  , .list_labs = 
    list(
      x = NULL
      , y = 'Employability Scores (%)'
      , title = 'Employability Comparison of Different Matching Models'
      , subtitle = paste0('Which percentage of available job posts are suitable for ', df_input$occupation, '?')
      , color = NULL
    )
  , .chr_manual.pal = 
    set_names(
      viridis::viridis(5, direction = -1)
      , c(
        'Very High Employability'
        , 'High Employability'
        , 'Medium Employability'
        , 'Low Employability'
        , 'Very Low Employability'
      )
    )
  , .list_axis.y.args = 
    list(
      limits = c(0,1)
      , breaks = seq(0,1, length.out = 7)
    )
  , .fun_format.y = percent
  ) #+ 
# annotate(
#   x = ceiling(nrow(df_employability) / 3)
#   , y = 0.9 * max(as.numeric(df_employability$employability))
#   , geom = 'text'
#   , label = 
#     str_wrap(
#       'The old models, which are still in use, are much slower compared to the weighted knn revised algorithm, as well as some regression approches.
#       Concerning these newer matching models, we can see they generally run in less than 1s.
#       However, Beta, Logit, Probit, and Tobit regressions are slow.'
#       , width = 50
#     )
# )

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

# # - Employability benchmarks ----------------------------------------------
# c(
#   "High school diploma or equivalent"
#   , "No formal educational credential"
# ) -> chr_education.low
# 
# df_occupations %>% 
#   ggplot(aes(
#     x = annual_wage_2021
#     , weight = employment2
#   )) + 
#   geom_density() + 
#   geom_textvline(
#     xintercept = 
#       weighted.mean(
#         x = df_occupations$
#           annual_wage_2021
#         , w = df_occupations$
#           employment2
#       )
#     , label = 
#       'Sample-Weighted Average Wage'
#   )
# 
# c(
#   df_occupations %>% 
#     mutate(
#       workforce = 
#         sum(employment2)
#     ) %>% 
#     filter(
#       annual_wage_2021 <= 
#         weighted.mean(
#           x = df_occupations$
#             annual_wage_2021
#           , w = df_occupations$
#             employment2
#         )
#     ) %>% 
#     reframe(
#       pct_wages.low = 
#         sum(employment2) / 
#         unique(workforce)
#     )
#   
#   , df_occupations %>% 
#     mutate(
#       workforce = 
#         sum(employment2)
#     ) %>% 
#     filter(
#       entry_level_education %in%
#         chr_education.low
#     ) %>% 
#     reframe(
#       pct_education.low = 
#         sum(employment2) / 
#         unique(workforce)
#     )
# )

# - Interpretability ------------------------------------------------------
df_models.long %>% 
  relocate(
    model
  ) %>% 
  mutate(
    `I(s)` = 
      fun_interchangeability(
        similarity
      ) %>% round(4)
  ) -> df_models.long

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
  filter(
    occupation != 
      df_input$
      occupation
  ) %>% 
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
  # filter(str_detect(
  #   str_to_lower(
  #     model
  #   ), 'bvls'
  # )) %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    ), 'econo'
  )) %>% 
  print(n = nrow(.))

df_models.long %>%
  filter(
    occupation != 
      df_input$
      occupation
  ) %>% 
  group_by(model) %>% 
  arrange(
    similarity
  ) %>%
  slice(1) %>% 
  ungroup() %>% 
  arrange(desc(
    similarity
  ))

df_occupations %>% 
  select(
    entry_level_education
    , education_years
  ) %>% 
  unique() %>% 
  arrange(
    education_years
  ) %>% 
  mutate(
    degree_interchangeability = 
      fun_interchangeability(
        education_years / 
          nth(education_years, 3)
        , .dbl_scaling = 
          nth(education_years, 3)
      ) %>% 
      pmin(1) %>%
      round(4)
  ) -> dsdsds

dsdsds %>% 
  fun_plot.line(aes(
    x = education_years
    , y = degree_interchangeability
  )
  , .list_labs = 
    list(
      title = paste(str_to_title(nth(dsdsds$entry_level_education, 3)), 'vs Other Degrees')
      , subtitle = 'Interchangeability of years of education'
      , x = 'Years of Education'
      , y = 'Degree Interchangeability'
    )
  , .fun_format.x = number
  , .fun_format.y = percent
  )


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
  group_by(model) %>% 
  arrange(desc(
    similarity
  )) %>% 
  slice(
    1:10
    , (n()-10+1):n()
  ) %>% 
  ungroup() %>%
  filter(
    model %in% 
      c('bvls.wgt', 'logit.wgt', 'pearson.wgt')
  ) %>% 
  print(n = nrow(.))

df_models.long %>%
  split(.$model) %>%
  map(
    ~ .x %>% 
      arrange(desc(
        similarity
      ))
  ) -> list_models

# df_models.long %>% 
#   filter(
#     model == 'bvls.wgt'
#   ) %>% 
#   ggplot(aes(
#     x = similarity
#     , y = model
#     # , fill = stat(quantile)
#   )) + 
#   geom_density_ridges(
#     quantile_lines = T
#     # , quantiles = c(0.5)
#     , quantiles = 2
#   , alpha = 0.7
#   ) + 
#   scale_x_continuous(
#     limits = c(0,1)
#     , breaks = seq(0,1,0.25)
#     , labels = percent
#   ) + 
#   theme_ridges()



# list_models %>% 
#   map(
#     ~ .x %>%
#       filter(str_detect(
#         str_to_lower(
#           occupation
#         ), 'statisticians'
#       )) %>% 
#       arrange(desc(2))
#   )

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
    'bvls.wgt'
    , 'logit.wgt'
    , 'probit.wgt'
    # , 'pearson.wgt'
  ) 
  , ok = c(
    'tobit'
    , 'bvls'
    , 'logit'
    , 'probit'
    # , 'pearson'
    # , 'pearson.wgt'
    # , 'knn.wgt'
    # , 'knn.0'
    # , 'knn.17'
    # , 'knn.100' 
  ) 
  , shit = c(
    'pearson'
    # , 'pearson.wgt'
    , 'knn.wgt'
    , 'knn.0'
    , 'knn.17'
    , 'knn.100'
    , 'beta'
    , 'beta.wgt'
  )
)
