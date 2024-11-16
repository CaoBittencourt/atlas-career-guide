# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
  , 'caret' #ML algorithms
  , 'weights' #Weighted correlation
  , 'Hmisc' #Weighted variance
  , 'MASS' #Simulate correlated data
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

select <- dplyr::select

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# [FUNCTIONS] -------------------------------------------------------------
# - Generate fake data ----------------------------------------------------
fun_fake_data <- function(
    df_data, 
    dbl_scale_ub = NULL, 
    dbl_scale_lb = NULL, 
    dbl_weights = NULL
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." = 
      all(
        is.data.frame(df_data),
        df_data %>% 
          map_lgl(is.numeric) %>% 
          any()
      )
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." = 
      any(
        is.null(dbl_weights),
        all(
          is.numeric(dbl_weights),
          length(dbl_weights) ==
            nrow(df_data)
        )
      )
  )
  
  stopifnot(
    "'dbl_scale_ub' must be either NULL or numeric." = 
      any(
        is.null(dbl_scale_ub),
        is.numeric(dbl_scale_ub)
      )
  )
  
  stopifnot(
    "'dbl_scale_lb' must be either NULL or numeric." = 
      any(
        is.null(dbl_scale_lb),
        is.numeric(dbl_scale_lb)
      )
  )
  
  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  if(!length(dbl_weights)){
    
    rep(1, nrow(df_data)) ->
      dbl_weights
    
  }
  
  df_data %>% 
    select(where(
      is.numeric
    )) -> df_data
  
  # Descriptive statistics
  df_data %>%
    mutate(
      .before = 1
      , wgt = dbl_weights
    ) %>%
    pivot_longer(
      cols = -wgt,
      names_to = 'item',
      values_to = 'item_score'
    ) %>% 
    group_by(item) %>% 
    reframe(
      wgt.sd =
        sqrt(wtd.var(
          item_score,
          wgt,
          na.rm = T
        )),
      wgt.mean = 
        wtd.mean(
          item_score, 
          wgt, 
          na.rm = T
        )
    ) -> df_data_stats
  
  # # Correlation
  # wtd.cors(
  #   df_data
  #   , weight =
  #     dbl_weights
  # ) -> mtx_data_cors
  
  # Generate fake data
  set.seed(19)
  
  # 5 * nrow(df_data) -> int_fake_data
  3 * nrow(df_data) -> int_fake_data
  
  # Statistically define "off the charts"
  # Bounded data
  # Unbounded data
  
  # Correlated, but off the charts data
  
  # c(
  #   runif(
  #     n = 50,
  #     min = 0,
  #     max = 0.5
  #   ),
  #   runif(
  #     n = 50,
  #     min = 1.5,
  #     max = 2
  #   )
  # ) -> dbl_levels
  # 
  # map(
  #   .x = dbl_levels,
  #   ~ mvrnorm(
  #     n = ceiling((int_fake_data / 2) / length(dbl_levels)),
  #     mu = df_data_stats$wgt.mean * .x,
  #     Sigma = mtx_data_cors,
  #   ) %>% 
  #     as_tibble()
  # ) %>% 
  #   list_rbind() -> 
  #   df_fake_data
  # 
  # # 0.5 * mtx_data_cors ->
  # map(
  #   runif(
  #     nrow(mtx_data_cors),
  #     min = 0.5,
  #     max = 1
  #   ),
  #   function(.x){
  #     
  #     .x * 
  #       mtx_data_cors ->
  #       mtx_x
  #     
  #     diag(mtx_x) <- 1
  #     
  #     pmin(mtx_x, 1) -> 
  #       mtx_x
  #     
  #     pmax(mtx_x, -1) -> 
  #       mtx_x
  #     
  #     return(mtx_x)
  #     
  #   }
  # # ) %>% 
  # ) -> dsds
  # return(dsds)
  # stop()
  #   map(
  #     ~ 
  #       mvrnorm(
  #         n = ceiling(int_fake_data / 2),
  #         mu = df_data_stats$wgt.mean,
  #         Sigma = .x
  #       )
  #     
  #   ) %>% 
  #   list_rbind() ->
  #   df_fake_data
  
  # mvrnorm(
  #   n = ceiling(int_fake_data / 2),
  #   mu = df_data_stats$wgt.mean,
  #   Sigma = mtx_data_cors
  # ) -> df_fake_data
  
  # map(
  #   .x = dbl_levels,
  #   ~ mvrnorm(
  #     n = ceiling(int_fake_data / 2),
  #     mu = df_data_stats$wgt.mean * .x,
  #     Sigma = mtx_data_cors
  #   )
  # ) -> df_fake_data
  
  # Uncorrelated data
  # Working with row shuffling and 3 * nrow(df_data)
  runif(
    n = 100,
    min = 0,
    max = 5
  ) -> dbl_levels
  
  # Working without row shuffling
  # c(
  #   runif(
  #     n = 10,
  #     min = 0,
  #     max = 0.125
  #   ),
  #   runif(
  #     n = 100,
  #     min = 0,
  #     max = 5
  #   )
  # ) -> dbl_levels
  
  # seq(0, 7, 0.5) -> 
  #   dbl_levels
  # 
  # map2(
  #   .x = 
  #     lag(dbl_levels) %>%
  #     replace_na(0),
  #   .y = dbl_levels,
  #   .f = ~ runif(
  #     n = 10,
  #     min = .x,
  #     max = .y
  #   )
  # ) %>% 
  #   list_c() -> 
  #   dbl_levels
  # 
  # unique(c(
  #   -dbl_levels,
  #   dbl_levels
  # )) -> 
  #   dbl_levels
  
  map(
    .x = dbl_levels,
    ~ mvrnorm(
      n = ceiling(int_fake_data / length(dbl_levels)),
      # n = ceiling((int_fake_data / 2) / length(dbl_levels)),
      mu = df_data_stats$wgt.mean * .x,
      # mu = df_data_stats$wgt.mean + 
      #   .x * df_data_stats$wgt.sd,
      Sigma = diag(nrow(df_data_stats))
    )
  ) -> df_fake_data
  
  do.call(
    rbind,
    df_fake_data
  ) %>%
    as_tibble() %>%
    set_names(
      df_data_stats$
        item
    ) -> df_fake_data
  
  df_fake_data %>%
    map(sample) %>%
    as_tibble() ->
    df_fake_data
  
  # Data wrangling
  if(length(dbl_scale_ub)){
    
    df_fake_data %>% 
      mutate(across(
        .cols = everything()
        ,.fns = 
          ~ pmin(.x, dbl_scale_ub)
      )) -> df_fake_data
    
    # df_fake_data %>% 
    #   bind_rows(
    #     as_tibble(rbind(
    #       rep(
    #         dbl_scale_ub, 
    #         ncol(
    #           df_fake_data
    #         ))
    #     )) %>% 
    #       set_names(names(
    #         df_fake_data
    #       ))
    #   ) -> df_fake_data
    
  }
  
  if(length(dbl_scale_lb)){
    
    df_fake_data %>% 
      mutate(across(
        .cols = everything()
        ,.fns = 
          ~ pmax(.x, dbl_scale_lb)
      )) -> df_fake_data
    
    # df_fake_data %>% 
    #   bind_rows(
    #     as_tibble(rbind(
    #       rep(
    #         dbl_scale_lb, 
    #         ncol(
    #           df_fake_data
    #         ))
    #     )) %>% 
    #       set_names(names(
    #         df_fake_data
    #       ))
    #   ) -> df_fake_data
    
  }
  
  # Bind data
  df_data %>% 
    mutate(
      .before = 1
      , class = 'real'
    ) %>% 
    bind_rows(
      df_fake_data %>% 
        mutate(
          .before = 1
          , class = 'fake'
        )
    ) %>%
    mutate(
      class = factor(class)
    ) -> df_fake_data
  
  # Fake weights
  df_fake_data %>% 
    mutate(
      .after = 1
      , wgt = c(
        dbl_weights
        , rep(
          # max(dbl_weights)
          mean(dbl_weights)
          , nrow(df_fake_data) -
            nrow(df_data)
        ))
    ) -> df_fake_data
  
  # Output
  return(df_fake_data)
  
}

# - Train classification model -------------------------------------------
fun_train_classification <- function(
    df_data,
    dbl_weights = NULL,
    int_folds = 4
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." = 
      all(
        is.data.frame(df_data),
        df_data %>% 
          map(is.numeric) %>% 
          any()
      )
  )
  
  stopifnot(
    "'int_folds' must be an integer." = 
      is.numeric(int_folds)
  )
  
  # Data wrangling
  ceiling(int_folds) -> int_folds
  
  # Logistic regression with cross validation
  train(
    class ~  .,
    data = 
      df_data %>% 
      select(where(
        is.numeric
      ), class), 
    trControl = 
      trainControl(
        method = 'cv',
        number = int_folds,
        classProbs = T
      ),
    weights = dbl_weights,
    method = 'glm',
    family = binomial(link = 'logit'),
    metric = 'Accuracy'
  ) -> model_classification
  
  # Output
  return(model_classification)
  
}

# - Evaluate classification model -----------------------------------------
fun_evaluate_classification <- function(pct_accuracy){
  
  # Arguments validation
  stopifnot(
    "'pct_accuracy' must be a percentage." =
      all(
        is.numeric(pct_accuracy),
        pct_accuracy <= 1,
        pct_accuracy >= 0
      )
  )
  
  # Data wrangling
  pct_accuracy[[1]] -> pct_accuracy
  
  # Evaluate model accuracy
  case_match(
    .x = findInterval(
      pct_accuracy,
      c(seq(0, 0.9, 0.1), 0.95)
    ),
    c(1:5) ~ 'worst than a coin flip',
    6 ~ 'coin flip',
    7 ~ 'poor',
    8 ~ 'ok',
    9 ~ 'good',
    10 ~ 'very good',
    11 ~ 'probably overfitting'
  ) -> chr_accuracy_evaluation
  
  # Output
  return(chr_accuracy_evaluation)
  
}

# - Fraud detection model -------------------------------------------------
fun_fraud_model <- function(
    df_data,
    dbl_scale_ub = NULL,
    dbl_scale_lb = NULL,
    dbl_weights = NULL
){
  
  # Arguments validation via helper functions
  
  # Generate fake data
  fun_fake_data(
    df_data = df_data,
    dbl_scale_ub = dbl_scale_ub,
    dbl_scale_lb = dbl_scale_lb,
    dbl_weights = dbl_weights
  ) -> df_fake_data
  
  rm(df_data)
  rm(dbl_scale_ub)
  rm(dbl_scale_lb)
  rm(dbl_weights)
  
  # Train classification model (cross validation)
  fun_train_classification(
    df_data = 
      df_fake_data %>% 
      select(!wgt),
    dbl_weights = 
      df_fake_data$wgt
  ) -> model_fraud
  
  # Evaluate classification model
  fun_evaluate_classification(
    pct_accuracy =
      model_fraud$
      results$
      Accuracy
  ) -> chr_accuracy_evaluation
  
  # Output
  return(list(
    'model' = model_fraud,
    'accuracy' = model_fraud$results$Accuracy,
    'evaluation' = chr_accuracy_evaluation,
    'data' = df_fake_data
  ))
  
}

# - Fraud detection function ----------------------------------------------
fun_fraud_detect <- function(df_query, model_fraud){
  
  # Arguments validation
  stopifnot(
    "'model_fraud' must be a 'train.formula' object generated with the 'train' function from the 'caret' package." =
      any(
        class(model_fraud) == 'train',
        class(model_fraud) == 'train.formula'
      ))
  
  stopifnot(
    "'df_query' must be a data frame with the same variables as 'model_fraud'." = 
      all(
        is.data.frame(df_query),
        all(
          model_fraud$
            coefnames %in% 
            names(df_query)
        ))
  )
  
  # Predict fraud using trained classification model
  predict(
    object = model_fraud,
    newdata = 
      df_query %>% 
      select(all_of(
        model_fraud$
          coefnames
      )),
    type = 'prob'
  ) %>% 
    as_tibble() -> 
    df_predicted
  
  # Data wrangling
  df_query %>% 
    select(!all_of(
      model_fraud$
        coefnames
    )) %>% 
    bind_cols(
      df_predicted %>% 
        round(4)
    ) %>% 
    relocate(
      !where(is.numeric),
      names(df_predicted),
      everything()
    ) -> df_predicted
  
  # Output
  return(df_predicted)
  
}

# # [TEST] ------------------------------------------------------------------
# # - Data ------------------------------------------------------------------
# read_csv(
#   'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
# ) -> df_occupations
# 
# read_csv(
#   'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
# ) -> df_input
# 
# df_occupations %>% 
#   select(
#     occupation,
#     employment2,
#     ends_with('.l')
#   ) -> df_occupations
# 
# # - Run model -------------------------------------------------------------
# fun_fraud_model(
#   df_data = 
#     df_occupations %>%
#     select(names(
#       df_input
#     )),
#   dbl_scale_ub = 100,
#   dbl_scale_lb = 0,
#   dbl_weights = 
#     df_occupations$
#     employment2
# ) -> list_fraud_model
# 
# list_fraud_model$accuracy
# list_fraud_model$evaluation
# 
# # - Test model ------------------------------------------------------------
# fun_fraud_detect(
#   df_query = 
#     df_input,
#     # df_input[-1]*0,
#     # df_occupations %>%
#     # slice_sample(n = 1) %>%
#     # mutate(across(
#     #   .cols = -1,
#     #   .fns = ~ .x * runif(1,1,1.5)
#     # )),
#     
#     # df_occupations %>%
#     # slice_sample(n = 1),
#     # # slice_tail(n = 500),
#   model_fraud = 
#     list_fraud_model$
#     model
# )
