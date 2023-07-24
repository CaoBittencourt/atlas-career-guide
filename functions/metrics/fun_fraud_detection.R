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
      wgt.mean = 
        wtd.mean(
          item_score, 
          wgt, 
          na.rm = T
        ),
      wgt.sd = 
        sqrt(wtd.var(
          item_score, 
          wgt, 
          na.rm = T
        ))
    ) -> df_data_stats
  
  # Correlation
  wtd.cors(
    df_data
    , weight =
      dbl_weights
  ) -> mtx_data_cors
  
  # Generate fake data
  nrow(df_data) -> int_fake_data
  
  # Statistically define "off the charts"
  # Bounded data
  # Unbounded data
  
  # Correlated, but off the charts data
  set.seed(19)
  
  c(
    runif(
      n = 50,
      min = 0,
      max = 0.5
    ),
    runif(
      n = 50,
      min = 1.5,
      max = 2
    )
  ) -> dbl_levels
  
  map(
    .x = dbl_levels,
    ~ mvrnorm(
      n = round((int_fake_data / 2) / length(dbl_levels)),
      mu = df_data_stats$wgt.mean * .x,
      Sigma = mtx_data_cors,
    ) %>% 
      as_tibble()
  ) %>% 
    list_rbind() -> 
    df_fake_data
  
  # Uncorrelated data
  runif(
    n = 100, 
    min = 0.5, 
    max = 2.5
  ) -> dbl_levels
  
  map(
    .x = dbl_levels,
    ~ mvrnorm(
      n = round((int_fake_data / 2) / length(dbl_levels)),
      mu = df_data_stats$wgt.mean * .x,
      Sigma = diag(nrow(df_data_stats)),
    ) %>% 
      as_tibble()
  ) %>% 
    list_rbind() %>%
    set_names(names(
      df_fake_data
    )) %>% 
    bind_rows(
      df_fake_data
    ) %>% 
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
    
  }
  
  if(length(dbl_scale_lb)){
    
    df_fake_data %>% 
      mutate(across(
        .cols = everything()
        ,.fns = 
          ~ pmax(.x, dbl_scale_lb)
      )) -> df_fake_data
    
  }
  
  # Bind data
  df_data %>% 
    mutate(
      .before = 1
      , fake = 0
    ) %>% 
    bind_rows(
      df_fake_data %>% 
        mutate(
          .before = 1
          , fake = 1
        )
    ) %>%
    mutate(
      fake = factor(fake)
    ) -> df_fake_data
  # ) -> df_fake_data
  
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
fun_train_classification <- function(df_data, int_labels, dbl_weights = NULL){
  
  # Arguments validation
  
  # Data wrangling
  
  # Logistic regression with cross validation
  
  
  # Output
  return(model_classification)
  
}

# - Test classification model --------------------------------------------
fun_test_classification <- function(df_data, int_labels, model_classification){
  
  # Arguments validation
  
  # Data wrangling
  
  # Confusion matrix
  
  # Output
  return(model_classification)
  
}

# - Evaluate classification model -----------------------------------------


# - Fraud detection model -------------------------------------------------
fun_fraud_model <- function(df_data, dbl_weights = NULL){
  
  # Arguments validation
  
  # Data wrangling
  
  # Generate fake data
  fun_fake_data(
    df_data = df_data, 
    dbl_weights = dbl_weights
  ) -> df_data_fraud
  
  # Label data (fraud = 1, legit = 0)
  c(
    rep(0, nrow(df_data)),
    rep(1, nrow(df_data_fraud))
  ) -> int_labels_fraud
  
  # Add weights for fake data
  # dbl_weights ... -> dbl_weights_fraud
  
  # Bind data
  rbind(
    df_data, 
    df_data_fraud
  ) -> df_data_fraud
  
  rm(df_data)
  
  # Train classification model (cross validation)
  fun_train_classification(
    df_data = df_data_fraud,
    int_labels = int_labels_fraud,
    dbl_weights = dbl_weights_fraud
  ) -> model_fraud
  
  # Test classification model (cross validation)
  fun_test_classification(
    df_data = df_data_fraud,
    int_labels = int_labels_fraud,
    model_classification = model_fraud
  ) -> dsds
  
  # Evaluate classification model
  
  # Output
  return(list(
    'model' = model_fraud,
    # 'precision' = ,
    # 'evaluation' = ,
  ))
  
}

# - Fraud detection function ----------------------------------------------
fun_fraud_detect <- function(df_query, model_fraud){
  
  # Arguments validation
  
  # Data wrangling
  
  # Predict fraud using trained classification model
  predict.glm(
    object = model_fraud,
    newdata = df_query,
  ) -> list_predicted
  
  # Data wrangling
  
  # Output
  return(dbl_prob_fake)
  
}

# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
) -> df_input

df_occupations$
  employment2 ->
  dbl_weight

df_occupations %>% 
  select(
    occupation,
    ends_with('.l')
  ) -> df_occupations

# - Run model -------------------------------------------------------------
# - Test model ------------------------------------------------------------
fun_fake_data(
  df_data = df_occupations,
  dbl_scale_ub = 100,
  dbl_scale_lb = 0,
  dbl_weights = dbl_weight
) -> df_fake_data

train(
  fake ~ .,
  data = 
    df_fake_data %>% 
    select(!wgt), 
  trControl = 
    trainControl(
      method = 'cv',
      # method = 'repeatedcv',
      number = 10#,
      # search = 'random'
      # repeats = 3
    ),
  weights = df_fake_data$wgt,
  # method = 'bayesglm',
  # method = 'rf',
  # tuneLength = 15,
  method = 'glm',
  # method = 'lasso',
  family = binomial(link = 'logit'),
  metric = 'Accuracy'
) -> model_fraud

model_fraud

df_fake_data %>% 
  select(!wgt) %>%
  filter(
    fake == '1'
  ) %>% 
  slice_sample(
    n = 1
  ) %>% 
  select(!fake)

wtd.cors(
  df_occupations[-1]
  , weight = dbl_weight
) -> dsds

runif(200, min = 0, max = 100) %>% 
  t() %>% 
  as_tibble() %>% 
  set_names(colnames(
    dsds
  )) -> lalala

fun_similarity(
  df_data_rows = as_tibble(rowMeans(lalala) * (1 + dsds)/2),
  # df_query_rows = lalala,
  df_query_rows = slice_sample(df_occupations, n = 1),
  chr_method = 'pearson',
  mtx_weights = matrix(1, nrow = nrow(dsds), ncol = ncol(dsds)),
  dbl_scale_ub = 100,
  dbl_scale_lb = 0
) -> dsdsds

dsdsds$df_similarity$similarity











