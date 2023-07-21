# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
# [FUNCTIONS] -------------------------------------------------------------
# - Generate fake data ----------------------------------------------------
fun_fake_data <- function(df_data, dbl_weights = NULL){
  
  # Arguments validation
  
  # Data wrangling
  
  # Descriptive statistics
  
  # Generate fake data
  # Uncorrelated data
  # Reversely correlated data
  # Correlated, but off the charts data
  
  # Data wrangling
  
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
  
  # Test predictions
  
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
# - Run model -------------------------------------------------------------
# - Test model ------------------------------------------------------------
