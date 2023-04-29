# --- SETUP ------------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
c(
  'fastDummies' #Dummy variables
  , 'sandwich', 'lmtest' #Diagnósticos e ajustes
  , 'ivreg', 'minpack.lm' #Modelos 
  , 'broom', 'tidyverse', 'Hmisc' #Manipulação de dados
) -> pkg

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# --- FUNCTIONS -----------------------------------------------------------
# ARGUMENT NAMES AS VECTOR (AUXILIARY FUNCTION) ----------------------------------------------------------
fun_args.aux <- function(args){
  
  # Get arguments
  enexpr(args) -> args
  
  # Argument list
  if(length(args) == 1){
    
    as.list(args) -> list_args
    
  } else if(length(list_args)) {
    
    as.list(args)[-1] -> list_args
    
  }
  
  # Output
  if(length(list_args)){
    
    return(sapply(list_args, quo_name))
    
  } else {
    
    return(NULL)
    
  }
  
}

# ARGUMENT NAMES AS VECTOR (MAIN FUNCTION) ----------------------------------------------------------
fun_args <- function(df, args){
  
  if(
    fun_args.aux(!!enexpr(args)) %>% 
    as.character() %>% 
    intersect(names(df)) %>%
    length()
  ){
    
    fun_args.aux(!!enexpr(args)) %>% 
      as.character() %>% 
      intersect(names(df)) %>% 
      return()
    
  } else { 
    
    args %>% 
      enexpr() %>% 
      eval() %>% 
      return()
  }
  
}

# R SQUARED ----------------------------------------------------------------------
fun_r2 <- function(
    .int_vars.independent
    , .lgc_intercept = T
    , .dbl_observations
    , .dbl_fitted
){
  
  # Total sum of squares
  sum(
    (
      .dbl_observations - as.numeric(.lgc_intercept) * mean(.dbl_observations)
    ) ^ 2
  ) -> sst
  
  # Residual sum of squares
  sum(
    (
      .dbl_observations - .dbl_fitted
    ) ^ 2
  ) -> sse
  
  # R2
  1 - (sse / sst) -> r2
  
  # Adjusted R2
  1 - (1 - r2) * 
    (length(.dbl_observations) - 1) / 
    (length(.dbl_observations) -
       .int_vars.independent - 
       as.numeric(.lgc_intercept)
    ) -> r2.adj
  
  # Output
  return(
    list(
      'r2' = r2
      , 'r2.adjusted' = r2.adj
    )
  )
  
}

# ROOT MEAN SQUARED ERROR ----------------------------------------------------------------------
fun_rmse <- function(
    .dbl_observations
    , .dbl_fitted
    , .dbl_weights = NULL
){
  
  # RMSE
  sqrt(
    wtd.mean(
      (.dbl_observations - .dbl_fitted) ^ 2
      , .dbl_weights
    )
  ) -> dbl_rmse
  
  # Normalized RMSE
  dbl_rmse / diff(range(.dbl_observations)) -> dbl_nrmse
  
  # Output
  return(list(
    'rmse' = dbl_rmse
    , 'nrmse' = dbl_nrmse
  )) 
  
}

# MEAN ABSOLUTE ERROR ----------------------------------------------------------------------
fun_mae <- function(
    .dbl_observations
    , .dbl_fitted
    , .dbl_weights = NULL
){
  
  # MAE
  wtd.mean(
    abs(.dbl_observations - .dbl_fitted)
    , .dbl_weights
  ) -> dbl_mae
  
  # Normalized MAE
  dbl_mae / diff(range(.dbl_observations)) -> dbl_nmae
  
  # Output
  return(list(
    'mae' = dbl_mae
    , 'nmae' = dbl_nmae
  )) 
  
}

# FITTED MEAN ----------------------------------------------------------------------
fun_fitted.mean <- function(
    .dbl_observations
    , .dbl_fitted
    , .dbl_weights = NULL
){
  
  # Fitted mean
  wtd.mean(.dbl_fitted, .dbl_weights) -> dbl_fitted.mean
  
  # Original data mean
  wtd.mean(.dbl_observations, .dbl_weights) -> dbl_data.mean
  
  # Normalized fitted mean
  dbl_fitted.mean / dbl_data.mean -> dbl_fitted.nmean
  
  # Mean fitted to data ratio
  wtd.mean(.dbl_fitted / .dbl_observations, .dbl_weights) -> dbl_fitted.ratio.mean
  
  # Output
  return(list(
    'fitted.mean' = dbl_fitted.mean
    , 'data.mean' = dbl_data.mean
    , 'fitted.mean.norm' = dbl_fitted.nmean
    , 'fitted.ratio.mean' = dbl_fitted.ratio.mean
  )) 
  
}

# FITTED SD ----------------------------------------------------------------------
fun_fitted.sd <- function(
    .dbl_observations
    , .dbl_fitted
    , .dbl_weights = NULL
){
  
  # Weighted var
  wtd.var(.dbl_fitted, .dbl_weights, method = 'ML') -> dbl_fitted.var
  
  # Fitted sd
  sqrt(dbl_fitted.var) -> dbl_fitted.sd
  
  # Weighted var
  wtd.var(.dbl_observations, .dbl_weights, method = 'ML') -> dbl_data.var
  
  # Fitted sd
  sqrt(dbl_data.var) -> dbl_data.sd
  
  
  # Output
  return(list(
    'fitted.sd' = dbl_fitted.sd
    , 'data.sd' = dbl_data.sd
  )) 
  
}

# # HETEROSKEDASTICITY DIAGNOSIS AND TREATMENT ------------------------------
# fun_heteroskedasticity.lm <- function(
    #     .model 
#     , .chr_type = 'HC3'
#     , .dbl_significance = 0.05
# ){
#   
#   if(bptest(.model)$p.value <= .dbl_significance){
#     
#     .model %>%
#       coeftest(
#         vcov = vcovHC(.model, type = .chr_type)
#       ) %>% 
#       return()
#     
#   }
#   else {
#     
#     return(.model)
#     
#   }
#   
# }
# 
# # read_csv('C:/Users/Cao/Documents/Github/Atlas-Research/Data/occupations.csv') %>%
# #   mutate(
# #     across(
# #       .cols = ends_with('.l')
# #       ,.fns = function(x){100*x}
# #     )
# #   ) -> df_occupations
# 
# df_occupations %>% 
#   mutate(
#     annual_wage_2021 = annual_wage_2021 / 12
#   ) %>% 
#   fun_lm(
#     .sym_vars.dependent = 'annual_wage_2021'
#     , .sym_vars.independent = 
#       df_occupations %>% 
#       select(ends_with('.l')) %>% 
#       names()
#     , .dbl_lower.bounds = 0
#     , .lgc_intercept = F
#     , .lgc_diagnostics = F
#   ) -> mdl_kcost
# 
# 
#   dsds %>% 
#     summary()
# 
# mdl_kcost$model.tidy
# mdl_kcost$r2
# mdl_kcost$r2.adjusted
# 
# 
# fun_heteroskedasticity.nls <- function(.nls_model){
#   
#   b <- coef(.nls_model)
#   
#   m <- .nls_model$m
#   
#   resid <- m$resid()
#   
#   hmat <- m$gradient()
#   
#   x <- hmat
#   
#   y <- resid + hmat %*% b
#   
#   lm(y ~ x + 0) %>%
#     vcovHC() %>% 
#     return()
#   
# }
# 
# 
# vcov(mdl_kcost$model.fit) = fun_heteroskedasticity.nls(mdl_kcost$model.fit)
# 
# fun_heteroskedasticity2 <- function(
    #     .model 
#     , .chr_type = 'HC3'
#     , .dbl_significance = 0.05
# ){
#   
#   if(bptest(.model)$p.value <= .dbl_significance){
#     
#     .model %>%
#       coeftest(
#         vcov = vcovHC(.model, type = .chr_type)
#       )
#     
#   }
#   else {
#     
#     return(.model)
#     
#   }
#   
# }
# 
# bptest(mdl_kcost$model.fit)
# b <- coef(mdl_kcost$model.fit)
# m <- mdl_kcost$model.fit$m
# resid <- m$resid()
# hmat <- m$gradient()
# fakex <- hmat
# fakey <- resid + hmat %*% b
# lmout <- lm(fakey ~ fakex + 0)
# vcovHC(lmout)
# 
# 
# mdl_kcost$model.fit 
#   fun_heteroskedasticity()

# GENERIC REGRESSION FUNCTION: NNLS & BVLS ---------------------------------------------
fun_lm <- function(
    .df_data
    , .sym_vars.dependent
    , .sym_vars.independent
    , .sym_vars.dummies = c()
    , .sym_vars.instrumental = c()
    , .sym_vars.weights = NULL
    , .lgc_intercept = T
    , .dbl_upper.bounds = c()
    , .dbl_lower.bounds = c()
    , .lgc_diagnostics = F
    , .lgc_dummy.remove = F
){
  
  
  # Dummy variables for non-numerical data
  if(length(.sym_vars.dummies)){
    
    .df_data %>% 
      select(all_of(.sym_vars.dummies)) %>% 
      names() -> chr_dummies
    
    .df_data %>% 
      dummy_cols(
        select_columns = chr_dummies
        , remove_selected_columns = T
        , remove_most_frequent_dummy = .lgc_dummy.remove
      ) -> .df_data
    
    .df_data %>% 
      select(starts_with(chr_dummies)) %>%
      names() -> chr_dummies
    
    c(.sym_vars.independent, chr_dummies) -> .sym_vars.independent
    
    if(length(.sym_vars.instrumental)){
      
      c(.sym_vars.instrumental, chr_dummies) -> .sym_vars.instrumental
      
    }
    
    
  }
  
  
  # # Argument names
  # if(
  #   fun_args.aux(!!enexpr(.sym_vars.independent)) %>% 
  #   as.character() %>% 
  #   intersect(names(.df_data)) %>%
  #   length()
  # ){
  #   
  #   fun_args.aux(!!enexpr(.sym_vars.independent)) %>% 
  #     as.character() %>% 
  #     intersect(names(.df_data)) %>% 
  #     return()
  #   
  # } else { 
  #   
  #   args %>% 
  #     enexpr() %>% 
  #     eval() %>% 
  #     return()
  # } -> .sym_vars.independent
  # 
  # # fun_args(.df_data, !!enexpr(.sym_vars.dependent)) -> .sym_vars.dependent
  # # 
  # # fun_args(.df_data, !!enexpr(.sym_vars.independent)) -> .sym_vars.independent
  # # 
  # # fun_args(.df_data, !!enexpr(.sym_vars.weights)) -> .sym_vars.weights
  # 
  # return(list(
  #   # .sym_vars.dependent
  #   .sym_vars.independent
  #   # , .sym_vars.weights
  # ))
  
  # fun_args.aux(!!enexpr(.sym_vars.dependent)) -> .sym_vars.dependent
  # 
  # fun_args.aux(!!enexpr(.sym_vars.independent)) -> .sym_vars.independent
  # 
  # fun_args.aux(!!enexpr(.sym_vars.weights)) -> .sym_vars.weights
  
  # Sample weights
  # NNLS with weights
  # => sqrt(weights) * independent variable = independent variable
  # => sqrt(weights) * dependent variable = dependent variable
  
  # Bounds
  if(length(.dbl_upper.bounds) == 1){
    
    rep(
      .dbl_upper.bounds
      , as.numeric(.lgc_intercept) + length(.sym_vars.independent)
    ) -> .dbl_upper.bounds 
    
  }
  
  if(length(.dbl_lower.bounds) == 1){
    
    rep(
      .dbl_lower.bounds
      , as.numeric(.lgc_intercept) + length(.sym_vars.independent)
    ) -> .dbl_lower.bounds 
    
  }
  
  
  # # Bounds
  #   if(!length(.dbl_upper.bounds)){
  #     
  #     rep(Inf, length(.sym_vars.independent)) -> .dbl_upper.bounds 
  #     
  #   } else if(length(.dbl_upper.bounds) == 1){
  #     
  #     rep(.dbl_upper.bounds, length(.sym_vars.independent)) -> .dbl_upper.bounds 
  #     
  #   }
  #   
  #   if(!length(.dbl_lower.bounds)){
  #     
  #     rep(-Inf, length(.sym_vars.independent)) -> .dbl_lower.bounds 
  #     
  #   } else if(length(.dbl_upper.bounds) == 1){
  #     
  #     rep(.dbl_lower.bounds, length(.sym_vars.independent)) -> .dbl_lower.bounds 
  #     
  #   }
  #   
  
  # Troublesome variable names
  make.names(.sym_vars.dependent) -> .sym_vars.dependent.names
  
  make.names(.sym_vars.independent) -> .sym_vars.independent.names
  
  make.names(names(.df_data)) -> names(.df_data)
  
  # .sym_vars.dependent %>% 
  #   str_replace_all(' ', '_') -> .sym_vars.dependent
  # 
  # .sym_vars.independent
  # str_replace_all(' ', '_') -> .sym_vars.independent
  
  # Methods of estimation
  if(length(.sym_vars.instrumental)){
    # Troublesome variable names
    make.names(.sym_vars.instrumental) -> .sym_vars.instrumental.names
    
    # .sym_vars.instrumental %>% 
    #   str_replace_all(' ', '_') -> .sym_vars.instrumental
    
    # 2SLS
    # Dependent variable
    .sym_vars.dependent.names[[1]] %>%
      paste('~') -> chr_formula.right
    
    if(!.lgc_intercept){
      
      paste0(chr_formula.right, '0 + ') -> chr_formula.right
      
    } 
    
    # Independent variables
    paste(
      .sym_vars.independent.names
      , collapse = '+'
    ) %>% 
      paste('|') %>% 
      paste(
        paste(
          .sym_vars.instrumental.names
          , collapse = ' + '
        )) -> chr_formula.left
    
    # Formula
    chr_formula.right %>%
      paste0(chr_formula.left) %>%
      as.formula() -> fml_formula
    
    # Run model
    ivreg(
      formula = fml_formula
      , data = .df_data
      , weights = 
        if(length(.sym_vars.weights)){
          .df_data[[make.names(.sym_vars.weights)]]
        } else { NULL }
    ) -> mdl_fit
    
  } else {
    # Other methods
    # Dependent variable
    .sym_vars.dependent.names[[1]] %>%
      paste('~') -> chr_formula.right
    
    if(.lgc_intercept){
      
      # paste0(chr_formula.right, '(Intercept) + ') -> chr_formula.right
      paste0(chr_formula.right, 'a + ') -> chr_formula.right
      
    } 
    
    # Independent variables
    paste0('b', 1:length(.sym_vars.independent.names), '*') %>%
      paste(.sym_vars.independent.names, collapse = '+') -> chr_formula.left
    
    # Formula
    chr_formula.right %>%
      paste0(chr_formula.left) %>%
      as.formula() -> fml_formula
    
    # Run model
    if(length(.sym_vars.weights)){
      
      nlsLM(
        formula = fml_formula
        , data = .df_data
        , weights = .df_data[[make.names(.sym_vars.weights)]]
        , upper = .dbl_upper.bounds
        , lower = .dbl_lower.bounds
        , control = list(scaleOffset = 1)
      ) -> mdl_fit 
      
    } else { 
      
      nlsLM(
        formula = fml_formula
        , data = .df_data
        , upper = .dbl_upper.bounds
        , lower = .dbl_lower.bounds
        , control = list(scaleOffset = 1)
      ) -> mdl_fit 
      
    }
    
    # # Run model
    # nlsLM(
    #   formula = fml_formula
    #   , data = .df_data
    #   , weights = 
    #     if(length(.sym_vars.weights)){
    #       .df_data[[.sym_vars.weights]]
    #     } else { NULL }
    #   , upper = .dbl_upper.bounds
    #   , lower = .dbl_lower.bounds
    #   , control = list(scaleOffset = 1)
    # ) -> mdl_fit
    # 
  }
  
  # Tidy model
  if(.lgc_intercept){
    
    c(
      '(Intercept)'
      , .sym_vars.independent
    ) -> chr_term
    
  } else { 
    
    .sym_vars.independent -> chr_term
    
  }
  
  mdl_fit %>% 
    broom::tidy() %>% 
    mutate(
      term = chr_term
      # , significance = 
      #   findInterval(
      #     x = p.value
      #     , vec = c(0, 0.001, 0.01, 0.05, 0.1, 1)
      #   ) %>% 
      #   recode(
      #     .default = ''
      #     , '4' = '.'
      #     , '3' = '*'
      #     , '2' = '**'
      #     , '1' = '***'
      #   )
    ) -> mdl_fit.tidy
  
  # Fitted values
  if(length(.sym_vars.instrumental)){
    
    as.numeric(mdl_fit$fitted.values) -> dbl_fitted
    
  } else { 
    
    as.numeric(mdl_fit$m$fitted()) -> dbl_fitted
    
  }
  
  # R squared
  fun_r2(
    .int_vars.independent = length(.sym_vars.independent.names)
    , .dbl_observations = .df_data %>% pull(.sym_vars.dependent.names[[1]])
    , .dbl_fitted = dbl_fitted
    , .lgc_intercept = .lgc_intercept
  ) -> list_r2
  
  # RMSE
  fun_rmse(
    .dbl_observations = .df_data %>% pull(.sym_vars.dependent.names[[1]])
    , .dbl_fitted = dbl_fitted
    , .dbl_weights = 
      if(length(.sym_vars.weights)){
        1 / .df_data[[make.names(.sym_vars.weights)]]
      } else { NULL }
  ) -> list_rmse
  
  # MAE
  fun_mae(
    .dbl_observations = .df_data %>% pull(.sym_vars.dependent.names[[1]])
    , .dbl_fitted = dbl_fitted
    , .dbl_weights = 
      if(length(.sym_vars.weights)){
        1 / .df_data[[make.names(.sym_vars.weights)]]
      } else { NULL }
  ) -> list_mae
  
  # Fitted mean
  fun_fitted.mean(
    .dbl_observations = .df_data %>% pull(.sym_vars.dependent.names[[1]])
    , .dbl_fitted = dbl_fitted
    , .dbl_weights = 
      if(length(.sym_vars.weights)){
        1 / .df_data[[make.names(.sym_vars.weights)]]
      } else { NULL }
  ) -> list_fitted.mean
  
  # Fitted sd
  fun_fitted.sd(
    .dbl_observations = .df_data %>% pull(.sym_vars.dependent.names[[1]])
    , .dbl_fitted = dbl_fitted
    , .dbl_weights =
      if(length(.sym_vars.weights)){
        1 / .df_data[[make.names(.sym_vars.weights)]]
      } else { NULL }
  ) -> list_fitted.sd
  
  # F test
  
  # Summary
  if(.lgc_diagnostics){
    
    mdl_fit %>% 
      summary(diagnostics = T) %>%
      print()
    
  }
  
  # Output
  return(list(
    'model.tidy' = mdl_fit.tidy
    , 'model.fit' = mdl_fit
    # , 'f.test'
    , 'r2' = list_r2
    , 'rmse' = list_rmse
    , 'mae' = list_mae
    , 'fitted.mean' = list_fitted.mean
    , 'fitted.sd' = list_fitted.sd
    , 'fitted' = dbl_fitted
    , 'data' = .df_data
  ))
  
}
