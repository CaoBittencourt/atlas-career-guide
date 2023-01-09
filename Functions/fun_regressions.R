# --- SETUP ------------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
)
install.packages('systemfit')
install.packages('ivreg')

# library(systemfit)
library(ivreg)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# TEST DATA ---------------------------------------------------------------
read_csv('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/bucket/occupations.csv') -> occupations

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

# HETEROSKEDASTICITY DIAGNOSIS AND TREATMENT ------------------------------
fun_heteroskedasticity <- function(
    .model 
    , .chr_type = 'HC3'
    , .dbl_significance = 0.05
){
  
  if(bptest(model)$p.value <= significance){
    
    model %>%
      coeftest(
        vcov = vcovHC(., type = .type)
      )
    
  }
  else
    model
}

# GENERIC REGRESSION FUNCTION: OLS & 2SLS ---------------------------------------------
iv.engel.rothbarth <- function(
    .df_data
    , .chr_regression.type = c(
      'ols', '2sls', 'nnls', 'bvls'
    )
    , .dbl_vars.dependent
    , .dbl_vars.independent
    , .dbl_vars.iv = c()
    , .dbl_vars.control = c()
    , .dbl_upper.bounds = c()
    , .dbl_lower.bounds = c()
    , .dbl_vars.weights = NULL
    , .lgc_diagnostics = F
){
  
  # Regression type 
  # NNLS with weights 
  # => sqrt(weights) * independent variable = independent variable
  # => sqrt(weights) * dependent variable = dependent variable
  
  # Sample weights
  
  # Dependent variable
  
  # Independent variables
  
  # Run model
  
  # R squared
  
  # F test
  
  # 
  
  # Output
  
}

# GENERIC REGRESSION FUNCTION: NNLS ---------------------------------------------
fun_regression.truncated <- function(
    .df_data
    , .dbl_vars.dependent
    , .dbl_vars.independent
    # , .dbl_vars.control = c()
    , .dbl_vars.weights = NULL
    , .lgc_diagnostics = F
){
  
  # Sample weights
  # NNLS with weights 
  # => sqrt(weights) * independent variable = independent variable
  # => sqrt(weights) * dependent variable = dependent variable
  if(length(.dbl_vars.weights)){
    
    .df_data %>%
      mutate(across(
        .cols = c(
          .dbl_vars.dependent
          , .dbl_vars.independent
        )
        ,.fns = function(x){x * sqrt(.dbl_vars.weights)}
      )) -> .df_data
    
  }
  
  # Dependent variable
  .dbl_vars.dependent %>%
    paste0('~') -> chr_formula.right
  
  # Independent variables
  .dbl_vars.independent %>%
    paste0(collapse = '+') -> chr_formula.left
  
  # Formula
  chr_formula.right %>%
    paste0(chr_formula.left) %>%
    as.formula() -> fml_formula
  
  # Run model
  
  
  # R squared
  
  # F test
  
  # 
  
  # Output
  
}

# GENERIC REGRESSION FUNCTION: NNLS & BVLS ---------------------------------------------
fun_regression.truncated <- function(
    .df_data
    , .sym_vars.dependent
    , .sym_vars.independent
    , .sym_vars.instrumental = c()
    , .lgc_intercept = T
    , .dbl_upper.bounds = c()
    , .dbl_lower.bounds = c()
    , .sym_vars.weights = NULL
    , .lgc_diagnostics = F
){
  
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
  if(!length(.dbl_upper.bounds)){
    
    rep(Inf, length(.sym_vars.independent)) -> .dbl_upper.bounds 
    
  } else if(length(.dbl_upper.bounds) == 1){
    
    rep(.dbl_upper.bounds, length(.sym_vars.independent)) -> .dbl_upper.bounds 
    
  }
  
  if(!length(.dbl_lower.bounds)){
    
    rep(-Inf, length(.sym_vars.independent)) -> .dbl_lower.bounds 
    
  } else if(length(.dbl_upper.bounds) == 1){
    
    rep(.dbl_lower.bounds, length(.sym_vars.independent)) -> .dbl_lower.bounds 
    
  }
  
  
  # Methods of estimation
  if(length(.sym_vars.instrumental)){
    # 2SLS
    # Dependent variable
    .sym_vars.dependent[[1]] %>%
      paste('~') -> chr_formula.right
    
    if(!.lgc_intercept){
      
      paste0(chr_formula.right, '0 + ') -> chr_formula.right
      
    } 
    
    # Independent variables
    paste(
      .sym_vars.independent
      , collapse = '+'
    ) %>% 
      paste('|') %>% 
      paste(
        paste(
          .sym_vars.instrumental
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
      , weights = .sym_vars.weights
    ) -> mdl_fit
    
  } else {
    # Other methods
    # Dependent variable
    .sym_vars.dependent[[1]] %>%
      paste('~') -> chr_formula.right
    
    if(.lgc_intercept){
      
      paste0(chr_formula.right, '(Intercept) + ') -> chr_formula.right
      
    } 
    
    # Independent variables
    paste0('b', 1:length(.sym_vars.independent), '*') %>%
      paste(.sym_vars.independent, collapse = '+') -> chr_formula.left
    
    # Instrumental variables
    if(length(.sym_vars.instrumental)){
      
      chr_formula.left %>%
        paste('|') %>% 
        paste(
          paste(
            .sym_vars.instrumental
            , collapse = ' + '
          )) -> chr_formula.left
      
    }
    
    # Formula
    chr_formula.right %>%
      paste0(chr_formula.left) %>%
      as.formula() -> fml_formula
    
    # Run model
    nls(
      formula = fml_formula
      , data = .df_data
      , weights = .sym_vars.weights
      , upper = .dbl_upper.bounds
      , lower = .dbl_lower.bounds
      , algorithm = 'port'
    ) -> mdl_fit
    
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
      , significance = 
        findInterval(
          x = p.value
          , vec = c(0, 0.001, 0.01, 0.05, 0.1, 1)
        ) %>% 
        recode(
          .default = ''
          , '4' = '.'
          , '3' = '*'
          , '2' = '**'
          , '1' = '***'
        )
    ) -> mdl_fit.tidy
  
  # R squared
  fun_r2(
    .int_vars.independent = length(.sym_vars.independent)
    , .dbl_observations = .df_data %>% pull(.sym_vars.dependent[[1]])
    , .dbl_fitted = 
      if(length(.sym_vars.instrumental)){
        mdl_fit$fitted.values
      } else { mdl_fit$m$fitted() }
    , .lgc_intercept = .lgc_intercept
  ) -> list_r2
  
  # F test
  
  # Summary
  if(.lgc_diagnostics){
    
    mdl_fit %>% 
      summary(diagnostics = T) %>%
      print()
    
  }
  
  
  # Output
  return(list(
    'model' = mdl_fit.tidy
    , 'r2' = list_r2[[1]]
    , 'r2.adjusted' = list_r2[[2]]
  ))
  
}

# test --------------------------------------------------------------------
# lm(
#   formula = price ~ carat + x + y + z 
#   , data = diamonds %>% slice(1:100)
# ) -> lalala
# 
# lalala %>% summary()
# lalala %>% broom::tidy()
# 
# lalala$coefficients
# # lalala$effects
# lalala$rank
# lalala$qr$qr
# names(lalala)
# 
# 
# dsdsdsds$model %>% view
# 
# fun_r2(
#   .int_vars.independent = lalala$rank
#   , .lgc_intercept = F
#   , .dbl_observations = diamonds %>% slice(1:100) %>% pull(price)
#   , .dbl_fitted = lalala$fitted.values
# )
# 
# nnls::nnls(
#   A = diamonds %>% slice(1:100) %>% select(carat) %>% as.matrix() * sqrt(diamonds$y)[1:100]
#   , b = diamonds %>% slice(1:100) %>% select(price) %>% as.matrix() * sqrt(diamonds$y)[1:100]
# )
# 
# nls(
#   formula = price ~ a + b1 * carat
#   , data = diamonds %>% slice(1:100)
#   , weights = NULL
#   # , upper = 10000
#   # , lower = 0
#   # , algorithm = 'port'
# ) 
# 
# lm(
#   formula = price ~ carat
#   , weights = y
#   , data = diamonds %>% slice(1:100)
# )
# 
# nls(
#   formula = price ~ `(Intercept)` + b1 * carat
#   , data = diamonds %>% slice(1:100)
#   , weights = y
#   # , upper = 1000
#   , lower = 0
#   , algorithm = 'port'
# ) %>% broom::tidy() -> dsds
# 
# 
# dsdsds$m$trace()
# 
# fun_r2(
#   .int_vars.independent = length(dsds$x)
#   , .lgc_intercept = F
#   , .dbl_observations = diamonds %>% slice(1:100) %>% select(price) %>% as.matrix()
#   , .dbl_fitted = dsds$fitted
# )
# 
# dsds$fitted
# dsds$residuals
# 
# 
# 1 - (lala$m$deviance() / sum((dsds[[1]] - mean(dsds[[1]]))^2))
# 
# lala$data
# 
# 
# 1 - (sum((lalala$residuals)^2) / sum((dsds[[1]] - mean(dsds[[1]]))^2))
# 
# 1 - (sum((model$residuals)^2) / sum((data$var - mean(data$var))^2))
# 
# p = #vars
#   n = #obs
#   1 - (1 - r2)(n-1)/(n-p)
# 
# lalala$deviance
# 
# lalala$x

occupations %>% 
  mutate(
    across(
      .cols = ends_with('.l')
      , ~ 100 * .x)
    , annual_wage_2021 = annual_wage_2021 / 12
  ) %>%
  fun_regression.truncated(
    .sym_vars.dependent = 'annual_wage_2021'
    , .sym_vars.independent = 
      occupations %>% 
      select(ends_with('.l')) %>% 
      names() %>%
      head(2)
    , .dbl_lower.bounds = 0
    , .lgc_intercept = F
    , .lgc_diagnostics = T
  )

systemfit()
