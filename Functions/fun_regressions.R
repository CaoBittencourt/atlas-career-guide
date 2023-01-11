# --- SETUP ------------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
c(
  'sandwich', 'lmtest', #Diagnósticos e ajustes
  'ivreg', #Modelos 
  # 'np', 'Matching', 'systemfit', # Para as escalas semi/não-paramétricas e sistemas de dispêndio
  'plyr', 'glue', 'broom', 'purrr', 'tidyverse' #Manipulação de dados
) -> pkg

# library(systemfit)
library(ivreg)

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
    , .sym_vars.instrumental = c()
    , .sym_vars.weights = NULL
    , .lgc_intercept = T
    , .dbl_upper.bounds = c()
    , .dbl_lower.bounds = c()
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
      , weights = 
        if(length(.sym_vars.weights)){
          .df_data[[.sym_vars.weights]]
          } else { NULL }
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
      , weights = 
        if(length(.sym_vars.weights)){
          .df_data[[.sym_vars.weights]]
        } else { NULL }
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
    'model.tidy' = mdl_fit.tidy
    , 'model.fit' = mdl_fit
    , 'r2' = list_r2[[1]]
    , 'r2.adjusted' = list_r2[[2]]
  ))
  
}

# test --------------------------------------------------------------------
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')

dsds <- 'sample_weight'
dsds <- 'active_listening.l'
dsds <- NULL

nls(
  formula = annual_wage_2021 ~ a + b1 * active_listening.l + b2 * basic.mathematics.l
  , weights = if(length(dsds)){df_occupations[[dsds]]} else {NULL}
  # , weights = df_occupations[['active_listening.l']]
  , algorithm = 'port'
  , data = 
    df_occupations %>%
    mutate(
      annual_wage_2021 = annual_wage_2021 / 12
      , across(
        .cols = ends_with('.l')
        ,.fns = function(x){100*x}
      )
      , sample_weight = sum(employment) / employment
    )
) -> dsdsds


df_occupations %>%
  mutate(
    annual_wage_2021 = annual_wage_2021 / 12
    , across(
      .cols = ends_with('.l')
      ,.fns = function(x){100*x}
    )
    , sample_weight = sum(employment) / employment
    ) %>% 
  fun_lm(
    .sym_vars.dependent = 'annual_wage_2021'
    , .sym_vars.independent =  
      df_occupations %>%
      select(ends_with('.l')) %>%
      names() %>% 
      head(2)
    , .sym_vars.weights = 'sample_weight'
    , .dbl_lower.bounds = 0
    , .lgc_intercept = F
  # ) -> list_kcost
  )

# -------- EXPORT ----------------------------------------------------
# XLSX --------------------------------------------------------------------
list_kcost$model.tidy %>% 
  arrange(desc(estimate), desc(p.value)) %>%
  mutate(p.value = round(p.value, 4)) %>% 
  openxlsx::write.xlsx('kcost.xlsx')


list_kcost$model.tidy %>% view
list_kcost$r2
list_kcost$r2.adjusted


