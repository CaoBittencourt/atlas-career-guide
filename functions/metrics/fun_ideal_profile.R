# [FUNCTION] --------------------------------------------------------------
# - User-Defined Ideal Profile --------------------------------------------
fun_ideal_profile <- function(
    
  efa_model
  , dbl_factor_preferences
  , dbl_coef_preferences = NULL
  , df_item_coefficients = NULL
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_method = c('gradient','bvls')
  
){
  
  # Arguments Validation
  stopifnot(
    "'efa_model' must be a factor analysis object." =
      any(
        str_to_lower(class(
          efa_model
        )) == 'factanal'
        , str_to_lower(class(
          efa_model
        )) == 'fa'
      )
  )
  
  t(loadings(efa_model)[,]) %>% 
    `rownames<-`(
      loadings(efa_model)[,] %>%
        colnames() %>%
        str_extract(
          '[[:digit:]]+'
        ) %>%
        paste0('factor',.)
    ) -> mtx_efa
  
  rm(efa_model)
  
  stopifnot(
    "'df_item_coefficients' must be a coefficients data frame with an 'item' column." =
      any(
        all(
          is.data.frame(df_item_coefficients)
          , 'item' %in% names(df_item_coefficients)
          , df_item_coefficients$item %in% 
            colnames(mtx_efa)
        )
        , is.null(df_item_coefficients)
      )
    
  ) 
  
  stopifnot(
    "'dbl_factor_preferences' must be a numeric vector." =
      all(
        is.numeric(dbl_coef_preferences)
        , length(dbl_factor_preferences) ==
          nrow(mtx_efa)
      )
  )
  
  stopifnot(
    "'dbl_coef_preferences' must be a numeric vector." =
      any(
        is.numeric(dbl_coef_preferences)
        , is.null(dbl_coef_preferences)
      )
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'chr_method' must be a character." = 
      is.character(chr_method)
  )
  
  # Data Wrangling
  mtx_efa[str_sort(rownames(
    mtx_efa), numeric = T
  ),] -> mtx_efa
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  chr_method[[1]] -> chr_method
  
  as.numeric(
    dbl_factor_preferences
  ) -> dbl_factor_preferences
  
  if(all(
    length(df_item_coefficients),
    length(dbl_coef_preferences)
  )){
    
    dbl_coef_preferences[
      1:(nrow(df_item_coefficients) - 1)
    ] -> dbl_coef_preferences
    
    df_item_coefficients[
      colnames(mtx_efa),
    ] -> df_item_coefficients
    
    # dsdsds
    
  }
  
  # Estimate User-Defined Ideal Profile
  if(
    chr_method ==
    'gradient'
  ){
    
    # Constrains Matrix
    list_coef
    
    # Constrained Optimization Problem I
    constrOptim(
      # starting values
      theta = 
        runif(
          ncol(mtx_efa), 
          dbl_scale_lb, 
          dbl_scale_ub
        )
      # objective function
      , f = function(
    a
    , mtx_factor_scores = 
      mtx_efa
    , dbl_factor_pref = 
      dbl_factor_preferences
    
      ){
        
        1 - 
          sum(((
            mtx_factor_scores * a - dbl_factor_pref
          ) ^ 2) / nrow(mtx_factor_scores)) -> u
        
        return(u)
        
      }
    # gradient
    , grad = NULL
    # constraint matrix
    , ui = 
      rbind(
        diag(1, ncol(mtx_efa))
        , diag(-1, ncol(mtx_efa))
      )
    # constraint vector
    , ci = c(
      rep(dbl_scale_lb, ncol(mtx_efa))
      , rep(-dbl_scale_ub, ncol(mtx_efa))
    )
    , control = list(fnscale = -1)
    )$par -> dbl_attributes
    
    # Constrained Optimization Problem II
    
  } else { 
    
    # BVLS Regression Method
    
    # rbind(
    #   df_item_coefficients
    #   , mtx_efa
    # ) -> mtx_coef
    
    rep(
      dbl_scale_ub
      , ncol(mtx_efa)
    ) -> dbl_scale_ub
    rep(
      dbl_scale_lb
      , ncol(mtx_efa)
    ) -> dbl_scale_lb
    
    coef(bvls(
      mtx_coef
      , c(
        dbl_coef_preferences
        , dbl_factor_preferences
      )
      , bl = dbl_scale_lb
      , bu = dbl_scale_ub
    )) -> dbl_attributes
    
  }
  
  # Data Wrangling
  # colnames(mtx_coef) ->
  colnames(mtx_efa) ->
    names(dbl_attributes)
  
  # Output
  return(list(
    'profile' = dbl_attributes,
    # 'preferences' = mtx_coef,
    'preferences' = mtx_efa,
    'scale_ub' = dbl_scale_ub[[1]],
    'scale_lb' = dbl_scale_lb[[1]]
  ))
  
}

# # dsds --------------------------------------------------------------------
# fun_ideal_profile(
#   efa_model = efa_model
#   , dbl_user_preferences = 
#     c(83, 0, 0, 0, 67, 50, 100, 83, 0, 33, 0, 100, 0, 17, 0)
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
#   , chr_method = 'gradient'
# ) -> dsds
# 
# view(dsds$profile)
# view(dsdsds$par)
# 
# constrOptim(
#   # starting values
#   theta = 
#     runif(
#       ncol(dsds$efa), 
#       0, 
#       100
#     )
#   # objective function
#   , f = function(
#     a
#     , mtx_factor_scores = 
#       dsds$efa
#     , dbl_factor_pref = 
#       dsds$pref
#     
#   ){
#     
#     1 - 
#       sum(((
#         mtx_factor_scores * a - dbl_factor_pref
#       ) ^ 2) / nrow(mtx_factor_scores)) -> u
#     
#     return(u)
#     
#   }
#   # gradient
#   , grad = NULL
#   # constraint matrix
#   , ui = 
#     rbind(
#       diag(1, ncol(dsds$efa))
#       , diag(-1, ncol(dsds$efa))
#     )
#   # constraint vector
#   , ci = c(
#     rep(0, ncol(dsds$efa))
#     , rep(-100, ncol(dsds$efa))
#   )
#   , control = list(fnscale = -1)
# ) -> dsdsds
# 
# colnames(dsds$efa) ->
#   names(dsdsds$par)
# view(dsdsds$par)
# 
# constrOptim(
#   # starting values
#   theta = runif(2, 0, 100)
#   # objective function
#   , f = function(fct1,fct2){1 + (fct1-0)^2 + (fct2 - 83)^2}
#   # , f = function(x){1 + (x[1]-0)^2 + (x[2] - 83)^2}
#   # gradient
#   , grad = function(fct1,fct2){c(fct1,fct2)}
#   # , grad = function(x){c(x[1],x[2])}
#   # constraint matrix
#   , ui = rbind(1,1,1,1)
#   # constraint vector
#   , ci = c(100,100,0,0)
#   , control = list(fnscale = -1)
# )
# 
# constrOptim(
#   # starting values
#   theta = runif(3, 0, 1)
#   # theta = runif(3, 0, 100)
#   # objective function
#   , f = function(a){
#     # 100 - 
#     1 - 
#       # ((c(0.5,0.25,-0.3)*a - 0.5) ^ 2) / 2 -
#       # ((c(-0.1,0.3,0.9)*a - 0.83) ^ 2) / 2 -> u
#       ((0.5*a[1] + 0.25*a[2] -0.3*a[3] - 0.5) ^ 2) / 2 -
#       # ((50*a[1] + 25*a[2] - 30*a[3] - 50) ^ 2) / 2 -
#       ((-0.1*a[1] + 0.3*a[2] +0.9*a[3] - 0.83) ^ 2) / 2 -> u
#     # ((-10*a[2] + 30*a[2] + 90*a[3] - 83) ^ 2) / 2 -> u
#     
#     return(u)
#     
#   }
#   # gradient
#   # , grad = function(a){a}
#   , grad = NULL
#   # , grad = function(x){c(x[1],x[2])}
#   # constraint matrix
#   , ui = 
#     rbind(
#       diag(1, 3)
#       , diag(-1, 3)
#     )
#   # constraint vector
#   , ci = c(rep(0,3),rep(-1,3))
#   # , ci = c(rep(0,3),rep(-100,3))
#   , control = list(fnscale = -1)
# )
# 
# constrOptim(
#   # starting values
#   theta = runif(2, 0, 1)
#   # objective function
#   , f = function(a){
#     1 - 
#       ((1*a[1] + 0*a[2] - 0.5) ^ 2) / 2 -
#       ((0*a[1] + 1*a[2] - 0.83) ^ 2) / 2 -> u
#     
#     return(u)
#     
#   }
#   # gradient
#   , grad = NULL
#   # constraint matrix
#   , ui = 
#     rbind(
#       diag(1, 2)
#       , diag(-1, 2)
#     )
#   # constraint vector
#   , ci = c(rep(0,2),rep(-1,2))
#   , control = list(fnscale = -1)
# )
# 
# constrOptim(
#   # starting values
#   theta = runif(2, 0, 1)
#   # objective function
#   , f = function(a){
#     1 - 
#       sum(
#         ((c(1,0)*a - 0.5) ^ 2) / 2 -
#           ((c(0,1)*a - 0.83) ^ 2) / 2
#       ) -> u
#     
#     return(u)
#     
#   }
#   # gradient
#   , grad = NULL
#   # constraint matrix
#   , ui = 
#     rbind(
#       diag(1, 2)
#       , diag(-1, 2)
#     )
#   # constraint vector
#   , ci = c(rep(0,2),rep(-1,2))
#   , control = list(fnscale = -1)
# )
# 
# fun_ideal_profile <- function(
#     
#   efa_model,    
#   dbl_factor_pref,
#   
#   
# ){
#   
#   # Arguments Validation
#   
#   # Data Wrangling
#   
#   # Estimate User-Defined Ideal Profile
#   # Constrained Optimization Problem I
#   constrOptim(
#     # starting values
#     theta = runif(3, 0, 1)
#     # objective function
#     , f = function(
#     a,
#     mtx_factor_scores = diag(3),
#     dbl_factor_pref = c(0.5,0.83,0)
#     
#     ){
#       
#       1 - 
#         sum(((
#           mtx_factor_scores * a - dbl_factor_pref
#         ) ^ 2) / nrow(mtx_factor_scores)) -> u
#       
#       return(u)
#       
#     }
#     # gradient
#     , grad = NULL
#     # constraint matrix
#     , ui = 
#       rbind(
#         diag(1, 3)
#         , diag(-1, 3)
#       )
#     # constraint vector
#     , ci = c(rep(0,3),rep(-1,3))
#     , control = list(fnscale = -1)
#   )
#   
#   # Constrained Optimization Problem II
#   
#   # BVLS Regression
#   
#   # Data Wrangling
#   
#   # Output
#   # return()
#   
# }
# 
# constrOptim(
#   # starting values
#   theta = runif(3, 0, 1)
#   # objective function
#   , f = function(a, n = 3){
#     1 - 
#       sum(((
#         diag(n) * a - c(0.5,0.83,0)
#       ) ^ 2) / n) -> u
#     
#     return(u)
#     
#   }
#   # gradient
#   , grad = NULL
#   # constraint matrix
#   , ui = 
#     rbind(
#       diag(1, 3)
#       , diag(-1, 3)
#     )
#   # constraint vector
#   , ci = c(rep(0,3),rep(-1,3))
#   , control = list(fnscale = -1)
# )$par
# 
# constrOptim(
#   # starting values
#   theta = runif(3, 0, 1)
#   # objective function
#   , f = function(
#     a,
#     mtx_factor_scores = diag(3),
#     dbl_factor_pref = c(0.5,0.83,0)
#     
#   ){
#     
#     1 - 
#       sum(((
#         mtx_factor_scores * a - dbl_factor_pref
#       ) ^ 2) / nrow(mtx_factor_scores)) -> u
#     
#     return(u)
#     
#   }
#   # gradient
#   , grad = NULL
#   # constraint matrix
#   , ui = 
#     rbind(
#       diag(1, 3)
#       , diag(-1, 3)
#     )
#   # constraint vector
#   , ci = c(rep(0,3),rep(-1,3))
#   , control = list(fnscale = -1)
# )
# 
# 
# install.packages('NlcOptim')
# library(NlcOptim)
# dbl_starting_values <- runif(3, 0, 100) / 100
# dbl_starting_values <- c(50,50) / 100
# dbl_starting_values <- c(50,50,50) / 100
# dbl_starting_values <- c(0,83) / 100
# # dbl_starting_values <- c(0,0)
# dbl_starting_values <- c(0,0,0)
# 
# fun_objective <- function(x){
#   
#   1 - 
#     ((0.5*x[1] + 0.25*x[2] -0.3*x[3] - 0.5) ^ 2) / 2 -
#     # ((c(0.5,0.25,-0.3) * x - 0.5) ^ 2) / 2 -
#     ((-0.1*x[2] + 0.3*x[2] +0.9*x[3] - 0.83) ^ 2) / 2 -> u
#   # ((c(-0.1,0.3,0.9) * x - 0.83) ^ 2) / 2 -> u
#   
#   # 1 - 
#   #   ((x[1] - 0.5) ^ 2) / 2 -
#   #   ((x[2] - 0.83) ^ 2) / 2 -> u
#   
#   return(-u)
#   
# }
# 
# round(solnl(
#   X = runif(3, 0, 1)
#   , objfun = fun_objective
#   # , A = matrix(-1, nrow = 2, ncol = 2)
#   # , A = rbind(c(-1,-1),c(1,1))
#   # , B = rbind(1,0.25)
#   , lb = rep(0, 3)
#   , ub = rep(1, 3)
# )$par, 2)
# 
# 
# read_rds(
#   "C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds"
# ) -> efa_model
# 
# fun_kflex_micro(
#   df_data_rows = df_occupations
#   , efa_model = efa_model
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
#   , dbl_weights = 
#     df_occupations$
#     employment2
# ) -> list_kflex_micro
# 
# coef(bvls(
#   df_occupations %>% 
#     select(ends_with('.l')) %>% 
#     as.matrix() * 
#     sqrt(
#       df_occupations$
#         employment2
#     )
#   , df_occupations$
#     annual_wage_2021 * 
#     sqrt(
#       df_occupations$
#         employment2
#     )
#   , bl = rep(0,200)
#   , bu = rep(Inf,200)
# )) %>% 
#   set_names(
#     df_occupations %>% 
#       select(ends_with('.l')) %>% 
#       names()
#   ) -> dbl_marginal_cost
# 
# view(dbl_marginal_cost)
# 
# fun_ideal_profile(
#   efa_model = efa_model
#   , dbl_user_preferences = 
#     # runif(10, min = 0, max = 100)
#     # c(83, 0, 0, 0, 83, 67, 100, 100, 0, 50, 0, 100, 0, 17, 0)
#     # c(80000,
#     c(100000,
#       # c(200000,
#       # 50, 
#       c(83, 0, 0, 0, 67, 50, 100, 83, 0, 33, 0, 100, 0, 17, 0))
#   , 'kcost' = dbl_marginal_cost[list_kflex_micro$overall_micro_kflex$item]
#   # , 'micro_kflex' = list_kflex_micro$overall_micro_kflex$kflex_micro
# ) -> dsds
# 
# view(dsds$profile)
# 
# dsds$
#   preferences %>% 
#   as_tibble(
#     rownames = 'criteria'
#   ) %>% 
#   select(
#     criteria, 
#     contains('flex')
#   ) %>% 
#   mutate(
#     user_preferences = 
#       c(80000, 83, c(83, 0, 0, 0, 67, 50, 100, 83, 0, 33, 0, 100, 0, 17, 0))
#   ) %>% 
#   view
# 
# dsds$scale_ub
# dsds$scale_lb
# 
# df_factors %>% 
#   select(
#     factor,
#     factor.name
#   ) %>% 
#   unique() %>% 
#   as_tibble()
# 
# bvls(
#   dsds$preferences
#   # , as.numeric(t(runif(10, min = 0, max = 100)))
#   , c(83, 0, 0, 0, 83, 67, 100, 100, 0, 50, 0, 100, 0, 17, 0)
#   , bl = rep(0, ncol(dsds$preferences))
#   , bu = rep(100, ncol(dsds$preferences))
# )
