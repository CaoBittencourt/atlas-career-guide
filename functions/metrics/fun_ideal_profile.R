# [FUNCTION] --------------------------------------------------------------
# - User-Defined Ideal Profile --------------------------------------------
fun_ideal_profile <- function(
    
  efa_model
  , dbl_user_preferences
  , ...
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  
){
  
  # Dynamic Dots
  list2(...) -> list_coef
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
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
    "'...' must be a collection of numeric vectors the same length as the number of items in 'efa_model'." =
      any(
        !length(list_coef)
        , all(
          is.numeric(c(...))
          , map_lgl(
            list_coef
            , ~ length(.x) ==
              ncol(mtx_efa)
          ))
      )
  )
  
  stopifnot(
    "'dbl_user_preferences' must be a numeric vector the same length as the number of coefficients." =
      any(
        !length(list_coef)
        , all(
          is.numeric(dbl_user_preferences)
          , length(dbl_user_preferences) ==
            sum(
              length(list_coef)
              , nrow(mtx_efa)
            )
        ))
  )
  
  # Data Wrangling
  mtx_efa[str_sort(rownames(
    mtx_efa), numeric = T
  ),] -> mtx_efa
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  rep(
    dbl_scale_ub
    , ncol(mtx_efa)
  ) -> dbl_scale_ub
  rep(
    dbl_scale_lb
    , ncol(mtx_efa)
  ) -> dbl_scale_lb
  
  rbind(
    do.call(rbind, list_coef)
    , mtx_efa
  ) -> mtx_coef
  
  as.numeric(
    dbl_user_preferences
  ) -> dbl_user_preferences
  
  # Estimate User-Defined Ideal Profile
  # Economic Problem I
  
  # Economic Problem II
  
  # BVLS Regression Method
  coef(bvls(
    mtx_coef
    , dbl_user_preferences
    , bl = dbl_scale_lb
    , bu = dbl_scale_ub
  )) -> dbl_attributes
  
  colnames(mtx_efa) ->
    names(dbl_attributes)
  
  # Output
  return(list(
    'profile' = dbl_attributes,
    'preferences' = mtx_coef,
    'scale_ub' = dbl_scale_ub[[1]],
    'scale_lb' = dbl_scale_lb[[1]]
  ))
  
}

# dsds --------------------------------------------------------------------
constrOptim(
  # starting values
  theta = runif(2, 0, 100)
  # objective function
  , f = function(fct1,fct2){1 + (fct1-0)^2 + (fct2 - 83)^2}
  # , f = function(x){1 + (x[1]-0)^2 + (x[2] - 83)^2}
  # gradient
  , grad = function(fct1,fct2){c(fct1,fct2)}
  # , grad = function(x){c(x[1],x[2])}
  # constraint matrix
  , ui = rbind(1,1,1,1)
  # constraint vector
  , ci = c(100,100,0,0)
  , control = list(fnscale = -1)
)

install.packages('NlcOptim')
library(NlcOptim)
dbl_starting_values <- runif(2, 0, 100) / 100
dbl_starting_values <- c(50,50) / 100
dbl_starting_values <- c(0,83) / 100
dbl_starting_values <- c(0,0)

fun_objective <- function(x){
  
  1 - 
    ((x[1] - 0.5) ^ 2) / 2 -
    ((x[2] - 0.83) ^ 2) / 2 -> u
  
  return(-u)
  
}

round(solnl(
  X = dbl_starting_values
  , objfun = fun_objective
  # , A = matrix(-1, nrow = 2, ncol = 2)
  , A = rbind(c(-1,-1),c(1,1))
  , B = rbind(1,0.25)
  , lb = rep(0, length(dbl_starting_values))
  , ub = rep(1, length(dbl_starting_values))
)$par, 2)


read_rds(
  "C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds"
) -> efa_model

fun_kflex_micro(
  df_data_rows = df_occupations
  , efa_model = efa_model
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , dbl_weights = 
    df_occupations$
    employment2
) -> list_kflex_micro

coef(bvls(
  df_occupations %>% 
    select(ends_with('.l')) %>% 
    as.matrix() * 
    sqrt(
      df_occupations$
        employment2
    )
  , df_occupations$
    annual_wage_2021 * 
    sqrt(
      df_occupations$
        employment2
    )
  , bl = rep(0,200)
  , bu = rep(Inf,200)
)) %>% 
  set_names(
    df_occupations %>% 
      select(ends_with('.l')) %>% 
      names()
  ) -> dbl_marginal_cost

view(dbl_marginal_cost)

fun_ideal_profile(
  efa_model = efa_model
  , dbl_user_preferences = 
    # runif(10, min = 0, max = 100)
    # c(83, 0, 0, 0, 83, 67, 100, 100, 0, 50, 0, 100, 0, 17, 0)
    # c(80000,
    c(100000,
      # c(200000,
      # 50, 
      c(83, 0, 0, 0, 67, 50, 100, 83, 0, 33, 0, 100, 0, 17, 0))
  , 'kcost' = dbl_marginal_cost[list_kflex_micro$overall_micro_kflex$item]
  # , 'micro_kflex' = list_kflex_micro$overall_micro_kflex$kflex_micro
) -> dsds

view(dsds$profile)

dsds$
  preferences %>% 
  as_tibble(
    rownames = 'criteria'
  ) %>% 
  select(
    criteria, 
    contains('flex')
  ) %>% 
  mutate(
    user_preferences = 
      c(80000, 83, c(83, 0, 0, 0, 67, 50, 100, 83, 0, 33, 0, 100, 0, 17, 0))
  ) %>% 
  view

dsds$scale_ub
dsds$scale_lb

df_factors %>% 
  select(
    factor,
    factor.name
  ) %>% 
  unique() %>% 
  as_tibble()

bvls(
  dsds$preferences
  # , as.numeric(t(runif(10, min = 0, max = 100)))
  , c(83, 0, 0, 0, 83, 67, 100, 100, 0, 50, 0, 100, 0, 17, 0)
  , bl = rep(0, ncol(dsds$preferences))
  , bu = rep(100, ncol(dsds$preferences))
)
