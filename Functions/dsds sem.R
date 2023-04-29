# install.packages("quadprog")
# install.packages("pracma")
install.packages("cmna")
install.packages("leaps")
library(quadprog)
library(pracma)
library(cmna)
library(leaps)

fun_quadratic.problem <- function(.df_data){
  
  # Data type
  stopifnot(
    "'.df_data' must be a data frame or numeric matrix." = 
      is.data.frame(.df_data) & 
      .df_data %>% 
      map_lgl(is.numeric) %>% 
      any() |
      is.matrix(.df_data) & 
      is.numeric(.df_data)
  )
  
  # Numeric columns only
  if(is.data.frame(.df_data)){
    
    .df_data %>% 
      select(where(
        is.numeric
      )) %>% 
      as.matrix() -> .df_data
    
  }
  
  # Column sequence
  int_col <- seq(1, ncol(.df_data))
  
  # Solve quadratic problem
  map(
    set_names(
      int_col
      , colnames(.df_data)
    )
    , function(n){
      
      mtx_x <- .df_data[,int_col[-n]]
      
      mtx_y <- .df_data[,n]
      
      solve(chol(
        t(mtx_x) %*%
          mtx_x
      )) -> mtx_r.inverse
      
      cbind(
        mtx_x %>%
          ncol() %>%
          rep(x = 1)
        , mtx_x %>%
          ncol() %>%
          diag()
      ) -> mtx_constraints
      
      dbl_bvec <- c(1, rep(0, ncol(mtx_x)))
      
      dbl_dvec <- (t(mtx_y) %*% mtx_x)
      
      solve.QP(
        Dmat = mtx_r.inverse
        , factorized = T
        , dvec = dbl_dvec
        , Amat = mtx_constraints
        , bvec = dbl_bvec
        , meq = 1
      ) -> list_model
      
      # Because of computational reasons,
      # some coefficients are very slightly negative,
      # when they should in fact be zero,
      # as the problem constraints coefficients from
      # being negative. However, if coefficients are
      # round up to the 14th decimal place, then the
      # negative coefficients round to zero. Therefore, 
      # it is seems adequate to substitute all negative
      # coefficients with zero, instead of rounding, as
      # the solutions will be more precise this way.
      list_model$solution[
        list_model$solution < 0
      ] <- 0
      
      list_model$solution[
        list_model$solution > 1
      ] <- 1
      
      set_names(
        list_model$solution
        , colnames(mtx_x)
      ) -> list_model$solution
      
      set_names(
        list_model$unconstrained.solution
        , colnames(mtx_x)
      ) -> list_model$unconstrained.solution
      
      return(list_model)
      
    }
    
  ) -> list_models
  
  # Solutions only
  list_models %>%
    map(~ .x$solution) -> list_solutions
  
  # Tidy solutions
  # list_solutions %>%
  #   map(
  #     ~ .x %>%
  #       as_tibble(rownames = 'item') %>% 
  #       rename(item.composition = value) %>% 
  #       arrange(desc(item.composition))
  #   ) -> list_df_solutions
  
  list_solutions %>%
    map(
      ~ .x %>%
        as_tibble(rownames = 'item') %>%
        rename(item.composition = value)# %>%
      # arrange(desc(item.composition))
    ) -> list_df_solutions
  
  # Output
  return(list(
    solutions.tidy = list_df_solutions
    , solutions = list_solutions
    , models = list_models
  ))
  
}

# P.S.: population weight impacts results!!!
# => Run analysis on population-weighted data frame (doesn't take too long)
# System of linear equations
# Set critical thinking = 0
# All other variables are therefore defined
# How to solve systems of linear equations in R?

fun_predictors <- function(
    .df_data
    , .int_predictors.max = 5
    , .lgc_intercept = F
    , .lgc_run.lm = F
){
  
  map(
    set_names(
      1:ncol(.df_data)
      , names(.df_data)
    )
    , ~ 
      leaps::regsubsets(
        x = 
          .df_data %>% 
          names() %>%
          nth(.x) %>% 
          paste('~ .') %>% 
          as.formula()
        , data = 
          .df_data
        , really.big = T
        , intercept = 
          .lgc_intercept
        , nvmax = 
          .int_predictors.max
        , nbest = 1
      )
  ) -> list_models
  
  list_models %>% 
    map(
      ~ summary(.x)$adjr2 %>% 
        which.max()
    ) -> list_models.npredictors
  
  list_models %>% 
    map2(
      .x = list_models
      , .y = list_models.npredictors
      , .f =
        ~ 
        summary(.x)$which[.y,] %>% 
        as_tibble(rownames = 'item') %>% 
        rename(include = value) %>% 
        filter(include) %>% 
        filter(item != '(Intercept)') %>% 
        pull(item)
    ) -> list_predictors
  
  list_predictors %>%
    as_tibble_col(
      column_name = 'predictor'
    ) %>% 
    unnest(cols = predictor) %>% 
    group_by(predictor) %>% 
    tally() %>%
    mutate(
      pct = n / sum(n)
    ) %>% 
    arrange(desc(n)) -> df_predictors

  list_models.lm <- NULL
  list_models.lm.tidy <- NULL
  
  if(.lgc_run.lm){
    
    map2(
      .x = list_predictors
      , .y = names(list_predictors)
      , ~ 
        paste(ifelse(
          .lgc_intercept
          , paste(.y, '~') 
          , paste(.y, '~ 0 + ')
        )
        , paste(.x, collapse = '+')
        ) %>%
        as.formula() %>%
        lm(data = .df_data)
    ) -> list_models.lm
    
    list_models.lm %>%
      map(broom::tidy) -> list_models.lm.tidy
    
  }
  
  # Output
  return(compact(list(
    predictors = list_predictors
    , npredictors = list_models.npredictors
    , predictors.tally = df_predictors
    , models.lm = list_models.lm
    , models.tidy = list_models.lm.tidy
    , models = list_models
  )))
  
}

fun_predictors(
  .df_data = 
    df_occupations %>% 
    select(ends_with('.l')) %>%
    mutate(across(
      .cols = everything()
      ,.fns = ~ .x * 100
    ))
  , .int_predictors.max = NULL
  , .lgc_run.lm = F
  , .lgc_intercept = F
) -> dsdsd


dsdsd$predictors %>%
  as_tibble() %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'predictor'
  ) %>% 
  group_by(predictor) %>% 
  tally() %>% 
  arrange(desc(n))
  unique() -> chr_predictors.unique


dsdsd$predictors$active_learning.l
dsdsd$models.tidy$critical_thinking.l
summary(dsdsd$models.lm$active_learning.l)
dsdsd$models.tidy$active_learning.l %>% view

dsdsd$npredictors
dsdsd$predictors %>% 
  flatten_chr() %>% 
  unique()

dsds$intercept


dsds$nvmax

fun_quadratic.problem(
  # df_occupations.efa.comp %>% 
  df_occupations %>% 
    select(ends_with('.l'))
) -> lalala

lalala$solutions.tidy

lalala$solutions %>% 
  map(sum)

lalala$solutions.tidy$psychology.l %>% 
  mutate(item.composition = round(item.composition, 4)) %>% 
  filter(item.composition > 0) %>% 
  arrange(desc(item.composition))

cor(
  df_occupations.efa.comp$psychology.l
  , df_occupations.efa.comp$deductive_reasoning.l
)

lalala$solutions.tidy$psychology.l %>% 
  filter(item == 'deductive_reasoning.l') 

do.call(
  rbind
  , map(
    1:length(lalala$solutions.tidy)
    , ~ 
      lalala$solutions.tidy[[.x]] %>% 
      select(2) %>% 
      add_row(
        item.composition = -1
        , .before = .x
      ) %>% 
      flatten_dbl() * (-1)
  )
) -> mtx_eq.system

diag(mtx_eq.system)

# df_occupations.efa.comp %>% 
df_occupations %>% 
  select(ends_with('.l')) %>%
  names() -> rownames(mtx_eq.system)

rownames(mtx_eq.system) -> colnames(mtx_eq.system)

view(mtx_eq.system)

# rref(mtx_eq.system) %>% view
refmatrix(mtx_eq.system) -> mtx_eq.gauss

round(mtx_eq.gauss, 13) %>% view
all(round(mtx_eq.gauss, 13)[33,] == 0)

mtx_eq.gauss %>%
  round(13) %>% 
  as_tibble(
    rownames = 'item'
  ) %>%
  mutate(
    zero.count = rowSums(. == 0)
    , nonzero.count = ncol(mtx_eq.gauss) - zero.count
  ) %>% 
  select(
    item
    , zero.count
    , nonzero.count
  ) %>%
  arrange(nonzero.count) -> df_eq.id

view(df_eq.id)

mtx_eq.gauss %>%
  round(4) %>% 
  as_tibble(
    rownames = 'item'
  ) %>% 
  tail() %>% 
  view

mtx_eq.gauss %>%
  round(13) %>% 
  t() %>% 
  as_tibble(
    rownames = 'item'
  ) %>%
  select(
    item
    , df_eq.id %>% 
      slice_max(
        zero.count
        , n = 1
      ) %>%
      pull(item)
  ) %>% 
  filter(if_any(
    .cols = is.numeric
    ,.fns = ~ .x == 0
  )) %>% 
  pull(item)

colnames(mtx_eq.gauss)[29]

solve(
  round(mtx_eq.system[nrow(mtx_eq.system) - 1,], 4) %>% view
  , rep(0,)
)

# round(det(mtx_eq.system[,-ncol(mtx_eq.system)]), 18) == 0
round(det(mtx_eq.gauss), 18) == 0
Rank(mtx_eq.system)
Rank(mtx_eq.gauss)

qr.R(qr(mtx_eq.system)) %>% view
qr.R(qr(mtx_eq.gauss)) %>% view

# pracma::rref()
# pracma::quadprog()
# quadprog::solve.QP(
#   Dmat = C
#   , d = dvec
#   , Amat = A
#   , bvec = b
#   , med = Aeq
#   , lb = 0
#   , ub = 1
# )

# source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_regressions.R')
# 
# df_occupations.efa.comp %>% 
#   select(ends_with('.l')) %>% 
#   names() -> chr_var.names
# 
# df_occupations.efa.comp %>% 
#   select(ends_with('.l')) -> dsds
# 
# df_occupations.efa.comp %>%
#   select(ends_with('.l')) %>%
#   names() %>%
#   str_replace_all(
#     '_', '.dsds.'
#   ) -> chr_var.names
# install.packages("sem")
# library(sem)
# 
# map(
#   set_names(
#     1:length(chr_var.names)
#     , chr_var.names
#   ) 
#   , ~ paste(
#     chr_var.names[.x]
#     , '~'
#     , paste(
#       chr_var.names[-.x]
#       , collapse = '+'
#     )) %>%
#     as.formula()
# ) -> list_var.sem
# 
# map(
#   set_names(
#     1:length(chr_var.names)
#     , chr_var.names
#   ) 
#   , ~ paste(
#     '~'
#     , paste(
#       chr_var.names[-.x]
#       , collapse = '+'
#     )) %>%
#     as.formula()
# ) -> list_inst.sem
# 
# c(
#   paste(chr_var.names, '>= 0')
#   , paste(chr_var.names, '<= 1')
#   , paste(chr_var.names, collapse = ' + ') %>%
#     paste('= 1')
# ) -> chr_coefficients.restrict
# 
# rbind(
#   paste(chr_var.names, '>= 0')
#   , paste(chr_var.names, '<= 1')
#   , paste(chr_var.names, collapse = ' + ') %>%
#     paste('= 1')
# ) -> mtx_coefficients.restrict
# 
# systemfit(
#   formula = list_var.sem
#   , data = 
#     df_occupations.efa.comp %>% 
#     select(ends_with('.l')) %>% 
#     rename_with( 
#       ~ str_replace_all(.x, '_', '.dsds.')
#     )
#   , inst = list_inst.sem
#   # , restrict.matrix = chr_coefficients.restrict
#   # , method = 'SUR'
#   , method = '3SLS'
# ) -> list_model.sem
# 
# summary(
#   list_model.sem
#   , residCov = F
#   , equations = F
# )
# 
# list_model.sem %>% 
#   broom::tidy() -> df_model.sem
# 
# view(df_model.sem)

df_occupations.efa.comp %>% 
  select(ends_with('.l')) %>% 
  cor() %>% 
  as_tibble(
    rownames = 'item'
  ) %>%
  filter(item == 'administrative.l') %>% 
  mutate(across(
    .cols = is.numeric
    # ,.fns = ~ .x * 0.3875
    ,.fns = ~ (1 + .x) * 0.3875 / 2
  )) %>%
  select(!item) %>% 
  t() %>%
  as_tibble(
    rownames = 'item'
  ) %>%
  rename(item.ai = 2) %>%
  arrange(desc(item.ai)) %>% 
  print(n = 33)

