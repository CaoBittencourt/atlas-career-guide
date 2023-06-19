# --- SETUP ------------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
c(
  'tidyverse' #Manipulação de dados
) -> pkg

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})


# FUNCTIONS ---------------------------------------------------------------
# Regressions
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/fun_regressions.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')

# DATA --------------------------------------------------------------------
# # POPULATION-WEIGHTED OCCUPATIONS DATA FRAME
# source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')

# POPULATION-WEIGHTED EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')

# List of test data
list(
  'efa.comp' = df_occupations.efa.comp
) -> list_df_occupations

# --- HUMAN CAPITAL COST ESTIMATION ---------------------------------------------
# DATA WRANGLING ----------------------------------------------------------
list_df_occupations %>% 
  lapply(
    function(df){
      
      df %>%
        mutate(
          across(
            .cols = ends_with('.l')
            ,.fns = function(x){100*x}
          )
          , sample_weight = sum(employment2) / employment2
          , education.years = recode(
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
          , education.years = education.years - 12
          # , education.years = education.years * (8 / 24)
          # , education.years = education.years * (24 / 8)
          # , education.years = education.years - min(education.years)
          # , education.years = (education.years - min(education.years)) * (24 / 8)
          # , education.hours = education.years * 8760
          # , education.hours = education.years * 8760 * (8 / 24)
          # , education.hours = education.years * 8760 * (24 / 8)
        ) %>% 
        return()
      
    }
  ) -> list_df_occupations

# NNLS REGRESSION ---------------------------------------------------
Map(
  function(
    chr_dummies
    , df_data
    , dbl_lb
    , lgc_intercept
  ){
    
    df_data %>% 
      fun_lm(
        # .sym_vars.dependent = 'education.hours'
        .sym_vars.dependent = 'education.years'
        , .sym_vars.independent =  
          df_data %>% 
          select(
            ends_with('.l')
          ) %>% 
          names()
        , .sym_vars.dummies = chr_dummies
        , .sym_vars.weights = 'sample_weight'
        , .dbl_lower.bounds = dbl_lb
        , .lgc_intercept = lgc_intercept
        , .lgc_dummy.remove = F
      ) %>% 
      return()
    
  }
  , chr_dummies = list('efa.comp' = character())
  # , chr_dummies = list('efa.comp' = 'typical_on_the_job_training')
  # , dbl_lb = 0
  # , dbl_lb = 15
  , dbl_lb = 1 / 8760
  , lgc_intercept = F
  , df_data = list_df_occupations
) -> list_models

# REGRESSION STATISTICS COMPARISON ---------------------------------------------------
# R2
list_models$efa.comp$r2

# RMSE
list_models$efa.comp$rmse

# MAE
list_models$efa.comp$mae

# Fitted mean
list_models$efa.comp$fitted.mean

# Fitted standard deviation
list_models$efa.comp$fitted.sd

# Parameters
list_models$efa.comp$model.tidy %>%
  mutate(
    estimate.years = estimate
    , low.years = 17 * estimate.years + 12
    , medium.years = 50 * estimate.years + 12
    , high.years = 83 * estimate.years + 12
    , across(
      .cols = where(is.numeric)
      , .fns = function(x){round(x, 4)}
    )
    , .after = estimate
  ) %>% view

# EXPECTED WAGE -----------------------------------------------------------
# Expected wage data frames
Map(
  function(mdl_model, df_data){
    
    df_data %>% 
      mutate(
        human_capital = mdl_model$fitted
        , wage_model.ratio = human_capital / annual_wage_2021
        , .after = annual_wage_2021
      ) %>% 
      return()
    
  }
  , mdl_model = list_models
  , df_data = rep(list_df_occupations, 2)
) -> list_df_occupations.model

# Expected wage population-weighted data frames
list_df_occupations.model %>%
  lapply(
    function(df){
      
      df %>% 
        drop_na() %>% 
        mutate(
          employment2 = employment2 / min(employment2, na.rm = T)
          , employment2 = round(employment2)
        ) %>% 
        group_by(occupation) %>%
        slice(rep(1:n(), first(employment2))) %>% 
        ungroup() %>% 
        return()
      
    }
  ) -> list_df_occupations.model.pop

# --- VISUALIZATION -------------------------------------------------------
# CAPITAL COST BY ATTRIBUTE -----------------------------------------------

# CAPITAL COST DISTRIBUTION -----------------------------------------------

# EXPECTED WAGE VS ACTUAL WAGE DISTRIBUTION -------------------------------
list_df_occupations.model.pop %>%
  lapply(
    function(df){
      
      df %>%
        fun_plot.histogram(aes(
          x = wage_model.ratio
        )
        # , .sym_facets = entry_level_education
        # , .int_facets = 4
        # , .reorder_desc = T
        , .dbl_limits.x = c(0, NA)
        ) +
        geom_vline(
          xintercept = seq(0.75, 1.25, 0.25)
        ) +
        geom_vline(
          xintercept = mean(df$wage_model.ratio)
          , linetype = 'dashed'
          , color = 'red'
        ) %>%
        
        return()
      
    }
  ) -> list_plt_ratio

# EXPECTED WAGE VS ACTUAL WAGE DISTRIBUTION (FACETED) -------------------------------
list_df_occupations.model.pop %>%
  lapply(
    function(df){
      
      df %>%
        group_by(entry_level_education) %>% 
        mutate(ratio.mean = mean(wage_model.ratio)) %>% 
        ungroup() %>% 
        fun_plot.histogram(aes(
          x = wage_model.ratio
        )
        , .sym_facets = entry_level_education
        , .int_facets = 4
        , .reorder_desc = T
        , .dbl_limits.x = c(0, NA)
        ) +
        geom_vline(
          xintercept = seq(0.75, 1.25, 0.25)
        ) +
        geom_vline(
          aes(xintercept = ratio.mean)
          , linetype = 'dashed'
          , color = 'red'
        ) %>%
        
        return()
      
    }
  ) -> list_plt_ratio.facets

# --- EXPORT ----------------------------------------------------
# XLSX --------------------------------------------------------------------
Map(
  function(name, model){
    
    model$model.tidy %>%
      arrange(desc(estimate), desc(p.value)) %>%
      mutate(across(
        .cols = where(is.numeric)
        , .fns = function(x){round(x, 4)}
      )) %>%
      openxlsx::write.xlsx(paste0('kcost.', name, '.xlsx'))
    
  }
  , name = names(list_models)
  , model = list_models
)


# employment --------------------------------------------------------------
df_occupations %>% 
  select(
    occupation
    , annual_wage_2021
    , entry_level_education
    , employment2
  ) %>% 
  full_join(
    df_occupations.ai
  ) %>% 
  mutate(
    .after = 1
    , coef.employment =
      100 * employment2 /
      sum(employment2)
    # , coef.employment =
    #   100 * employment2 /
    #   max(employment2)
    # , coef.wage = 
    #   annual_wage_2021 / 
    #   max(annual_wage_2021)
    # , coef.employment = 
    #   100 * coef.wage * 
    #   employment2 / 
    #   max(employment2)
  ) -> dsds

seq(0,1,0.001) %>% 
  as_tibble() %>% 
  mutate(
    coef1 = value
    , coef2 = 
      (1-value) * value^2 +
      value * value
  ) %>% 
  reframe(
    all(coef1 >= coef2)
  )
  

curve(
  x^1
  , from = 0
  , to = 1
  , col = 1
)
curve(
  x^5
  # x^2
  , from = 0
  , to = 1
  , add = T
  , col = 2
)
curve(
  (1-x)*x^2 + x*(x^1)
  , from = 0
  , to = 1
  , add = T
  , col = 3
)
curve(
  # (1-x)*x^(1/x)^(1/x) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  (1-x)*x^(1/x)^((1/x)^2) + x*(x^1)
  , from = 0
  , to = 1
  , add = T
  , col = 4
)
curve(
  # (1-x)*x^(1/x)^(1/x) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  (1-x)*x^(1/x)^((1/x)) + x*(x^1)
  , from = 0
  , to = 1
  , add = T
  , col = 5
)
curve(
  # (1-x)*x^(1/x)^(1/x) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^2) + x*(x^1)
  , from = 0
  , to = 1
  , add = T
  , col = 6
)
curve(
  # (1-x)*x^(1/x)^(1/x) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  (1-x)*x^(1/x)^((1/x)^(1/x)) + x*(x^1)
  # (1-x)*x^(1/x)^((1/x)^2) + x*(x^1)
  , from = 0
  , to = 1
  , add = T
  , col = 6
)
curve(
  (x^2)*(1-x) + (x^1)*x
  # (x^2)^x + (x^1)^(1-x)
  , from = 0
  , to = 1
  , add = T
)
curve(
  (1-x)*x^(3) + (x)*x^(1/2)
  , from = 0
  , to = 1
  , add = T
)
curve(
  (x^2)*(1-x)^2 + (x^(1))*(x)^2
  , from = 0
  , to = 1
  , add = T
)
curve(
  (1-x)*(x^2) + (x)*(x^(2/3))
  , from = 0
  , to = 1
  , add = T
)


dsds %>% 
  select(
    ends_with('.l')
  ) %>% 
  names() -> chr_dsds

lm(
  paste0(
    'coef.employment ~ 0 +'
    , paste0(
      chr_dsds
      , collapse = '+'
    )) %>% 
    as.formula()
  , data = dsds
  , weights = employment2
) -> dsdsds

summary(dsdsds)

r2ww <- function(x,w,resid,observed){
  SSe <- sum(x$w*(x$resid)^2)
  observed <- x$resid+x$fitted
  SSt <- sum(x$w*(observed-weighted.mean(observed,x$w))^2)
  value <- 1-SSe/SSt;
  return(value);
}

fun_r2.weighted <- function(.list_model){
  
  .list_model %>% 
    residuals() -> dbl_residuals
  
  .list_model %>% 
    fitted() -> dbl_fitted
  
  .list_model %>% 
    weights() -> dbl_weights
  
  if(!length(dbl_weights)){
    
    rep(1, length(
      dbl_fitted
    )) -> dbl_weights
    
  }
  
  dbl_residuals + 
    dbl_fitted -> dbl_observed
  
  sum(
    dbl_weights *
      dbl_residuals ^ 2
  ) -> dbl_sse
  
  sum(
    dbl_weights * 
      (
        dbl_observed - 
          weighted.mean(
            dbl_observed
            , dbl_weights
          )
      ) ^ 2
  ) -> dbl_sst
  
  dbl_r2 <- 1 - dbl_sse / dbl_sst
  
  return(dbl_r2)
  
}

?apply(
  array
  , margin
  , ...
)

df_occupations.ai %>% 
  # select(
  #   ends_with('.l')
  # ) %>% 
  group_nest(
    # row_number()
    occupation
  ) %>% 
  rename(
    occupation.scores = 
      data
  ) %>% 
  mutate(
    list_knn = 
      FNN::get.knnx(
        data = 
          df_occupations.ai %>% 
          select(ends_with('.l'))
        , query = occupation.scores
        , k = 873
      ) %>% 
      pluck(2)
  )

df_occupations.ai %>% 
  select(
    ends_with('.l')
  ) %>%
  group_nest(
    row_number()
  ) %>% 
  pull(data) %>%
  map(
    ~ FNN::get.knnx(
      data =
        df_occupations.ai %>%
        select(ends_with('.l'))
      , query = .x
      , k = 873
    )
  ) %>% 
  set_names(
    df_occupations.ai$
      occupation
  ) -> list_knn

map(
  list_knn
  , ~ df_occupations %>% 
    slice(as.vector(
      .x$nn.index
    )) %>% 
    mutate(
      .after = occupation
      , euclidean_distance = 
        as.vector(
          .x$nn.dist
        )
      , similarity = 
        fun_similarity(
          df_occupations.ai
          , euclidean_distance
          , .dbl_scale.lb = 0
          , .dbl_scale.ub = 100
        )
    )
) -> list_knn.matching

list_knn.matching$
  `Chief Executives` %>% 
  slice_head(n = 10) %>% 
  view

list_knn.matching %>% 
  map(
    ~ .x %>% 
      rename(
        occupation.comparison = 
          occupation
      )) %>% 
  bind_rows(
    .id = 'occupation'
  ) -> df_knn.matching

df_knn.matching %>% 
  group_by(
    occupation
  ) %>% 
  reframe(
    employment.wgt = 
      sum(
        similarity * 
          employment2
      )
    , labor_force = 
      sum(employment2)
  ) %>% 
  mutate(
    .after = 1
    , employment.coef1 = 
      employment.wgt / 
      sum(employment.wgt)
    , employment.coef2 = 
      employment.wgt / 
      labor_force
  ) -> df_employment.coef

df_employment.coef %>% 
  full_join(
    df_occupations %>% 
      mutate(
        .after = 
          employment2
        , employment.coef3 = 
          employment2 / 
          sum(employment2)
      )
  ) %>% 
  mutate(across(
    .cols = contains('employment.coef')
    ,.fns = ~ round(.x, 10)
  )) %>% 
  relocate(
    occupation
    , contains('employment.coef')
    , everything()
  ) -> df_employment.coef
 
df_employment.coef %>%
  view


list_knn.matching$`Chief Executives`

df_occupations.ai %>%
  slice()

list_knn$
  `Chief Executives`$
  nn.index

list_knn$`Chief Executives`$nn.dist
list_knn$`Chief Executives`$nn.index

list_knn$`Chief Sustainability Officers`$nn.dist
list_knn$`Chief Sustainability Officers`$nn.index


FNN::get.knnx(
  data = 
    df_occupations.ai %>% 
    select(ends_with('.l'))
  , query = 
    df_occupations.ai %>% 
    select(ends_with('.l'))
  , k = 873
) -> list_knn


list_knn$nn.dist %>% diag

mtx_knn.similarity %>% 
  as_tibble() %>% 
  slice(1) %>% 
  set_names(
    df_occupations.ai$
      occupation
  ) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'occupation.comparison'
    , values_to = 'similarity'
  ) %>% 
  mutate(
    .before = 1
    , occupation =
      df_occupations.ai %>%
      slice(1) %>% 
      pull(occupation)
  ) %>% 
  full_join(
    df_occupations %>% 
      select(
        occupation
        , employment2
      )
    , by = c(
      'occupation.comparison' = 
        'occupation'
    )
  ) %>% 
  reframe(
    employment.wgt = 
      sum(
        similarity * 
          employment2
      )
  )


list_knn$
  nn.dist

fun_similarity(
  .df_data.numeric = 
    df_occupations.ai
  , .dbl_distance = 
    list_knn$nn.dist
  , .dbl_scale.lb = 0
  , .dbl_scale.ub = 100
) -> mtx_knn.similarity


fun_r2.weighted(dsdsds)
fun_r2.weighted(list_employment.model$model.fit)

list_employment.model$model.fit$weights
list_employment.model$model.fit$m

fitted(list_employment.model)
fitted(list_employment.model$model.fit)

fitted(list_employment.model) ==
  fitted(list_employment.model$model.fit)

weights(list_employment.model)
weights(list_employment.model$model.fit)

# weights(list_employment.model) ==
# weights(list_employment.model$model.fit)

residuals(list_employment.model$model.fit)
list_employment.model$model.fit$m$fitted() %>% 
  as.numeric
list_employment.model$
  
  
  fun_r2(
    .int_vars.independent = 
      dsds %>% 
      select(
        ends_with('.l')
      ) %>% 
      ncol()
    , .dbl_observations = 
      dsds$
      coef.employment
    , .dbl_fitted = 
      dsdsds$
      fitted.values
    , .lgc_intercept = F
  )

list_employment.model$
  model.tidy %>% view

dsdsds %>% 
  broom::tidy() %>% 
  arrange(desc(
    estimate
  )) %>% 
  print(n = 200)

dsds %>% 
  mutate(
    .after = coef.employment
    , lm.employment = 
      dsdsds$fitted.values
    , lm.employment = 
      pmax(lm.employment, 0)
  ) %>% 
  # filter(
  #   # coef.employment > 0.5
  #   str_detect(
  #     str_to_lower(occupation)
  #     , 'econom|data|analys'
  #     # , 'data'
  #     # , 'engineer'
  #   )
  # ) %>%
  arrange(desc(
    coef.employment
  )) %>% 
  print(n = 100)

dsds$coef.employment[1]
dsdsds$fitted.values[1]

df_sample %>%
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item.score'
  ) %>% 
  mutate(
    item.score = 
      100 * item.score
  ) %>% 
  full_join(
    dsdsds$
      coefficients %>%
      as_tibble(
        rownames = 'item'
      ) %>% 
      rename(
        item.employment = 2
      )
  ) %>% 
  drop_na() %>%
  group_by(
    occupation
  ) %>% 
  reframe(
    coef.employment = sum(
      item.score * 
        item.employment
    )
  )

dsdsds %>% 
  summary()

fun_lm(
  .df_data = dsds
  , .sym_vars.dependent = 
    'coef.employment'
  , .sym_vars.independent = 
    dsds %>% 
    select(
      ends_with('.l')
    ) %>% 
    names()
  # , .sym_vars.dummies = 
  #   'entry_level_education'
  , .sym_vars.weights =
    'employment2'
  , .lgc_intercept = F
  # , .dbl_lower.bounds = 0
) -> list_employment.model

list_employment.model$
  model.tidy %>% view
mutate(
  estimate = round(estimate, 2)
) %>% 
  arrange(desc(
    estimate
  )) %>% 
  view


list_employment.model$r2
list_employment.model$rmse
list_employment.model$mae
list_employment.model$fitted.mean
list_employment.model$fitted.sd
