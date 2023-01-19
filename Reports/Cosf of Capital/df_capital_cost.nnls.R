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
  'all' = df_occupations
  , 'efa' = df_occupations.efa
  , 'efa.comp' = df_occupations.efa.comp
  , 'efa.context' = df_occupations.efa.context
) -> list_df_occupations

# --- HUMAN CAPITAL COST ESTIMATION ---------------------------------------------
# DATA WRANGLING ----------------------------------------------------------
list_df_occupations %>% 
  lapply(
    function(df){
      
      df %>%
        mutate(
          annual_wage_2021 = annual_wage_2021
          , across(
            .cols = ends_with('.l')
            ,.fns = function(x){100*x}
          )
          , sample_weight = sum(employment2) / employment2
          # , entry_level_education = recode(
          #   entry_level_education
          #   , "Bachelor's degree" = 17 + 4
          #   , "Postsecondary nondegree award" = 17 + 4 + 2
          #   , "High school diploma or equivalent" = 17
          #   , "Master's degree" = 17 + 5
          #   , "Associate's degree" = 17 + 2
          #   , "No formal educational credential" = 14
          #   , "Some college, no degree" = 17 + 2
          #   , "Doctoral or professional degree" = 17 + 7
          # )
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
        .sym_vars.dependent = 'annual_wage_2021'
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
  , chr_dummies = list(
    'all' = character()
    , 'efa' = character()
    , 'efa.comp' = character()
    , 'efa.context' = character()
    # Control
    , 'all.control' = 'entry_level_education'
    , 'efa.control' = 'entry_level_education'
    , 'efa.comp.control' = 'entry_level_education'
    , 'efa.context.control' = 'entry_level_education'
    # 
    # , 'all.control' = c('entry_level_education', 'projected_growth_2020.2030')
    # , 'efa.control' = c('entry_level_education', 'projected_growth_2020.2030')
    # , 'efa.comp.control' = c('entry_level_education', 'projected_growth_2020.2030')
    # , 'efa.context.control' = c('entry_level_education', 'projected_growth_2020.2030')
    # 
    # , 'all.control' = 'projected_growth_2020.2030'
    # , 'efa.control' = 'projected_growth_2020.2030'
    # , 'efa.comp.control' = 'projected_growth_2020.2030'
    # , 'efa.context.control' = 'projected_growth_2020.2030'
  )
  , dbl_lb = rep(0, 8)
  , lgc_intercept = rep(F, 8)
  , df_data = rep(list_df_occupations, 2)
) -> list_models

# REGRESSION STATISTICS COMPARISON ---------------------------------------------------
# R2
list_models$all$r2
list_models$efa$r2
list_models$efa.comp$r2
list_models$efa.context$r2

list_models$all.control$r2
list_models$efa.control$r2
list_models$efa.comp.control$r2
list_models$efa.context.control$r2

# RMSE
list_models$all$rmse
list_models$efa$rmse
list_models$efa.comp$rmse
list_models$efa.context$rmse

list_models$all.control$rmse
list_models$efa.control$rmse
list_models$efa.comp.control$rmse
list_models$efa.context.control$rmse

# MAE
list_models$all$mae
list_models$efa$mae
list_models$efa.comp$mae
list_models$efa.context$mae

list_models$all.control$mae
list_models$efa.control$mae
list_models$efa.comp.control$mae
list_models$efa.context.control$mae

# Fitted mean
list_models$all$fitted.mean
list_models$efa$fitted.mean
list_models$efa.comp$fitted.mean
list_models$efa.context$fitted.mean

list_models$all.control$fitted.mean
list_models$efa.control$fitted.mean
list_models$efa.comp.control$fitted.mean
list_models$efa.context.control$fitted.mean

# Fitted standard deviation
list_models$all$fitted.sd
list_models$efa$fitted.sd
list_models$efa.comp$fitted.sd
list_models$efa.context$fitted.sd

list_models$all.control$fitted.sd
list_models$efa.control$fitted.sd
list_models$efa.comp.control$fitted.sd
list_models$efa.context.control$fitted.sd

# Parameters
list_models$all$model.tidy %>% view
list_models$efa$model.tidy %>% view
list_models$efa.comp$model.tidy %>% view
list_models$efa.context$model.tidy %>% view

list_models$all.control$model.tidy %>% view
list_models$efa.control$model.tidy %>% view
list_models$efa.comp.control$model.tidy %>% view
list_models$efa.context.control$model.tidy %>% view

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
