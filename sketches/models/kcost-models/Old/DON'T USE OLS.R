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
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.R')

# POPULATION-WEIGHTED EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')

# --- HUMAN CAPITAL COST ESTIMATION ---------------------------------------------
# DATA WRANGLING ----------------------------------------------------------
df_occupations %>%
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
  ) -> dfdf

# OLS x NNLS REGRESSION ---------------------------------------------------
Map(
  function(
    chr_dummies
    , dbl_lb
    , lgc_intercept
  ){
    
    dfdf %>% 
      fun_lm(
        .sym_vars.dependent = 'annual_wage_2021'
        , .sym_vars.independent =  
          df_occupations %>% 
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
    'ols.control' = 'entry_level_education'
    , 'ols' = character()
    , 'nnls.control' = 'entry_level_education'
    , 'nnls' = character()
    )
  , dbl_lb = c(-Inf, -Inf, 0, 0)
  , lgc_intercept = c(F, F, F, F)
) -> list_models

# OLS DOESN'T MAKE SENSE!!! -----------------------------------------------
# # REGRESSION STATISTICS ---------------------------------------------------
# list_models$ols.control$r2
# list_models$ols.control$rmse
# list_models$ols.control$mae
# list_models$ols.control$fitted.mean
# list_models$ols.control$fitted.sd
# list_models$ols.control$fitted
# list_models$ols.control$model.tidy %>% view
# 
# list_models$ols$r2
# list_models$ols$rmse
# list_models$ols$mae
# list_models$ols$fitted.mean
# list_models$ols$fitted.sd
# list_models$ols$fitted
# list_models$ols$model.tidy %>% view
# 
# list_models$nnls.control$r2
# list_models$nnls.control$rmse
# list_models$nnls.control$mae
# list_models$nnls.control$fitted.mean
# list_models$nnls.control$fitted.sd
# list_models$nnls.control$fitted
# list_models$nnls.control$model.tidy %>% view
# 
# list_models$nnls$r2
# list_models$nnls$rmse
# list_models$nnls$mae
# list_models$nnls$fitted.mean
# list_models$nnls$fitted.sd
# list_models$nnls$fitted
# list_models$nnls$model.tidy %>% view
# 
# # EXPECTED WAGE -----------------------------------------------------------
# # dfdf %>% 
# #   mutate(
# #     human_capital = list_kcost$model.fit$m$fitted()
# #     , wage_model.ratio = human_capital / annual_wage_2021
# #     , .after = annual_wage_2021
# #   ) %>% 
# #   full_join(
# #     df_occupations.pop %>% 
# #       select(
# #         !ends_with('.l')
# #       ) 
# #   ) %>% view
# 
# list_models %>%
#   lapply(
#     function(model){
#       
#       full_join(
#         x = dfdf %>%
#           pivot_longer(
#             cols = ends_with('.l')
#             , names_to = 'attribute'
#             , values_to = 'level'
#           )
#         , y = model$model.tidy
#         , by = c('attribute' = 'term')
#       ) %>% 
#         group_by(
#           occupation
#         ) %>% 
#         summarise(
#           human_capital = model$fitted
#         ) %>% 
#         full_join(
#           df_occupations.pop %>% 
#             select(
#               !ends_with('.l')
#             )
#         ) %>% 
#         mutate(
#           wage_model.ratio = human_capital / annual_wage_2021
#           , .after = human_capital
#         ) %>% 
#         arrange(
#           desc(wage_model.ratio)
#         ) -> df_kcost.pop
#       
#       df_kcost.pop %>% 
#         unique() -> df_kcost
#       
#       return(list(
#         'kcost' = df_kcost
#         , 'kcost.pop' = df_kcost.pop
#       ))
#       
#     }
#   ) -> list_kcost
# 
# full_join(
#   x = dfdf %>%
#     pivot_longer(
#       # cols = c(ends_with('.l'), 'entry_level_education')
#       cols = ends_with('.l')
#       , names_to = 'attribute'
#       , values_to = 'level'
#     )
#   , y = list_kcost$model.tidy %>% 
#     filter(!str_detect(term,c(
#       'entry_level_education|career_cluster|typical_on_the_job_training'
#     )))
#   , by = c('attribute' = 'term')
# ) %>% 
#   group_by(
#     occupation
#   ) %>% 
#   summarise(
#     human_capital = sum(estimate * level)
#   ) %>% 
#   full_join(
#     df_occupations.pop %>% 
#       select(
#         !ends_with('.l')
#       ) 
#   ) %>% 
#   mutate(
#     wage_model.ratio = human_capital / annual_wage_2021
#     , .after = human_capital
#   ) %>% 
#   arrange(
#     desc(wage_model.ratio)
#   ) -> df_kcost.pop
# 
# df_kcost.pop %>% 
#   unique() -> df_kcost
# 
# # --- VISUALIZATION -------------------------------------------------------
# # CAPITAL COST BY ATTRIBUTE -----------------------------------------------
# 
# # CAPITAL COST DISTRIBUTION -----------------------------------------------
# 
# # HUMAN CAPITAL PER OCCUPATION (POPULATION-WEIGHTED) --------------------------------------------
# df_kcost.pop %>%
#   filter(
#     wage_model.ratio >= 0.75
#     , wage_model.ratio <= 1.25
#   ) %>% 
#   unique() %>% 
#   nrow() / nrow(df_kcost.pop)
# 
# df_kcost.pop %>% 
#   filter(
#     wage_model.ratio >= 0.85
#     , wage_model.ratio <= 1.15
#   ) %>% 
#   unique() %>% 
#   nrow() / nrow(df_kcost.pop)
# 
# df_kcost.pop %>% 
#   filter(
#     round(wage_model.ratio, 1) == 1
#   ) %>% 
#   unique() %>% 
#   nrow() / nrow(df_kcost.pop)
# 
# # HUMAN CAPITAL PER OCCUPATION --------------------------------------------
# df_kcost %>%
#   filter(
#     wage_model.ratio >= 0.75
#     , wage_model.ratio <= 1.25
#   ) %>% 
#   unique() %>% 
#   nrow() / nrow(df_kcost)
# 
# df_kcost %>% 
#   filter(
#     wage_model.ratio >= 0.85
#     , wage_model.ratio <= 1.15
#   ) %>% 
#   unique() %>% 
#   nrow() / nrow(df_kcost)
# 
# df_kcost %>% 
#   filter(
#     round(wage_model.ratio, 1) == 1
#   ) %>% 
#   unique() %>% 
#   nrow() / nrow(df_kcost)
# 
# # EXPECTED WAGE VS ACTUAL WAGE DISTRIBUTION (POPULATION-WEIGHTED) -------------------------------
# df_kcost.pop %>% 
#   fun_plot.histogram(aes(
#     x = wage_model.ratio
#   )
#   # , .sym_facets = entry_level_education
#   # , .int_facets = 4
#   # , .reorder_desc = T
#   , .dbl_limits.x = c(0, NA)
#   ) + 
#   geom_vline(
#     xintercept = seq(0.75, 1.25, 0.25)
#   )
# 
# # EXPECTED WAGE VS ACTUAL WAGE DISTRIBUTION -------------------------------
# df_kcost %>% 
#   fun_plot.histogram(aes(
#     x = wage_model.ratio
#   )
#   # , .sym_facets = entry_level_education
#   # , .int_facets = 4
#   # , .reorder_desc = T
#   , .dbl_limits.x = c(0, NA)
#   ) + 
#   geom_vline(
#     xintercept = seq(0.75, 1.25, 0.25)
#   )
# 
# # --- EXPORT ----------------------------------------------------
# # XLSX --------------------------------------------------------------------
# list_kcost$model.tidy %>% 
#   arrange(desc(estimate), desc(p.value)) %>%
#   mutate(across(
#     .cols = where(is.numeric)
#     , .fns = function(x){round(x, 4)}
#   )) %>% 
#   openxlsx::write.xlsx('kcost.EFA2.xlsx')
# 
# 
# list_kcost$model.tidy %>% view
# list_kcost$r2
# list_kcost$r2.adjusted
# 
# 
