library(devtools)

# use_dev_package(
#   package = 'CaoBittencourtFerreira/atlas.ftools'
#   , type = 'Depends'
# )

remove.packages('atlas.ftools')
remove.packages('atlas.efa')

# install_github('CaoBittencourtFerreira/atlas.ftools')
# install_github('caobittencourtferreira/atlas.ftools')
#
# library(atlas.ftools)

install_github('CaoBittencourtFerreira/atlas.efa')
install_github('caobittencourtferreira/atlas.efa')

library(atlas.efa)
library(atlas.ftools)

# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

# - Test ------------------------------------------------------------------
atlas.efa::fun_efa_fa(
  df_data =
    df_occupations %>%
    select(ends_with('.l'))
  , int_factors = 15
  , chr_rotation = 'equamax'
  , dbl_weights =
    df_occupations$
    employment2
  , int_min_items_factor = 3
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
  , lgc_show_diagrams = T
  , lgc_show_results = F
) -> list_efa

list_efa

# fun_efa_fa(
#   df_data =
#     df_occupations %>%
#     select(ends_with('.l'))
#   , dbl_weights =
#     df_occupations$
#     employment2
#   , int_factors = 15
#   # , chr_rotation = c('equamax', 'oblimin', 'varimin')
#   , chr_rotation = 'equamax'
#   , int_min_items_factor = 3
#   , lgc_remove_low_msai_items = T
#   , lgc_adequacy_testing = F
#   # , lgc_optimal_nfactors = T
#   , lgc_optimal_nfactors = F
#   , lgc_show_diagrams = F
#   , lgc_show_results = F
# ) -> list_efa
#
# list_efa$model_performance
# list_efa$reliability_metrics
# list_efa$reliability_evaluation
# list_efa$factor_correlations
# list_efa$loadings_long
# list_efa$adequacy_tests
# list_efa$nfactors
# list_efa$model
#
# map(list_efa, class)

# fun_efa_vfa_top_items(
#   df_data =
#     df_occupations %>%
#     select(ends_with('.l'))
#   , dbl_weights =
#     df_occupations$
#     employment2
#   , int_factors = 15
#   , chr_rotation = c('equamax', 'oblimin')
#   , int_items_total_vector = c(50, 100, 200)
#   , lgc_uneven_factors = T
#   , int_min_items_factor = 3
#   , lgc_remove_low_msai_items = T
#   , lgc_adequacy_testing = F
#   , lgc_optimal_nfactors = F
#   , lgc_show_diagrams = F
#   , lgc_show_results = F
# ) -> list_efa_top_items
#
# list_efa_top_items$efa$model_performance
# list_efa_top_items$efa$reliability_metrics
# list_efa_top_items$efa$reliability_evaluation
# list_efa_top_items$efa$factor_correlations
# list_efa_top_items$efa$loadings_long
# list_efa_top_items$efa$adequacy_tests
# list_efa_top_items$efa$nfactors
# list_efa_top_items$efa$models
#
# list_efa_top_items$top_items
#
# list_efa_top_items$
#   top_items %>%
#   list_flatten() %>%
#   map(class)
