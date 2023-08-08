library(devtools)

remove.packages('atlas.ftools')
remove.packages('atlas.fstatics')
remove.packages('atlas.efa')
remove.packages('atlas.eqvl')
remove.packages('atlas.match')

install_github('CaoBittencourtFerreira/atlas.ftools')
install_github('caobittencourtferreira/atlas.ftools')

install_github('CaoBittencourtFerreira/atlas.efa')
install_github('caobittencourtferreira/atlas.efa')

install_github('CaoBittencourtFerreira/atlas.fstatics')
install_github('caobittencourtferreira/atlas.fstatics')

install_github('CaoBittencourtFerreira/atlas.eqvl')
install_github('caobittencourtferreira/atlas.eqvl')

install_github('CaoBittencourtFerreira/atlas.match')
install_github('caobittencourtferreira/atlas.match')

library(atlas.efa)
library(atlas.ftools)
library(atlas.fstatics)
library(atlas.eqvl)
library(atlas.match)

# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

# - EFA ------------------------------------------------------------------
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

# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)
library(tictoc)

# read_rds(
read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

# - Factor-analytic comparative statics -----------------------------------
tic()
fun_fstatics_impact(
  df_data =
    df_occupations
  , dbl_weights =
    df_occupations$
    employment2
  , efa_model =
    efa_model
  , dbl_factors_impact =
    runif(
      efa_model$
        factors
      , min = -100
      , max = 100
    )
  , lgc_aggregate = T
) -> dsds
toc()

dsds

# [TEST] ------------------------------------------------------------------

# - Equivalence coefficient -----------------------------------------------
atlas.eqvl::fun_eqvl_equivalence(runif(1, 0, 1))



# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)
library(tictoc)

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
) -> df_input

# - BVLS similarity test ------------------------------------------------------------------
rm(dsds)

tic()
fun_match_similarity(
  df_data_rows =
    df_occupations %>%
    select(
      occupation
      , ends_with('.l')
    )
  , chr_method = 'bvls'
  , df_query_rows =
    df_input
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
) -> dsds
toc()

dsds$
  df_similarity %>%
  select(!ends_with('.l')) %>%
  arrange(desc(similarity)) %>%
  print(n = nrow(.))

# - Pearson similarity test ------------------------------------------------------------------
rm(dsds)

tic()
fun_match_similarity(
  df_data_rows =
    df_occupations %>%
    select(
      occupation
      , ends_with('.l')
    )
  , chr_method = 'pearson'
  , df_query_rows =
    df_input
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
) -> dsds
toc()

dsds$
  df_similarity %>%
  select(!ends_with('.l')) %>%
  arrange(desc(similarity)) %>%
  print(n = nrow(.))

# - Logit similarity test ------------------------------------------------------------------
rm(dsds)

tic()
fun_match_similarity(
  df_data_rows =
    df_occupations %>%
    select(
      occupation
      , ends_with('.l')
    )
  , chr_method = 'logit'
  , df_query_rows =
    df_input
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
) -> dsds
toc()

dsds$
  df_similarity %>%
  select(!ends_with('.l')) %>%
  arrange(desc(similarity)) %>%
  print(n = nrow(.))

# - Similarity matrix test ------------------------------------------------
rm(dsds)

tic()
fun_match_similarity(
  df_data_rows =
    df_occupations %>%
    slice(1:10) %>%
    select(
      occupation
      , ends_with('.l')
    )
  , df_query_rows =
    df_occupations %>%
    slice(1:10) %>%
    select(
      occupation
      , ends_with('.l')
    )
  , chr_method = 'bvls'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col =
    'occupation'
) -> dsds
toc()

dsds

