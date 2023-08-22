library(devtools)

devtools::install_github('CaoBittencourtFerreira/atlas.ftools', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.efa', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.fstatics', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.eqvl', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.match', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.intc', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.skew', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.kcoef', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.notiq', force = T)
devtools::install_github('CaoBittencourtFerreira/atlas.misc', force = T)

# library(atlas.efa)
# library(atlas.ftools)
# library(atlas.fstatics)
# library(atlas.eqvl)
# library(atlas.match)
# library(atlas.intc)
# library(atlas.skew)
# library(atlas.kcoef)
# library(atlas.notiq)


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
) -> list_efa

atlas.efa::fun_efa_vfa(
  df_data =
    df_occupations %>%
    select(ends_with('.l'))
  , chr_rotation = 'equamax'
  , dbl_weights =
    df_occupations$
    employment2
  , int_min_items_factor = 3
  , lgc_remove_low_msai_items = T
  , lgc_adequacy_testing = T
  , lgc_optimal_nfactors = T
) -> list_efa

list_efa$
  loadings_long$
  EFA_equamax_14factors %>% 
  View

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

# [TEST] ------------------------------------------------------------------
# - Interchangeability test 1 -----------------------------------------------
fun_intc_interchangeability(
  dbl_similarity = runif(1, 0, 1)
  , dbl_scaling = 1
)

# - Interchangeability test 2 -----------------------------------------------
fun_intc_interchangeability(
  dbl_similarity = runif(1, 0, 1)
  , dbl_scaling = 1
  , dbl_years_education = 21
  , dbl_years_education_min = 25
)

# [TEST] ------------------------------------------------------------------
# - Sd-adjusted mode 1 ------------------------------------------------------
fun_skew_sdmode(
  dbl_var = pmax(rnorm(1000, 50, sd = 15), 0)
  , dbl_weights = runif(1000, 25000, 250000)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_discount = 0.25
)

# - Sd-adjusted mode 2 ------------------------------------------------------
fun_skew_sdmode(
  dbl_var =
    pmax(
      cbind(
        rnorm(1000, 50, sd = 15),
        rnorm(1000, 50, sd = 15),
        rnorm(1000, 50, sd = 15),
        rnorm(1000, 50, sd = 15),
        rnorm(1000, 50, sd = 15)
      ), 0
    )
  , dbl_weights = runif(1000, 25000, 250000)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_discount = 0.25
)

# - Sd-adjusted mode 3 ------------------------------------------------------
pmax(
  cbind(
    rnorm(1000, 50, sd = 15),
    rnorm(1000, 50, sd = 15),
    rnorm(1000, 50, sd = 15),
    rnorm(1000, 50, sd = 15),
    rnorm(1000, 50, sd = 15)
  ), 0
) -> dsds

colnames(dsds) <- letters[1:ncol(dsds)]

fun_skew_sdmode(
  dbl_var = dsds
  , dbl_weights = runif(1000, 25000, 250000)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_discount = 0.25
)

# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

# - Capital flexibility ------------------------------------------------------
fun_kcoef_kflex_macro(
  dbl_var = df_occupations$active_listening.l
  , dbl_weights = df_occupations$employment2
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_discount = 0.25
)

# - Capital flexibility data frame ------------------------------------------------------
fun_kcoef_kflex_macro_df(
  df_data =
    df_occupations %>%
    select(
      occupation
      , ends_with('.l')
    )
  , dbl_weights =
    df_occupations$
    employment2
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_discount = 0.25
  , lgc_sample_variance = F
) -> df_kflex

df_kflex

# [TEST] ------------------------------------------------------------------
# - NOT IQ ----------------------------------------------------------------
fun_notiq_quotient(
  dbl_proxy_scores =
    pmin(pmax(
      rnorm(30, mean = 33, sd = 14.8)
      , 0), 100)
  , dbl_proxy_mean = 33
  , dbl_proxy_sd = 14.8
)
