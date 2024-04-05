# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
  , 'dplyr', 'tidyr', 'stringr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.match',
  'CaoBittencourt' = 'atlas.comp',
  'CaoBittencourt' = 'atlas.gene',
  'CaoBittencourt' = 'atlas.aeq',
  'CaoBittencourt' = 'atlas.intc',
  # 'CaoBittencourt' = 'atlas.notiq',
  'CaoBittencourt' = 'atlas.class'
)

# Activate / install CRAN packages
lapply(
  chr_pkg
  , function(pkg){

    if(!require(pkg, character.only = T)){

      install.packages(pkg)

    }

    require(pkg, character.only = T)

  }
)

# Activate / install Git packages
Map(
  function(git, profile){

    if(!require(git, character.only = T)){

      install_github(
        paste0(profile, '/', git)
        , upgrade = F
        , force = T
      )

    }

    require(git, character.only = T)

  }
  , git = chr_git
  , profile = names(chr_git)
)

# - Data ------------------------------------------------------------------
# Occupations data frame
df_occupations <- read_csv('/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv')

# My preference-adjusted career profile
df_profile_adjusted <- read_csv('/home/Cao/Storage/github/atlas-research/data/questionnaires/questionnaire_Cao.csv')

# - Sample occupations ----------------------------------------------------
# Define sample occupations
c(
  'Mechanical Engineers',
  'Physicists',
  'Credit Analysts',
  'Dishwashers'
) -> chr_sample

# Sample occupations data frame
df_occupations %>%
  filter(
    occupation %in%
      chr_sample
  ) %>%
  arrange(factor(
    occupation
    , levels =
      chr_sample
  )) -> df_sample

# Select only occupations and attributes
df_sample %>%
  select(
    occupation
    , starts_with('skl_')
    , starts_with('abl_')
    , starts_with('knw_')
  ) -> df_sample

# [MIDPOINTS] ----------------------------------------------------------------
# - Generality and Competence ----------------------------------------------
# Use skill set generality as midpoint for attribute equivalence
# Use skill set competence as midpoint for interchangeability
df_sample %>%
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item_score'
  ) %>%
  group_by(
    occupation
  ) %>%
  reframe(
    generality =
      fun_gene_generality(
        item_score
      ),
    competence =
      fun_comp_competence(
        dbl_profile = item_score,
        dbl_scale_ub = 100,
        dbl_scale_lb = 0,
        dbl_generality = generality
      )
  ) -> df_midpoint

# [LINEAR WEIGHTS] ------------------------------------------------------
# - Euclidean matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_linear_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_linear

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_linear_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_linear_sub

# - BVLS matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_linear_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_linear

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_linear_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_linear_sub

# - Pearson matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_linear_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_linear

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_linear_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_linear_sub

# - Logit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_linear_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_linear

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_linear_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_linear_sub

# - Probit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_linear_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_linear

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_linear_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'linear'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_linear_sub

# [QUADRATIC WEIGHTS] ------------------------------------------------------
# - Euclidean matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_quadratic_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_quadratic

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_quadratic_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_quadratic_sub

# - BVLS matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_quadratic_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_quadratic

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_quadratic_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_quadratic_sub

# - Pearson matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_quadratic_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_quadratic

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_quadratic_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_quadratic_sub

# - Logit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_quadratic_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_quadratic

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_quadratic_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_quadratic_sub

# - Probit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_quadratic_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_quadratic

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_quadratic_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'quadratic'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_quadratic_sub

# [SPEC-ROOT WEIGHTS] ------------------------------------------------------
# - Euclidean matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_spec_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_spec

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_spec_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_spec_sub

# - BVLS matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_spec_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_spec

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_spec_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_spec_sub

# - Pearson matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_spec_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_spec

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_spec_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_spec_sub

# - Logit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_spec_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_spec

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_spec_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_spec_sub

# - Probit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_spec_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_spec

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_spec_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'speciality-root'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_spec_sub

# [Ä WEIGHTS] ------------------------------------------------------
# - Euclidean matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_eqvl_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_euclidean_eqvl

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_eqvl_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'euclidean'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_euclidean_eqvl_sub

# - BVLS matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_eqvl_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_bvls_eqvl

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'bvls'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_eqvl_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'bvls'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_bvls_eqvl_sub

# - Pearson matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_eqvl_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_pearson_eqvl

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'pearson'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_eqvl_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'pearson'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_pearson_eqvl_sub

# - Logit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_eqvl_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_logit_eqvl

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'logit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_eqvl_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'logit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_logit_eqvl_sub

# - Probit matching ----------------------------------------------------
# Regular matching
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_eqvl_mine

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_probit_eqvl

# With overqualification substitution
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'probit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_eqvl_mine_sub

# Sample occupations' matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows = df_sample
  , chr_method = 'probit'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
) -> list_probit_eqvl_sub

# [DATA WRANGLING] --------------------------------------------------------
# - Lists' names -------------------------------------------------------------
# Get names of lists in environment
str_split(
  string =
    ls()[
      str_detect(
        ls(),
        '(?=.*list)(?=.*mine)'
      )
    ]
  , pattern = '_'
  , simplify = T
)[,2] %>%
  unique() ->
  chr_methods

# My matches
ls()[
  str_detect(
    ls()
    , paste0(
      '(?=.*'
      , paste0(
        chr_methods
        , collapse = '|'
      )
      , ')(?=.*mine)'
    )
  )
] -> chr_matches_mine

# Occupations' matches
ls()[
  str_detect(
    ls()
    , paste0(
      chr_methods
      , collapse = '|'
    )
  ) &
    str_detect(
      ls()
      , 'mine'
      , negate = T
    )
] -> chr_matches

# Euclidean matching
chr_matches_mine %>%
  syms() %>%
  set_names(
    chr_matches_mine
  ) %>%
  map(eval) %>%
  map(
    ~ .x$mtx_similarity %>%
      as_tibble(
        rownames =
          'occupation'
      ) %>%
      rename(
        similarity = 2
      )
  ) %>%
  bind_rows(
    .id = 'model'
  ) %>%
  mutate(
    model =
      str_split(
        model
        , '_'
        , simplify = T
      )[,c(2:3,5)]
    , model =
      paste0(
        model[,1],
        '_',
        model[,2],
        '_',
        model[,3]
      )
    , model =
      str_remove_all(
        model
        , '^*_$'
      )
  ) %>%
  group_by(
    model
  ) %>%
  arrange(desc(
    similarity
  ), .by_group = T
  ) %>%
  ungroup() ->
  df_matches_mine

df_matches_mine %>%
  group_by(
    model
  ) %>%
  reframe(
    max = max(similarity),
    min = min(similarity),
    range = max - min,
    mean = mean(similarity),
    sd = sd(similarity)
  ) %>%
  left_join(
    df_matches_mine %>%
      select(
        model,
        occupation
      ) %>%
      rename(
        best_match =
          occupation
      ) %>%
      group_by(
        model
      ) %>%
      slice(1)
  ) %>%
  left_join(
    df_matches_mine %>%
      select(
        model,
        occupation
      ) %>%
      rename(
        worst_match =
          occupation
      ) %>%
      group_by(
        model
      ) %>%
      slice(n())
  ) %>%
  arrange(desc(
    range
  )) -> df_models_mine

df_models_mine %>%
  print(
    n = Inf
  )

# - Euclidean matching ----------------------------------------------------
# My career matches



list(
  list_euclidean_eqvl_mine,
  list_euclidean_eqvl_mine_sub
) %>%
  map(
    ~ .x$mtx_similarity
  )


# Sample occupations' matches

# - Career matching (s, ß) -------------------------------------------------------
# My career matches
fun_match_similarity(
  df_data_rows = df_sample
  , df_query_rows =
    df_profile_adjusted
  # df_sample %>%
  # select(
  #   occupation,
  #   career_cluster,
  #   starts_with('skl_'),
  #   starts_with('abl_'),
  #   starts_with('knw_')
  # ) %>%
  # slice(1:2)
  , chr_method = 'euclidean'
  # , chr_method = 'pearson'
  # , chr_method = 'bvls'
  # , chr_method = 'logit'
  # , chr_method = 'probit'
  # , chr_weights = 'linear'
  # , chr_weights = 'quadratic'
  # , chr_weights = ''speciality-root''
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = F
) -> list_matches

# list_matches$matches %>% map(length)
#
# list_matches
#
# df_profile_adjusted
#
# list_matches$query %>% tail()
# list_matches$data %>% tail()
#
# list_matches$query %>% head(874) %>% tail()
# list_matches$data %>% head(874) %>% tail()

# 0 = shit, 1 = ok, 2 = good, 3 = great, 4 = awesome
# euclidean linear 1
# euclidean quadratic 3
# euclidean 'speciality-root' 2
# euclidean attribute-eqvl 3.5

# pearson linear 2.5
# pearson quadratic 1.5
# pearson 'speciality-root' 2
# pearson attribute-eqvl 1

# bvls linear 0
# bvls quadratic 0
# bvls 'speciality-root' 0
# bvls attribute-eqvl 0.5

# logit/probit linear 2
# logit/probit quadratic 2
# logit/probit 'speciality-root' 2
# logit/probit attribute-eqvl 3

list_matches$
  mtx_similarity %>%
  as_tibble(
    rownames = 'comparison_occupation'
  ) %>%
  pivot_longer(
    cols = -1
    , names_to = 'occupation'
    , values_to = 'similarity'
  ) %>%
  left_join(
    df_sample %>%
      select(
        occupation,
        education_years
      ) %>%
      rename(
        years_min = education_years
      )
    , by = c(
      'comparison_occupation' =
        'occupation'
    )
  ) %>%
  left_join(
    df_sample %>%
      select(
        occupation,
        education_years
      ) %>%
      rename(
        years = education_years
      )
  ) %>%
  left_join(
    df_midpoint
    , by = c(
      'comparison_occupation' =
        'occupation'
    )
  ) -> df_match

df_match$similarity[df_match$similarity < 0]

df_match %>%
  mutate(
    years = if_else(
      occupation == 'Cao'
      , 22
      , years
    )
    , ß = fun_intc_ss(
      dbl_similarity = similarity
      , dbl_competence = competence
      , dbl_years = years
      , dbl_years_min = years_min
    )
    , hireability =
      fun_eqvl_bin(ß)
  ) %>%
  select(
    comparison_occupation,
    occupation,
    comp_class,
    gene_class,
    similarity,
    ß,
    hireability
  ) -> df_match

df_match %>%
  split(.$occupation) ->
  list_match

list_match %>%
  map(
    arrange,
    similarity
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

list_match %>%
  map(
    arrange,
    -similarity
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

list_match %>%
  map(
    arrange,
    ß
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

list_match %>%
  map(
    arrange,
    -ß
  ) %>%
  map(
    print,
    n = 22
  ) %>%
  invisible()

df_match %>%
  left_join(
    df_sample %>%
      select(
        occupation,
        employment_variants
      )
    , by = c(
      'comparison_occupation' =
        'occupation'
    )
  ) %>%
  group_by(
    occupation
  ) %>%
  reframe(
    employability = sum(
      weighted.mean(
        # x = hireability,
        # x = ß,
        x = hireability * ß,
        w = employment_variants
      )
    )
  )




