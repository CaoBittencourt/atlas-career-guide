# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
  , 'weights' #Fast/weighted correlation method
  , 'dplyr', 'tidyr', 'stringr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.match',
  'CaoBittencourt' = 'atlas.comp',
  'CaoBittencourt' = 'atlas.gene',
  'CaoBittencourt' = 'atlas.aeq',
  'CaoBittencourt' = 'atlas.intc',
  'CaoBittencourt' = 'atlas.plot',
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
# # Define sample occupations
# c(
#   'Mechanical Engineers',
#   'Physicists',
#   'Credit Analysts',
#   'Dishwashers',
#   'Registered Nurses',
#   'Hospitalists',
#   'Philosophy and Religion Teachers, Postsecondary'
# ) -> chr_sample

df_occupations$
  occupation ->
  chr_sample

# Sample occupations data frame
df_occupations %>%
  filter(
    occupation %in%
      chr_sample
  ) %>%
  mutate(
    occupation = factor(
      occupation
      , levels =
        chr_sample
    )
  ) %>%
  arrange(
    occupation
  ) -> df_sample

# Select only occupations and attributes
df_sample %>%
  select(
    occupation
    , starts_with('skl_')
    , starts_with('abl_')
    , starts_with('knw_')
  ) -> df_sample

# [MODELS] ----------------------------------------------------------------
# - Midpoints ----------------------------------------------
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

df_midpoint %>%
  arrange(desc(
    competence
  )) %>%
  print(
    n = Inf
  )

# - Matching methods ------------------------------------------------------
# Matching methods to apply
c(
  # 'bvls',
  # 'logit',
  # 'probit',
  # 'pearson',
  'euclidean'
) -> chr_methods

# - Weighting methods ------------------------------------------------------
# Weighting methods to apply
c(
  # 'linear',
  # 'quadratic',
  # 'speciality-root',
  'attribute-eqvl'
) -> chr_weights

# - Overqualification substitution ----------------------------------------
# Whether or not to apply overqualification substitution
# c(T, F) -> lgc_over_sub
c(F) -> lgc_over_sub

# - Matching-weights-sub combinations -----------------------------------------
# Generate all model matching-weights-sub combinations
crossing(
  method = chr_methods,
  weight = chr_weights,
  sub = lgc_over_sub
) -> df_methods

df_methods %>%
  mutate(
    .before = 1
    , model = paste0(
      method, '_', weight, if_else(
        sub, '_sub', ''
      )
    )
    , model = str_replace_all(
      model, '-', '_'
    )
  ) -> df_methods

# # - Define baseline model -------------------------------------------------
# # Most basic matching method
# 'euclidean_linear' -> chr_baseline

# [MATCHING] --------------------------------------------------------------
# # - My matches ------------------------------------------------------------
# # My career matches
# Map(
#   function(model, method, weight, over_sub){
#
#     # Apply matching function
#     return(
#       fun_match_similarity(
#         df_data_rows = df_occupations
#         , df_query_rows = df_profile_adjusted
#         , chr_method = method
#         , chr_weights = weight
#         , dbl_scale_ub = 100
#         , dbl_scale_lb = 0
#         , chr_id_col = 'occupation'
#         , lgc_sort = T
#         , lgc_overqualification_sub = over_sub
#       )
#     )
#
#   }
#   , model = df_methods$model
#   , method = df_methods$method
#   , weight = df_methods$weight
#   , over_sub = df_methods$sub
# ) -> list_matches_mine

# - Sample occupations' matches ------------------------------------------------------------
# My career matches
Map(
  function(model, method, weight, over_sub){

    # Apply matching function
    return(
      fun_match_similarity(
        df_data_rows = df_occupations
        , df_query_rows = df_sample
        , chr_method = method
        , chr_weights = weight
        , dbl_scale_ub = 100
        , dbl_scale_lb = 0
        , chr_id_col = 'occupation'
        , lgc_sort = T
        , lgc_overqualification_sub = over_sub
      )
    )

  }
  , model = df_methods$model
  , method = df_methods$method
  , weight = df_methods$weight
  , over_sub = df_methods$sub
) -> list_matches_sample

# [TAXONOMY] --------------------------------------------------------------
# - Enforce symmetry function ---------------------------------------------
fun_taxa_symmetric <- function(
    mtx_square,
    chr_transform = c('min', 'mean', 'max')
){

  # arguments validation
  stopifnot(
    "'mtx_square' must be a numeric square matrix." =
      all(
        is.numeric(mtx_square)
        , is.matrix(mtx_square)
        , nrow(mtx_square) ==
          ncol(mtx_square)
      )
  )

  stopifnot(
    "'chr_transform' must be either 'min', 'mean', or 'max'." =
      any(chr_transform == c('min', 'mean', 'max'))
  )

  # data wrangling
  chr_transform[[1]] -> chr_transform

  # if matrix is symmetric exit
  if(isSymmetric.matrix(mtx_square)){

    # warning
    warning("'mtx_square' is already symmetric.")

    # output
    return(mtx_square)

    stop()

  }

  # transpose matrix
  t(mtx_square) -> mtx_square_t

  # if matrix is not symmetric
  # enforce symmetry using transformation
  if(chr_transform == 'min'){

    # get whichever value is lesser
    mtx_square * (
      mtx_square <
        mtx_square_t
    ) + mtx_square_t * (
      mtx_square_t <=
        mtx_square
    ) -> mtx_square

  }

  if(chr_transform == 'mean'){

    # get mean values
    (
      mtx_square +
        mtx_square_t
    ) / 2 -> mtx_square

  }

  if(chr_transform == 'max'){

    # get whichever value is greater
    mtx_square * (
      mtx_square >
        mtx_square_t
    ) + mtx_square_t * (
      mtx_square_t >=
        mtx_square
    ) -> mtx_square

  }

  # output
  return(mtx_square)

}

# - Enforce symmetry ---------------------------------------------
list_matches_sample$
  euclidean_attribute_eqvl$
  mtx_similarity %>%
  fun_taxa_symmetric(
    chr_transform = 'min'
  ) -> mtx_taxa

# - Classify without fun_class_classifier ---------------------------
tibble(
  taxon = factor(c('Reino', 'Filo', 'Classe', 'Ordem', 'Família', 'Gênero', 'Espécie'))
  , belong = seq(0, 1, length.out = 7)
) -> df_categories

df_occupations$
  occupation %>%
  set_names(.) %>%
  map(~ df_categories) %>%
  bind_rows(
    .id = 'occupation'
  ) %>%
  right_join(
    mtx_taxa %>%
      as_tibble(
        rownames =
          'comparison_occupation'
      ) %>%
      pivot_longer(
        cols = -1,
        names_to = 'occupation',
        values_to = 'similarity'
      )
  ) %>%
  mutate(
    belong =
      similarity >=
      belong
  ) %>%
  relocate(
    !names(df_categories)
  ) -> df_taxa

df_taxa %>%
  pivot_wider(
    names_from = taxon,
    values_from = belong
  ) -> dsds

dsds

# - Classify matrix --------------------------------------------
mtx_taxa %>%
  fun_class_classifier(
    dbl_scale_lb = 0,
    dbl_scale_ub = 1,
    int_levels = 7,
    chr_class_labels = c(
      'Reino',
      'Filo',
      'Classe',
      'Ordem',
      'Família',
      'Gênero',
      'Espécie'
    )
  ) -> mtx_taxa

# - Taxonomy data frame ---------------------------------------------------
# mtx_taxa -> dsds

mtx_taxa[
  upper.tri(
    mtx_taxa,
    diag = F
  )
] <- NA

mtx_taxa %>%
  as_tibble(
    rownames =
      'comparison_occupation'
  ) %>%
  pivot_longer(
    cols = -1,
    names_to = 'occupation',
    values_to = 'taxon'
  ) %>%
  filter(
    !is.na(taxon)
  ) %>%
  mutate(
    taxon = factor(
      taxon,
      levels = c(
        'Reino',
        'Filo',
        'Classe',
        'Ordem',
        'Família',
        'Gênero',
        'Espécie'
      )
    )
  ) %>%
  group_by(
    occupation
  ) %>%
  complete(
    taxon
  ) %>%
  ungroup() ->
  df_taxa

df_taxa %>%
  pivot_wider(
    names_from = 'taxon',
    values_from = 'comparison_occupation'
  ) -> dsds

dsds %>%
  filter(
    # occupation == 'Statisticians'
    occupation == 'Mathematicians'
    # occupation == 'Economists'
  ) %>%
  # pull(Reino)
  # pull(Filo)
  # pull(Classe)
  # pull(Ordem)
  # pull(Família)
  # pull(Gênero)
  pull(Espécie)

# df_midpoint %>%
#   slice_min(
#     competence
#   ) %>%
#   pull(
#     occupation
#   ) %>%
#   clipr::write_clip()

dsds %>%
  filter(
    occupation ==
      # 'Mechanical Engineers'
      'Cleaners of Vehicles and Equipment'
  )

dsds %>%
  pivot_longer(
    cols = -1
  )

# df_taxa %>%
#   split(.$occupation) %>%
#   map(deframe) ->
#   list_taxa

# [PLOTTING] --------------------------------------------------------------
# - My models' plots ------------------------------------------------------
# Similarity density plot
df_matches_mine %>%
  mutate(
    sub = str_detect(
      model, '_sub'
    )
    , model =
      str_remove_all(
        model
        , '_sub'
      )
  ) %>%
  fun_plot.ridges(aes(
    x = similarity,
    y = model,
    fill = sub
  )
  , .list_axis.x.args = list(
    limits = c(-0, 1)
    , breaks = seq(0, 1, .25)
  )
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'Matching Models Comparison',
    subtitle = "For Cao Bittencourt's Career Profile",
    fill = 'Overqualification Substitution'
  )) -> plt_density_mine

# Ranges dumbbell plot
df_models_mine %>%
  pivot_longer(
    cols = c(max, min)
    # cols = c(max, mean, min)
    , names_to = 'match'
    , values_to = 'similarity'
  ) %>%
  fun_plot.dumbbell(aes(
    x = similarity,
    y = fct_reorder(
      model,
      range
    ),
    color = match
  )
  , .list_axis.x.args = list(
    limits = c(-0, 1)
    , breaks = seq(0, 1, .25)
  )
  , .chr_manual.pal = c('blue', 'red')
  # , .chr_manual.pal = c('blue', 'yellow', 'red')
  , .reorder_fct = F
  , .fun_format.x = percent
  , .list_labs = list(
    title = 'Matching Models Comparison',
    subtitle = "For Cao Bittencourt's Career Profile",
    x = 'Similarity',
    color = NULL
  )) -> plt_dumbbell_mine

plt_density_mine

plt_dumbbell_mine

# - Sample occupations models' plots --------------------------------------
# - Career matching (s, ß) -------------------------------------------------------
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
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




~