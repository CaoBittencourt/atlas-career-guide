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
  'CaoBittencourt' = 'atlas.intc',
  'CaoBittencourt' = 'atlas.taxa',
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

# Similarity matrix
mtx_similarity <- read_rds('/home/Cao/Storage/github/atlas-research/data/mtx_similarity.rds')

# # [MODELS] ----------------------------------------------------------------
# # - Midpoints ----------------------------------------------
# # Use skill set generality as midpoint for attribute equivalence
# # Use skill set competence as midpoint for interchangeability
# df_sample %>%
#   pivot_longer(
#     cols = -1
#     , names_to = 'item'
#     , values_to = 'item_score'
#   ) %>%
#   group_by(
#     occupation
#   ) %>%
#   reframe(
#     generality =
#       fun_gene_generality(
#         item_score
#       ),
#     competence =
#       fun_comp_competence(
#         dbl_profile = item_score,
#         dbl_scale_ub = 100,
#         dbl_scale_lb = 0,
#         dbl_generality = generality
#       )
#   ) -> df_midpoint
#
# df_midpoint %>%
#   arrange(desc(
#     competence
#   )) %>%
#   print(
#     n = Inf
#   )
#
# # - Matching methods ------------------------------------------------------
# # Matching methods to apply
# c(
#   # 'bvls',
#   # 'logit',
#   # 'probit',
#   # 'pearson',
#   'euclidean'
# ) -> chr_methods
#
# # - Weighting methods ------------------------------------------------------
# # Weighting methods to apply
# c(
#   # 'linear',
#   # 'quadratic',
#   # 'speciality-root',
#   'attribute-eqvl'
# ) -> chr_weights
#
# # - Overqualification substitution ----------------------------------------
# # Whether or not to apply overqualification substitution
# # c(T, F) -> lgc_over_sub
# c(F) -> lgc_over_sub
#
# # - Matching-weights-sub combinations -----------------------------------------
# # Generate all model matching-weights-sub combinations
# crossing(
#   method = chr_methods,
#   weight = chr_weights,
#   sub = lgc_over_sub
# ) -> df_methods
#
# df_methods %>%
#   mutate(
#     .before = 1
#     , model = paste0(
#       method, '_', weight, if_else(
#         sub, '_sub', ''
#       )
#     )
#     , model = str_replace_all(
#       model, '-', '_'
#     )
#   ) -> df_methods
#
# # [MATCHING] --------------------------------------------------------------
# # - Similarity matrix ------------------------------------------------------------
# # My career matches
# Map(
#   function(model, method, weight, over_sub){
#
#     # Apply matching function
#     return(
#       fun_match_similarity(
#         df_data_rows = df_occupations
#         , df_query_rows = df_sample
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
# ) -> list_matches_sample
#
# [TAXONOMY] --------------------------------------------------------------
# - Hierarchical clustering of ß matrix --------------------
# taxonomy data frame
rm(df_taxonomy)

mtx_similarity %>%
  fun_taxa_hclust(
    int_levels = 7
    , chr_levels = c(
      'kingdom',
      'phylum',
      'class',
      'order',
      'family',
      'genus',
      'species'
    )
    , chr_method =
      'complete'
      # 'ward.D2'

      # 'mcquitty'
      # 'centroid' #cutree bug
      # 'single'
      # 'average'
      # 'median' #cutree bug
      # 'ward.D'

    # dbl_height =
    # c(0, 0.5, 1)
    # seq(0, 1, length.out = 7) %>%
    # atlas.eqvl::fun_eqvl_equivalence(dbl_scaling = 0.5)
  ) -> df_taxonomy

df_taxonomy

# taxonomy descriptive statistics
df_taxonomy %>%
  fun_taxa_desc()

# taxonomy data frame (unnested)
df_taxonomy %>%
  fun_taxa_unnest()

# taxonomy list
df_taxonomy %>%
  fun_taxa_list() ->
  list_taxonomy

# taxonomy list (unnested)
df_taxonomy %>%
  fun_taxa_list(
    lgc_unnest = T
  ) -> list_taxonomy_unnested

list_taxonomy[1]
list_taxonomy_unnested[1]

list_taxonomy %>%
  map(
    ~ .x %>%
      filter(
        map_lgl(
          set,
          ~ 'Mathematicians' %in% .x
          # ~ 'Statisticians' %in% .x
          # ~ 'Chief Executives' %in% .x
          # ~ 'Economists' %in% .x
          # ~ 'Mechanical Engineers' %in% .x
          # ~ 'Physicists' %in% .x
          # ~ 'Credit Analysts' %in% .x
          # ~ 'Dishwashers' %in% .x
          # ~ 'Registered Nurses' %in% .x
          # ~ 'Hospitalists' %in% .x
          # ~ 'Philosophy and Religion Teachers, Postsecondary' %in% .x
        )
      ) %>%
      unnest(set)
  )

# - Enforce symmetry function ---------------------------------------------
fun_misc_symmetric <- function(
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
# Taxonomic categories data frame
tibble(
  # taxon = factor(c('Reino', 'Filo', 'Classe', 'Ordem', 'Família', 'Gênero', 'Espécie'))
  taxon = factor(c('kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species'))
  , belong = seq(0, 1, length.out = 7) #cutoffs to belong to each category
) -> df_categories

# Taxonomic classification
mtx_taxa %>%
  colnames() %>%
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

# df_taxa %>%
#   pivot_wider(
#     names_from = taxon,
#     values_from = belong
#   ) -> dsds

# Unique taxonomic groups
df_taxa %>%
  filter(
    belong
  ) %>%
  pivot_wider(
    id_cols = occupation
    # id_cols = c(occupation, similarity)
    , names_from = taxon
    , values_from =
      comparison_occupation
  ) %>%
  pivot_longer(
    cols = -1
    , names_to = 'taxon'
    , values_to = 'set'
  ) %>%
  group_by(
    taxon
  ) %>%
  reframe(
    set =
      unique(
        set
      )
  ) -> dsdsds

# df_taxa %>%
#   filter(
#     belong
#   ) %>%
#   pivot_wider(
#     id_cols = occupation
#     , names_from = taxon
#     , values_from =
#       comparison_occupation
#   ) %>%
#   pivot_longer(
#     cols = -1
#     , names_to = 'taxon'
#     , values_to = 'set'
#   ) %>%
#   group_by(
#     taxon
#   ) %>%
#   reframe(
#     set =
#       unique(
#         set
#       )
#   ) -> dsdsds

dsdsds %>% filter(taxon == 'kingdom')
dsdsds %>% filter(taxon == 'phylum')
dsdsds %>% filter(taxon == 'class')
dsdsds %>% filter(taxon == 'order')
dsdsds %>% filter(taxon == 'family')
dsdsds %>% filter(taxon == 'genus')
dsdsds %>% filter(taxon == 'species')

dsdsds %>%
  filter(
    taxon == 'genus'
  ) %>%
  arrange(-map_dbl(
    set,
    length
  )) %>%
  slice(1:3) -> dsds

setdiff(
  list_c(dsds[1,]$set),
  list_c(dsds[2,]$set)
)

setdiff(
  list_c(dsds[2,]$set),
  list_c(dsds[1,]$set)
)

intersect(
  list_c(dsds[2,]$set),
  list_c(dsds[1,]$set)
)

setdiff(
  list_c(dsds[3,]$set),
  intersect(
    list_c(dsds[2,]$set),
    list_c(dsds[1,]$set)
  )
)

for(k in 2:nrow(dsds)){

  unique(
    dsds$set[[k - 1]],
    dsds$set[[k]]
  )

}

# dsds %>%
dsdsds %>%
  filter(
    taxon == 'genus'
  ) %>%
  arrange(map_dbl(
    # arrange(-map_dbl(
    set,
    length
  )) %>%
  mutate(
    cum_set =
      map2(
        .x = set,
        .y = lag(set)
        , ~ unique(c(.x, .y))
      )
    , set_diff =
      map2(
        .x = set,
        .y = cum_set
        , ~ setdiff(.y, .x)
      )
    # set_diff =
    #   map2(
    #     .x = set,
    #     .y = lead(set),
    #     .f = ~ setdiff(.x, .y)
    #   )
  )

dsds$set %>% length()
dsds$set %>% map(length)
dsds$set[[1]]
dsds$set[[2]]
dsds$set[[3]]


unique(
  c(
    lag(dsds$set),
    dsds$set
  )
) %>% map(length)


dsds %>%
  mutate(
    checkmark = list(
      unique(list_c(c(
        lag(set)
        , set
        # dsds[3,]$set,
        # dsds[2,]$set,
        # dsds[1,]$set
      )))
    )
  )

unique(list_c(c(
  dsds[3,]$set,
  dsds[2,]$set,
  dsds[1,]$set
)))

union(

)

intersect(
  list_c(dsds[2,]$set),
  list_c(dsds[1,]$set)
)


# dsdsds %>%
#   filter(
#     taxon == 'class'
#   ) %>%
#   arrange(map_dbl(
#     set,
#     length
#   )) %>%
#   slice(1:2) -> dsds

fun_taxa_filter <- function(df_taxa){

  # arguments validation

  # data wrangling
  # unique taxonomic groups
  df_taxa %>%
    filter(
      belong
    ) %>%
    pivot_wider(
      id_cols = occupation
      , names_from = taxon
      , values_from =
        comparison_occupation
    ) %>%
    pivot_longer(
      cols = -1
      , names_to = 'taxon'
      , values_to = 'set'
    ) %>%
    group_by(
      taxon
    ) %>%
    reframe(
      set = unique(set)
    ) -> df_taxa

  # lag-filter function
  fun_help_filter <- function(x, y){

    # output
    return(x[!(x %in% y)])

  }

  # dplyr::setdiff()

}

df_taxa %>%
  split(.$taxon) ->
  list_taxa

list_taxa$species %>% reframe(set_length = map_dbl(set, length))

list_taxa$
  genus %>%
  arrange(-desc(
    map_dbl(
      set,
      length
    )
  )) -> df_genus

df_taxa %>%
  group_by(
    taxon
  ) %>%
  reframe(
    max(
      map_dbl(
        set,
        length
      ))
  )



for(k in 2:nrow(df_genus)){

  list(setdiff(
    list_c(df_genus[k,]$set),
    list_c(df_genus[k - 1,]$set)
  )) -> df_genus[k,]$set

}

df_class %>%
  arrange(-desc(
    map_dbl(
      set,
      length
    )
  ))

dsds %>%
  slice(2) %>%
  unnest(set) %>%
  filter(!(
    set %in% (
      dsds %>%
        slice(1) %>%
        pull(set) %>%
        list_c()
    )
  ))


# - Classify with fun_class_classifier --------------------------------------------
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
