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

# [TAXONOMY] --------------------------------------------------------------
# - Hierarchical clustering of ß matrix --------------------
# taxonomy data frame
mtx_similarity %>%
  fun_taxa_hclust(
    # dbl_height = 0
    # c(0, 0.5, 1)
    # seq(0, 1, length.out = 7) %>%
    # atlas.eqvl::fun_eqvl_equivalence(dbl_scaling = 0.5)
    int_levels = 7
    # int_levels = 10
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
  ) -> df_taxonomy

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

# - Taxonomic-based economic model ----------------------------------------
fun_econ_taxa <- function(
    mtx_hireability,
    df_taxonomy
){

  # arguments validation
  stopifnot(
    "'mtx_hireability' must be a square matrix of hireability scores between 0 and 1." =
      all(
        is.matrix(mtx_hireability),
        is.numeric(mtx_hireability),
        all(diag(mtx_hireability) == 1),
        all(mtx_hireability >= 0),
        all(mtx_hireability <= 1)
      )
  )

  stopifnot(
    "'df_taxa' must be a data frame with the 'df_taxa' subclass." =
      all(
        is.data.frame(df_taxa)
        , any(class(df_taxa) == 'df_taxa')
      )
  )

  # data wrangling
  mtx_similarity %>%
    as_tibble(
      rownames =
        'comparison_set'
    ) -> df_hireability

  rm(mtx_similarity)

  # hireability data frame
  # filter by hireability to reduce join
  df_hireability %>%
    pivot_longer(
      cols = -1
      , names_to = 'competing_set'
      , values_to = 'hireability'
    ) %>%
    filter(
      hireability > 0
    ) -> df_hireability

  # taxo-economic competition data frame
  df_taxonomy %>%
    unnest(set) %>%
    rename(
      comparison_set = set
    ) %>%
    left_join(
      df_hireability
      , multiple = 'all'
      , relationship = 'many-to-many'
    ) %>%
    relocate(any_of(
      names(df_taxonomy)
    )) %>%
    group_by(
      taxon,
      taxon_id
    ) %>%
    mutate(
      .after = n
      , ncomp =
        unique(competing_set) %>%
        length()
    ) -> df_econ

  rm(df_hireability)
  rm(df_taxonomy)

  # data frame subclass
  new_data_frame(
    df_econ
    , class = c(
      class(df_econ)
      , 'df_econ'
    )
  ) -> df_econ

  # output
  return(df_econ)

}


list_taxonomy_unnested
list_taxonomy$kingdom

df_taxonomy %>%
  unnest(set) %>%
  rename(
    comparison_occupation = set
  ) %>%
  left_join(
    df_similarity
    , multiple = 'all'
    , relationship = 'many-to-many'
  ) %>%
  relocate(any_of(
    names(df_taxonomy)
  )) %>%
  mutate(
    ß =
      fun_intc_ss(
        similarity
        , rep(0.5, n())
      )
  ) %>%
  filter(
    ß >= 0.5
  ) %>%
  group_by(
    taxon,
    taxon_id
  ) %>%
  mutate(
    .after = n
    , ncomp =
      unique(competing_occupation) %>%
      length()
  ) %>%
  ungroup() ->
  dsds


dsds %>%
  filter(
    comparison_occupation ==
      'Statisticians'
  ) %>%
  group_by(
    taxon
  ) %>%
  slice(1)
arrange(desc(ß))
# arrange(ß)

dsds %>%
  filter(
    ncomp < 873
  ) %>%
  arrange(desc(
    -ncomp
  ))


df_similarity %>%
  right_join(
    list_taxonomy_unnested$
      # kingdom %>%
      species %>%
      bind_rows(
        # .id = 'taxon_name'
      )
    , by = c(
      'comparison_occupation' =
        'set'
    )
  ) %>%
  relocate(
    starts_with('taxon')
  ) %>%
  filter(
    comparison_occupation == 'Mathematicians'
  ) %>%
  arrange(desc(
    -similarity
  )) %>%
  filter(
    similarity >= 0.75
  ) %>%
  group_by(
    taxon,
    taxon_id
  ) %>%
  mutate(
    competing = n()
  )

fun_intc_ss()






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
