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
  'CaoBittencourt' = 'atlas.econ',
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
# - Taxonomic-based economic model ----------------------------------------
# taxonomy
# division, subdivision?
fun_taxa_hclust(
  mtx_similarity
  , int_levels = 10
  , chr_levels = c(
    'sector',
    'subsector',
    'industry',
    'subindustry',
    'field',
    'branch',
    'market',
    'segment',
    'role',
    'job'
  )
) -> df_taxonomy

# # taxonomy
# fun_taxa_hclust(
#   mtx_similarity
#   , int_levels = 7
#   , chr_levels = c(
#     'sector',
#     'subsector',
#     'industry',
#     'subindustry',
#     'market',
#     'segment',
#     'occupation'
#   )
# ) -> df_taxonomy

# economic model
fun_econ_taxa(
  mtx_similarity
  , df_taxonomy
  , dbl_employment =
    df_occupations$
    employment_variants
  , dbl_wages =
    df_occupations$
    wage
) -> df_econ

# # - Economic model test ----------------------------------------
# # taxonomy
# # division, subdivision?
# fun_taxa_hclust(
#   mtx_similarity
#   , int_levels = 10
#   , chr_levels = c(
#     'sector',
#     'subsector',
#     'industry',
#     'subindustry',
#     'field',
#     'branch',
#     'market',
#     'segment',
#     'role',
#     'job'
#   )
# ) -> df_taxonomy
#
# # # taxonomy
# # fun_taxa_hclust(
# #   mtx_similarity
# #   , int_levels = 7
# #   , chr_levels = c(
# #     'sector',
# #     'subsector',
# #     'industry',
# #     'subindustry',
# #     'market',
# #     'segment',
# #     'occupation'
# #   )
# # ) -> df_taxonomy
#
# # fun_taxa_hclust(
# #   mtx_similarity
# #   , int_levels = 5
# #   , chr_levels = c(
# #     'sector',
# #     'industry',
# #     'market',
# #     'segment',
# #     'occupation'
# #   )
# # ) -> df_taxonomy
#
# # market (family) level seems to be optimal level of aggregation for defining competition / interchangeability
# # this also seems to be (or to be near) the level where degrees are interchangeable
#
# df_taxonomy %>%
#   fun_taxa_desc()
#
# df_taxonomy %>%
#   unnest(set) %>%
#   filter(
#     # set == 'Mechanical Engineers'
#     # set == 'Physicists'
#     # set == 'Dishwashers'
#     # set == 'Credit Analysts'
#     set == 'Actuaries'
#     # set == 'Chief Executives'
#     # set == 'Economists'
#     # set == 'Mathematicians'
#     # set == 'Statisticians'
#     # set == 'Dancers'
#     # set == 'Security Guards'
#   ) %>%
#   select(
#     taxon,
#     taxon_id
#   ) %>%
#   left_join(
#     df_taxonomy
#   ) %>%
#   unnest(set) %>%
#   split(.$taxon) %>%
#   map(print, n = Inf) %>%
#   invisible()
#
# # economic model
# fun_econ_taxa(
#   mtx_similarity
#   , df_taxonomy
#   , dbl_employment =
#     df_occupations$
#     employment_variants
#   , dbl_wages =
#     df_occupations$
#     wage
# ) -> df_econ

# - Employability ---------------------------------------------------
# calculate employability
# for each occupation
df_econ %>%
  group_by(
    taxon,
    taxon_id,
    competing_set
  ) %>%
  reframe(
    employability =
      weighted.mean(
        hireability
        , employment
      )
    # , employment_potential =
    #   sum(
    #     hireability *
    #       employment
    #   )
  ) -> df_employability

df_econ %>%
  group_by(
    taxon,
    taxon_id,
    competing_set
  ) %>%
  mutate(
    employability =
      weighted.mean(
        hireability
        , employment
      )
  ) %>%
  group_by(
    taxon,
    taxon_id,
    set
  ) %>%
  mutate(
    competitiveness =
      weighted.mean(
        hireability
        , competing_employment
      )
  ) %>%
  ungroup() %>%
  filter(
    set == competing_set
  # ) -> df_employability
  ) -> dsds

df_employability %>%
  filter(
    competing_set ==
      # 'Statisticians'
      # 'Chief Executives'
      # 'Mathematicians'
      # 'Firefighters'
      'Economists'
  ) %>%
  right_join(
    df_taxonomy %>%
      fun_taxa_unnest() %>%
      filter(
        # set == 'Statisticians'
        # set == 'Chief Executives'
        # set == 'Mathematicians'
        # set == 'Firefighters'
        set == 'Economists'
      ) %>%
      pivot_longer(
        cols = -1
        , names_to = 'taxon'
        , values_to = 'taxon_id'
      )
  )


# occupations' employability at each level
fun_econ_employability(df_econ) -> df_employability

# taxa employability
fun_econ_employability(df_econ, lgc_taxon = T) -> df_employability_taxa

df_employability %>% head()


df_taxonomy %>%
  fun_taxa_unnest() %>%
  filter(
    set == 'Statisticians'
  ) %>%
  pivot_longer(
    cols = -1
    , names_to = 'taxon'
    , values_to = 'taxon_id'
  )

df_employability %>%
  filter(
    taxon == 'job',
    taxon_id == 115
  )

df_employability %>%
  filter(
    competing_set == 'Statisticians',
    taxon == 'job'
  ) %>%
  reframe(
    employability =
      weighted.mean(
        employability
        , employment
      )
  )

df_employability_taxa %>%
  filter(
    taxon == 'job',
    taxon_id == 115
  )

# - Competitiveness -------------------------------------------------
# - Value -----------------------------------------------------------




