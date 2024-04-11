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
  'CaoBittencourt' = 'atlas.aeq',
  'CaoBittencourt' = 'atlas.plot',
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

# [MODELS] ----------------------------------------------------------------
# - Midpoints ----------------------------------------------
# My generality
df_profile_adjusted[,-1] %>%
  as.numeric() %>%
  fun_gene_generality() ->
  dbl_generality

# Occupations' skill set generality
df_occupations %>%
  select(
    occupation
    , starts_with('skl_')
    , starts_with('abl_')
    , starts_with('knw_')
  ) %>%
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
      )
  ) -> df_generality

# - Ä function ------------------------------------------------------------
# My ä data frame
tibble(
  a = seq(0, 1, 0.001)
) %>%
  mutate(
    ä = fun_aeq_aequivalence(
      a, dbl_generality
    )
  ) %>%
  pivot_longer(
    cols = c(a, ä)
  ) -> df_attribute_eqvl_mine

# Attribute equivalence data frame
replicate(
  n = nrow(df_generality)
  , seq(0, 1, 0.001)
  , simplify = T
) %>%
  as_tibble() %>%
  set_names(
    df_generality$
      occupation
  ) %>%
  pivot_longer(
    cols = everything()
    , names_to = 'occupation'
    , values_to = 'a'
  ) %>%
  left_join(
    df_generality
  ) %>%
  group_by(
    occupation
  ) %>%
  mutate(
    ä = fun_aeq_aequivalence(
      a, generality
    )
  ) %>%
  ungroup() %>%
  pivot_longer(
    cols = c(a, ä)
  ) -> df_attribute_eqvl

# [PLOTTING] ----------------------------------------------------------------
# - My attribute equivalence function -------------------------------------
# Apply line graph
df_attribute_eqvl_mine %>%
  fun_plot.line(aes(
    x = rep(
      seq(0, 1, 0.001)
      , each = 2
    )
    , y = value
    , color = name
  )) -> plt_attribute_eqvl_mine

# - Occupations' attribute equivalence ------------------------------------
# Split apply line graph
df_attribute_eqvl %>%
  split(.$occupation) %>%
  map(
    ~ .x %>%
      fun_plot.line(aes(
        x = rep(
          seq(0, 1, 0.001)
          , each = 2
        )
        , y = value
        , color = name
      ))
  ) -> list_plt_attribute_eqvl
