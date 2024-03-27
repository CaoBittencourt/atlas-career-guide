# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
  , 'dplyr', 'tidyr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.match',
  'CaoBittencourt' = 'atlas.comp',
  'CaoBittencourt' = 'atlas.gene',
  'CaoBittencourt' = 'atlas.aeq',
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

# [MODELS] ----------------------------------------------------------------
# - Career matching -------------------------------------------------------
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = df_profile_adjusted
  , chr_method = 'euclidean'
  # , chr_method = 'pearson'
  # , chr_method = 'bvls'
  # , chr_method = 'logit'
  # , chr_method = 'probit'
  # , chr_weights = 'linear'
  # , chr_weights = 'quadratic'
  # , chr_weights = 'speciality-root'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
) -> list_matches

# 0 = shit, 1 = ok, 2 = good, 3 = great, 4 = awesome
# euclidean linear 1
# euclidean quadratic 3
# euclidean speciality-root 2
# euclidean attribute-eqvl 3.5

# pearson linear 2.5
# pearson quadratic 1.5
# pearson speciality-root 2
# pearson attribute-eqvl 1

# bvls linear 0
# bvls quadratic 0
# bvls speciality-root 0 
# bvls attribute-eqvl 0.5

# logit/probit linear 2
# logit/probit quadratic 2
# logit/probit speciality-root 2
# logit/probit attribute-eqvl 3

list_matches$
  mtx_similarity %>%
  as_tibble(
    rownames = 'occupation'
  ) %>%
  mutate(
    Cao = round(Cao, 4)
  ) %>%
  arrange(
    Cao
  ) %>%
  print(n = 22)

list_matches$
  mtx_similarity %>% 
  as_tibble(
    rownames = 'occupation'
  ) %>% 
  mutate(
    Cao = round(Cao, 4)
  ) %>% 
  arrange(desc(
    Cao
    )) %>%
  print(n = 22)

# - Generality vs Competence ----------------------------------------------
df_occupations %>% 
  select(
    occupation,
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
  ) %>% 
  pivot_longer(
    cols = -1,
    names_to = 'item',
    values_to = 'item_score'
  ) %>% 
  group_by(
    occupation
  ) %>% 
  reframe(
    generality = 
      fun_gene_generality(
        dbl_profile = item_score,
        dbl_scale_ub = 100,
        dbl_scale_lb = 0
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
  mutate(
    class = 
      fun_class_classifier(
        competence,
        int_levels = 5,
        chr_class_labels = c(
          'very low level',
          'low level',
          'mid level',
          'high level',
          'very high level'
        )
      )
  ) %>% 
  print(
    n = 50
  )

# - Generality vs Competence correlation ----------------------------------
# generalists vs specialists
df_occupations %>% 
  select(
    occupation,
    employment_variants
  ) %>%
  right_join(
    df_midpoint
  ) %>%
  reframe(
    correlation_generality_competence = 
      weights::wtd.cors(
        x = generality,
        y = competence,
        weight = 
          employment_variants
      ) %>% 
      as.numeric()
  )


# - Generality ------------------------------------------------------------
# My generality
df_profile_adjusted[,-1] %>% 
  as.numeric() %>% 
  fun_gene_generality(
    dbl_scale_lb = 0,
    dbl_scale_ub = 100
  ) -> dbl_generality

# Occupations' generality
df_occupations %>% 
  select(
    occupation,
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
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
        item_score,
        dbl_scale_lb = 0,
        dbl_scale_ub = 100
      )
  ) %>% 
  arrange(desc(
    generality
  )) -> df_generality

df_generality %>% print(n = 10)
df_generality %>% tail(5)
dbl_generality

# - Competence ------------------------------------------------------------
# My competence
df_profile_adjusted[,-1] %>% 
  as.numeric() %>% 
  fun_comp_competence(
    dbl_scale_lb = 0,
    dbl_scale_ub = 100
  ) -> dbl_competence

# Occupations' competence
df_occupations %>% 
  select(
    occupation,
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
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
    competence =
      fun_comp_competence(
        item_score,
        dbl_scale_lb = 0,
        dbl_scale_ub = 100
      )
  ) %>% 
  arrange(desc(
    competence
  )) -> df_competence

df_competence %>% print(n = 50)
df_competence %>% print(n = 10)
df_competence %>% tail(5)
dbl_competence

# - Core attributes -------------------------------------------------------
# My core attributes
df_profile_adjusted %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item_score'
  ) %>% 
  mutate(
    item_eqvl = 
      fun_aeq_aequivalence(
        dbl_profile = item_score
        , dbl_scale_lb = 0
      )
    , item_eqvl = 
      round(item_eqvl, 4)
    , item_class = 
      fun_class_classifier(
        dbl_var = item_eqvl
        , dbl_scale_lb = 0
        , dbl_scale_ub = 1
        , int_levels = 4
        , chr_class_labels = c(
          'minor',
          'auxiliary',
          'important',
          'core'
        )
      )
  ) -> df_attribute_eqvl

df_attribute_eqvl %>% 
  arrange(
    item_eqvl
  ) %>%
  print(
    n = Inf
  )
