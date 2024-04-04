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

# [MODELS] ----------------------------------------------------------------
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
  mutate(
    gene_class = 
      fun_class_classifier(
        generality,
        int_levels = 5,
        chr_class_labels = c(
          'very specialist',
          'specialist',
          'balanced',
          'generalist',
          'very generalist'
        )
      ),
    comp_class = 
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
  ) -> df_midpoint

df_midpoint %>% 
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
    corr_gene_comp = 
      weights::wtd.cors(
        x = generality,
        y = competence,
        weight = 
          employment_variants
      ) %>% 
      as.numeric()
  )

# - My generality ------------------------------------------------------------
df_profile_adjusted[,-1] %>% 
  as.numeric() %>% 
  fun_gene_generality() -> 
  dbl_generality

# Occupations' generality
df_midpoint %>% 
  select(
    occupation,
    generality,
    gene_class
  )

dbl_generality

# - My competence ------------------------------------------------------------
df_profile_adjusted[,-1] %>% 
  as.numeric() %>% 
  fun_comp_competence(
    dbl_scale_lb = 0,
    dbl_scale_ub = 100
  ) -> dbl_competence

# Occupations' competence
df_midpoint %>% 
  select(
    occupation,
    competence,
    comp_class
  )

dbl_competence

# - Career matching (s, ß) -------------------------------------------------------
# My career matches
fun_match_similarity(
  df_data_rows = df_occupations
  , df_query_rows = 
    df_profile_adjusted
  # df_occupations %>%
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
  # , chr_weights = 'speciality-root'
  , chr_weights = 'attribute-eqvl'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 'occupation'
  , lgc_sort = T
  , lgc_overqualification_sub = T
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
    rownames = 'comparison_occupation'
  ) %>%
  pivot_longer(
    cols = -1
    , names_to = 'occupation'
    , values_to = 'similarity'
  ) %>%
  left_join(
    df_occupations %>% 
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
    df_occupations %>% 
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
    df_occupations %>% 
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
        item_score
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

