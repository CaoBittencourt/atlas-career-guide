# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read and write data
  , 'tidyr', 'dplyr', 'stringr', 'scales' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.match',
  'CaoBittencourt' = 'atlas.comp'
)

# Activate / install CRAN packages
lapply(
  chr_pkg
  , function(pkg){
    
    if(!require(pkg, character.only = T)){
      
      install.packages(pkg, dependencies = T)
      
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
        , dependencies = T
        , upgrade = F
        , force = T
      )
      
    }
    
    require(git, character.only = T)
    
  }
  , git = chr_git
  , profile = names(chr_git)
)

# chr_pkg <- c(
#   'devtools' #GitHub packages
# )

# - Data ------------------------------------------------------------------
# Occupations data frame
read_csv(
  '/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv'
) -> df_occupations

# Matching results
read_rds(
  '/home/Cao/Storage/github/atlas-research/papers/matching/list_matching.rds'
) -> list_matching

# Attribute names
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vStiX7PF7ltnjqVTCLJchWtaAW_BhVCwUM1hRCXGolCOLCS8mCygyde6FfhcvhZiAvke-tujkTBpXoA/pub?gid=0&single=true&output=csv'
) -> df_attribute_names

# [DATA] ------------------------------------------------------------------
# - Matching data frame ---------------------------------------------------
# Select only competencies
df_occupations %>%
  select(
    occupation,
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) -> df_matching

# [MODEL] --------------------------------------------------------------
# - Estimate models ---------------------------------------------------------
# Run weighted Euclidean matching
fun_match_similarity(
  df_data_rows =
    df_matching
  , df_query_rows =
    df_matching
  , chr_method = 'knn'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col =
    'occupation'
  , lgc_sort = T
) -> list_matching

list_matching$
  mtx_similarity %>%
  View()

# [RESULTS] ---------------------------------------------------------------
# # - Select most competent occupation --------------------------------------------------
# df_matching %>%
#   pivot_longer(
#     cols = -1,
#     names_to = 'item',
#     values_to = 'item_score'
#   ) %>%
#   group_by(
#     occupation
#   ) %>%
#   reframe(
#     competence =
#       fun_comp_competence(
#         item_score,
#         dbl_scale_ub = 100,
#         dbl_scale_lb = 0
#       )
#   ) %>%
#   arrange(desc(
#     competence
#   )) -> df_comp
# 
# df_comp %>%
#   print(
#     n = Inf
#   )
# 
# df_comp %>%
#   filter(
#     competence %in%
#       c(
#         quantile(
#           competence
#         )
#       )
#   )
# 
# df_comp %>%
#   filter(
#     competence <= 0.5
#     & competence >= 0.4
#   ) %>%
#   print(
#     n = Inf
#   )
# 
# list_matching$
#   mtx_similarity %>%
#   as_tibble(
#     rownames = 'comparison_occupation'
#   ) %>%
#   pivot_longer(
#     cols = -1
#     , names_to = 'occupation'
#     , values_to = 'similarity'
#   ) %>%
#   group_by(
#     occupation
#   ) %>%
#   arrange(desc(
#     similarity
#   ), .by_group = T
#   ) %>%
#   slice(-1) %>%
#   slice(1, n()) %>%
#   ungroup() %>%
#   arrange(
#     similarity
#   )
# 
# # Match to most competent occupation
# fun_match_similarity(
#   df_data_rows =
#     df_matching %>%
#     filter(
#       occupation ==
#         df_comp %>%
#         slice(1) %>%
#         pull(
#           occupation
#         )
#     )
#   , df_query_rows =
#     df_matching
#   , chr_method = 'knn'
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
#   , chr_id_col =
#     'occupation'
#   , lgc_sort = T
# ) -> list_matching_comp
# 
# list_matching_comp$
#   mtx_similarity %>%
#   t() %>%
#   as_tibble(
#     rownames = 'occupation'
#   ) %>%
#   rename(
#     similarity = 2
#   ) %>%
#   arrange(desc(
#     similarity
#   )) -> df_similarity
# 
# df_similarity %>%
#   filter(
#     similarity %in%
#       c(
#         quantile(
#           similarity
#         )
#       )
#   )
# 
# df_similarity %>%
#   filter(
#     similarity <= 0.67
#     & similarity >= 0.50
#   ) %>%
#   print(
#     n = Inf
#   )
# 
# df_similarity %>%
#   filter(
#     str_detect(
#       str_to_lower(
#         occupation
#       ), 'manager'
#     )
#   ) %>%
#   print(
#     n = Inf
#   )

# - Select occupations to showcase matching results -----------------------
# Selected occupations
c(
  'Mechanical Engineers',
  'Physicists',
  'Credit Analysts',
  'Dishwashers'
) -> chr_occupations

# Similarity matrix
list_matching$
  mtx_similarity[
    chr_occupations,
    chr_occupations
  ] -> mtx_similarity

mtx_similarity %>%
  View()

# list_matching$
#   mtx_similarity[
#     ,chr_occupations
#   ] %>% 
#   View()

# - Top 10 matches --------------------------------------------------------
# Select top 10 matches
list_matching$
  list_similarity[
    chr_occupations
  ] %>% 
  map(head, 11) %>% 
  map(
    as_tibble
    , rownames =
      'Comparison Occupation'
  ) %>% 
  map(
    rename
    , Similarity = 2
  ) -> list_df_top10

# # Add clusters
# list_df_top10 %>% 
#   map(
#     ~ df_occupations %>% 
#       select(
#         occupation,
#         career_cluster
#       ) %>% 
#       pivot_longer(
#         cols = -1
#         , values_to =
#           'Cluster'
#       ) %>% 
#       select(
#         -name
#       ) %>% 
#       rename(
#         `Comparison Occupation` =
#           occupation
#       ) %>%
#       right_join(.x) %>% 
#       relocate(
#         `Comparison Occupation`,
#         Cluster,
#         Similarity
#       )
#   ) -> list_df_top10

# Arrange by similarity
list_df_top10 %>% 
  map(
    arrange
    , -Similarity
  ) -> list_df_top10

# - General descriptive statistics ----------------------------------------
# Descriptive statistics for all clusters
df_occupations %>% 
  mutate(
    market_value = 
      sum(
        employment_variants * 
          wage 
      )
  ) %>% 
  group_by(
    career_cluster
  ) %>% 
  reframe(
    `Number of Occupations` = n()
    , Employment = sum(
      employment_variants
    )
    , Wage = weighted.mean(
      wage
      , employment_variants
    )
    , `Market Share` = sum(
      wage * employment_variants
    ) / first(market_value)
  ) %>% 
  arrange(desc(
    `Market Share`
  )) -> df_clusters

# Format table
df_clusters %>%
  rename(
    Cluster = career_cluster
  ) %>% 
  mutate(
    Employment = 
      Employment %>% 
      ceiling() %>%
      number(
        big.mark = ','
        , accuracy = 1
      )
    , Wage = dollar(
      Wage
    )
    , `Market Share` = 
      percent(
        `Market Share`
        , .01
      )
  ) -> df_clusters

# - Selected occupations table -----------------------------------------------------
# Descriptive table data 
df_occupations %>% 
  filter(
    occupation %in% 
      chr_occupations
  ) %>% 
  select(
    id_soc_code,
    occupation,
    career_cluster,
    employment,
    wage
  ) -> df_desc

# Format descriptive table
df_desc %>% 
  arrange(desc(
    wage
  )) %>% 
  mutate(
    employment = 
      number(employment)
    , wage = dollar(wage, .01)
  ) -> df_desc

c(
  'SOC Code',
  'Occupation',
  'Cluster',
  'Employment',
  'Wage (2021)'
) -> names(df_desc)

# - Attributes table -----------------------------------------------------
# Descriptive table data 
df_matching %>% 
  filter(
    occupation %in% 
      chr_occupations
  ) %>% 
  left_join(
    chr_occupations %>% 
      as_tibble() %>% 
      rename(
        occupation = 1
      )
  ) %>% 
  pivot_longer(
    cols = -1,
    names_to = 'attribute'
  ) %>% 
  pivot_wider(
    values_from = 'value',
    names_from = 'occupation'
  ) %>% 
  # select(
  #   Competency,
  #   any_of(
  #     chr_occupations
  #   )
  # ) %>% 
  arrange(desc(across(
    .cols = chr_occupations
  ))) -> df_attributes

# Competency names
df_attributes %>% 
  right_join(
    df_attribute_names
  ) %>% 
  select(
    Competency,
    any_of(
      chr_occupations
    )
  ) -> df_attributes

# [EXPORT] ----------------------------------------------------------------
# # - Save rds objects to save time -----------------------------------------
# # Matching results
# write_rds(
#   x = list_matching,
#   file = '/home/Cao/Storage/github/atlas-research/papers/matching/list_matching.rds'
# )

# - LaTeX tables ----------------------------------------------------------
# Export general descriptive statistics
df_clusters %>% 
  xtable::xtable(
    caption = 'General Occupational Statistics'
    , align = 'cccccc'
    , digits = 2
  ) %>%
  print(
    tabular.environment = 'talltblr'
    , include.rownames = F
  )

# Export selected occupations table
df_desc %>% 
  xtable::xtable(
    caption = 'Summary of Occupations'
    , align = 'cccccc'
    , digits = 2
  ) %>%
  print(
    tabular.environment = 'tabular*'
    , include.rownames = F
  )

# Export top 10 matches
map2(
  .x = list_df_top10
  , .y = names(list_df_top10)
  , ~ .x %>% 
    xtable::xtable(
      caption = paste0(
        'Best Career Matches -- '
        , .y
      )
      , align = 'ccc'
      , digits = 2
    ) %>%
    print(
      tabular.environment = 'talltblr'
      , include.rownames = F
    )
)

# Export similarity matrix
mtx_similarity %>% 
  xtable::xtable(
    caption = 'Compatibility Matrix'
    , align = 'ccccc'
    , digits = 2
  ) %>%
  print(
    tabular.environment = 'tabular*'
    , include.rownames = F
  )

# Export detailed skill sets
df_attributes %>% 
  xtable::xtable(
    caption = 'Detailed Skill Sets'
    , align = 'cccccc'
    , digits = 0
  ) %>%
  print(
    tabular.environment = 'tabular*'
    , include.rownames = F
  )
