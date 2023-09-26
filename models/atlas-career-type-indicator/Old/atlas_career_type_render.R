# [SETUP] -----------------------------------------------------------------
# - Workspace -------------------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

tryCatch(
  
  load('./atlas_career_type_image.RData')
  
  , error = function(e){
    
    source('./atlas_career_type_setup.R')
    
  }
  
)

# - Packages --------------------------------------------------------------
lapply(
  chr_profile
  , function(x){
    if(!require(
      x, character.only = T
    )){
      install.packages(x)
      require(x)
    }}
)

# - Data ------------------------------------------------------------------
# User data
read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=47461225&single=true&output=csv'
) -> df_input

# df_occupations %>%
#   slice_sample(
#     n = 1
#   ) %>%
#   select(
#     occupation
#     , ends_with('.l')
#   ) -> df_input

# - Parameters --------------------------------------------------------------
# EFA parameters
# Sample weights
df_occupations$
  employment2 ->
  .dbl_weights

# Orthogonal rotations
.chr_rotation <- 'equamax'
# .chr_rotation <- 'varimax'
# .chr_rotation <- 'varimin'
# .chr_rotation <- 'quartimax'

# Oblique rotations
# .chr_rotation <- 'oblimin'
# .chr_rotation <- 'Promax'
# .chr_rotation <- 'promax'
# .chr_rotation <- 'bentlerQ'
# .chr_rotation <- 'cluster'

# Number of factors
# int_nfactors <- 15
.auto_select.nfactors <- T

# Minimum factor size
.int_min.factor_size <- 3

# MSAi
.remove_unacceptable_MSAi.items <- F

# Underloadings and crossloadings
.remove_under_loading.items <- F
.remove_cross_loading.items <- F
.dbl_under_loading.threshold <- 0.5
.dbl_cross_loading.threshold <- 0.2

# Diagrams and tests
.show_diagrams <- T
.show_results <- T

# - Functions -------------------------------------------------------------
source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_plots.R')
# [Derive career types from factor scores] ----------------------------------------------
# Items -> Factors (individual electrical signals in the brain)
# Factors -> Factor scores (cognitive activation patterns)
# Factor scores -> Factors PCA (cognitive functions)
# Factors PCA -> K-means Clusters (personality types)
# Things
# Simple patterns of things
# Composite patterns of simple patterns of things
# Simple patterns of composite patterns of things
# items
# groups of items
# combinations of groups
# groups of combinations
# Se -> Si -> Ne -> Ni
# - Calculate factor scores ---------------------------------------------
# fun_professional_type(
#   df_data = df_occupations
#   , efa_model = efa_model
# ) %>% 
#   select(
#     occupation
#     , factor
#     , factor_score
#   ) %>% 
#   pivot_wider(
#     id_cols = occupation
#     , names_from = factor
#     , values_from = factor_score
#   ) -> df_career_types

fun_factor_scores(
  df_data = df_occupations
  , efa_model = efa_model
  , lgc_pivot = T
) %>%
  select(
    occupation
    , starts_with('factor')
  ) %>%
  pivot_wider(
    id_cols = occupation
    , names_from = factor
    , values_from = factor_score
  ) -> df_career_types

# - PCA on factor scores ---------------------------------------------
# Items -> Factors (individual cognitive electrical signals)
# Factors -> Factor scores (cognitive electrical patterns)
# Factor scores -> Factors PCA (cognitive functions)
# Factors PCA -> K-means Clusters (personality types)
# Se -> Si -> Ne -> Ni
fun_factor_scores(
  df_data = df_occupations
  , efa_model = efa_model
) -> dsdsds

dsdsds %>% 
  select(
    starts_with(
      'factor')
  ) %>% 
  fun_efa.nfactors(
    .dbl_weights = 
      df_occupations$
      employment2
  )

df_occupations %>%
  select(ends_with('.l')) %>%
  t() %>% 
  `colnames<-`(
    df_occupations$
      occupation
  ) %>% 
  pca(
    nfactors = 4
    # , rotate =
    #   'equamax'
    , weight =
      df_occupations$
      employment2
    ) -> lalalala

lalalala %>% 
  fun_factor_loadings() %>% 
  pivot_longer(
    cols = -1
    , names_to = 'component'
    , values_to = 'loading'
  ) %>% 
  mutate(
    component =
      str_replace_all(
        component
        , 'factor'
        , 'component'
      )
  ) %>% 
  group_by(item) %>% 
  filter(
    loading ==
      max(loading)
  ) %>%
  ungroup() %>% 
  view
  



view(as_tibble(loadings(lalalala)[,], rownames = 'item'))

dsdsds %>% 
  select(
    starts_with(
      'factor')
  ) %>%
  pca(
    nfactors = 4
    , weight = 
      df_occupations$
      employment2
    ) -> 
  lalala

lalala$values

lalala$rotation

lalala$loadings

loadings(lalala)

fun_factor_loadings()


kmeans(
  df_career_types[-1]
  , centers = 16
) -> list_kmeans_types

df_occupations %>% 
  select(
    occupation
    , career_cluster
  ) %>% 
  mutate(
    type = 
      paste0(
        'type',
        list_kmeans_types$
          cluster
      )
  ) %>%
  view


list_kmeans_types$
  centers %>% 
  t() %>% 
  as_tibble() %>% 
  set_names(
    paste0('type',1:ncol(.))
  ) %>% 
  cor() %>%
  as_tibble(
    rownames = 'type'
  ) %>%
  pivot_longer(
    cols = -1
    , names_to = 'comparison'
    , values_to = 'r'
  ) %>% 
  fun_plot.heatmap(aes(
    x = type
    , y = comparison
    , fill = r
    , label = number(r,accuracy = .01)
  )
  , .reorder_fct = T
  , .scale_colors = 
    scale_fill_gradient2(low = 'red', mid = 'white', high = 'blue')
  )


list_kmeans_types$
  centers %>%
  as_tibble() %>% 
  mutate(
    .before = 1
    , type = row_number()
    , type = paste0('type',type)
    , type = factor(type)
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'factor'
    , values_to = 'factor_loading'
  ) %>% 
  mutate(
    factor = recode(
      factor
      , 'factor1' = 'discern'
      , 'factor2' = 'mechanical'
      , 'factor3' = 'health'
      , 'factor4' = 'transport'
      , 'factor5' = 'manage'
      , 'factor6' = 'social'
      , 'factor7' = 'analytical'
      , 'factor8' = 'business'
      , 'factor9' = 'dexterity'
      , 'factor10' = 'admin'
      , 'factor11' = 'building'
      , 'factor12' = 'IQ'
      , 'factor13' = 'industrial'
      , 'factor14' = 'arts'
      , 'factor15' = 'robust'
    )
    , factor = fct_inorder(factor)
  ) -> df_types_kmeans

df_types_kmeans %>% 
  group_by(type) %>% 
  reframe(
    sd = sd(factor_loading)
  ) %>% 
  mutate(
    sd_desc = cut(sd, 2, labels = F)
    , sd_desc = recode(
      sd_desc,
      '1' = 'generalist',
      '2' = 'specialist'
    )
  ) %>%
  select(-sd) %>%
  left_join(
    df_types_kmeans %>% 
      group_by(type) %>% 
      mutate(
        factor_loading = 
          factor_loading / 
          max(factor_loading)
      ) %>% 
      mutate(
        factor_loading_desc = 
          cut(
            factor_loading, 3,
            labels = F
          )
        , factor_loading_desc = 
          factor(factor_loading_desc)
        , factor_loading_desc =
          recode(
            factor_loading_desc
            , '1' = 'low'
            , '2' = 'aux'
            , '3' = 'high'
          )
      ) %>% 
      group_by(type) %>% 
      reframe(
        functions = 
          paste(
            factor_loading_desc,
            '-',
            factor,
            collapse = ', '
          )
      )
  ) %>% 
  arrange(sd_desc)

df_types_kmeans %>%
  group_by(factor) %>% 
  mutate(
    factor_loading = 
      factor_loading / 
      max(factor_loading)
  ) %>%
  ungroup() %>%
  fun_plot.heatmap(aes(
    x = factor
    , y = type
    , fill = factor_loading
    , label = number(factor_loading,accuracy = .01)
  )
  , .reorder_fct = T
  )

c(
  'type1' = ,
  'type2' = ,
  'type3' = ,
  'type4' = ,
  'type5' = ,
  'type6' = 'proletariat',
  'type7' = ,
  'type8' = 'engineer',
  'type9' = ,
  'type10' = ,
  'type11' = ,
  'type12' = ,
  'type13' = 'business managers',
  'type14' = 'high-level healer',
  'type15' = ,
  'type16' = 'proletariat'
)


df_career_types %>% 
  mutate(
    .after = 1 
    , career_type = 
      (kmeans(
        df_career_types[-1]
        , 8
      ))$cluster
  ) %>% 
  view

lalala$cluster

pca(
  df_career_types[-1]
  , 8
) -> dsds

loadings(dsds)[,]

# pca(
#   # df_career_types[-1]
#   df_occupations %>% 
#     select(ends_with('.l'))
#   , 15
#   ) -> dsds
# 
# dsds$Vaccounted
# 
# as_tibble(loadings(dsds)[,])

list_efa_factor_scores$
  EFA.workflow$
  EFA$
  EFA.4factors$
  model %>% 
  fun_factor_loadings()

dsds























# - Output ----------------------------------------------
