# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.letters' #Letter-shaped profiles
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

# # Activate / install Git packages
# Map(
#   function(git, profile){
#     
#     if(!require(git, character.only = T)){
#       
#       install_github(
#         paste0(profile, '/', git)
#         , upgrade = F
#         , force = T
#       )
#       
#     }
#     
#     require(git, character.only = T)
#     
#   }
#   , git = chr_git
#   , profile = names(chr_git)
# )

chr_pkg <- c(
  'devtools' #GitHub packages
)

# - Data ------------------------------------------------------------------
# Occupations data frame
df_occupations <- read_csv('/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv')

# # Questionnaire
# df_questionnaire <- read_csv('/home/Cao/Storage/github/atlas-research/data/efa/questionnaire_efa_equamax_14factors_60items.csv')

# My own professional profile
df_input <- read_csv('/home/Cao/Storage/github/atlas-research/data/questionnaires/questionnaire_Cao.csv')

# # Factor model
# efa_model <- read_rds('/home/Cao/Storage/github/atlas-research/data/efa/efa_equamax_14factors.rds')

# # - Parameters ------------------------------------------------------------
# # Factor symbols
# df_questionnaire$
#   factor_abbv %>% 
#   unique() -> 
#   chr_factor_labels

# [DATA] ------------------------------------------------------------------
# - Letters vs occupations match ------------------------------------------
# fun_letters_data(
#   chr_font = 'latin'
# ) %>%
#   fun_letters_plot(
#     list_letters_match = NULL
#   )

fun_letters_similarity(
  df_letters_profile = 
    fun_letters_data() %>%
    fun_letters_profiles(
      # int_items = 120
      int_items = 60
      , chr_id_col =
        'occupation'
      , lgc_pivot_long = F
    ) %>% 
    select(
      occupation,
      starts_with('item_')
    )
  , df_query_rows = 
    # df_input
  # df_input %>%
  # bind_rows(
  #   df_input
  # )
    df_occupations %>% 
    select(any_of(
      names(df_input)
    )) %>% 
    slice_head(
      n = 10
    )
  , chr_method =
    'bvls'
  ## 'logit'
  ## 'pearson'
  ## 'knn'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 
    'occupation'
  , lgc_sort = T
) -> list_letters_match

list_letters_match$list_similarity
list_letters_match$mtx_similarity %>% View()

library(plyr)

list_letters_match %>%
  plyr::join_all(
    by = 'occupation',
    type = 'inner'
  ) %>% 
  View()

list_letters_match %>% 
  bind_rows(
    .id = 'dsds'
  ) %>% 
  as.matrix()

list_letters_match[[2]] %>% 
  bind_cols() %>%
  as.matrix()

list_letters_match$
  mtx_similarity %>% 
  as_tibble(
    rownames = 'id_glyph'
  )

list_letters_match$
  df_similarity

fun_letters_similarity(
  df_letters_profile = 
    fun_letters_data() %>%
    fun_letters_profiles(
      # int_items = 120
      int_items = 60
      , chr_id_col =
        'occupation'
      , lgc_pivot_long = F
    ) %>% 
    select(
      occupation,
      starts_with('item_')
    )
  , df_query_rows = 
    fun_letters_data() %>%
    fun_letters_profiles(
      # int_items = 120
      int_items = 60
      , chr_id_col =
        'occupation'
      , lgc_pivot_long = F
    ) %>% 
    select(
      occupation,
      starts_with('item_')
    )
  
  # df_input %>% 
  # bind_rows(
  #   df_input
  # )
  
  # df_occupations %>%
  # select(
  #   occupation,
  #   starts_with('skl_'),
  #   starts_with('abl_'),
  #   starts_with('knw_')
  # ) %>% 
  # slice(1)
  , chr_method =
    'bvls'
  ## 'logit'
  ## 'pearson'
  ## 'knn'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 
    'occupation'
  , lgc_sort = F
) -> list_letters_match

fun_letters_data() %>%
  fun_letters_profiles(
    # int_items = 120
    int_items = 60
    , chr_id_col =
      'occupation'
    , lgc_pivot_long = F
  )

list_letters_match$list_similarity
list_letters_match$mtx_similarity %>% View()

# fun_match_similarity(
#   df_data_rows = 
#     df_input %>% 
#     bind_rows(
#       df_input
#     )
#   , df_query_rows = 
#     df_input %>% 
#     bind_rows(
#       df_input
#     )
#   , chr_method = 
#     'bvls'
#   , dbl_scale_ub = 100
#   , dbl_scale_lb = 0
#   , chr_id_col = 
#     'occupation'
# )

list_letters_match$
  mtx_similarity %>% 
  View()

library(atlas.plot)

list_letters_match$
  df_similarity %>% 
  select(
    occupation,
    similarity
  )

fun_letters_data() %>% 
  filter(
    font == 'latin',
    glyph == 86
    # font == 'cyrillic',
    # glyph == 95
  ) %>% 
  fun_letters_plot()

list_letters_match$
  df_similarity %>% 
  select(
    occupation,
    similarity
  ) %>% 
  fun_plot.density(aes(
    x = similarity
  )
  , .list_axis.x.args = list(
    limits = c(-.1, 1.1)
    , breaks = seq(0, 1, 0.25)
  )
  , .fun_format.x = percent
  )

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
  arrange(
    item_score
  ) %>% 
  mutate(
    item = paste0('item_', 1:n())
  ) %>% 
  pivot_wider(
    id_cols = 1
    , names_from = 'item'
    , values_from = 'item_score'
  ) %>% 
  ungroup() -> dsds

fun_letters_data() %>% 
  fun_letters_profiles(
    int_items = 
      ncol(dsds) - 1
    , chr_id_col =
      'occupation'
    , lgc_pivot_long = F
  ) -> lalala

fun_letters_data(
  chr_font = 'greek'
  , int_glyph = 64
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_upside_down = F
)

lalala %>% 
  filter(
    font == 'greek'
    , glyph == 64
  ) %>% 
  pull(
    item_score
  )

library(atlas.gene)
library(atlas.comp)

# dsds %>% 
#   group_by(
#     occupation
#   ) %>% 
#   pivot_longer(
#     cols = -1,
#     names_to = 'item',
#     values_to = 'item_score'
#   ) %>%
lalala %>%
  group_by(
    glyph,
    font
  ) %>%
  mutate(
    generality = 
      fun_gene_generality(
        dbl_profile = item_score
        , dbl_scale_ub = 100
        , dbl_scale_lb = 0
      ) %>% 
      as.numeric()
    , competence = 
      fun_comp_competence(
        dbl_profile = item_score
        , dbl_scale_ub = 100
        , dbl_scale_lb = 0
        , generality
      )
  ) %>% 
  slice(1) %>%
  ungroup() %>%
  mutate(
    gene_class = 
      fun_class_classifier(
        dbl_var = generality
        , dbl_scale_lb = 0
        , dbl_scale_ub = 1
        , int_levels = 6
        , chr_class_labels = rev(c(
          'very generalist',
          'generalist',
          'somewhat generalist',
          'somewhat specialist',
          'specialist',
          'very specialist'
        ))
      )
    , comp_class = 
      fun_class_classifier(
        dbl_var = competence
        , dbl_scale_lb = 0
        , dbl_scale_ub = 1
        , int_levels = 6
        , chr_class_labels = rev(c(
          'very competent',
          'competent',
          'somewhat competent',
          'somewhat incompetent',
          'incompetent',
          'very incompetent'
        ))
      )
  ) %>% 
  group_by(
    # comp_class,
    gene_class
  ) %>% 
  tally() %>% 
  print(
    n = Inf
  )

reframe(
  min = min(generality),
  max = max(generality)
)

select(
  occupation
  , generality
) %>% 
  fun_plot.density(aes(
    x = generality
  )
  , .list_axis.x.args = list(
    limits = c(-.19, 1.19)
    , breaks = seq(0, 1, .25)
  )
  , .fun_format.x = percent
  )

atlas.gene::fun_gene_generality(rep(0,120), 0)

atlas.comp::fun_comp_competence()

atlas.match::fun_match_similarity(
  df_data_rows = 
    lalala %>%
    filter(
      font == 'cyrillic'
    )
  , df_query_rows = 
    dsds %>% 
    slice(1)
  # , chr_method = 'logit'
  , chr_method = 'pearson'
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , chr_id_col = 
    'occupation'
) -> dsdsds

dsdsds

dsdsds$
  df_similarity %>% 
  select(
    glyph,
    font,
    similarity
  ) %>% 
  arrange(desc(
    similarity
  ))

fun_letters_data(
  chr_font = 'cyrillic'
  , int_glyph = 19
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_upside_down = F
) %>% 
  # filter(
  #   glyph < 0
  # ) %>% 
  fun_letters_plot()

fun_letters_data(
  chr_font = 'cyrillic'
  , int_glyph = 69
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , lgc_upside_down = F
) %>% 
  mutate(
    .before = 1
    , occupation = 
      paste0(
        glyph, '_', font
      )
  ) %>%
  ggplot(aes(
    x = x,
    y = item_score,
    # group = stroke,
    color = occupation
  )) +
  geom_path(aes(
    group = stroke
  )
  , linewidth = 1.19
  ) +
  geom_point(
    data = 
      dsds %>%
      slice(1) %>%
      pivot_longer(
        cols = -1
        , names_to = 'item'
        , values_to = 'item_score'
      ) %>%
      right_join(
        fun_letters_data(
          int_glyph = 69
          , chr_font = 'cyrillic'
          , lgc_upside_down = F
        ) %>%
          # filter(
          #   glyph < 0
          # ) %>%
          fun_letters_profiles(
            int_items = 120
            , lgc_pivot_long = T
          ) %>%
          select(
            x, item
          )
      )
    , size = 3
    , alpha = 0.9
  ) +
  coord_equal() +
  theme_minimal() +
  scale_y_reverse() +
  ylim(0, 100)

bind_rows(
  dsds %>%
    slice(1) %>%
    pivot_longer(
      cols = -1
      , names_to = 'item'
      , values_to = 'item_score'
    ) %>%
    right_join(
      fun_letters_data(
        int_glyph = 69
        , chr_font = 'cyrillic'
        , lgc_upside_down = F
      ) %>%
        # filter(
        #   glyph < 0
        # ) %>%
        fun_letters_profiles(
          int_items = 120
          , lgc_pivot_long = T
        ) %>%
        select(
          x, item
        )
    )
  
) %>%
  ggplot(aes(
    x = x,
    y = item_score,
    group = stroke,
    color = occupation
  )) +
  geom_path() +
  geom_point() +
  facet_grid(
    rows = vars(occupation)
  )
coord_equal() +
  theme_minimal() +
  # scale_y_reverse() +
  ylim(100, 0)

dsds %>%
  slice(1) %>%
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item_score'
  ) %>%
  right_join(
    fun_letters_data(
      int_glyph = 69
      , chr_font = 'cyrillic'
      , lgc_upside_down = F
    ) %>%
      # filter(
      #   glyph < 0
      # ) %>%
      fun_letters_profiles(
        int_items = 120
        , lgc_pivot_long = T
      ) %>%
      select(
        x, item
      )
  ) %>%
  ggplot(aes(
    x = x,
    y = item_score,
    # y = y,
    # group = stroke
  )) +
  # geom_path() +
  geom_point() +
  coord_equal() +
  theme_minimal() +
  scale_y_reverse() +
  ylim(100, 0)

dsdsds$
  df_similarity %>% 
  select(
    glyph,
    font,
    similarity
  ) %>% 
  arrange(desc(
    similarity
  )) %>% 
  fun_plot.density(aes(
    x = similarity
  )
  , .list_axis.x.args = list(
    limits = c(-0.19, 1.19)
    , breaks = seq(0, 1, 0.25)
  )
  , .fun_format.x = percent
  )

fun_letters_data() %>%
  filter(
    glyph == -1,
    font == 'cyrillic'
  ) %>% 
  fun_letters_profiles(
    int_items = 120
  ) %>%
  atlas.match::fun_match_similarity(
    df_query_rows = 
      dsds %>% 
      slice(1)
    , chr_method = 'bvls'
    , dbl_scale_ub = 100
    , dbl_scale_lb = 0
    , chr_id_col = 
      'occupation'
  ) -> lalala

lalala$
  df_similarity$
  similarity

# [MODEL] --------------------------------------------------------------
# - Estimate factor scores ---------------------------------------------------------
# Apply factor scores function
df_occupations %>% 
  fun_ftools_factor_scores(
    efa_model = efa_model,
    lgc_factors_only = F,
    lgc_pivot = T
  ) %>%
  select(
    occupation,
    starts_with('factor')
  ) -> df_factor_scores

df_factor_scores %>% 
  mutate(
    factor_class =
      fun_class_classifier(
        dbl_var = factor_score,
        dbl_scale_lb = 0,
        dbl_scale_ub = 100,
        int_levels = 4,
        chr_class_labels = c(
          'untrained',
          'low',
          'mid',
          'high'
        )
      )
  ) -> df_factor_class

df_factor_class %>% 
  group_by(
    occupation,
    factor_class
  ) %>% 
  tally() %>% 
  filter(
    # occupation == 'Cao'
    # occupation == 'Statisticians'
    # occupation == 'Physicists'
    # occupation == 'Mathematicians'
    # occupation == 'Chief Executives'
    # occupation == 'Psychiatrists'
    occupation == 'Coroners'
  )

# df_factor_scores %>% 
#   mutate(across(
#     .cols = starts_with('factor')
#     ,.fns = ~ fun_class_classifier(
#       dbl_var = .x,
#       dbl_scale_lb = 0,
#       dbl_scale_ub = 100,
#       int_levels = 4,
#       chr_class_labels = c(
#         'untrained',
#         'low',
#         'mid',
#         'high'
#       )
#     )
#   ))

# [PLOTS] -----------------------------------------------------------------
# - Plot ACTI molecules ---------------------------------------------------------
# Apply ACTI plot function
df_acti %>%
  fun_acti_plot_molecule() ->
  list_plt_acti

# [CLEAR] -----------------------------------------------------------------
# - ACTI table list -------------------------------------
# Split ACTI data frame into a list
df_acti %>% 
  split(.$id_profile) ->
  list_df_acti

# - Keep only necessary variables --------------------------
# Variables to keep
c(
  'list_plt_acti',
  'list_df_acti',
  'chr_pkg',
  'chr_git'
) -> chr_var_keep

# Remove everything else
rm(
  list =
    .GlobalEnv %>% 
    as.list() %>% 
    names() %>% 
    subset(!(
      .GlobalEnv %>% 
        as.list() %>% 
        names() %in% 
        chr_var_keep
    ))
)

# [EXPORT] ----------------------------------------------------------------
# - Working directory -----------------------------------------------------
setwd(dirname(
  rstudioapi::getSourceEditorContext()$path
))

# - Save .RData image --------------------------------------------------
save.image('./image_acti.RData')
