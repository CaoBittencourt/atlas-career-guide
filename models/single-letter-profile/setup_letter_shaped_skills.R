# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
  , 'tidyr', 'dplyr' #Data wrangling
  , 'ggplot2' #Data visualization
  , 'vctrs' #Data frame subclasses
  , 'numbers' #Lowest common multiple
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.match', #Matching 
  'CaoBittencourt' = 'atlas.plot', #Data viz (temp)
  'CaoBittencourt' = 'atlas.class', #Classification
  'coolbutuseless' = 'hershey' #Vector letters
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

chr_pkg <- c(
  'devtools' #GitHub packages
)

# - Data ------------------------------------------------------------------
# Occupations data frame
df_occupations <- read_csv('/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv')

# Questionnaire
df_questionnaire <- read_csv('/home/Cao/Storage/github/atlas-research/data/efa/questionnaire_efa_equamax_14factors_60items.csv')

# My own professional profile
df_input <- read_csv('/home/Cao/Storage/github/atlas-research/data/questionnaires/questionnaire_Cao.csv')

# Factor model
efa_model <- read_rds('/home/Cao/Storage/github/atlas-research/data/efa/efa_equamax_14factors.rds')

# - Parameters ------------------------------------------------------------
# Factor symbols
df_questionnaire$
  factor_abbv %>% 
  unique() -> 
  chr_factor_labels

# - Letters --------------------------------------------------------------
hershey::hershey %>%
  filter(
    
    # very cool (single stroke)
    # font == 'rowmans'
    
    # very cool (double / triple stroke)
    # font == 'timesg'
    # font == 'timesr'
    # font == 'timesrb'
    # font == 'rowmand'
    # font == 'rowmant'
    # font == 'cyrilc_1'
    # font == 'cyrillic'
    
    # font == 'mathlow'
    # font == 'mathupp'
    # font == 'greek'
    # font == 'greeks'
    # font == 'greekc'
    # font == 'futuram'
    
    # no 
    # font == 'markers'
    # font == 'scriptc'
    # font == 'scripts'
    # font == 'meteorology'
    # font == 'astrology'
    # font == 'japanese'
    # font == 'music'
    # font == 'scripts'
    # font == 'scriptc'
    # font == 'cursive'
    # font == 'symbolic'
    
    # good but no (non essential)
    # font == 'timesi'
    # font == 'timesib'
    # font == 'futural' # redundant with rowmans
  ) %>% 
  # filter(
  #   glyph %in% 1:20
  # ) %>%
  ggplot() + 
  geom_path(aes(x, y, group = stroke)) + 
  coord_equal() + 
  theme_void() + 
  facet_wrap(~glyph, labeller = label_both, ncol = 20)

# 1. [ ] select fonts
# 2. [ ] select glyphs
# 3. [x] normalize glyph scale by font max and min
# 4. [x] invert all glyphs (bind_row upside down)
# 5. [/] convert glyph coordinates to professional profile
# 6. [ ] letter matching function
# 7. [/] letter plotting function


fun_letters_plot <- function(chr_string = 'A', chr_font = c()){
  
  # Arguments validation
  stopifnot(
    "'chr_string' must be a character string or a number coercible to a character string." = 
      any(
        is.character(chr_string),
        is.numeric(chr_string)
      )
  )
  
  # Data wrangling
  chr_string[[1]] -> chr_string
  
  # Plot string
  tryCatch(
    expr = {
      
      hershey::hershey$
        font %>% 
        unique()
      
      create_string_df(
        # chr_string
        # 'b', 'greek'
        # 'b', 'greekc'
        # 'a', 'greeks'
        # '5', 'mathlow'
        # 'A', 'cyrillic'
        # 'A', 'gothgrt'
        # 'A', 'cursive'
        # 'A', 'cyrilc_1'
        # 'T', 'futural'
        # 'T', 'timesg'
        # 'T', 'timesrb'
        # 'T', 'rowmans'
        'T', 'rowmant'
        # 'A', 'futuram'
      ) %>% 
        mutate(
          # y = -(y + min(y)),
          y = -(y + min(y)),
          y = y * 100 / max(y)
          # y = pmax(y, 0),
          # y = pmin(y, 100)
          # , x = factor(x, labels = '')
        ) %>% 
        ggplot(aes(
          x = x,
          y = y,
          group = stroke
        )) + 
        scale_y_reverse() +
        geom_path() + 
        geom_point() +
        coord_equal() + 
        theme_minimal() + 
        labs(
          x = 'Skill',
          y = 'Competence'
        )
      
    }
  )
  
  hershey %>%
    group_by(
      char
    ) %>% 
    tally()
  
  
  
}

hershey::hershey %>% 
  group_by(
    char, font
  ) %>% 
  slice(1) %>% 
  group_by(font) %>% 
  tally()  %>% 
  # filter(
  #   char != '',
  #   char != ' '
  # ) %>% 
  print(
    n = Inf
  )

fun_letter_profile <- function(){
  
  hershey::hershey %>% 
    group_by(
      char, font,
      factor(x)
    ) %>% 
    slice_min(
      order_by = y,
      n = 1
    ) %>% 
    ungroup() ->
    df_letters
  
  # df_letters %>%
  hershey::hershey %>%
    filter(
      char == 'P',
      # font == 'rowmant'
      # font == 'futuram'
      # font == 'futural'
      font == 'greek'
    ) %>% 
    # nrow()
    ggplot(aes(
      x = x,
      y = y,
      group = stroke
    )) + 
    geom_path() +
    geom_point() + 
    coord_equal() + 
    theme_minimal()
  
  # decide methodology
  df_letters %>% 
    group_by(
      char,
      font
    ) %>% 
    mutate(
      item = factor(x),
      # item_score = -(y + min(y)),
      # item_score = 
      #   item_score * 100 / 
      #   max(item_score)
      item_score = 
        y / (max(y) - min(y)) - 
        min(y) / (max(y) - min(y)),
      item_score = 100 * item_score
    ) %>%
    ungroup() %>%
    select(
      char,
      font,
      item,
      item_score
    ) %>% 
    group_by(
      char, font
    ) %>% 
    reframe(
      min = min(item_score),
      max = max(item_score)
    )
  pivot_wider(
    
  )
  
  
  
}


hershey::create_string_df('dsds') %>%
  ggplot(aes(
    x = x,
    y = y,
    group = stroke
  )) + 
  geom_path() +
  geom_point() + 
  coord_equal() + 
  theme_minimal()

# [FUNCTIONS] -------------------------------------------------------------
# - Letters data frame ----------------------------------------------------
fun_letters_data <- function(
    chr_font = c('cyrillic', 'greek', 'rowmans')
    , int_glyph = NULL
    , dbl_scale_ub = 100
    , dbl_scale_lb = 0
    , lgc_upside_down = T
){
  
  # Arguments validation
  stopifnot(
    "'chr_font' must be 'cyrillic', 'greek', and/or 'rowmans'." = 
      chr_font %in% c('cyrillic', 'greek', 'rowmans')
  )
  
  stopifnot(
    "'int_glyph' must be either NULL or an integer vector." = 
      any(
        is.numeric(int_glyph),
        is.null(int_glyph)
      )
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." =
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'lgc_upside_down' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_upside_down),
        !is.na(lgc_upside_down)
      )
  )
  
  # Data wrangling
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  hershey::hershey %>%
    filter(
      font %in% 
        chr_font
    ) -> df_letters
  
  if(length(int_glyph)){
    
    df_letters %>%
      filter(
        glyph %in%
          ceiling(
            int_glyph
          )
      ) -> df_letters
    
  }
  
  # Upside down characters
  if(lgc_upside_down){
    
    df_letters %>%
      group_by(font) %>% 
      mutate(
        glyph = -glyph,
        char = paste0(
          char, '_upside_down'
        ),
        y = -y
      ) %>% 
      bind_rows(
        df_letters
      ) -> df_letters
    
  }
  
  # Normalize font scales
  df_letters %>% 
    group_by(font) %>% 
    mutate(
      .after = y
      , item_score = 
        y / (max(y) - min(y)) - 
        min(y) / (max(y) - min(y))
      , item_score = 
        dbl_scale_ub * 
        item_score
    ) %>% 
    group_by(
      glyph,
      font
    ) %>% 
    mutate(
      .before = item_score
      , item = paste0(
        'item_'
        , 1:n()
      )
    ) %>% 
    ungroup() -> 
    df_letters
  
  # Add 'df_letters' subclass
  df_letters %>%
    new_data_frame(
      class = c(
        class(df_letters),
        'df_letters'
      )
    ) -> df_letters
  
  # Output
  return(df_letters)
  
}

# - Plot letters -----------------------------------------------------------
fun_letters_plot <- function(df_letters){
  
  # Arguments validation
  stopifnot(
    "'df_letters' must be a data frame with any of the following subclasses: 'df_letters', 'df_letters_profile', 'df_letters_profile_long'." =
      any(
        class(df_letters) == 'df_letters',
        class(df_letters) == 'df_letters_profile',
        class(df_letters) == 'df_letters_profile_long'
      )
  )
  
  # Plot df_letters
  if(any(class(df_letters) == 'df_letters')){
    
    # }
    
    # if(any(class(
    #   df_letters == 
    #   'df_letters'
    #   # df_letters_profile
    # ))){
    
    df_letters %>%
      ggplot(aes(
        x = x,
        y = item_score,
        # y = y,
        group = stroke
      )) + 
      geom_path() +
      geom_point() + 
      coord_equal() +
      theme_minimal() -> 
      plt_letters
    
  }
  
  # # Plot df_letters_long
  # if(any(class(
  #   df_letters == 
  #   'df_letters_profile_long'
  # ))){
  #   
  #   fun_letters_data(
  #     int_glyph = 90
  #     , chr_font = 'rowmans'
  #     , lgc_upside_down = T
  #   ) %>% 
  #     filter(
  #       glyph < 0 
  #     ) %>%
  #     ggplot(aes(
  #       x = x,
  #       y = item_score,
  #       # y = y,
  #       group = stroke
  #     )) + 
  #     geom_path() +
  #     geom_point() + 
  #     coord_equal() +
  #     theme_minimal() +
  #     scale_y_reverse() + 
  #     ylim(100, 0)
  #   
  #   dsds %>% 
  #     slice(1) %>% 
  #     pivot_longer(
  #       cols = -1
  #       , names_to = 'item'
  #       , values_to = 'item_score'
  #     ) %>% 
  #     right_join(
  #       fun_letters_data(
  #         int_glyph = 90
  #         , chr_font = 'rowmans'
  #         , lgc_upside_down = T
  #       ) %>% 
  #         filter(
  #           glyph < 0 
  #         ) %>%
  #         fun_letters_profiles(
  #           int_items = 120
  #           , lgc_pivot_long = T
  #         ) %>%
  #         select(
  #           x, item
  #         )
  #     ) %>%
  #     ggplot(aes(
  #       x = x,
  #       y = item_score,
  #       # y = y,
  #       # group = stroke
  #     )) + 
  #     geom_path() +
  #     geom_point() + 
  #     coord_equal() +
  #     theme_minimal() +
  #     scale_y_reverse() + 
  #     ylim(100, 0)
  #   
  #   
  #   fun_letters_data(
  #     int_glyph = 19
  #     , chr_font = 'rowmans'
  #     , lgc_upside_down = F
  #   ) %>% 
  #     fun_letters_profiles(
  #       int_items = 7
  #       , lgc_pivot_long = T
  #     ) %>%
  #     ggplot(aes(
  #       x = x,
  #       # y = item_score,
  #       y = y,
  #       group = stroke
  #     )) + 
  #     geom_path() +
  #     geom_point() + 
  #     coord_equal() +
  #     theme_minimal() -> 
  #     plt_letters
  #   
  # }
  
  # Output
  return(plt_letters)
  
}

# - Letters to professional profiles ------------------------------------------
fun_letters_profiles <- function(
    df_letters
    , int_items
    , lgc_pivot_long = F
    , chr_id_col = NULL
){
  
  # Arguments validation
  stopifnot(
    "'df_letters' must be a data frame with the 'df_letters' subclass." = 
      any(class(df_letters) == 'df_letters')
  )
  
  stopifnot(
    "'int_items' must be numeric." = 
      is.numeric(int_items)
  )
  
  stopifnot(
    "'lgc_pivot_long' must be either TRUE or FALSE." = 
      all(
        is.logical(lgc_pivot_long),
        !is.na(lgc_pivot_long)
      )
  )
  
  stopifnot(
    "'chr_id_col' must be either NULL or a character string." = 
      any(
        is.character(chr_id_col)
        , is.null(chr_id_col)
      )
  )
  
  # Data wrangling
  int_items[[1]] -> int_items
  
  lgc_pivot_long[[1]] -> lgc_pivot_long
  
  chr_id_col[[1]] -> chr_id_col
  
  # Lowest common multiple of row number
  df_letters %>% 
    group_by(
      glyph,
      font
    ) %>% 
    mutate(
      lcm_rows = 
        numbers::mLCM(c(
          int_items,
          n()
        )),
      rep_rows = 
        lcm_rows / 
        n()
    ) %>% 
    ungroup() -> 
    df_letters
  
  # Convert letters to professional profiles
  df_letters %>% 
    group_by(
      glyph, 
      font
    ) %>% 
    slice(rep(
      1:n(),
      first(
        rep_rows
      )
    )) %>% 
    arrange(
      item_score
    ) %>% 
    mutate(
      item = 
        rep(
          paste0('item_', 1:int_items)
          , each = n() / int_items
        )
    ) %>% 
    group_by(
      glyph,
      font,
      item,
      char
    ) %>%
    reframe(
      item_score = 
        mean(
          item_score
        )
      , across(
        .fns = first
      )
    ) -> df_letters_profile
  
  rm(df_letters)
  rm(int_items)
  
  # Pivot
  if(!lgc_pivot_long){
    
    df_letters_profile %>% 
      pivot_wider(
        id_cols = c(
          'glyph',
          'font',
          'char'
        )
        , names_from = 'item'
        , values_from = 'item_score'
      ) -> df_letters_profile
    
    # Add 'df_letters_profile' subclass
    df_letters_profile %>%
      new_data_frame(
        class = c(
          class(df_letters_profile),
          'df_letters_profile'
        )
      ) -> df_letters_profile
    
  } else {
    
    # Add 'df_letters_profile_long' subclass
    df_letters_profile %>%
      new_data_frame(
        class = c(
          class(df_letters_profile),
          'df_letters_profile_long'
        )
      ) -> df_letters_profile
    
  }
  
  # ID column
  if(!is.null(chr_id_col)){
    
    paste0(
      df_letters_profile$font,
      '_',
      df_letters_profile$glyph
    ) -> df_letters_profile[[
      chr_id_col
    ]]
    
  }
  
  # Output
  return(df_letters_profile)
  
}

# - Letter matching ----------------------------------------------
fun_letters_similarity <- function(){
  
  # Arguments validation
  
  # Data wrangling
  df_data %>% 
    select(
      chr_id_col
      , where(
        is.numeric
      )
    )
  
  # Apply matching function
  
  # Output
  
}

# [DATA] ------------------------------------------------------------------
# - Letters vs occupations match ------------------------------------------
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

weighted.mean(
  dsds$item_120
  , df_occupations$
    employment_variants
)

fun_letters_data() %>% 
  fun_letters_profiles(
    int_items = 
      ncol(dsds) - 1
    , chr_id_col =
      'occupation'
    , lgc_pivot_long = T
  ) -> lalala

lalala %>% names()

lalala %>% 
  mutate(
    .before = 1
    , occupation = 
      paste0(
        glyph,
        '_', 
        font
      )
  ) -> lalala

atlas.match::fun_match_similarity(
  df_data_rows = 
    lalala %>%
    filter(
      font == 'cyrillic'
    )
  , df_query_rows = 
    dsds %>% 
    slice(1)
  , chr_method = 'logit'
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
  , int_glyph = 69
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
