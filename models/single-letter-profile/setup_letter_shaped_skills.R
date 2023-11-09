# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
  , 'tidyr', 'dplyr' #Data wrangling
  , 'ggplot2' #Data visualization
  , 'vctrs' #Data frame subclasses
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.ftools', #Factor scores
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
  
  # Upside down characters
  if(lgc_upside_down){
    
    df_letters %>%
      group_by(font) %>% 
      mutate(
        glyph = glyph + max(glyph),
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
      , item = x
      , item_score = 
        y / (max(y) - min(y)) - 
        min(y) / (max(y) - min(y))
      , item_score = 
        dbl_scale_ub * 
        item_score
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

fun_letters_data() %>% 
  group_by(
    glyph, font
  ) %>% 
  slice(1) %>% 
  ungroup() %>% 
  nrow()
  reframe(
    max(item_score),
    min(item_score)
  )

hershey::hershey %>%
  group_by(
    glyph, font
  ) %>% 
  reframe(
    min = min(y),
    max = max(y),
  ) %>% 
  filter(
    font == 'futural'
  ) %>% 
  arrange(desc(
    max
  ))


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

# - Plot letter -----------------------------------------------------------
fun_letter_plot <- function(df_letter){
  
  # Arguments validation
  stopifnot(
    "'df_letter' must be a data frame with the 'df_letter' subclass." = 
      any(class(df_letter) == 'df_letter')
  )
  
  # Plot letter
  df_letter %>%
    ggplot(aes(
      x = x,
      y = y,
      group = stroke
    )) + 
    geom_path() +
    geom_point() + 
    coord_equal() + 
    theme_minimal() -> 
    plt_letter
  
  # Output
  return(plt_letter)
  
}

# [DATA] ------------------------------------------------------------------
# - Bind data frames ------------------------------------------------------
# Bind occupations and input data frames
df_occupations %>%
  bind_rows(df_input) -> 
  df_occupations

rm(df_input)

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
