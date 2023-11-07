# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
  , 'tidyr', 'dplyr' #Data wrangling
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
hershey::create_string_df(letters)

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
