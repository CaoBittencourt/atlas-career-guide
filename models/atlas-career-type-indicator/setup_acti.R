# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.acti' #Atlas Career Type Indicator
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
df_occupations <- read_csv('C:/Users/Cao/Documents/Github/Atlas-Research-dev/Data/df_occupations_2023_efa.csv')

# My own professional profile
df_input <- read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSj7u2N59j8MTa7MZqk2Y-VDVWIWEDzAR_0gkb_jB_pBX4sm8yMS1N26ClmY6iWXA/pub?gid=145103706&single=true&output=csv')

# Factor model
efa_model <- read_rds('C:/Users/Cao/Documents/Github/atlas-research-dev/data/efa/efa_equamax_14factors.rds')

# - Parameters ------------------------------------------------------------
# Factor symbols
c(
  'Ds', 'Eg', 'Hs',
  'Mn', 'Tr', 'Ad',
  'So', 'Ah', 'Hz',
  'An', 'Mt', 'Rb',
  'In', 'Mc'
) -> chr_factor_labels

# [DATA] ------------------------------------------------------------------
# - Bind data frames ------------------------------------------------------
# Bind occupations and input data frames
df_occupations %>%
  bind_rows(df_input) -> 
  df_occupations

rm(df_input)

# [ACTI] --------------------------------------------------------------
# - Estimate ACTI types ---------------------------------------------------------
# Apply ACTI function
fun_acti_type(
  df_data = df_occupations
  , efa_model = efa_model
  , chr_factor_labels = 
    chr_factor_labels
  , chr_id_col = 
    'occupation'
  , dbl_scale_lb = 0
) -> df_acti

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
save.image('image_acti.RData')
