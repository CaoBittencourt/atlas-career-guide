# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'readr' #Read data
  , 'dplyr', 'tidyr' #Data wrangling
)

# Git packages
chr_git <- c(
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
# My career profile
df_profile_adjusted <- read_csv('/home/Cao/Storage/github/atlas-research/data/questionnaires/questionnaire_Cao.xlsx')

# [MODEL] -----------------------------------------------------------------
# - All skill sets ------------------------------------------------------------------
# - Competence-only skill set ------------------------------------------------------------------
df_profile_adjusted

# - Preference-only skill set ------------------------------------------------------------------
df_profile_adjusted

# - Preference-adjusted (comp-pref) skill set ------------------------------------------------------------------
df_profile_adjusted
# - Preference-adjusted (pref-comp) skill set ------------------------------------------------------------------
df_profile_adjusted
# [OUTPUT] -----------------------------------------------------------------
# - Export career profiles ------------------------------------------------------------------
df_profile: occupation = c('Cao (comp)', 'Cao (pref)', 'Cao (comp_pref)', 'Cao (pref_comp)')
df_profile_pref: occupation = 'Cao (pref)'
df_profile_comp: occupation = 'Cao (comp)'
df_profile_comp_pref: occupation = 'Cao (comp_pref)'
df_profile_pref_comp: occupation = 'Cao (pref_comp)'
