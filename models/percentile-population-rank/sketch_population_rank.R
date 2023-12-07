# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'readr' #Read data
  , 'dplyr', 'tidyr' #Data wrangling
  , 'Hmisc' #Weighted variance
)

# Git packages
chr_git <- c(
  # 'CaoBittencourt' = 'atlas.letters' #Letter-shaped profiles
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
# df_questionnaire <- read_csv('/home/Cao/Storage/github/atlas-research/data/efa/questionnaire_efa_equamax_14factors_120items.csv')

# My own professional profile
df_input <- read_csv('/home/Cao/Storage/github/atlas-research/data/questionnaires/questionnaire_Cao.csv')

# # Factor model
# efa_model <- read_rds('/home/Cao/Storage/github/atlas-research/data/efa/efa_equamax_14factors.rds')

# [DATA] ------------------------------------------------------------------
# - Long data frame -------------------------------------------------------
df_occupations %>% 
  select(
    employment_variants,
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
  ) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item_score'
  ) -> df_attributes

# - Descriptive statistics for item scores --------------------------------
# Weighted mean and sd
df_attributes %>%
  group_by(
    item
  ) %>% 
  reframe(
    mean = 
      wtd.mean(
        x = item_score,
        w = employment_variants
      )
    , sd = sqrt(
      wtd.var(
        x = item_score,
        w = employment_variants
      )
    )
  ) -> df_stats

# - Percentiles for item scores -------------------------------------------
# Define scale
dbl_scale <- round(seq(0, 100, length.out = 7))

# Calculate scale percentiles
vapply(
    dbl_scale
    , function(x){
      
      pnorm(
        x, 
        df_stats$
          mean
        , df_stats$
          sd
      )
      
    }
    , FUN.VALUE = 
      numeric(nrow(
        df_stats
      ))
) -> mtx_percentiles

# colnames(mtx_percentiles) <- paste0('lower_tail_', dbl_scale)
colnames(mtx_percentiles) <- dbl_scale

rownames(mtx_percentiles) <- df_stats$item

mtx_percentiles %>% 
  as_tibble(
    rownames = 'item'
  ) %>% 
  pivot_longer(
    cols = -item
    , names_to = 'scale_point'
    , values_to = 'lower_tail'
  ) -> df_percentiles

# # - Recalculate percentiles for item scores -------------------------------
# df_stats %>% 
#   full_join(
#     df_attributes
#   ) %>% 
#   group_by(
#     item
#   ) %>% 
#   mutate(
#     q20 = 
#       qnorm(
#         0.20
#         , mean = 
#           mean
#         , sd = 
#           sd
#       )
#   ) %>% 
#   filter(
#     item_score >=
#       q20
#   ) %>% 
#   reframe(
#     mean = 
#       wtd.mean(
#         x = item_score,
#         w = employment_variants
#       )
#     , sd = sqrt(
#       wtd.var(
#         x = item_score,
#         w = employment_variants
#       )
#     )
#   ) -> df_stats
#   
# - Rank percentiles in terms of population -------------------
# Population for which each point on the scale is highest 
df_percentiles %>% 
  mutate(
    rank_population = floor(
      1 / (1 - lower_tail)
    )
  ) -> df_percentiles

# Replace Inf
df_percentiles %>% 
  group_by(
    item
  ) %>% 
  mutate(
    rank_population = 
      if_else(
        !is.infinite(
          rank_population
          )
        , rank_population
        , lag(rank_population, 1) * (
          lag(rank_population, 1) /
            lag(rank_population, 2)
        )
      )
  ) -> df_percentiles

library(atlas.plot)

df_occupations %>% 
  fun_plot.density(aes(
    # x = skl_technology_design
    x = abl_dynamic_flexibility
    , weights = employment_variants
  )
  , .list_axis.x.args = list(
    limits = c(-10, 110)
    , breaks = round(seq(0, 100, length.out = 7))
  )
  , .fun_format.x = number
  )
  
df_percentiles %>% 
  print(
    n = Inf
  )

# - Classify percentile ranks ---------------------------------------------
