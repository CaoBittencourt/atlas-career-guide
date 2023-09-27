# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages (temp)
  # , 'ggplot2', 'scales' #Data visualization
  , 'readr' #Read data (temp)
  , 'vctrs' #Data frame subclasses
  , 'tidyr', 'dplyr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.intc',
  'CaoBittencourt' = 'atlas.match'
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

rm(chr_pkg, chr_git)

# [FUNCTIONS] ---------------------------

# # - Hierarchical clustering function ----------------------------------------------------------------------
# fun_evol_hclust <- function(){
#   
#   # Arguments validation
#   
#   # Data wrangling
#   
#   # If no interchangeability matrix, estimate interchangeability
#   
#   # Convert interchangeability to a distance metric
#   
#   # Call hierarchical clustering function
#   
#   # dsds
#   
#   # Output
#   
# }
# 

# [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)

# read_rds(
#   'C:/Users/Cao/Documents/Github/atlas-research-dev/data/efa/efa_equamax_14factors.rds'
# ) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research-dev/Data/df_occupations_2023_efa.csv'
) -> df_occupations

# read_csv(
#   'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
# ) -> df_input

# # - Interchangeability ----------------------------------------------------
# fun_match_similarity(
#   df_data_rows = df_occupations,
#   df_query_rows = df_occupations,
#   chr_method = 'bvls',
#   dbl_scale_ub = 100,
#   dbl_scale_lb = 0,
#   chr_id_col = 'occupation'
# ) -> list_similarity
# 
# list_similarity$
#   mtx_similarity
# 
# fun_intc_interchangeability(
#   dbl_similarity = 
#     list_similarity$
#     mtx_similarity
# ) -> mtx_interchangeability
# 
# list_similarity$
#   mtx_similarity %>% 
#   as_tibble() %>% 
#   map_df(fun_intc_interchangeability) %>% 
#   as.matrix() -> 
#   # mtx_dist
#   mtx_interchangeability
# 
# # 1 - mtx_dist -> mtx_dist
# 
# # colnames(mtx_dist) ->
# #   rownames(mtx_dist)
# 
# colnames(mtx_interchangeability) ->
#   rownames(mtx_interchangeability)
# 
# 
# dist(
#   mtx_interchangeability
#   , method = 'euclidean'
# ) -> dist_interchangeability
# 
# hclust(dist_interchangeability) -> list_hclust
# 
# 
# 
# 
# 
