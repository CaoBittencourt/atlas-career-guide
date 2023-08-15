# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  'bvls'
  , 'fastglm'
  , 'weights'
  # 'atlas.ftools' #Factor analysis tools
  , 'dplyr', 'tidyr', 'purrr' #Data wrangling
  , 'remotes'
  , 'kselection' #K-means clustering
  # , 'vctrs' #Data wrangling
  # , 'modeest' #Mode
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# remotes::install_github('Van1yu3/SWKM')
library(SWKM)

# [FUNCTIONS] ---------------------------
# - Generalism function ---------------------------------------------------
fun_acti_generalism <- function(
    mtx_data
    , dbl_scale_lb = 0
    , dbl_discount = 0.25
){
  
  # Arguments validation
  stopifnot(
    "'mtx_data' must be a numeric matrix." =
      all(
        is.numeric(mtx_data)
      )
  )
  
  stopifnot(
    "'dbl_discount' must be a number between 0 and 1." =
      all(
        dbl_discount >= 0,
        dbl_discount <= 1
      )
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  # Data wrangling
  dbl_discount[[1]] -> dbl_discount
  
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  # Generalism helper function
  fun_acti_generalism_helper <- function(dbl_profile){
    
    # Divide each element of the vector by the max
    dbl_profile / 
      max(
        dbl_profile
        , na.rm = T
      ) -> dbl_profile
    
    dbl_scale_lb / 
      max(
        dbl_profile
        , na.rm = T
      ) -> dbl_scale_lb
    
    # Apply bounded variable skewness function
    fun_skew_sdmode(
      dbl_var = 
        dbl_profile
      , dbl_scale_lb = 
        dbl_scale_lb
      , dbl_scale_ub = 1
      , dbl_discount = 
        dbl_discount
    ) -> dbl_generalism
    
    rm(dbl_profile)
    
    # Output
    return(dbl_generalism)
    
  }
  
  # Apply generalism function to each row
  apply(
    mtx_data, 1
    , fun_acti_generalism_helper
  ) -> mtx_generalism
  
  # Output
  return(mtx_generalism)
  
}

# - Competency function ---------------------------------------------------
fun_acti_competency <- function(
    mtx_data
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    # , dbl_generalism
    # , int_levels = 3
){
  
  # Arguments validation
  
  # Data wrangling
  
  # Competency level helper function
  fun_acti_competency_helper <- function(dbl_profile){
    
    # Apply bounded variable skewness function
    # Discount is a function of variance
    # Weight variable with itself
    fun_skew_sdmode(
      # dbl_var = 
      # (dbl_profile ^ 2) / 
      # dbl_scale_ub
      dbl_var = 
        dbl_profile
      , dbl_scale_lb = 
        dbl_scale_lb
      , dbl_scale_ub = 
        dbl_scale_ub
      , dbl_discount = 
        1 -  mlv(
          dbl_profile /
            dbl_scale_ub
        )
    ) -> dbl_competency
    
    # Output
    return(dbl_competency)
    
  }
  
  # Apply competency level helper function
  apply(
    mtx_data, 1
    , fun_acti_competency_helper
  ) -> mtx_competency
  
  # Output
  return(mtx_competency)
  
}

# - Classifier function -------------------------------------------------
fun_acti_classifier <- function(
    dbl_var
    , dbl_scale_lb = 0
    , dbl_scale_ub = 1
    , int_levels = 5
){
  
  # Arguments validation
  stopifnot(
    "'dbl_var' must be numeric." = 
      is.numeric(dbl_var)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." = 
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." = 
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'int_levels' must be an integer." = 
      is.numeric(int_levels)
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  int_levels[[1]] -> int_levels
  ceiling(int_levels) -> int_levels
  
  # Classify competency level
  findInterval(
    dbl_var
    , seq(
      dbl_scale_lb, 
      dbl_scale_ub, 
      length.out = 1 +
        int_levels
    )
    , all.inside = T
  ) -> int_class_id
  
  # Output
  return(int_class_id)
  
}

# - Create Career types -----------------------------------------------------
fun_acti_derive_types <- function(
    df_data
    , efa_model
    , dbl_weights = NULL
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , dbl_discount = 0.25
    # , int_types = 16
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame with numeric columns." =
      all(
        is.data.frame(df_data),
        df_data %>%
          map_lgl(is.numeric) %>%
          any()
      )
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." =
      any(
        is.null(dbl_weights),
        all(
          is.numeric(dbl_weights),
          length(dbl_weights) ==
            nrow(df_data)
        )
      )
  )
  
  # Data wrangling
  
  # Determine generalist vs specialist scope descriptor
  
  
  # Determine high-mid-low level competency descriptor
  
  # Determine career type
  # Calculate factor scores
  atlas.ftools::fun_ftools_factor_scores(
    df_data = df_data
    , efa_model = efa_model
    , lgc_pivot = T
  ) -> df_factor_scores
  
  rm(df_data)
  
  # Calculate capital flexibility for each factor
  atlas.kcoef::fun_kcoef_kflex_macro_df(
    df_data = df_factor_scores
    , dbl_weights = dbl_weights
    , dbl_scale_lb = dbl_scale_lb
    , dbl_scale_ub = dbl_scale_ub
    , dbl_discount = dbl_discount
    , lgc_sample_variance = F
  ) -> df_kflex_macro
  
  # Define relevance as 1 - kflex_macro
  df_kflex_macro %>% 
    mutate(
      relevance = 
        1 - kflex_macro
    ) %>% 
    select(!kflex_macro) -> 
    df_kflex_macro
  
  # Join data frames
  df_factor_scores %>% 
    left_join(
      df_kflex_macro
    ) -> df_factor_scores
  
  rm(df_kflex_macro)
  
  # Truncate factor scores
  df_factor_scores
  
  # Remove useless factors
  
  # Classify type
  
  
  
  # Output
  
}

# - ACTI code -------------------------------------------------------------
fun_acti_code <- function(
  dbl_factor_scores
  # , dsds
  , dbl_generalism
  , dbl_competency
  , int_generalism_levels = 2
  , int_competency_levels = 5
){
  
  # Arguments validation
  
  # Data wrangling
  
  # Apply generalism classifier
  fun_acti_classifier(
    dbl_var = dbl_generalism
    , dbl_scale_lb = 0
    , dbl_scale_ub = 1
    , int_levels = 
      int_generalism_levels
  ) -> int_generalism_id
  
  # Apply competency classifier
  fun_acti_classifier(
    dbl_var = dbl_competency
    , dbl_scale_lb = 0
    , dbl_scale_ub = 1
    , int_levels = 
      int_competency_levels
  ) -> int_competency_id
  
  # Compose ACTI code
  paste0() ->
    chr_acti_code
  
  # Output
  return(list(
    'acti_code' = chr_acti_code,
    'generalism_id' = int_generalism_id,
    'competency_id' = int_competency_id
  ))
  
}

# - ACTI estimator helper function --------------------------------------------------------

# - ACTI estimator --------------------------------------------------------

# - ACTI matching ---------------------------------------------------------
# fun_eqvl_equivalence_acti

# # [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
) -> df_occupations

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023.csv'
) -> df_occupations

read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
) -> df_input

# - Generalism test -------------------------------------------------------
df_occupations %>% 
  transmute(
    occupation = occupation
    , generalism = 
      df_occupations %>% 
      select(starts_with(
        'item'
      )) %>% 
      as.matrix() %>% 
      fun_acti_generalism()
  ) -> dsdsds

dsdsds %>% 
  arrange(desc(
    generalism
  )) %>% 
  filter(stringr::str_detect(
    tolower(occupation)
    , 'data|statis'
  )) %>%
  print(n = Inf)

df_input[-1] %>% 
  as.matrix() %>% 
  rbind(.,.) %>% 
  fun_acti_generalism()

fun_acti_generalism(matrix(1, 2, 5) * rnorm(10, 0.5, 0.025))
fun_acti_generalism(matrix(1, 2, 5) * rnorm(10, 0.5, 0.25))
fun_acti_generalism(matrix(1, 2, 5) * rnorm(10, 0.5, 1000))

# - Competency test -------------------------------------------------------
df_occupations %>% 
  transmute(
    occupation = occupation
    , competency = 
      df_occupations %>% 
      select(starts_with(
        'item'
      )) %>% 
      as.matrix() %>% 
      fun_acti_competency(
        dbl_scale_ub = 100
        , dbl_scale_lb = 0
      )
    , competency_level =
      fun_acti_classifier(
        dbl_var = competency
        , dbl_scale_lb = 0
        , dbl_scale_ub = 1
        , int_levels = 5
      )
  ) -> dsdsds

dsdsds %>% 
  arrange(desc(
    competency
  )) %>% 
  print(n = Inf)

dsdsds %>% 
  arrange(desc(
    competency
  )) %>% 
  filter(stringr::str_detect(
    tolower(occupation)
    , 'data|statis'
  )) %>%
  print(n = Inf)

dsdsds %>% 
  arrange(desc(
    competency
  )) %>% 
  filter(stringr::str_detect(
    tolower(occupation)
    , 'engineers'
  )) %>%
  print(n = Inf)

# LL
# LM
# MM
# HM
# HH
library(stringr)

df_occupations %>% 
  filter(str_detect(
    str_to_lower(occupation)
    , 'k-12'
  )) %>% 
  select(starts_with(
    'item'
  )) %>% 
  as.numeric() %>% 
  qplot(geom = 'density')

df_input[-1] %>% 
  as.matrix() %>%
  rbind(.,.) %>% 
  fun_acti_competency(
    dbl_scale_ub = 100
    , dbl_scale_lb = 0
  ) %>% 
  fun_acti_classifier(
    int_levels = 5
  )

# - Atlas Career Type Indicator test --------------------------------------
fun_acti_derive_types

# dsds --------------------------------------------------------------------
library(readr)

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023.csv'
) -> df_occupations

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
) -> efa_model

library(atlas.kcoef)
library(atlas.ftools)

atlas.ftools::fun_ftools_factor_scores(
  df_data = df_occupations
  , efa_model = efa_model
  , lgc_pivot = F
) -> df_factor_scores

atlas.kcoef::fun_kcoef_kflex_macro_df(
  df_data = 
    df_occupations %>% 
    select(starts_with(
      'item_'
    ))
  , dbl_weights = 
    df_occupations$
    employment_variants
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_discount = 0.25
  , lgc_sample_variance = F
) -> df_kflex_macro

# df_kflex_macro %>%
#   mutate(
#     relevance = 
#       1 - kflex_macro
#   ) %>% 
#   select(!kflex_macro) -> 
#   df_kflex_macro

df_kflex_macro %>% 
  arrange(desc(
    relevance
  )) %>% 
  print(n = Inf)

rm(df_kflex_macro)

df_occupations %>% 
  select(starts_with(
    'item_'
  ), employment_variants
  ) %>% 
  group_by(
    row_number()
  ) %>% 
  slice(rep(
    1:n()
    , employment_variants
  ))
kselection::kselection(
  max_centers = 100
)
