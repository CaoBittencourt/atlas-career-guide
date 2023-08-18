# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
pkg <- c(
  # 'bvls'
  # , 'fastglm'
  # , 'weights'
  # 'atlas.ftools' #Factor analysis tools
  'dplyr', 'tidyr'#, 'purrr' #Data wrangling
  # , 'kselection' #K-means clustering
  # , 'vctrs' #Data wrangling
  # , 'atlas.skew'
  , 'atlas.skew'
  , 'atlas.ftools'
  # , 'atlas.kcoef'
  , 'atlas.eqvl'
  , 'ggplot2'
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
# library(SWKM)

# [CLASSIFICATION FUNCTIONS] ---------------------------
# - Generalism function ---------------------------------------------------
fun_acti_generalism <- function(mtx_data){
  
  # Arguments validation
  stopifnot(
    "'mtx_data' must be a numeric matrix." =
      all(
        is.numeric(mtx_data)
      )
  )
  
  # Data wrangling
  rbind(mtx_data) -> mtx_data
  
  # Generalism helper function
  fun_acti_generalism_helper <- function(dbl_profile){
    
    # Drop NAs
    dbl_profile[!is.na(
      dbl_profile
    )] -> dbl_profile
    
    # Apply bounded variable skewness function
    fun_skew_sdmode(
      dbl_var =
        dbl_profile
      , dbl_scale_lb =
        min(dbl_profile)
      , dbl_scale_ub =
        max(dbl_profile)
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
    , mtx_weights = NULL
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , dbl_generalism
){
  
  # Arguments validation
  stopifnot(
    "'mtx_data' must be a numeric matrix." =
      all(
        is.numeric(mtx_data)
      )
  )
  
  stopifnot(
    "'mtx_weights' must be a numeric matrix." =
      any(
        is.null(mtx_weights)
        , all(
          is.numeric(mtx_weights)
          , dim(mtx_weights) ==
            dim(mtx_data)
        )
      )
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
    "'dbl_generalism' must be a numeric vector with one element for each row in 'mtx_data'." =
      all(
        is.numeric(dbl_generalism)
        , length(dbl_generalism) ==
          nrow(rbind(mtx_data))
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  rbind(mtx_data) -> mtx_data
  
  # Weigh each attribute by its capital macro-flexibility?
  # Competency level helper function
  fun_acti_competency_helper <- function(
    dbl_profile
    , dbl_generalism
    , dbl_weights
  ){
    
    # Drop NA's
    dbl_profile[!is.na(
      dbl_profile
    )] -> dbl_profile
    
    # Weighted mean of normalized item scores
    # adjusted by item importance and generalism
    weighted.mean(
      x =
        dbl_profile / (
          dbl_scale_ub -
            dbl_scale_lb
        ) -
        dbl_scale_lb / (
          dbl_scale_ub -
            dbl_scale_lb
        )
      , w =
        fun_eqvl_equivalence(
          dbl_var =
            dbl_profile
          , dbl_scale_lb =
            min(dbl_profile)
          , dbl_scale_ub =
            max(dbl_profile)
          , dbl_scaling =
            (1 - dbl_generalism)
        ) +
        fun_eqvl_equivalence(
          dbl_var =
            dbl_weights
          , dbl_scale_lb =
            min(dbl_weights)
          , dbl_scale_ub =
            max(dbl_weights)
          , dbl_scaling =
            (1 - dbl_generalism)
        )
    ) -> dbl_competency
    
    # Output
    return(dbl_competency)
    
  }
  
  # Weights
  if(!length(mtx_weights)){
    
    mtx_data -> mtx_weights
    
  }
  
  # Apply competency level helper function
  mapply(
    function(profile, generalism, weight){
      
      # Call helper function
      fun_acti_competency_helper(
        dbl_profile = profile
        , dbl_generalism = generalism
        , dbl_weights = weight
      ) -> dbl_competency
      
      # Output
      return(dbl_competency)
      
    }
    , profile = as.data.frame(t(mtx_data))
    , generalism = dbl_generalism
    , weight = as.data.frame(t(mtx_weights))
  ) -> mtx_competency
  
  # Output
  return(mtx_competency)
  
}

# - Competency function (ps: this only measures intra-occupation competency! it doesn't account for attribute difficulty) ---------------------------------------------------
fun_acti_competency <- function(
    mtx_data
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , dbl_generalism
){
  
  # Arguments validation
  stopifnot(
    "'mtx_data' must be a numeric matrix." =
      all(
        is.numeric(mtx_data)
      )
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
    "'dbl_generalism' must be a numeric vector with one element for each row in 'mtx_data'." =
      all(
        is.numeric(dbl_generalism)
        , length(dbl_generalism) ==
          nrow(rbind(mtx_data))
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  rbind(mtx_data) -> mtx_data
  
  # Weigh each attribute by its capital macro-flexibility?
  # Competency level helper function
  fun_acti_competency_helper <- function(
    dbl_profile
    , dbl_generalism
  ){
    
    # Drop NA's
    dbl_profile[!is.na(
      dbl_profile
    )] -> dbl_profile
    
    # Weighted mean of normalized item scores
    # adjusted by item importance and generalism
    weighted.mean(
      x =
        dbl_profile / (
          dbl_scale_ub -
            dbl_scale_lb
        ) -
        dbl_scale_lb / (
          dbl_scale_ub -
            dbl_scale_lb
        )
      , w =
        fun_eqvl_equivalence(
          dbl_var =
            dbl_profile
          , dbl_scale_lb =
            min(dbl_profile)
          , dbl_scale_ub =
            max(dbl_profile)
          , dbl_scaling =
            (1 - dbl_generalism)
        )
    ) -> dbl_competency
    
    # Output
    return(dbl_competency)
    
  }
  
  # Apply competency level helper function
  mapply(
    function(profile, generalism){
      
      # Call helper function
      fun_acti_competency_helper(
        dbl_profile = profile
        , dbl_generalism = generalism
      ) -> dbl_competency
      
      # Output
      return(dbl_competency)
      
    }
    , profile = as.data.frame(t(mtx_data))
    , generalism = dbl_generalism
  ) -> mtx_competency
  
  # Output
  return(mtx_competency)
  
}

# # - Competency function ---------------------------------------------------
# fun_acti_competency <- function(
    #     mtx_data
#     , dbl_scale_lb = 0
#     , dbl_scale_ub = 100
# ){
# 
#   # Arguments validation
# 
#   # Data wrangling
# 
#   # Weigh each attribute by its capital macro-flexibility?
#   # Competency level helper function
#   fun_acti_competency_helper <- function(dbl_profile){
# 
#     # Drop NAs
#     dbl_profile[!is.na(
#       dbl_profile
#     )] -> dbl_profile
# 
#     # Normalize item scores
#     dbl_profile / (
#       dbl_scale_ub -
#         dbl_scale_lb
#     ) -> dbl_profile
# 
#     dbl_profile -
#       dbl_scale_lb / (
#         dbl_scale_ub -
#           dbl_scale_lb
#       ) -> dbl_profile
# 
#     # Self-weighted mean of item scores
#     weighted.mean(
#       x = dbl_profile
#       , w = dbl_profile
#       # , w = 1 + dbl_profile
#       # , w =
#       #   dbl_profile /
#       #   max(dbl_profile)
#     ) -> dbl_competency
# 
#     rm(dbl_profile)
# 
#     # Output
#     return(dbl_competency)
# 
#   }
# 
#   # Apply competency level helper function
#   apply(
#     mtx_data, 1
#     , fun_acti_competency_helper
#   ) -> mtx_competency
# 
#   # Output
#   return(mtx_competency)
# 
# }

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

# [K-MEANS ACTI] ----------------------------------------------------------

# - Create Clustering Career Types ---------------------------------------------------
fun_acti_kmeans_types <- function(
    df_data
    , efa_model
    , dbl_weights = NULL
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , int_types = 16  
){
  
  # Arguments validation
  
  # Data wrangling
  
  # Estimate factor scores
  
  # K-means clustering
  
  # Remove irrelevant factors
  
  # Output
  
}

# [NUMERICAL ACTI] --------------------------------------------------------
# - Create Numerical Career types -----------------------------------------------------
fun_acti_derive_types <- function(
    df_data
    , efa_model
    , dbl_weights = NULL
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
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

# read_rds(
#   'C:/Users/Cao/Documents/Github/atlas-research/data/efa_model_equamax_15_factors.rds'
# ) -> efa_model
# 
# read_csv(
#   'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_atlas_complete_equamax_15_factors.csv'
# ) -> df_occupations

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023.csv'
) -> df_occupations

read_csv(
  'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
) -> df_input

# kmeans ------------------------------------------------------------------
df_occupations %>% 
  select(starts_with(
    'item'
  )) %>%
  fun_ftools_factor_scores()

mtx_occupations[rep(
  1:nrow(mtx_occupations)
  , df_occupations$
    employment_norm
), ] -> mtx_occupations

kmeans(
  mtx_occupations
  , centers = 16
) -> list_kmeans

source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_plots.R')

list_kmeans$
  centers %>% 
  as_tibble() %>% 
  mutate(
    .before = 1
    , acti = 
      paste0(
        'ACTI'
        , row_number()
      )
  ) %>%
  select(1, 201:218) %>% 
  pivot_longer(
    cols = -1
    , names_to = 'item'
    , values_to = 'item_score'
  ) %>%
  fun_plot.heatmap(aes(
    x = item
    , y = acti
    , fill = item_score
  )
  , .reorder_fct = F
  , .reorder_desc = F
  )

# dsdsds ------------------------------------------------------------------
df_occupations %>% 
  select(starts_with(
    'item'
  )) -> dsdsds

dsdsds %>% 
  as.matrix() ->
  dsdsds

fun_skew_center_data(
  dbl_var = dsdsds
  , dbl_weights = 
    df_occupations$
    employment_variants
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , lgc_sample_variance = F
) -> lalala

fun_acti_competency(
  mtx_data = 
    dsdsds
  , mtx_weights = 
    lalala
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_generalism = 
    fun_acti_generalism(
      dsdsds
    )
) -> lala

lala %>% 
  as_tibble() %>% 
  mutate(
    wgt = 
      df_occupations$
      employment_variants
  ) %>% 
  ggplot(aes(
    x = value
    , weights = wgt
  )) + 
  geom_density() + 
  xlim(c(0,1))


weighted.mean(100 * lalala[,1] - dsdsds[,1], w = df_occupations$employment_variants)
weighted.mean(100 * lalala[,190] - dsdsds[,190], w = df_occupations$employment_variants)
weighted.mean(100 * lalala[,37] - dsdsds[,37], w = df_occupations$employment_variants)
weighted.mean(100 * lalala[,200] - dsdsds[,200], w = df_occupations$employment_variants)
weighted.mean(100 * lalala[,19] - dsdsds[,19], w = df_occupations$employment_variants)

apply(
  as.data.frame(dsdsds - lalala * 100)
  , 2
  , function(x){weighted.mean(x, df_occupations$employment_variants)}
) -> lala

qplot(as.numeric(lalala))

apply(
  as.data.frame(lalala)
  , 2, function(x){
    
    sqrt(wtd.var(
      x, weights = 
        df_occupations$
        employment_variants
    ))
    
  }
) -> sdsd

View(cbind(sdsd))

lalala %>% 
  as_tibble() %>% 
  ggplot(aes(
    x = 
      item_oral_expression
    # item_fine_arts
    # item_programming
    # item_written_comprehension
    # item_electronic_mail
    , weights =
      df_occupations$
      employment_variants
  )) + 
  geom_density()

df_occupations %>% 
  ggplot(aes(
    x = 
      item_oral_expression
    # item_programming
    # item_written_comprehension
    # item_electronic_mail
    , weights = 
      df_occupations$
      employment_variants
  )) + 
  geom_density()

View(lalala)

wtd.var(lalala, weights = df_occupations$employment_variants)

# dsds --------------------------------------------------------------------
df_input[-1] %>%
  as.matrix() -> 
  dsds

fun_acti_generalism(dsds) -> generalism

generalism

fun_acti_competency(
  mtx_data = dsds
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
  , dbl_generalism =
    generalism
)

weighted.mean(
  dsds
  , w = 
    fun_eqvl_equivalence(
      dsds
      , dbl_scale_ub = 100
      , dbl_scaling = 1 - generalism
    )
)

library(purrr)
sd(c(rep(0,1),1))
sd(c(rep(0,10),1))
sd(c(rep(1,10),0))


c(100,0,0,0) -> dsds

fun_acti_competency(
  dsds
  , 0, 100, fun_acti_generalism(dsds)
)

c(100,100,100,100,100) %>% 
  fun_skew_sdmode(
    dbl_scale_lb = 0
    , dbl_scale_ub = 100
  )

c(100,0,0,0,0) %>% 
  fun_skew_sdmode(
    dbl_scale_lb = 0
    , dbl_scale_ub = 100
  )

c(100,0,0,0,20) %>% 
  fun_skew_sdmode(
    dbl_scale_lb = 0
    , dbl_scale_ub = 100
  )

map(
  list(
    c(100,0,0,0),
    c(100,20,20,20),
    c(100,50,50,40),
    c(100,50,50,50),
    c(100,50,50,60),
    c(100,70,70,70),
    c(100,100,100,100)
  )
  , ~ fun_acti_generalism(.x) #%>% 
  #   fun_acti_competency(
  #     mtx_data = .x
  #   , dbl_scale_lb = 0
  #   , dbl_scale_ub = 100
  #   , dbl_generalism = .
  # )
)


# - Generalism test -------------------------------------------------------
df_occupations %>% 
  transmute(
    occupation = 
      occupation
    , employment_norm =
      employment_norm
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
  print(n = Inf)

dsdsds %>% 
  mutate(
    generalism = 
      fun_acti_classifier(
        generalism
        , dbl_scale_lb = 0
        , dbl_scale_ub = 1
        , int_levels = 2
      )
    , generalism =
      generalism - 1
  ) %>% 
  reframe(
    weighted.mean(
      generalism
      , employment_norm
    )
  )

dsdsds %>% 
  arrange(desc(
    generalism
  )) %>% 
  ggplot(aes(
    x = 
      generalism
    , weights = 
      employment_norm
  )) + 
  geom_density() + 
  geom_vline(
    xintercept = 0.5
  ) +
  xlim(c(0,1))


dsdsds %>% 
  arrange(desc(
    generalism
  )) %>% 
  # mutate(
  #   generalism = 
  #     fun_acti_classifier(
  #       generalism
  #       , dbl_scale_lb = 0
  #       , dbl_scale_ub = 1
  #       , int_levels = 2
  #     ) %>% 
  #     case_match(
  #       1 ~ 'specialist'
  #       , .default = 'generalist'
#     )
# ) %>% 
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
fun_acti_generalism(matrix(1, 2, 5) * rnorm(10, 0, 1000))

# - Competency test -------------------------------------------------------
df_occupations %>% 
  transmute(
    occupation = 
      occupation
    , employment_norm =
      employment_norm
    , competency = 
      df_occupations %>% 
      select(starts_with(
        'item'
      )) %>% 
      as.matrix() %>% 
      fun_acti_competency(
        dbl_scale_lb = 0
        , dbl_scale_ub = 100
        , dbl_generalism = 
          df_occupations %>% 
          select(starts_with(
            'item'
          )) %>% 
          as.matrix() %>% 
          fun_acti_generalism()
      )
    # fun_acti_competency(
    #   dbl_scale_lb = 0
    #   , dbl_scale_ub = 100
    # )
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
  ggplot(aes(
    x = 
      competency
    , weights = 
      employment_norm
  )) + 
  geom_density() + 
  xlim(c(0,1))

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
    dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , dbl_generalism = 
      df_input[-1] %>% 
      as.matrix() %>%
      rbind(.,.) %>% 
      fun_acti_generalism()
  )

# - Generalism vs Competency test -------------------------------------------------------
df_occupations %>% 
  transmute(
    occupation = 
      occupation
    , wage_mean = 
      wage_mean
    , employment_norm =
      employment_norm
    , generalism = 
      df_occupations %>% 
      select(starts_with(
        'item'
      )) %>% 
      as.matrix() %>%
      fun_acti_generalism()
    , competency = 
      df_occupations %>% 
      select(starts_with(
        'item'
      )) %>% 
      as.matrix() %>% 
      fun_acti_competency(
        dbl_scale_lb = 0
        , dbl_scale_ub = 100
        , dbl_generalism = 
          generalism
      )
  ) -> dsdsds

df_occupations %>% 
  filter(
    str_detect(
      str_to_lower(occupation)
      , 'maids'
    )
  ) %>% 
  select(starts_with(
    'item'
  )) %>%
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'item_score'
  ) %>% 
  arrange(desc(
    item_score
  )) %>% 
  print(n = Inf)

dsdsds %>% 
  arrange(desc(
    competency
  )) %>% 
  print(n = Inf)

weights::wtd.cors(
  x = dsdsds$generalism
  , y = dsdsds$competency
  , weight = dsdsds$employment_norm
)

weights::wtd.cors(
  x = dsdsds$generalism
  , y = dsdsds$wage_mean
  , weight = dsdsds$employment_norm
)

weights::wtd.cors(
  x = dsdsds$competency
  , y = dsdsds$wage_mean
  , weight = dsdsds$employment_norm
)

weights::wtd.cors(
  x = dsdsds$employment_norm
  , y = dsdsds$wage_mean
  , weight = dsdsds$employment_norm
)

dsdsds %>% 
  ggplot(aes(
    x = 
      competency
    , weights = 
      employment_norm
  )) + 
  geom_density() + 
  xlim(c(0,1))

dsdsds %>% 
  # group_by(occupation) %>%
  # slice(rep(1:n(), employment_norm)) %>%
  # ungroup() %>%
  ggplot(aes(
    x = 
      generalism
    , y = 
      competency
    , weights = 
      employment_norm
  )) + 
  geom_point() + 
  xlim(c(0,1)) + 
  ylim(c(0,1)) 

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

100 - 
  df_occupations %>% 
  filter(str_detect(
    str_to_lower(occupation)
    , 'k-12'
  )) %>% 
  select(starts_with(
    'item'
  )) %>%
  as.numeric() %>%
  mlv()

df_occupations %>% 
  filter(str_detect(
    str_to_lower(occupation)
    , 'k-12'
  )) %>% 
  select(starts_with(
    'item'
  )) %>%
  as.numeric() %>%
  fun_skew_sdmode(
    dbl_scale_lb = 0
    , dbl_scale_ub = 100
  )

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

tibble(
  occupation = 'Cao'
  , generalism = 
    df_input[-1] %>% 
    as.matrix() %>% 
    fun_acti_generalism()
  , competency = 
    df_input[-1] %>% 
    as.matrix() %>% 
    fun_acti_competency(
      dbl_scale_ub = 100
      , dbl_scale_lb = 0
      , dbl_generalism = 
        generalism
    )
)

# - Atlas Career Type Indicator test --------------------------------------
fun_acti_derive_types

# dsds --------------------------------------------------------------------
df_input[-1] %>%
  as.numeric() %>%
  set_names(
    names(df_input[-1])
  ) -> dsds

weighted.mean(
  x = dsds / 100
  , w = dsds / max(dsds)
)

mlv(dsds/100)
median(dsds/100)
mean(dsds/100)

fun_acti_generalism(
  mtx_data = 
    rbind(dsds)
  , dbl_scale_lb = 0
) -> dsds_generalism

dsds %>% 
  unname() %>%
  kmeans(centers = 2) ->
  kmeans_dsds

weighted.mean(
  x = sort(kmeans_dsds$centers) / 100
  , w = c(dsds_generalism, 1 - dsds_generalism)
)

kmeans_dsds$centers[1,] == mean(dsds[!as.logical(kmeans_dsds$cluster - 1)])
kmeans_dsds$centers[2,] == mean(dsds[as.logical(kmeans_dsds$cluster - 1)])


kmeans_dsds$centers
kmeans_dsds$
  cluster %>% 
  as_tibble(
    rownames = 'item'
  ) %>%
  rename(
    class = value
  ) %>%
  left_join(
    df_input[-1] %>% 
      pivot_longer(
        cols = everything()
        , names_to = 'item'
        , values_to = 'item_score'
      )
  ) %>% 
  arrange(desc(
    item_score
  )) %>% 
  group_by(class) %>% 
  reframe(
    item_score = 
      mean(item_score)
  )
print(n = Inf)
















