# [SETUP] -----------------------------------------------------------------
# - Packages ----------------------------------------------------------------
# CRAN packages
chr_pkg <- c(
  'devtools' #GitHub packages
  , 'ggplot2' #Data visualization
  , 'readr' #Read data (temp)
  , 'viridis' #Palette (temp)
  , 'vctrs' #Data frame subclasses
  , 'tidyr', 'dplyr' #Data wrangling
)

# Git packages
chr_git <- c(
  'CaoBittencourt' = 'atlas.skew',
  'CaoBittencourt' = 'atlas.ftools',
  'CaoBittencourt' = 'atlas.eqvl'
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
# - Generalism function ---------------------------------------------------
fun_acti_generalism <- function(
    dbl_profile
    , dbl_scale_lb = 0
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be numeric." =
      is.numeric(dbl_profile)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  # Drop NAs
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile
  
  # Apply bounded variable skewness function
  fun_skew_sdmode(
    dbl_var =
      dbl_profile
    , dbl_scale_lb =
      dbl_scale_lb
    , dbl_scale_ub =
      max(dbl_profile)
  ) -> dbl_generalism
  
  rm(dbl_profile)
  
  # Output
  return(dbl_generalism)
  
}

# - Indispensability function ---------------------------------------------
fun_acti_indispensability <- function(
    dbl_profile
    , dbl_scale_lb = 0
    , dbl_generalism = NULL
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be a numeric." =
      is.numeric(dbl_profile)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_generalism' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generalism)
        , is.null(dbl_generalism)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  if(is.null(dbl_generalism)){
    
    fun_acti_generalism(
      dbl_profile
      , dbl_scale_lb =
        dbl_scale_lb
    ) -> dbl_generalism
    
  }
  
  dbl_generalism[[1]] -> dbl_generalism
  
  dbl_profile[!is.na(
    dbl_profile
  )] -> dbl_profile
  
  # Equivalence of normalized scores
  fun_eqvl_equivalence(
    dbl_var =
      dbl_profile
    , dbl_scale_lb =
      dbl_scale_lb
    , dbl_scale_ub =
      max(dbl_profile)
    , dbl_scaling =
      1 - dbl_generalism
  ) -> dbl_indispensability
  
  if(is.matrix(dbl_profile)){
    
    colnames(dbl_profile) ->
      names(dbl_indispensability)
    
  } else {
    
    names(dbl_profile) ->
      names(dbl_indispensability)
    
  }
  
  # Output
  return(dbl_indispensability)
  
}

# - Competency function (ps: this only measures intra-occupation competency! it doesn't account for attribute difficulty) ---------------------------------------------------
fun_acti_competency <- function(
    dbl_profile
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
    , dbl_generalism = NULL
){
  
  # Arguments validation
  stopifnot(
    "'dbl_profile' must be numeric." =
      is.numeric(dbl_profile)
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
    "'dbl_generalism' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generalism)
        , is.null(dbl_generalism)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  if(is.null(dbl_generalism)){
    
    fun_acti_generalism(
      dbl_profile
      , dbl_scale_lb =
        dbl_scale_lb
    ) -> dbl_generalism
    
  }
  
  dbl_generalism[[1]] -> dbl_generalism
  
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
      fun_acti_indispensability(
        dbl_profile = 
          dbl_profile
        , dbl_scale_lb = 
          dbl_scale_lb
        , dbl_generalism =
          dbl_generalism
      )
  ) -> dbl_competency
  
  # Output
  return(dbl_competency)
  
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
  
  names(dbl_var) -> 
    names(int_class_id)
  
  # Output
  return(int_class_id)
  
}

# - Numerical ACTI --------------------------------------------------------
fun_acti_type <- function(
    df_data
    , efa_model
    , chr_factor_labels = NULL
    , chr_data_id = NULL
    , dbl_scale_lb = 0
    , dbl_generalism = NULL
){
  
  # Arguments validation
  stopifnot(
    "'df_data' must be a data frame containing item scores." =
      all(
        is.data.frame(df_data)
        , any(
          loadings(efa_model)[,] %>%
            rownames() %in%
            names(df_data)
        )))
  
  stopifnot(
    "'chr_factor_labels' must be either NULL or a character vector with labels for each factor." =
      any(
        is.null(chr_factor_labels)
        , all(
          is.character(chr_factor_labels)
          , length(chr_factor_labels) ==
            efa_model$factors
        )
      )
  )
  
  stopifnot(
    "'chr_data_id' must be either NULL or a character vector with labels for each observation." =
      any(
        is.null(chr_data_id)
        , all(
          is.character(chr_data_id)
          , length(chr_data_id) ==
            nrow(df_data)
        )
      )
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." =
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_generalism' must be either NULL or numeric." =
      any(
        is.numeric(dbl_generalism)
        , is.null(dbl_generalism)
      )
  )
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  df_data %>%
    select(any_of(
      efa_model$
        model %>%
        colnames()
    )) -> df_data
  
  dbl_generalism[[1]] -> dbl_generalism
  
  if(is.null(dbl_generalism)){
    
    apply(
      df_data, 1
      , fun_acti_generalism
      , dbl_scale_lb =
        dbl_scale_lb
    ) -> dbl_generalism
    
  }
  
  # Factor scores
  fun_ftools_factor_scores(
    df_data =
      df_data
    , efa_model =
      efa_model
    , lgc_pivot = F
  ) -> df_factor_scores
  
  rm(df_data)
  
  # Apply indispensability function
  mapply(
    function(profile, generalism){
      
      # Calculate item indispensability
      fun_acti_indispensability(
        dbl_profile = profile
        , dbl_scale_lb = dbl_scale_lb
        , dbl_generalism = generalism
      ) -> dbl_indispensability
      
      # Output
      return(dbl_indispensability)
      
    }
    , profile = as_tibble(t(
      df_factor_scores
    )) 
    , generalism = dbl_generalism
  ) %>% 
    t() %>%
    as_tibble() -> 
    df_factor_scores
  
  # Name factors
  if(is.null(chr_factor_labels)){
    
    paste0('F', 1:ncol(
      df_factor_scores
    )) -> chr_factor_labels
    
  }
  
  names(
    df_factor_scores
  ) <- chr_factor_labels
  
  rm(chr_factor_labels)
  
  # Sort
  apply(
    df_factor_scores, 1
    , sort
    , decreasing = T
    , simplify = F
  ) -> list_factor_scores
  
  rm(df_factor_scores)
  
  # Classify scores
  lapply(
    list_factor_scores
    , function(x){
      
      fun_acti_classifier(
        dbl_var = x
        , dbl_scale_lb = 0
        , dbl_scale_ub = 1
        , int_levels = 3
      )
      
    }
  ) -> list_classification
  
  # Keep only dominant and auxiliary factors (drop minor)
  lapply(
    list_classification
    , function(x){return(x[x > 1])}
  ) -> list_classification
  
  Map(
    function(factor_scores, factor_class){
      
      # Get dominant and auxiliary factors
      factor_scores[
        names(factor_scores) %in%
          names(factor_class)
      ] -> factor_scores
      
      # Output
      return(factor_scores)
      
    }
    , factor_scores = list_factor_scores
    , factor_class = list_classification
  ) -> list_factor_scores
  
  chr_data_id -> names(list_classification)
  chr_data_id -> names(list_factor_scores)
  chr_data_id -> names(dbl_generalism)
  
  rm(chr_data_id)
  
  # ACTI data frame
  dbl_generalism %>%
    as_tibble(
      rownames = 'occupation'
    ) %>%
    rename(
      generalism = 2
    ) %>% 
    full_join(
      bind_rows(
        list_factor_scores
        , .id = 'occupation'
      ) %>% 
        pivot_longer(
          cols = where(is.numeric)
          , names_to = 'factor'
          , values_to = 'acti_score'
        )
    ) %>%
    full_join(
      bind_rows(
        list_classification
        , .id = 'occupation'
      ) %>% 
        pivot_longer(
          cols = where(is.numeric)
          , names_to = 'factor'
          , values_to = 'class'
        )
    ) -> df_acti
  
  rm(list_factor_scores)
  rm(list_classification)
  rm(dbl_generalism)
  
  df_acti %>% 
    mutate(
      class = 
        case_match(
          class
          , 2 ~ 'Aux'
          , 3 ~ 'Dom'
        )
    ) %>%
    drop_na() %>% 
    group_by(
      occupation
    ) %>% 
    arrange(desc(
      acti_score
    ), .by_group = T
    ) %>% 
    mutate(
      rank = row_number()
    ) %>% 
    ungroup() -> 
    df_acti
  
  # Factor and font color
  df_acti %>%
    mutate(
      atom_color = 
        if_else(
          class == 'Aux'
          , class
          , factor
        )
      , font_color = 
        atom_color
    ) -> df_acti
  
  # ACTI type acronym helper function
  fun_acti_type_helper <- function(df_data){
    
    # ACTI type acronym
    df_data %>%
      filter(
        class == 'Dom'
      ) %>%
      pull(factor) %>%
      paste0(
        collapse = '-'
      ) -> chr_dom
    
    df_data %>%
      filter(
        class != 'Dom'
      ) %>%
      pull(factor) %>%
      paste0(
        collapse = '-'
      ) -> chr_aux
    
    if(chr_aux != ''){
      
      paste0(
        '/', chr_aux
      ) -> chr_aux
      
    }
    
    paste0(
      chr_dom
      , chr_aux
    ) -> chr_acti_type
    
    # Output
    return(chr_acti_type)
    
  }
  
  # Calculate ACTI acronyms
  df_acti %>% 
    split(.$occupation) %>% 
    sapply(
      fun_acti_type_helper
    ) %>% 
    as_tibble(
      rownames = 'occupation'
    ) %>%
    rename(
      acti_type = 2
    ) %>% 
    left_join(
      df_acti
    ) -> df_acti
  
  # 'df_acti' subclass
  df_acti %>%
    new_data_frame(
      class = c('df_acti', 'tbl')
    ) -> df_acti
  
  # Output
  return(df_acti)
  
}

# # - keep actual factor scores in df_acti data frame --------------------------------------------------------
# fun_acti_type <- function(
    #     df_data
#     , efa_model
#     , chr_factor_labels = NULL
#     , chr_data_id = NULL
#     , dbl_scale_lb = 0
#     , dbl_generalism = NULL
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'df_data' must be a data frame containing item scores." =
#       all(
#         is.data.frame(df_data)
#         , any(
#           loadings(efa_model)[,] %>%
#             rownames() %in%
#             names(df_data)
#         )))
#   
#   stopifnot(
#     "'chr_factor_labels' must be either NULL or a character vector with labels for each factor." =
#       any(
#         is.null(chr_factor_labels)
#         , all(
#           is.character(chr_factor_labels)
#           , length(chr_factor_labels) ==
#             efa_model$factors
#         )
#       )
#   )
#   
#   stopifnot(
#     "'chr_data_id' must be either NULL or a character vector with labels for each observation." =
#       any(
#         is.null(chr_data_id)
#         , all(
#           is.character(chr_data_id)
#           , length(chr_data_id) ==
#             nrow(df_data)
#         )
#       )
#   )
#   
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." =
#       is.numeric(dbl_scale_lb)
#   )
#   
#   stopifnot(
#     "'dbl_generalism' must be either NULL or numeric." =
#       any(
#         is.numeric(dbl_generalism)
#         , is.null(dbl_generalism)
#       )
#   )
#   
#   # Data wrangling
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   
#   df_data %>%
#     select(any_of(
#       efa_model$
#         model %>%
#         colnames()
#     )) -> df_data
#   
#   dbl_generalism[[1]] -> dbl_generalism
#   
#   if(is.null(dbl_generalism)){
#     
#     apply(
#       df_data, 1
#       , fun_acti_generalism
#       , dbl_scale_lb =
#         dbl_scale_lb
#     ) -> dbl_generalism
#     
#   }
#   
#   # Factor scores
#   fun_ftools_factor_scores(
#     df_data =
#       df_data
#     , efa_model =
#       efa_model
#     , lgc_pivot = F
#   ) -> df_factor_scores
#   
#   rm(df_data)
#   
#   # Apply indispensability function
#   mapply(
#     function(profile, generalism){
#       
#       # Calculate item indispensability
#       fun_acti_indispensability(
#         dbl_profile = profile
#         , dbl_scale_lb = dbl_scale_lb
#         , dbl_generalism = generalism
#       ) -> dbl_indispensability
#       
#       # Output
#       return(dbl_indispensability)
#       
#     }
#     , profile = as_tibble(t(
#       df_factor_scores
#     )) 
#     , generalism = dbl_generalism
#   ) %>% 
#     t() %>%
#     as_tibble() -> 
#     df_factor_scores
#   
#   # Name factors
#   if(is.null(chr_factor_labels)){
#     
#     paste0('F', 1:ncol(
#       df_factor_scores
#     )) -> chr_factor_labels
#     
#   }
#   
#   names(
#     df_factor_scores
#   ) <- chr_factor_labels
#   
#   rm(chr_factor_labels)
#   
#   # Sort
#   apply(
#     df_factor_scores, 1
#     , sort
#     , decreasing = T
#     , simplify = F
#   ) -> list_factor_scores
#   
#   rm(df_factor_scores)
#   
#   # Classify scores
#   lapply(
#     list_factor_scores
#     , function(x){
#       
#       fun_acti_classifier(
#         dbl_var = x
#         , dbl_scale_lb = 0
#         , dbl_scale_ub = 1
#         , int_levels = 3
#       )
#       
#     }
#   ) -> list_classification
#   
#   # Keep only dominant and auxiliary factors (drop minor)
#   lapply(
#     list_classification
#     , function(x){return(x[x > 1])}
#   ) -> list_classification
#   
#   Map(
#     function(factor_scores, factor_class){
#       
#       # Get dominant and auxiliary factors
#       factor_scores[
#         names(factor_scores) %in%
#           names(factor_class)
#       ] -> factor_scores
#       
#       # Output
#       return(factor_scores)
#       
#     }
#     , factor_scores = list_factor_scores
#     , factor_class = list_classification
#   ) -> list_factor_scores
#   
#   chr_data_id -> names(list_classification)
#   chr_data_id -> names(list_factor_scores)
#   chr_data_id -> names(dbl_generalism)
#   
#   rm(chr_data_id)
#   
#   # ACTI data frame
#   dbl_generalism %>%
#     as_tibble(
#       rownames = 'occupation'
#     ) %>%
#     rename(
#       generalism = 2
#     ) %>% 
#     full_join(
#       bind_rows(
#         list_factor_scores
#         , .id = 'occupation'
#       ) %>% 
#         pivot_longer(
#           cols = where(is.numeric)
#           , names_to = 'factor'
#           , values_to = 'acti_score'
#         )
#     ) %>%
#     full_join(
#       bind_rows(
#         list_classification
#         , .id = 'occupation'
#       ) %>% 
#         pivot_longer(
#           cols = where(is.numeric)
#           , names_to = 'factor'
#           , values_to = 'class'
#         )
#     ) -> df_acti
#   
#   rm(list_factor_scores)
#   rm(list_classification)
#   rm(dbl_generalism)
#   
#   df_acti %>% 
#     mutate(
#       class = 
#         case_match(
#           class
#           , 2 ~ 'Aux'
#           , 3 ~ 'Dom'
#         )
#     ) %>%
#     drop_na() %>% 
#     group_by(
#       occupation
#     ) %>% 
#     arrange(desc(
#       acti_score
#     ), .by_group = T
#     ) %>% 
#     mutate(
#       rank = row_number()
#     ) %>% 
#     ungroup() -> 
#     df_acti
#   
#   # Factor and font color
#   df_acti %>%
#     mutate(
#       atom_color = 
#         if_else(
#           class == 'Aux'
#           , class
#           , factor
#         )
#       , font_color = 
#         atom_color
#     ) -> df_acti
#   
#   # ACTI type acronym helper function
#   fun_acti_type_helper <- function(df_data){
#     
#     # ACTI type acronym
#     df_data %>%
#       filter(
#         class == 'Dom'
#       ) %>%
#       pull(factor) %>%
#       paste0(
#         collapse = '-'
#       ) -> chr_dom
#     
#     df_data %>%
#       filter(
#         class != 'Dom'
#       ) %>%
#       pull(factor) %>%
#       paste0(
#         collapse = '-'
#       ) -> chr_aux
#     
#     if(chr_aux != ''){
#       
#       paste0(
#         '/', chr_aux
#       ) -> chr_aux
#       
#     }
#     
#     paste0(
#       chr_dom
#       , chr_aux
#     ) -> chr_acti_type
#     
#     # Output
#     return(chr_acti_type)
#     
#   }
#   
#   # Calculate ACTI acronyms
#   df_acti %>% 
#     split(.$occupation) %>% 
#     sapply(
#       fun_acti_type_helper
#     ) %>% 
#     as_tibble(
#       rownames = 'occupation'
#     ) %>%
#     rename(
#       acti_type = 2
#     ) %>% 
#     left_join(
#       df_acti
#     ) -> df_acti
#   
#   # 'df_acti' subclass
#   df_acti %>%
#     new_data_frame(
#       class = c('df_acti', 'tbl')
#     ) -> df_acti
#   
#   # Output
#   return(df_acti)
#   
# }

# # # - Numerical ACTI 2 ------------------------------------------------------
# # fun_acti_type <- function(
    #     #     df_query
# #     , df_data
# #     , efa_model
# #     , chr_factor_labels = NULL
# #     , dbl_weights
# #     , dbl_scale_lb = 0
# #     , dbl_generalism = NULL
# # ){
# #   
# #   # Arguments validation
# #   stopifnot(
# #     "'df_data' must be a data frame containing item scores." =
# #       all(
# #         is.data.frame(df_data)
# #         , any(
# #           loadings(efa_model)[,] %>%
# #             rownames() %in%
# #             names(df_data)
# #         )))
# #   
# #   stopifnot(
# #     "'dbl_scale_lb' must be numeric." =
# #       is.numeric(dbl_scale_lb)
# #   )
# #   
# #   # stopifnot(
# #   #   "'dbl_scale_ub' must be numeric." =
# #   #     is.numeric(dbl_scale_ub)
# #   # )
# #   
# #   stopifnot(
# #     "'dbl_generalism' must be either NULL or numeric." =
# #       any(
# #         is.numeric(dbl_generalism)
# #         , is.null(dbl_generalism)
# #       )
# #   )
# #   
# #   stopifnot(
# #     "'chr_factor_labels' must be either NULL or a character vector with labels for each factor." =
# #       any(
# #         is.null(chr_factor_labels)
# #         , all(
# #           is.character(chr_factor_labels)
# #           , length(chr_factor_labels) ==
# #             efa_model$factors
# #         )
# #       )
# #   )
# #   
# #   # Data wrangling
# #   dbl_scale_lb[[1]] -> dbl_scale_lb
# #   
# #   # dbl_scale_ub[[1]] -> dbl_scale_ub
# #   
# #   df_data %>%
# #     select(any_of(
# #       efa_model$
# #         model %>%
# #         colnames()
# #     )) -> df_data
# #   
# #   if(is.null(dbl_generalism)){
# #     
# #     fun_acti_generalism(
# #       as.numeric(df_data)
# #       , dbl_scale_lb =
# #         dbl_scale_lb
# #     ) -> dbl_generalism
# #     
# #   }
# #   
# #   dbl_generalism[[1]] -> dbl_generalism
# #   
# #   # Factor scores
# #   fun_ftools_factor_scores(
# #     df_data =
# #       df_data
# #     , efa_model =
# #       efa_model
# #     , lgc_pivot = F
# #   ) -> df_factor_scores
# #   
# #   rm(df_data)
# #   rm(efa_model)
# #   
# #   df_factor_scores %>%
# #     as.matrix() %>%
# #     as.numeric() ->
# #     dbl_factor_scores
# #   
# #   rm(df_factor_scores)
# #   
# #   # Apply indispensability function
# #   fun_acti_indispensability(
# #     dbl_profile =
# #       dbl_factor_scores
# #     , dbl_scale_lb =
# #       dbl_scale_lb
# #     , dbl_generalism =
# #       dbl_generalism
# #   ) -> dbl_factor_scores
# #   
# #   # Normalize
# #   
# #   # Sort
# #   sort(
# #     dbl_factor_scores
# #     , decreasing = T
# #   ) -> dbl_factor_scores
# #   
# #   # Name factors
# #   if(is.null(chr_factor_labels)){
# #     
# #     paste0('F', 1:length(
# #       dbl_factor_scores
# #     )) -> chr_factor_labels
# #     
# #   }
# #   
# #   names(
# #     dbl_factor_scores
# #   ) <- chr_factor_labels
# #   
# #   rm(chr_factor_labels)
# #   
# #   # Classify scores
# #   fun_acti_classifier(
# #     dbl_var = dbl_factor_scores
# #     , dbl_scale_lb = 0
# #     , dbl_scale_ub = 1
# #     , int_levels = 3
# #   ) -> dbl_classification
# #   
# #   # Keep only dominant and auxiliary factors (drop minor)
# #   dbl_factor_scores[
# #     dbl_classification > 1
# #   ] -> dbl_factor_scores
# #   
# #   rm(dbl_classification)
# #   
# #   # Output
# #   return(dbl_factor_scores)
# #   
# # }
# 
# # [K-MEANS ACTI] ----------------------------------------------------------
# 
# # - Create Clustering Career Types ---------------------------------------------------
# fun_acti_kmeans_types <- function(
    #     df_data
#     , efa_model
#     , dbl_weights = NULL
#     , dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#     , int_types = 16  
# ){
#   
#   # Arguments validation
#   
#   # Data wrangling
#   
#   # Estimate factor scores
#   
#   # K-means clustering
#   
#   # Remove irrelevant factors
#   
#   # Output
#   
# }
# 
# # [NUMERICAL ACTI] --------------------------------------------------------
# # - Create Numerical Career types -----------------------------------------------------
# fun_acti_derive_types <- function(
    #     df_data
#     , efa_model
#     , dbl_weights = NULL
#     , dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#     # , int_types = 16
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'df_data' must be a data frame with numeric columns." =
#       all(
#         is.data.frame(df_data),
#         df_data %>%
#           map_lgl(is.numeric) %>%
#           any()
#       )
#   )
#   
#   stopifnot(
#     "'dbl_weights' must be either NULL or a numeric vector the same length as the number of rows in 'df_data'." =
#       any(
#         is.null(dbl_weights),
#         all(
#           is.numeric(dbl_weights),
#           length(dbl_weights) ==
#             nrow(df_data)
#         )
#       )
#   )
#   
#   # Data wrangling
#   
#   # Determine generalist vs specialist scope descriptor
#   
#   
#   # Determine high-mid-low level competency descriptor
#   
#   # Determine career type
#   # Calculate factor scores
#   atlas.ftools::fun_ftools_factor_scores(
#     df_data = df_data
#     , efa_model = efa_model
#     , lgc_pivot = T
#   ) -> df_factor_scores
#   
#   rm(df_data)
#   
#   # Calculate capital flexibility for each factor
#   atlas.kcoef::fun_kcoef_kflex_macro_df(
#     df_data = df_factor_scores
#     , dbl_weights = dbl_weights
#     , dbl_scale_lb = dbl_scale_lb
#     , dbl_scale_ub = dbl_scale_ub
#     , dbl_discount = dbl_discount
#     , lgc_sample_variance = F
#   ) -> df_kflex_macro
#   
#   # Define relevance as 1 - kflex_macro
#   df_kflex_macro %>% 
#     mutate(
#       relevance = 
#         1 - kflex_macro
#     ) %>% 
#     select(!kflex_macro) -> 
#     df_kflex_macro
#   
#   # Join data frames
#   df_factor_scores %>% 
#     left_join(
#       df_kflex_macro
#     ) -> df_factor_scores
#   
#   rm(df_kflex_macro)
#   
#   # Truncate factor scores
#   df_factor_scores
#   
#   # Remove useless factors
#   
#   # Classify type
#   
#   
#   
#   # Output
#   
# }
# 
# # - ACTI code -------------------------------------------------------------
# fun_acti_code <- function(
    #     dbl_factor_scores
#     # , dsds
#     , dbl_generalism
#     , dbl_competency
#     , int_generalism_levels = 2
#     , int_competency_levels = 5
# ){
#   
#   # Arguments validation
#   
#   # Data wrangling
#   
#   # Apply generalism classifier
#   fun_acti_classifier(
#     dbl_var = dbl_generalism
#     , dbl_scale_lb = 0
#     , dbl_scale_ub = 1
#     , int_levels = 
#       int_generalism_levels
#   ) -> int_generalism_id
#   
#   # Apply competency classifier
#   fun_acti_classifier(
#     dbl_var = dbl_competency
#     , dbl_scale_lb = 0
#     , dbl_scale_ub = 1
#     , int_levels = 
#       int_competency_levels
#   ) -> int_competency_id
#   
#   # Compose ACTI code
#   paste0() ->
#     chr_acti_code
#   
#   # Output
#   return(list(
#     'acti_code' = chr_acti_code,
#     'generalism_id' = int_generalism_id,
#     'competency_id' = int_competency_id
#   ))
#   
# }
# 
# # - ACTI estimator helper function --------------------------------------------------------
# 
# # - ACTI estimator --------------------------------------------------------
# 
# # - ACTI matching ---------------------------------------------------------
# # fun_eqvl_equivalence_acti
# 
# [PLOTTING FUNCTIONS] -----------------------------------------------------
# - Polygon helper function -----------------------------------------------
fun_acti_plot_polygon <- function(int_sides){
  
  # Arguments validation
  stopifnot(
    "'int_sides' must be numeric." =
      is.numeric(int_sides)
  )
  
  # Data wrangling
  ceiling(int_sides) -> int_sides
  
  # Calculate coordinates
  (2 * pi * 1:int_sides) /
    int_sides -> int_sq
  
  rm(int_sides)
  
  cbind(
    sin(int_sq),
    cos(int_sq)
  ) -> df_polygon
  
  rm(int_sq)
  
  # Data wrangling
  as_tibble(
    df_polygon
  ) -> df_polygon
  
  names(
    df_polygon
  ) <- c('x', 'y')
  
  new_data_frame(
    df_polygon
    , class = c(
      'tbl', 'df_polygon'
    )
  ) -> df_polygon
  
  # Output
  return(df_polygon)
  
}

# - Rotation matrix helper function ---------------------------------------
fun_acti_plot_rotate <- function(df_polygon, dbl_theta){
  
  # Arguments validation
  stopifnot(
    "'df_polygon' must be a data frame of the 'df_polygon' class." =
      any(class(df_polygon) == 'df_polygon')
  )
  
  stopifnot(
    "'dbl_theta' must be numeric." =
      is.numeric(dbl_theta)
  )
  
  # Data wrangling
  dbl_theta[[1]] -> dbl_theta
  
  # Rotation matrix
  rbind(
    c(cos(dbl_theta), -sin(dbl_theta)),
    c(sin(dbl_theta), cos(dbl_theta))
  ) -> mtx_rotation
  
  rm(dbl_theta)
  
  # Rotate polygon
  as.matrix(
    df_polygon[c(
      'x', 'y'
    )]
  ) %*%
    mtx_rotation ->
    df_polygon[c(
      'x', 'y'
    )]
  
  rm(mtx_rotation)
  
  # Output
  return(df_polygon)
  
}

# # - Displacement matrix helper function -----------------------------------
# fun_acti_plot_displacement <- function(int_nrow = 1, ...){
#
#   # Dynamic dots
#   c(...) -> dbl_args
#
#   # Arguments validation
#   stopifnot(
#     "'...' must be numeric." =
#       all(
#         is.numeric(dbl_args),
#         !is.matrix(dbl_args)
#       )
#   )
#
#   stopifnot(
#     "'int_nrow' must be numeric." =
#       is.numeric(int_nrow)
#   )
#
#   # Data wrangling
#   int_nrow[[1]] -> int_nrow
#   ceiling(int_nrow) -> int_nrow
#
#   # Displacement matrix
#   rbind(dbl_args)[rep(
#     1, int_nrow
#   ), ] -> mtx_displacement
#
#   rm(dbl_args)
#   rm(int_nrow)
#
#   unname(mtx_displacement) ->
#     mtx_displacement
#
#   # Output
#   return(mtx_displacement)
#
# }

# # - ACTI specialist plotting function ------------------------------
# # Specialist plotting function
# fun_acti_plot_specialist <- function(df_acti){
#   
#   # Arguments validation
#   stopifnot(
#     "'df_acti' must be a data frame of the 'df_acti' class." =
#       any(class(df_acti) == 'df_acti')
#   )
#   
#   # Specialist molecule helper functions
#   if(nrow(df_acti) == 1){
#     
#     fun_acti_plot_polygon(1) %>%
#       mutate(y = y - 0.75) ->
#       df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y
#       )) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-2, 2)) +
#       ylim(c(-2, 2)) ->
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 2){
#     
#     fun_acti_plot_polygon(2) ->
#       df_polygon
#     
#     df_polygon$group <- 1
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-2, 2)) +
#       ylim(c(-3, 2.5)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 3){
#     
#     fun_acti_plot_polygon(3) ->
#       df_polygon
#     
#     df_polygon$group <- 1
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-2, 2)) +
#       ylim(c(-2, 2)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 4){
#     
#     fun_acti_plot_polygon(4) ->
#       df_polygon
#     
#     df_polygon[c(1:4, 4), ] ->
#       df_polygon
#     
#     c(1, 2, 1, 1, 2) ->
#       df_polygon$
#       group
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-2, 2)) +
#       ylim(c(-2, 2)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 5){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(4),
#       
#       tibble(x = 0, y = 0)
#       
#     ) -> df_polygon
#     
#     c(1, 2, 1, 2, 1) ->
#       df_polygon$
#       group
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-2, 2)) +
#       ylim(c(-1.5, 1.5)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 6){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = -pi/2
#         ) %>%
#         mutate(
#           x = x + 2,
#           group = c(
#             1, 2, 1
#           )
#         ),
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ) %>%
#         mutate(
#           x = x - 2,
#           group = c(
#             2, 1, 1
#           )
#         )
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         slice(3, 6) %>%
#         mutate(group = 2)
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-3, 3)) +
#       ylim(c(-1.25, 1.25)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 7){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = -pi/2
#         ) %>%
#         mutate(
#           x = x + 3,
#           group = c(
#             1, 2, 1
#           )
#         ),
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ) %>%
#         mutate(
#           x = x - 3,
#           group = c(
#             2, 1, 1
#           )
#         ),
#       
#       tibble(x = 0, y = 0)
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         slice(3, 6) %>%
#         mutate(group = 2)
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-4, 4)) +
#       ylim(c(-1.25, 1.25)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 8){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(4) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = -pi/2
#         ) %>%
#         mutate(
#           x = x + 1.75,
#           group = c(
#             2, 1, 3, 1
#           )
#         ),
#       
#       fun_acti_plot_polygon(4) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ) %>%
#         mutate(
#           x = x - 1.75,
#           group = c(
#             3, 1, 2, 1
#           )
#         )
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         slice(4, 8) %>%
#         mutate(group = 2),
#       
#       df_polygon %>%
#         slice(4, 8) %>%
#         mutate(group = 3)
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-3, 3)) +
#       ylim(c(-1.5, 1.5)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 9){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = -pi/2
#         ) %>%
#         mutate(
#           x = x + 3,
#           group = c(
#             2, 1, 2
#           )
#         ),
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ) %>%
#         mutate(
#           x = x - 3,
#           group = c(
#             1, 2, 2
#           )
#         ),
#       
#       fun_acti_plot_polygon(2) %>%
#         mutate(group = 3),
#       
#       tibble(x = 0, y = 0) %>%
#         mutate(group = 2)
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         slice(3, 6, 9) %>%
#         mutate(group = 1),
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-4, 4)) +
#       ylim(c(-1.25, 1.25)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 10){
#     
#     bind_rows(
#       
#       bind_rows(
#         
#         fun_acti_plot_polygon(4) %>%
#           fun_acti_plot_rotate(
#             # dbl_theta = -pi/2
#             dbl_theta = -pi/4
#             # dbl_theta = -pi/3
#             # dbl_theta = -pi/6
#             # dbl_theta = -pi/9
#           ),
#         
#         tibble(x = 0, y = 0)
#         
#       ) %>%
#         mutate(
#           x = x + 1.5,
#           group = c(
#             1, 2, 1, 2, 2
#           )
#         ),
#       
#       bind_rows(
#         
#         fun_acti_plot_polygon(4) %>%
#           fun_acti_plot_rotate(
#             # dbl_theta = pi/2
#             dbl_theta = pi/4
#             # dbl_theta = pi/3
#             # dbl_theta = pi/6
#             # dbl_theta = pi/9
#           ),
#         
#         tibble(x = 0, y = 0)
#         
#       ) %>%
#         mutate(
#           x = x - 1.5,
#           group = c(
#             3, 2, 3, 2, 2
#           )
#         )
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-3, 3)) +
#       ylim(c(-1.5, 1.5)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 11){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(2) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = -pi/2
#         ) %>%
#         mutate(
#           x = x + 3,
#           group = c(
#             4, 4
#           )
#         ),
#       
#       fun_acti_plot_polygon(2) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ) %>%
#         mutate(
#           x = x - 3,
#           group = c(
#             4, 4
#           )
#         ),
#       
#       fun_acti_plot_polygon(2) %>%
#         mutate(group = 1),
#       
#       fun_acti_plot_polygon(2) %>%
#         mutate(
#           x = x - 3,
#           group = c(
#             2, 3
#           )
#         ),
#       
#       fun_acti_plot_polygon(2) %>%
#         mutate(
#           x = x + 3,
#           group = c(
#             2, 3
#           )
#         ),
#       
#       tibble(x = 0, y = 0) %>%
#         mutate(group = 1)
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         slice(2, 4) %>%
#         mutate(group = 2),
#       
#       df_polygon %>%
#         slice(2, 4) %>%
#         mutate(group = 3)
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-4, 4)) +
#       ylim(c(-1.25, 1.25)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 12){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(3) %>%
#         
#         fun_acti_plot_rotate(
#           dbl_theta = -pi/2
#         ) %>%
#         mutate(
#           x = x + 2
#         ),
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ) %>%
#         mutate(
#           x = x - 2
#         )
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         )
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       slice(
#         5, 6, 4, 6,
#         
#         12, 11, 12, 10, 12,
#         
#         3, 1, 3, 2, 3,
#         
#         9, 7, 9, 8, 9, 6
#         
#       ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y
#       )) +
#       geom_path(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-3, 3)) +
#       ylim(c(-3, 3)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 13){
#     
#     bind_rows(
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = -pi/2
#         ) %>%
#         mutate(
#           x = x + 3
#         ),
#       
#       fun_acti_plot_polygon(3) %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ) %>%
#         mutate(
#           x = x - 3
#         )
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         fun_acti_plot_rotate(
#           dbl_theta = pi/2
#         ),
#       
#       tibble(x = 0, y = 0)
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       slice(
#         5, 6, 4, 6,
#         
#         3, 1, 3, 2, 3,
#         
#         13, 12, 10, 12, 11, 12,
#         
#         9, 7, 9, 8
#         
#       ) %>% 
#       ggplot(aes(
#         x = x,
#         y = y
#       )) +
#       geom_path(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-4, 4)) +
#       ylim(c(-4, 4)) -> 
#       plt_polygon
#     
#   } else if(nrow(df_acti) == 14){
#     
#     bind_rows(
#       
#       bind_rows(
#         
#         fun_acti_plot_polygon(4) %>%
#           fun_acti_plot_rotate(
#             dbl_theta = -pi/2
#           ),
#         
#         tibble(x = 0, y = 0)
#         
#       ) %>%
#         mutate(
#           x = x + c(
#             2, 1.5, 2, 1.5, 1.5
#           ),
#           group = c(
#             2, 1, 3, 1, 1
#           )
#         ),
#       
#       bind_rows(
#         
#         fun_acti_plot_polygon(4) %>%
#           fun_acti_plot_rotate(
#             dbl_theta = pi/2
#           ),
#         
#         tibble(x = 0, y = 0)
#         
#       ) %>%
#         mutate(
#           x = x - c(
#             2, 1.5, 2, 1.5, 1.5
#           ),
#           group = c(
#             3, 1, 2, 1, 1
#           )
#         ),
#       
#       fun_acti_plot_polygon(2) %>%
#         mutate(
#           x = x - 0.5,
#           group = 4
#         ),
#       
#       fun_acti_plot_polygon(2) %>%
#         mutate(
#           x = x + 0.5,
#           group = 5
#         ),
#       
#     ) -> df_polygon
#     
#     bind_rows(
#       
#       df_polygon,
#       
#       df_polygon %>%
#         slice(5, 10) %>%
#         mutate(group = 2),
#       
#       df_polygon %>%
#         slice(5, 10) %>%
#         mutate(group = 3),
#       
#     ) -> df_polygon
#     
#     df_polygon %>%
#       ggplot(aes(
#         x = x,
#         y = y,
#         group = group
#       )) +
#       geom_line(
#         color = '#212121',
#         linewidth = 1.25
#       ) +
#       geom_point(
#         color = 'red'
#         , size = 20
#       ) +
#       xlim(c(-2.5, 2.5)) +
#       ylim(c(-1.5, 1.5)) -> 
#       plt_polygon
#     
#   } else {
#     
#     NULL -> plt_polygon
#     
#   }
#   
#   df_acti %>%
#     ggplot(aes(
#       x = x,
#       y = y,
#       color = color,
#       group = group
#     )) +
#     geom_line(
#       size = 2,
#       color = '#212121'
#     ) +
#     geom_point(
#       size = 10
#     ) +
#     xlim(c(1, 14)) +
#     ylim(c(1, 14)) +
#     guides(
#       size = 'none',
#       color = 'none'
#     ) +
#     scale_color_manual(
#       values = c(
#         viridis(nrow(df_acti)) %>%
#           set_names(
#             df_acti$factor
#           )
#         , 'aux' = 'lightgrey'
#       )
#     ) +
#     theme_void() ->
#     plt_acti_molecule
#   
#   rm(df_acti)
#   
#   # Output
#   return(plt_acti_molecule)
#   
# }

# - ACTI specialist plotting function ------------------------------
# Specialist plotting function
fun_acti_plot_specialist <- function(df_acti){
  
  # Arguments validation
  stopifnot(
    "'df_acti' must be a data frame of the 'df_acti' class." =
      any(class(df_acti) == 'df_acti')
  )
  
  # Specialist molecule helper functions
  if(nrow(df_acti) == 1){
    
    # Polygon
    fun_acti_plot_polygon(1) %>%
      mutate(
        y = y - 0.75,
        rank = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    NULL -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 2){
    
    # Polygon
    fun_acti_plot_polygon(2) %>%
      mutate(
        rank = c(2, 1),
        group = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-4, 2.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 3){
    
    # Polygon
    fun_acti_plot_polygon(3) %>%
      mutate(
        rank = c(2, 3, 1),
        group = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 4){
    
    # Polygon
    fun_acti_plot_polygon(4) ->
      df_polygon
    
    df_polygon %>% 
      slice(1:4, 4) %>%
      mutate(
        rank = c(2, 4, 3, 1, 1),
        group = c(1, 2, 1, 1, 2)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 5){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(4),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>%
      mutate(
        rank = c(2, 3, 5, 4, 1),
        group = c(1, 2, 1, 2, 1)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 6){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 2,
          group = c(
            1, 2, 1
          )
        ),
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 2,
          group = c(
            2, 1, 1
          )
        )
      
    ) -> df_polygon
    
    df_polygon %>%
      mutate(
        rank = c(
          6, 3, 2,
          5, 4, 1
        )
      ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        slice(3, 6) %>%
        mutate(group = 2)
      
    ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-3, 3)) -> scale_xlim
    ylim(c(-1.25, 1.25)) -> scale_ylim
    
  } else if(nrow(df_acti) == 7){
    
    #Polygon
    bind_rows(
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 3,
          group = c(
            1, 2, 1
          )
        ),
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 3,
          group = c(
            2, 1, 1
          )
        ),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>%
      mutate(
        rank = c(
          6, 7, 2,
          5, 4, 3,
          1
        )
      ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        slice(3, 6) %>%
        mutate(group = 2)
      
    ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-4, 4)) -> scale_xlim
    ylim(c(-1.25, 1.25)) -> scale_ylim
    
  } else if(nrow(df_acti) == 8){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(4) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 1.75,
          group = c(
            2, 1, 3, 1
          )
        ),
      
      fun_acti_plot_polygon(4) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 1.75,
          group = c(
            3, 1, 2, 1
          )
        )
      
    ) -> df_polygon
    
    df_polygon %>%
      mutate(
        rank = c(
          4, 8, 5, 1,
          6, 7, 3, 2
        )
      ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        slice(4, 8) %>%
        mutate(group = 2),
      
      df_polygon %>%
        slice(4, 8) %>%
        mutate(group = 3)
      
    ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-3, 3)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 9){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 3,
          group = c(
            2, 1, 2
          )
        ),
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 3,
          group = c(
            1, 2, 2
          )
        ),
      
      fun_acti_plot_polygon(2) %>%
        mutate(group = 3),
      
      tibble(x = 0, y = 0) %>%
        mutate(group = 2)
      
    ) -> df_polygon
    
    df_polygon %>%
      mutate(
        rank = c(
          6, 7, 2, 4,
          5, 3, 8, 9,
          1
        )
      ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        slice(3, 6, 9) %>%
        mutate(group = 1),
      
    ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-4, 4)) -> scale_xlim
    ylim(c(-1.25, 1.25)) -> scale_ylim
    
  } else if(nrow(df_acti) == 10){
    
    # Polygon
    bind_rows(
      
      bind_rows(
        
        fun_acti_plot_polygon(4) %>%
          fun_acti_plot_rotate(
            # dbl_theta = -pi/2
            dbl_theta = -pi/4
            # dbl_theta = -pi/3
            # dbl_theta = -pi/6
            # dbl_theta = -pi/9
          ),
        
        tibble(x = 0, y = 0)
        
      ) %>%
        mutate(
          x = x + 1.5,
          group = c(
            1, 2, 1, 2, 2
          )
        ),
      
      bind_rows(
        
        fun_acti_plot_polygon(4) %>%
          fun_acti_plot_rotate(
            # dbl_theta = pi/2
            dbl_theta = pi/4
            # dbl_theta = pi/3
            # dbl_theta = pi/6
            # dbl_theta = pi/9
          ),
        
        tibble(x = 0, y = 0)
        
      ) %>%
        mutate(
          x = x - 1.5,
          group = c(
            3, 2, 3, 2, 2
          )
        )
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        rank = c(
          8, 10, 6, 4, 1, 
          5, 9, 7, 3, 2 
        )
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-3, 3)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 11){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(2) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 3,
          group = c(
            4, 4
          )
        ),
      
      fun_acti_plot_polygon(2) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 3,
          group = c(
            4, 4
          )
        ),
      
      fun_acti_plot_polygon(2) %>%
        mutate(group = 1),
      
      fun_acti_plot_polygon(2) %>%
        mutate(
          x = x - 3,
          group = c(
            2, 3
          )
        ),
      
      fun_acti_plot_polygon(2) %>%
        mutate(
          x = x + 3,
          group = c(
            2, 3
          )
        ),
      
      tibble(x = 0, y = 0) %>%
        mutate(group = 1)
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        rank = c(
          11, 2, 10, 3, 5,
          4, 7, 6, 8, 9,
          1
        )
      ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        slice(2, 4) %>%
        mutate(group = 2),
      
      df_polygon %>%
        slice(2, 4) %>%
        mutate(group = 3)
      
    ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-4, 4)) -> scale_xlim
    ylim(c(-1.25, 1.25)) -> scale_ylim
    
  } else if(nrow(df_acti) == 12){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(3) %>%
        
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 2
        ),
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 2
        )
      
    ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        )
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        rank = c(
          9, 10, 2, 7, 8, 3,
          6, 5, 4, 11, 12, 1
        )
      ) -> df_polygon
    
    df_polygon %>%
      slice(
        5, 6, 4, 6,
        
        12, 11, 12, 10, 12,
        
        3, 1, 3, 2, 3,
        
        9, 7, 9, 8, 9, 6
        
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-3, 3)) -> scale_xlim
    ylim(c(-3, 3)) -> scale_ylim
    
  } else if(nrow(df_acti) == 13){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 3
        ),
      
      fun_acti_plot_polygon(3) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 3
        )
      
    ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        rank = c(
          6, 7, 2, 8, 9, 3,
          11, 10, 5, 12, 13, 4,
          1
        )
      ) -> df_polygon
    
    df_polygon %>%
      slice(
        5, 6, 4, 6,
        
        3, 1, 3, 2, 3,
        
        13, 12, 10, 12, 11, 12,
        
        9, 7, 9, 8
        
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-4, 4)) -> scale_xlim
    ylim(c(-4, 4)) -> scale_ylim
    
  } else if(nrow(df_acti) == 14){
    
    # Polygon
    bind_rows(
      
      bind_rows(
        
        fun_acti_plot_polygon(4) %>%
          fun_acti_plot_rotate(
            dbl_theta = -pi/2
          ),
        
        tibble(x = 0, y = 0)
        
      ) %>%
        mutate(
          x = x + c(
            2, 1.5, 2, 1.5, 1.5
          ),
          group = c(
            2, 1, 3, 1, 1
          )
        ),
      
      bind_rows(
        
        fun_acti_plot_polygon(4) %>%
          fun_acti_plot_rotate(
            dbl_theta = pi/2
          ),
        
        tibble(x = 0, y = 0)
        
      ) %>%
        mutate(
          x = x - c(
            2, 1.5, 2, 1.5, 1.5
          ),
          group = c(
            3, 1, 2, 1, 1
          )
        ),
      
      fun_acti_plot_polygon(2) %>%
        mutate(
          x = x - 0.5,
          group = 4
        ),
      
      fun_acti_plot_polygon(2) %>%
        mutate(
          x = x + 0.5,
          group = 5
        ),
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        rank= c(
          7, 10, 8, 1, 4, 6, 9,
          5, 2, 3, 11, 12, 13, 14
        )
      ) -> df_polygon
    
    bind_rows(
      
      df_polygon,
      
      df_polygon %>%
        slice(5, 10) %>%
        mutate(group = 2),
      
      df_polygon %>%
        slice(5, 10) %>%
        mutate(group = 3),
      
    ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2.5, 2.5)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else {
    
    # Warning
    warning("Invalid ACTI type.")
    
    # Output
    return(NULL)
    
  }
  
  # Plot ACTI molecule
  df_polygon %>%
    mutate(
      label = row_number()
    ) %>%
    left_join(df_acti) %>%
    ggplot(aes_map) +
    geom_connection +
    geom_point(aes(
      size = acti_score,
      color = atom_color
    )) +
    geom_text(aes(
      label = factor
    ), size = 5) +
    scale_xlim +
    scale_ylim +
    scale_size_continuous(
      range = c(20, 30)
    ) +
    # scale_color_manual() + 
    guides(
      size = 'none',
      color = 'none'
    ) +
    theme_void() ->
    plt_acti_molecule
  
  # rm(df_acti)
  # rm(df_polygon)
  
  # Output
  return(plt_acti_molecule)
  
}

# - ACTI generalist plotting function ------------------------------
# Generalist plotting function
fun_acti_plot_generalist <- function(df_acti){
  
  # Arguments validation
  stopifnot(
    "'df_acti' must be a data frame of the 'df_acti' class." =
      any(class(df_acti) == 'df_acti')
  )
  
  # Specialist molecule helper functions
  if(nrow(df_acti) == 1){
    
    # Polygon
    fun_acti_plot_polygon(1) %>%
      mutate(
        y = y - 0.75,
        rank = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    NULL -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 2){
    
    # Polygon
    fun_acti_plot_polygon(2) %>%
      fun_acti_plot_rotate(
        dbl_theta = pi/2
      ) %>% 
      mutate(
        rank = c(2, 1)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-4, 4)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 3){
    
    # Polygon
    fun_acti_plot_polygon(3) %>% 
      mutate(
        rank = c(2, 3, 1)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 4){
    
    # Polygon
    fun_acti_plot_polygon(4) %>%
      mutate(
        rank = c()
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 5){
    
    # Polygon
    fun_acti_plot_polygon(5) -> 
      df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2.25, 2.25)) -> scale_xlim
    ylim(c(-1.25, 1.25)) -> scale_ylim
    
  } else if(nrow(df_acti) == 6){
    
    # Polygon
    fun_acti_plot_polygon(6) %>% 
      fun_acti_plot_rotate(
        dbl_theta = pi/2
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-1.5, 1.5)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 7){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        3, 4, 5, 6, 1, 2, 3,
        
        7, 6,
        
        5, 7, 4, 7,
        
        1, 7, 2
        
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-1.5, 1.5)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 8){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          y = round(y),
          x = x * 1.5
        ),
      
      fun_acti_plot_polygon(2) %>% 
        mutate(y = round(y))
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        3, 
        4, 8, 5,
        6,
        1, 7, 2
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 9){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(2) %>% 
        fun_acti_plot_rotate(
          dbl_theta = -3*pi/2
        ) %>% 
        mutate(x = x + 3.5),
      
      fun_acti_plot_polygon(2) %>% 
        fun_acti_plot_rotate(
          dbl_theta = 3*pi/2
        ) %>% 
        mutate(x = x - 3.5),
      
      fun_acti_plot_polygon(2) %>%
        mutate(x = x - 1.25),
      
      fun_acti_plot_polygon(2) %>%
        mutate(x = x + 1.25),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        4, 6, 8, 2, 7, 5, 4,
        
        3, 6, 9, 8, 1,
        7, 9, 5, 3,
        
        9, 1, 2
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-5, 5)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 10){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(5) %>% 
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>% 
        mutate(
          x = x - 1.5,
          y = y * 3
        )
      ,
      
      fun_acti_plot_polygon(5) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          x = x + 1.5,
          y = y * 3
        )
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        5, 1, 2, 8, 9, 10,
        6, 7, 3, 4, 5,
        
        2, 3, 7, 8, 10,
        7, 3, 5,
        
        1, 9, 10, 6, 4
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-3, 3)) -> scale_xlim
    ylim(c(-4, 4)) -> scale_ylim
    
  } else if(nrow(df_acti) == 11){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(4) %>% 
        mutate(
          x = x * 2,
          y = y * 3
        ),
      
      fun_acti_plot_polygon(4) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/4
        ) %>% 
        mutate(
          x = x * 2,
          y = y * 3
        ),
      
      tibble(x = 0, y = 0),
      
      tibble(x = .85, y = 0),
      
      tibble(x = -.85, y = 0),
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        3, 7, 4, 8, 1, 5, 2, 6, 3,
        
        7, 11, 6, 2, 9, 4, 8, 10, 5, 1,
        
        10, 9, 11, 3
        
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-3, 3)) -> scale_ylim
    
  } else if(nrow(df_acti) == 12){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          y = round(y),
          color = 1
        ),
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          y = round(y),
          color = 2
          , y = y + 2
        ) %>% 
        filter(
          y != 1
        ),
      
      tibble(x = -1.5, y = 1),
      
      tibble(x = 1.5, y = 1)
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        11, 7, 8, 9, 10, 12, 6, 1, 2, 3,
        11, 7, 4, 3, 4, 5, 10, 5, 6, 5
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-1.5, 1.5)) -> scale_xlim
    ylim(c(-1.5, 3.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 13){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          y = round(y),
          x = x - 1
        ),
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          y = round(y), 
          x = x + 1
        ) %>% 
        filter(
          !(x == 0 & y == 0)
        ),
      
      tibble(x = -1, y = 0),
      
      tibble(x = 1, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        
        3, 4, 12, 5, 6, 9, 13, 10, 11,
        
        7, 13, 8, 6, 1, 12, 2, 3,
        
        4, 10, 11, 7, 2, 3, 11
        
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else if(nrow(df_acti) == 14){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          y = round(y),
          x = x - 1.5
        ),
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          y = round(y), 
          x = x + 1.5
        ) %>% 
        filter(
          !(x == 0 & y == 0)
        ),
      
      tibble(x = -1.5, y = 0),
      
      tibble(x = 1.5, y = 0),
      
    ) -> df_polygon
    
    df_polygon %>%
      slice(
        
        3, 4, 5, 10, 11, 12,
        7, 8, 1, 2, 3,
        
        4, 13, 5, 6, 1, 13, 2,
        
        3, 12,
        
        11, 14, 10, 9, 8,
        14, 7
        
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2.5, 2.5)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } else {
    
    # Warning
    warning("Invalid ACTI type.")
    
    # Output
    return(NULL)
    
  }
  
  # Plot ACTI molecule
  df_polygon %>%
    left_join(df_acti) %>%
    ggplot(aes_map) +
    geom_connection +
    geom_point(aes(
      size = acti_score,
      color = atom_color
    )) +
    geom_text(aes(
      label = factor
    ), size = 5) +
    scale_xlim +
    scale_ylim +
    scale_size_continuous(
      range = c(20, 30)
    ) +
    # scale_color_manual() + 
    guides(
      size = 'none',
      color = 'none'
    ) +
    theme_void() ->
    plt_acti_molecule
  
  # rm(df_acti)
  # rm(df_polygon)
  
  # Output
  return(plt_acti_molecule)
  
}

# - ACTI molecule plotting function ---------------------------------------
fun_acti_plot_molecule <- function(df_acti){
  
  # Arguments validation
  stopifnot(
    "'df_acti' must be a ACTI data frame." =
      any(class(df_acti) == 'df_acti')
  )
  
  # Data wrangling
  
  if(first(df_acti$generalism) > 0.5){
    
    # If generalist, call generalist function
    fun_acti_plot_generalist(df_acti) ->
      plt_acti_molecule
    
    
  } else {
    
    # If specialist, call specialist function
    fun_acti_plot_specialist(df_acti) ->
      plt_acti_molecule
    
  }
  
  # Output
  return(plt_acti_molecule)
  
}

# dsdsds --------------------------------------------------------------------
c(
  'Ds', 'Eg', 'Hs',
  'Mn', 'Tr', 'Ad',
  'So', 'Ah', 'Hz',
  'An', 'Mt', 'Rb',
  'In', 'Mc'
) -> chr_factor_pal

chr_factor_pal %>% 
  length() %>% 
  viridis() %>% 
  set_names(
    chr_factor_pal
  ) -> chr_factor_pal

c(
  chr_factor_pal
  , 'Aux' = 'lightgrey'
) -> chr_factor_pal

fun_acti_type(
  df_data = df_occupations
  , chr_factor_labels = c(
    'Ds', 'Eg', 'Hs',
    'Mn', 'Tr', 'Ad',
    'So', 'Ah', 'Hz',
    'An', 'Mt', 'Rb',
    'In', 'Mc'
  )
  , chr_data_id = 
    df_occupations$occupation
  , efa_model = efa_model
  , dbl_scale_lb = 0
) -> df_acti

df_acti %>% 
  filter(
    occupation == 'Crematory Operators'
  ) -> df_acti

map(
  1:nrow(df_acti)
  , ~ df_acti %>%
    mutate(generalism = 0) %>%
    slice_head(n = .x) %>% 
    fun_acti_plot_molecule()
) -> list_plt_acti

# lalala --------------------------------------------------------------------
c(
  'Ds', 'Eg', 'Hs',
  'Mn', 'Tr', 'Ad',
  'So', 'Ah', 'Hz',
  'An', 'Mt', 'Rb',
  'In', 'Mc'
) -> chr_factor_pal

chr_factor_pal %>% 
  length() %>% 
  viridis() %>% 
  set_names(
    chr_factor_pal
  ) -> chr_factor_pal

c(
  chr_factor_pal
  , 'Aux' = 'lightgrey'
) -> chr_factor_pal

df_occupations %>% 
  slice_sample(n = 1) -> 
  dsds

fun_acti_type(
  df_data = dsds
  , chr_factor_labels = c(
    'Ds', 'Eg', 'Hs',
    'Mn', 'Tr', 'Ad',
    'So', 'Ah', 'Hz',
    'An', 'Mt', 'Rb',
    'In', 'Mc'
  )
  , chr_data_id = 
    dsds$occupation
  , efa_model = efa_model
  , dbl_scale_lb = 0
) -> df_acti

df_acti

fun_acti_plot_polygon(6) %>% 
  fun_acti_plot_rotate(
    dbl_theta = pi/2
  ) -> df_polygon


df_polygon %>% round(1)

df_polygon %>% 
  mutate(
    rank = c(
      6, 3, 2,
      5, 4, 1
    )
  ) -> df_polygon

df_polygon %>% 
  left_join(
    df_acti
  ) %>% 
  mutate(
    atom_color = 
      if_else(
        class == 'Aux'
        , class
        , factor
      )
    , font_color = 
      if_else(
        atom_color == 'Aux'
        , '#212121'
        , 'white'
      )
  ) %>% 
  ggplot(aes(
    x = x,
    y = y,
    label = factor,
    size = acti_score
  )) +
  geom_polygon(
    color = 'grey',
    linewidth = 1.25,
    fill = NA
  ) +
  geom_point(aes(
    color = atom_color
  )) +
  geom_text(size = 5) + 
  scale_color_manual(
    values = chr_factor_pal
  ) + 
  scale_size_continuous(
    range = c(20, 30)
  ) + 
  guides(
    size = 'none',
    color = 'none'
  ) +
  xlim(c(-1.5, 1.5)) +
  ylim(c(-1.5, 1.5)) + 
  theme_void() + 
  theme(plot.margin = unit(rep(.5, 4), 'cm')) + 
  labs(
    title = dsds$occupation
    , subtitle = paste(
      'ACTI Type:',
      first(df_acti$acti_type)
      , '(Generalist)'
    )
  )

# - ACTI molecule plot ----------------------------------------------------
fun_acti_plot_molecule <- function(
    df_acti
    , chr_palette
){
  
  # Arguments validation
  
  # Data wrangling
  
  # Calculate bubble positions
  
  # Calculate molecule bonds
  
  # Plot profile molecule
  
  # Output
  
}

# [VECTORIZED FUNCTIONS] --------------------------------------------------
# # - Generalism function ---------------------------------------------------
# fun_acti_generalism <- function(
    #     mtx_data
#     , dbl_scale_lb = 0
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'mtx_data' must be a numeric matrix." =
#       all(
#         is.numeric(mtx_data)
#       )
#   )
#   
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." =
#       is.numeric(dbl_scale_lb)
#   )
#   
#   # Data wrangling
#   rbind(mtx_data) -> mtx_data
#   
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   
#   # Generalism helper function
#   fun_acti_generalism_helper <- function(dbl_profile){
#     
#     # Drop NAs
#     dbl_profile[!is.na(
#       dbl_profile
#     )] -> dbl_profile
#     
#     # Apply bounded variable skewness function
#     fun_skew_sdmode(
#       dbl_var =
#         dbl_profile
#       , dbl_scale_lb =
#         dbl_scale_lb
#       , dbl_scale_ub =
#         max(dbl_profile)
#     ) -> dbl_generalism
#     
#     rm(dbl_profile)
#     
#     # Output
#     return(dbl_generalism)
#     
#   }
#   
#   # Apply generalism function to each row
#   apply(
#     mtx_data, 1
#     , fun_acti_generalism_helper
#   ) -> mtx_generalism
#   
#   # Output
#   return(mtx_generalism)
#   
# }

# # - Indispensability function ---------------------------------------------
# fun_acti_indispensability <- function(
    #     mtx_data
#     , dbl_scale_lb
#     , dbl_generalism
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'mtx_data' must be a numeric matrix." =
#       all(
#         is.numeric(mtx_data)
#       )
#   )
#   
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." =
#       is.numeric(dbl_scale_lb)
#   )
#   
#   stopifnot(
#     "'dbl_generalism' must be a numeric vector with one element for each row in 'mtx_data'." =
#       all(
#         is.numeric(dbl_generalism)
#         , length(dbl_generalism) ==
#           nrow(rbind(mtx_data))
#       )
#   )
#   
#   # Data wrangling
#   rbind(mtx_data) -> mtx_data
#   
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   
#   # Indispensability helper function
#   fun_acti_indispensability_helper <- function(
    #     dbl_profile
#     , dbl_generalism
#   ){
#     
#     # Drop NA's
#     dbl_profile[!is.na(
#       dbl_profile
#     )] -> dbl_profile
#     
#     # Equivalence of normalized scores
#     fun_eqvl_equivalence(
#       dbl_var =
#         dbl_profile
#       , dbl_scale_lb =
#         dbl_scale_lb
#       , dbl_scale_ub =
#         max(dbl_profile)
#       , dbl_scaling =
#         1 - dbl_generalism
#     ) -> dbl_indispensability
#     
#     # Output
#     return(dbl_indispensability)
#     
#   }
#   
#   # Apply indispensability function to each row
#   mapply(
#     function(profile, generalism){
#       
#       # Call helper function
#       fun_acti_indispensability_helper(
#         dbl_profile = profile
#         , dbl_generalism = generalism
#       ) -> dbl_indispensability
#       
#       # Output
#       return(dbl_indispensability)
#       
#     }
#     , profile = as.data.frame(t(mtx_data))
#     , generalism = dbl_generalism
#   ) -> mtx_indispensability
#   
#   # Output
#   return(mtx_indispensability)
#   
# }
# 
# df_occupations %>% 
#   filter(
#     occupation ==
#       'Statisticians'
#   ) %>%
#   select(
#     occupation
#     , starts_with(
#       'item'
#     )
#   ) %>% 
#   pivot_longer(
#     cols = -1
#     , names_to = 'item'
#     , values_to = 'item_score'
#   ) %>% 
#   mutate(
#     generalism = 
#       fun_acti_generalism(
#         mtx_data = 
#           item_score
#         , dbl_scale_lb = 0
#       )
#     , item_indispensability = 
#       fun_acti_indispensability(
#         mtx_data = item_score
#         , dbl_scale_lb = 0
#         , dbl_generalism = 
#           first(generalism)
#       ) %>% 
#       as.numeric() %>%
#       round(4)
#   ) %>% 
#   arrange(desc(
#     item_score
#   )) %>% 
#   print(n = Inf)
# 
# fun_acti_indispensability(
#   mtx_data =
#     df_input[-1] %>% 
#     as.numeric()
#   , dbl_scale_lb = 0
#   , dbl_generalism = 
#     fun_acti_generalism(
#       mtx_data = 
#         df_input[-1] %>% 
#         as.numeric()
#       , dbl_scale_lb = 0
#     )
# ) %>% round(2)
# 
# 
# df_occupations %>%
#   select(starts_with(
#     'item'
#   )) %>% 
#   select(1:10) %>% 
#   slice_head(n = 1) %>%
#   as.numeric() -> dsds
# 
# (dsds * 0.5) %>% 
#   as_tibble() %>%
#   mutate(
#     .after = value
#     , normvalue = 
#       value / max(value)
#     , lalala = 
#       fun_eqvl_equivalence(
#         normvalue
#         , dbl_scale_lb = 0
#         , dbl_scale_ub = 1
#         , dbl_scaling = 
#           1 - fun_acti_generalism(value)
#       )
#     , lb0 = 
#       fun_eqvl_equivalence(
#         dbl_var = value
#         , dbl_scale_lb = 0
#         , dbl_scale_ub = 
#           max(value)
#         , dbl_scaling = 
#           1 - fun_acti_generalism(value)
#       )
#     , lbmin = 
#       fun_eqvl_equivalence(
#         dbl_var = value
#         , dbl_scale_lb = 
#           min(value)
#         , dbl_scale_ub = 
#           max(value)
#         , dbl_scaling = 
#           1 - fun_acti_generalism(value)
#       )
#     , lbmax_min = 
#       fun_eqvl_equivalence(
#         dbl_var = value
#         , dbl_scale_lb = 
#           max(value) - 
#           min(value)
#         , dbl_scale_ub = 
#           max(value)
#         , dbl_scaling = 
#           1 - fun_acti_generalism(value)
#       )
#   ) %>%
#   arrange(-value) %>% 
#   round(2)
# 
# fun_eqvl_equivalence(
#   dbl_var = dsds
#   , dbl_scale_lb =
#     # 0
#     # min(dsds)
#     max(dsds) - min(dsds)
#   , dbl_scale_ub =
#     max(dsds)
#   , dbl_scaling =
#     1 - fun_acti_generalism(dsds)
# ) %>% 
#   bind_cols(dsds) %>% 
#   rename(
#     
#   )

# # - Competency function (ps: this only measures intra-occupation competency! it doesn't account for attribute difficulty) ---------------------------------------------------
# fun_acti_competency <- function(
    #     mtx_data
#     , dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#     , dbl_generalism
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'mtx_data' must be a numeric matrix." =
#       all(
#         is.numeric(mtx_data)
#       )
#   )
#   
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." =
#       is.numeric(dbl_scale_lb)
#   )
#   
#   stopifnot(
#     "'dbl_scale_ub' must be numeric." =
#       is.numeric(dbl_scale_ub)
#   )
#   
#   stopifnot(
#     "'dbl_generalism' must be a numeric vector with one element for each row in 'mtx_data'." =
#       all(
#         is.numeric(dbl_generalism)
#         , length(dbl_generalism) ==
#           nrow(rbind(mtx_data))
#       )
#   )
#   
#   # Data wrangling
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   
#   dbl_scale_ub[[1]] -> dbl_scale_ub
#   
#   rbind(mtx_data) -> mtx_data
#   
#   # Weigh each attribute by its capital macro-flexibility?
#   # Competency level helper function
#   fun_acti_competency_helper <- function(
    #     dbl_profile
#     , dbl_generalism
#   ){
#     
#     # Drop NA's
#     dbl_profile[!is.na(
#       dbl_profile
#     )] -> dbl_profile
#     
#     # Weighted mean of normalized item scores
#     # adjusted by item importance and generalism
#     weighted.mean(
#       x =
#         dbl_profile / (
#           dbl_scale_ub -
#             dbl_scale_lb
#         ) -
#         dbl_scale_lb / (
#           dbl_scale_ub -
#             dbl_scale_lb
#         )
#       , w =
#         fun_eqvl_equivalence(
#           dbl_var =
#             dbl_profile
#           , dbl_scale_lb =
#             dbl_scale_lb
#           # max(dbl_profile) -
#           # min(dbl_profile)
#           # min(dbl_profile)
#           , dbl_scale_ub =
#             max(dbl_profile)
#           , dbl_scaling =
#             1 - dbl_generalism
#         )
#     ) -> dbl_competency
#     
#     # Output
#     return(dbl_competency)
#     
#   }
#   
#   # Apply competency level helper function
#   mapply(
#     function(profile, generalism){
#       
#       # Call helper function
#       fun_acti_competency_helper(
#         dbl_profile = profile
#         , dbl_generalism = generalism
#       ) -> dbl_competency
#       
#       # Output
#       return(dbl_competency)
#       
#     }
#     , profile = as.data.frame(t(mtx_data))
#     , generalism = dbl_generalism
#   ) -> mtx_competency
#   
#   # Output
#   return(mtx_competency)
#   
# }

# # - Competency function ---------------------------------------------------
# fun_acti_competency <- function(
    #     mtx_data
#     , mtx_weights = NULL
#     , dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#     , dbl_generalism
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'mtx_data' must be a numeric matrix." =
#       all(
#         is.numeric(mtx_data)
#       )
#   )
#   
#   stopifnot(
#     "'mtx_weights' must be a numeric matrix." =
#       any(
#         is.null(mtx_weights)
#         , all(
#           is.numeric(mtx_weights)
#           , dim(mtx_weights) ==
#             dim(mtx_data)
#         )
#       )
#   )
#   
#   stopifnot(
#     "'dbl_scale_lb' must be numeric." =
#       is.numeric(dbl_scale_lb)
#   )
#   
#   stopifnot(
#     "'dbl_scale_ub' must be numeric." =
#       is.numeric(dbl_scale_ub)
#   )
#   
#   stopifnot(
#     "'dbl_generalism' must be a numeric vector with one element for each row in 'mtx_data'." =
#       all(
#         is.numeric(dbl_generalism)
#         , length(dbl_generalism) ==
#           nrow(rbind(mtx_data))
#       )
#   )
#   
#   # Data wrangling
#   dbl_scale_lb[[1]] -> dbl_scale_lb
#   
#   dbl_scale_ub[[1]] -> dbl_scale_ub
#   
#   rbind(mtx_data) -> mtx_data
#   
#   # Weigh each attribute by its capital macro-flexibility?
#   # Competency level helper function
#   fun_acti_competency_helper <- function(
    #     dbl_profile
#     , dbl_generalism
#     , dbl_weights
#   ){
#     
#     # Drop NA's
#     dbl_profile[!is.na(
#       dbl_profile
#     )] -> dbl_profile
#     
#     # Weighted mean of normalized item scores
#     # adjusted by item importance and generalism
#     weighted.mean(
#       x =
#         dbl_profile / (
#           dbl_scale_ub -
#             dbl_scale_lb
#         ) -
#         dbl_scale_lb / (
#           dbl_scale_ub -
#             dbl_scale_lb
#         )
#       , w =
#         fun_eqvl_equivalence(
#           dbl_var =
#             dbl_profile
#           , dbl_scale_lb =
#             # min(dbl_profile)
#             dbl_scale_lb
#           , dbl_scale_ub =
#             max(dbl_profile)
#           , dbl_scaling =
#             1 - dbl_generalism
#         ) +
#         fun_eqvl_equivalence(
#           dbl_var =
#             dbl_weights
#           , dbl_scale_lb =
#             # min(dbl_weights)
#             dbl_scale_lb
#           , dbl_scale_ub =
#             max(dbl_weights)
#           , dbl_scaling =
#             1 - dbl_generalism
#         )
#     ) -> dbl_competency
#     
#     # Output
#     return(dbl_competency)
#     
#   }
#   
#   # Weights
#   if(!length(mtx_weights)){
#     
#     mtx_data -> mtx_weights
#     
#   }
#   
#   # Apply competency level helper function
#   mapply(
#     function(profile, generalism, weight){
#       
#       # Call helper function
#       fun_acti_competency_helper(
#         dbl_profile = profile
#         , dbl_generalism = generalism
#         , dbl_weights = weight
#       ) -> dbl_competency
#       
#       # Output
#       return(dbl_competency)
#       
#     }
#     , profile = as.data.frame(t(mtx_data))
#     , generalism = dbl_generalism
#     , weight = as.data.frame(t(mtx_weights))
#   ) -> mtx_competency
#   
#   # Output
#   return(mtx_competency)
#   
# }
# 
# # # - Competency function ---------------------------------------------------
# # fun_acti_competency <- function(
    #     #     mtx_data
# #     , dbl_scale_lb = 0
# #     , dbl_scale_ub = 100
# # ){
# # 
# #   # Arguments validation
# # 
# #   # Data wrangling
# # 
# #   # Weigh each attribute by its capital macro-flexibility?
# #   # Competency level helper function
# #   fun_acti_competency_helper <- function(dbl_profile){
# # 
# #     # Drop NAs
# #     dbl_profile[!is.na(
# #       dbl_profile
# #     )] -> dbl_profile
# # 
# #     # Normalize item scores
# #     dbl_profile / (
# #       dbl_scale_ub -
# #         dbl_scale_lb
# #     ) -> dbl_profile
# # 
# #     dbl_profile -
# #       dbl_scale_lb / (
# #         dbl_scale_ub -
# #           dbl_scale_lb
# #       ) -> dbl_profile
# # 
# #     # Self-weighted mean of item scores
# #     weighted.mean(
# #       x = dbl_profile
# #       , w = dbl_profile
# #       # , w = 1 + dbl_profile
# #       # , w =
# #       #   dbl_profile /
# #       #   max(dbl_profile)
# #     ) -> dbl_competency
# # 
# #     rm(dbl_profile)
# # 
# #     # Output
# #     return(dbl_competency)
# # 
# #   }
# # 
# #   # Apply competency level helper function
# #   apply(
# #     mtx_data, 1
# #     , fun_acti_competency_helper
# #   ) -> mtx_competency
# # 
# #   # Output
# #   return(mtx_competency)
# # 
# # }


# # [TEST] ------------------------------------------------------------------
# - Data ------------------------------------------------------------------
library(readr)

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa/efa_equamax_14factors.rds'
) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023_efa.csv'
) -> df_occupations

# read_csv(
#   'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
# ) -> df_input

# # - Skewness-centered data ------------------------------------------------
# map(
#   # seq(0, 1, length.out = 5) %>%
#   # seq(0, 1, length.out = 6) %>%
#   # c(0, 0.025, 0.05, 0.1, 0.2, 0.5, 1) %>%
#   c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1) %>%
#     set_names(paste0('desdmode_', .))
#   , ~
#     df_occupations %>%
#     select(
#       starts_with(
#         'item'
#       )) %>%
#     fun_skew_desdmode(
#       dbl_weights =
#         df_occupations$
#         employment_variants
#       , dbl_scale_lb = 0
#       , dbl_scale_ub = 100
#       , lgc_sample_variance = F
#       , dbl_pct_remove = .x
#     ) %>%
#     as_tibble()
# ) -> list_desdmode
# 
# map(
#   list_desdmode
#   , ~
#     df_occupations %>%
#     select(!starts_with(
#       'item'
#     )) %>%
#     bind_cols(.x)
# ) -> list_desdmode

# # - Mode-centered data ------------------------------------------------
# map(
#   # seq(0, 1, length.out = 5) %>%
#   # seq(0, .25, length.out = 6) %>%
#   c(0, 0.05, 0.1, 0.2, 0.5, 0.8, 1) %>%
#     set_names(paste0('demode_', .))
#   , ~
#     df_occupations %>%
#     select(
#       starts_with(
#         'item'
#       )) %>%
#     fun_skew_demode(
#       dbl_weights =
#         df_occupations$
#         employment_variants
#       , dbl_scale_lb = 0
#       , dbl_scale_ub = 100
#       , dbl_pct_remove = .x
#     ) %>%
#     as_tibble()
# ) -> list_demode
# 
# map(
#   list_demode
#   , ~
#     df_occupations %>%
#     select(!starts_with(
#       'item'
#     )) %>%
#     bind_cols(.x)
# ) -> list_demode

# # - Original data vs recentered data --------------------------------------
# map(
#   # list_desdmode
#   list_demode
#   , ~ .x %>%
#     fun_plot.density(aes(
#       # x = item_administrative
#       # x = item_oral_comprehension
#       # x = item_pure_mathematics
#       # x = item_electronic_mail
#       # x = item_engineering_and_technology
#       # x = item_economics_and_accounting
#       x = item_analyzing_data_or_information
#       , weight = employment_variants
#     )
#     , .list_axis.x.args = list(
#       limits = c(-.25,1.25) * 100
#     )
#     , .fun_format.x = number
#     , .list_labs = list(
#       y = NULL
#     ))
# ) -> list_plt_centered
# 
# list_plt_centered

# # kmeans ------------------------------------------------------------------
# df_occupations %>% 
#   select(starts_with(
#     'item'
#   )) %>%
#   fun_ftools_factor_scores()
# 
# mtx_occupations[rep(
#   1:nrow(mtx_occupations)
#   , df_occupations$
#     employment_norm
# ), ] -> mtx_occupations
# 
# kmeans(
#   mtx_occupations
#   , centers = 16
# ) -> list_kmeans
# 
# source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_plots.R')
# 
# list_kmeans$
#   centers %>% 
#   as_tibble() %>% 
#   mutate(
#     .before = 1
#     , acti = 
#       paste0(
#         'ACTI'
#         , row_number()
#       )
#   ) %>%
#   select(1, 201:218) %>% 
#   pivot_longer(
#     cols = -1
#     , names_to = 'item'
#     , values_to = 'item_score'
#   ) %>%
#   fun_plot.heatmap(aes(
#     x = item
#     , y = acti
#     , fill = item_score
#   )
#   , .reorder_fct = F
#   , .reorder_desc = F
#   )
# 
# # dsdsds ------------------------------------------------------------------
# df_occupations %>% 
#   select(starts_with(
#     'item'
#   )) -> dsdsds
# 
# dsdsds %>% 
#   as.matrix() ->
#   dsdsds
# 
# fun_skew_center_data(
#   dbl_var = dsdsds
#   , dbl_weights = 
#     df_occupations$
#     employment_variants
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , lgc_sample_variance = F
# ) -> lalala
# 
# fun_acti_competency(
#   mtx_data = 
#     dsdsds
#   , mtx_weights = 
#     lalala
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , dbl_generalism = 
#     fun_acti_generalism(
#       dsdsds
#     )
# ) -> lala
# 
# lala %>% 
#   as_tibble() %>% 
#   mutate(
#     wgt = 
#       df_occupations$
#       employment_variants
#   ) %>% 
#   ggplot(aes(
#     x = value
#     , weights = wgt
#   )) + 
#   geom_density() + 
#   xlim(c(0,1))
# 
# 
# weighted.mean(100 * lalala[,1] - dsdsds[,1], w = df_occupations$employment_variants)
# weighted.mean(100 * lalala[,190] - dsdsds[,190], w = df_occupations$employment_variants)
# weighted.mean(100 * lalala[,37] - dsdsds[,37], w = df_occupations$employment_variants)
# weighted.mean(100 * lalala[,200] - dsdsds[,200], w = df_occupations$employment_variants)
# weighted.mean(100 * lalala[,19] - dsdsds[,19], w = df_occupations$employment_variants)
# 
# apply(
#   as.data.frame(dsdsds - lalala * 100)
#   , 2
#   , function(x){weighted.mean(x, df_occupations$employment_variants)}
# ) -> lala
# 
# qplot(as.numeric(lalala))
# 
# apply(
#   as.data.frame(lalala)
#   , 2, function(x){
#     
#     sqrt(wtd.var(
#       x, weights = 
#         df_occupations$
#         employment_variants
#     ))
#     
#   }
# ) -> sdsd
# 
# View(cbind(sdsd))
# 
# lalala %>% 
#   as_tibble() %>% 
#   ggplot(aes(
#     x = 
#       item_oral_expression
#     # item_fine_arts
#     # item_programming
#     # item_written_comprehension
#     # item_electronic_mail
#     , weights =
#       df_occupations$
#       employment_variants
#   )) + 
#   geom_density()
# 
# df_occupations %>% 
#   ggplot(aes(
#     x = 
#       item_oral_expression
#     # item_programming
#     # item_written_comprehension
#     # item_electronic_mail
#     , weights = 
#       df_occupations$
#       employment_variants
#   )) + 
#   geom_density()
# 
# View(lalala)
# 
# wtd.var(lalala, weights = df_occupations$employment_variants)
# 
# # dsds --------------------------------------------------------------------
# df_input[-1] %>%
#   as.matrix() -> 
#   dsds
# 
# fun_acti_generalism(dsds) -> generalism
# 
# generalism
# 
# fun_acti_competency(
#   mtx_data = dsds
#   , dbl_scale_lb = 0
#   , dbl_scale_ub = 100
#   , dbl_generalism =
#     generalism
# )
# 
# weighted.mean(
#   dsds
#   , w = 
#     fun_eqvl_equivalence(
#       dsds
#       , dbl_scale_ub = 100
#       , dbl_scaling = 1 - generalism
#     )
# )
# 
# library(purrr)
# sd(c(rep(0,1),1))
# sd(c(rep(0,10),1))
# sd(c(rep(1,10),0))
# 
# 
# c(100,0,0,0) -> dsds
# 
# fun_acti_competency(
#   dsds
#   , 0, 100, fun_acti_generalism(dsds)
# )
# 
# c(100,100,100,100,100) %>% 
#   fun_skew_sdmode(
#     dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#   )
# 
# c(100,0,0,0,0) %>% 
#   fun_skew_sdmode(
#     dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#   )
# 
# c(100,0,0,0,20) %>% 
#   fun_skew_sdmode(
#     dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#   )
# 
# map(
#   list(
#     c(100,0,0,0),
#     c(100,20,20,20),
#     c(100,50,50,40),
#     c(100,50,50,50),
#     c(100,50,50,60),
#     c(100,70,70,70),
#     c(100,100,100,100)
#   )
#   , ~ fun_acti_generalism(.x) #%>% 
#   #   fun_acti_competency(
#   #     mtx_data = .x
#   #   , dbl_scale_lb = 0
#   #   , dbl_scale_ub = 100
#   #   , dbl_generalism = .
#   # )
# )
# 

# # - Generalism test -------------------------------------------------------
# df_occupations %>% 
#   transmute(
#     occupation = 
#       occupation
#     , employment_norm =
#       employment_norm
#     , generalism = 
#       df_occupations %>% 
#       select(starts_with(
#         'item'
#       )) %>% 
#       as.matrix() %>% 
#       fun_acti_generalism()
#   ) -> dsdsds
# 
# dsdsds %>% 
#   arrange(desc(
#     generalism
#   )) %>% 
#   print(n = Inf)
# 
# dsdsds %>% 
#   mutate(
#     generalism = 
#       fun_acti_classifier(
#         generalism
#         , dbl_scale_lb = 0
#         , dbl_scale_ub = 1
#         , int_levels = 2
#       )
#     , generalism =
#       generalism - 1
#   ) %>% 
#   reframe(
#     weighted.mean(
#       generalism
#       , employment_norm
#     )
#   )
# 
# dsdsds %>% 
#   arrange(desc(
#     generalism
#   )) %>% 
#   ggplot(aes(
#     x = 
#       generalism
#     , weights = 
#       employment_norm
#   )) + 
#   geom_density() + 
#   geom_vline(
#     xintercept = 0.5
#   ) +
#   xlim(c(0,1))
# 
# 
# dsdsds %>% 
#   arrange(desc(
#     generalism
#   )) %>% 
#   # mutate(
#   #   generalism = 
#   #     fun_acti_classifier(
#   #       generalism
#   #       , dbl_scale_lb = 0
#   #       , dbl_scale_ub = 1
#   #       , int_levels = 2
#   #     ) %>% 
#   #     case_match(
#   #       1 ~ 'specialist'
#   #       , .default = 'generalist'
# #     )
# # ) %>% 
# filter(stringr::str_detect(
#   tolower(occupation)
#   , 'data|statis'
# )) %>%
#   print(n = Inf)
# 
# df_input[-1] %>% 
#   as.matrix() %>% 
#   rbind(.,.) %>% 
#   fun_acti_generalism(
#     dbl_scale_lb = 0
#   )
# 
# fun_acti_generalism(matrix(1, 2, 5) * pmax(rnorm(10, 0.5, 0.025), 0))
# fun_acti_generalism(matrix(1, 2, 5) * pmax(rnorm(10, 0.5, 0.25), 0))
# fun_acti_generalism(matrix(1, 2, 5) * pmax(rnorm(10, 0, 1000), 0))
# 
# # - Competency test -------------------------------------------------------
# df_occupations %>% 
#   transmute(
#     occupation = 
#       occupation
#     , employment_norm =
#       employment_norm
#     , competency = 
#       df_occupations %>% 
#       select(starts_with(
#         'item'
#       )) %>% 
#       as.matrix() %>% 
#       fun_acti_competency(
#         dbl_scale_lb = 0
#         , dbl_scale_ub = 100
#         , dbl_generalism = 
#           df_occupations %>% 
#           select(starts_with(
#             'item'
#           )) %>% 
#           as.matrix() %>% 
#           fun_acti_generalism()
#       )
#     # fun_acti_competency(
#     #   dbl_scale_lb = 0
#     #   , dbl_scale_ub = 100
#     # )
#     , competency_level =
#       fun_acti_classifier(
#         dbl_var = competency
#         , dbl_scale_lb = 0
#         , dbl_scale_ub = 1
#         , int_levels = 5
#       )
#   ) -> dsdsds
# 
# dsdsds %>% 
#   arrange(desc(
#     competency
#   )) %>% 
#   print(n = Inf)
# 
# dsdsds %>% 
#   arrange(desc(
#     competency
#   )) %>% 
#   ggplot(aes(
#     x = 
#       competency
#     , weights = 
#       employment_norm
#   )) + 
#   geom_density() + 
#   xlim(c(0,1))
# 
# dsdsds %>% 
#   arrange(desc(
#     competency
#   )) %>% 
#   filter(stringr::str_detect(
#     tolower(occupation)
#     , 'data|statis'
#   )) %>%
#   print(n = Inf)
# 
# dsdsds %>% 
#   arrange(desc(
#     competency
#   )) %>% 
#   filter(stringr::str_detect(
#     tolower(occupation)
#     , 'engineers'
#   )) %>%
#   print(n = Inf)
# 
# # LL
# # LM
# # MM
# # HM
# # HH
# library(stringr)
# 
# df_occupations %>% 
#   filter(str_detect(
#     str_to_lower(occupation)
#     , 'k-12'
#   )) %>% 
#   select(starts_with(
#     'item'
#   )) %>% 
#   as.numeric() %>% 
#   qplot(geom = 'density')
# 
# df_input[-1] %>% 
#   as.matrix() %>%
#   rbind(.,.) %>% 
#   fun_acti_competency(
#     dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#     , dbl_generalism = 
#       df_input[-1] %>% 
#       as.matrix() %>%
#       rbind(.,.) %>% 
#       fun_acti_generalism()
#   )
# 
# # - Generalism vs Competency test -------------------------------------------------------
# df_occupations %>% 
#   transmute(
#     occupation = 
#       occupation
#     , wage_mean = 
#       wage_mean
#     , employment_norm =
#       employment_norm
#     , generalism = 
#       df_occupations %>% 
#       select(starts_with(
#         'item'
#       )) %>% 
#       as.matrix() %>%
#       fun_acti_generalism()
#     , competency = 
#       df_occupations %>% 
#       select(starts_with(
#         'item'
#       )) %>% 
#       as.matrix() %>% 
#       fun_acti_competency(
#         dbl_scale_lb = 0
#         , dbl_scale_ub = 100
#         , dbl_generalism = 
#           generalism
#       )
#   ) -> dsdsds
# 
# df_occupations %>% 
#   filter(
#     str_detect(
#       str_to_lower(occupation)
#       , 'maids'
#     )
#   ) %>% 
#   select(starts_with(
#     'item'
#   )) %>%
#   pivot_longer(
#     cols = everything()
#     , names_to = 'item'
#     , values_to = 'item_score'
#   ) %>% 
#   arrange(desc(
#     item_score
#   )) %>% 
#   print(n = Inf)
# 
# dsdsds %>% 
#   arrange(desc(
#     competency
#   )) %>% 
#   print(n = Inf)
# 
# weights::wtd.cors(
#   x = dsdsds$generalism
#   , y = dsdsds$competency
#   , weight = dsdsds$employment_norm
# )
# 
# weights::wtd.cors(
#   x = dsdsds$generalism
#   , y = dsdsds$wage_mean
#   , weight = dsdsds$employment_norm
# )
# 
# weights::wtd.cors(
#   x = dsdsds$competency
#   , y = dsdsds$wage_mean
#   , weight = dsdsds$employment_norm
# )
# 
# weights::wtd.cors(
#   x = dsdsds$employment_norm
#   , y = dsdsds$wage_mean
#   , weight = dsdsds$employment_norm
# )
# 
# dsdsds %>% 
#   ggplot(aes(
#     x = 
#       competency
#     , weights = 
#       employment_norm
#   )) + 
#   geom_density() + 
#   xlim(c(0,1))
# 
# dsdsds %>% 
#   # group_by(occupation) %>%
#   # slice(rep(1:n(), employment_norm)) %>%
#   # ungroup() %>%
#   ggplot(aes(
#     x = 
#       generalism
#     , y = 
#       competency
#     , weights = 
#       employment_norm
#   )) + 
#   geom_point() + 
#   xlim(c(0,1)) + 
#   ylim(c(0,1)) 
# 
# dsdsds %>% 
#   arrange(desc(
#     competency
#   )) %>% 
#   filter(stringr::str_detect(
#     tolower(occupation)
#     , 'data|statis'
#   )) %>%
#   print(n = Inf)
# 
# dsdsds %>% 
#   arrange(desc(
#     competency
#   )) %>% 
#   filter(stringr::str_detect(
#     tolower(occupation)
#     , 'engineers'
#   )) %>%
#   print(n = Inf)
# 
# # LL
# # LM
# # MM
# # HM
# # HH
# library(stringr)
# 
# 100 - 
#   df_occupations %>% 
#   filter(str_detect(
#     str_to_lower(occupation)
#     , 'k-12'
#   )) %>% 
#   select(starts_with(
#     'item'
#   )) %>%
#   as.numeric() %>%
#   mlv()
# 
# df_occupations %>% 
#   filter(str_detect(
#     str_to_lower(occupation)
#     , 'k-12'
#   )) %>% 
#   select(starts_with(
#     'item'
#   )) %>%
#   as.numeric() %>%
#   fun_skew_sdmode(
#     dbl_scale_lb = 0
#     , dbl_scale_ub = 100
#   )
# 
# # df_occupations %>% 
# list_desdmode$
#   demode_0.5 %>% 
#   filter(str_detect(
#     str_to_lower(occupation)
#     , 'k-12'
#   )) %>% 
#   select(starts_with(
#     'item'
#   )) %>% 
#   as.numeric() %>% 
#   qplot(geom = 'density')
# 
# tibble(
#   occupation = 'Cao'
#   , generalism = 
#     df_input[-1] %>% 
#     as.matrix() %>% 
#     fun_acti_generalism()
#   , competency = 
#     df_input[-1] %>% 
#     as.matrix() %>% 
#     fun_acti_competency(
#       dbl_scale_ub = 100
#       , dbl_scale_lb = 0
#       , dbl_generalism = 
#         generalism
#     )
# )

# # - Generalism with mode-centered data --------------------------------------
# map(
#   list_demode
#   # list_desdmode
#   , ~ .x %>%
#     select(
#       occupation
#       , employment_variants
#       , starts_with(
#         'item'
#       )) %>% 
#     pivot_longer(
#       cols = starts_with('item')
#       , names_to = 'item'
#       , values_to = 'item_score'
#     ) %>% 
#     group_by(
#       occupation
#       , employment_variants
#     ) %>% 
#     reframe(
#       generalism = 
#         fun_acti_generalism(
#           dbl_profile = 
#             item_score
#           , dbl_scale_lb = 0
#         )
#     )
# ) -> list_df_generalism
# 
# map(
#   list_df_generalism
#   , ~ .x %>% 
#     fun_plot.density(aes(
#       x = generalism
#       , weight = employment_variants
#     )
#     , .list_axis.x.args = list(
#       limits = c(-.25,1.25)
#     )
#     , .fun_format.x = percent
#     , .list_labs = list(
#       y = NULL
#     ))
# ) -> list_plt_generalism
# 
# list_plt_generalism
# 
# # - Competency with mode-centered data --------------------------------------
# map(
#   list_demode
#   # list_desdmode
#   , ~ .x %>% 
#     select(
#       occupation
#       , employment_variants
#       , starts_with(
#         'item'
#       )) %>% 
#     pivot_longer(
#       cols = starts_with('item')
#       , names_to = 'item'
#       , values_to = 'item_score'
#     ) %>% 
#     group_by(
#       occupation
#       , employment_variants
#     ) %>% 
#     reframe(
#       competency = 
#         fun_acti_competency(
#           dbl_profile = 
#             item_score
#           , dbl_scale_lb = 0
#           , dbl_scale_ub = 100
#         )
#     )
# ) -> list_df_competency
# 
# map(
#   list_df_competency
#   , ~ .x %>% 
#     fun_plot.density(aes(
#       x = competency
#       , weight = employment_variants
#     )
#     , .list_axis.x.args = list(
#       limits = c(-.25,1.25)
#     )
#     , .fun_format.x = percent
#     , .list_labs = list(
#       y = NULL
#     ))
# ) -> list_plt_competency
# 
# list_plt_competency
# 
# list_df_competency %>% 
#   map(
#     ~ .x %>% 
#       arrange(desc(
#         competency
#       ))
#   )
# 
# # - Atlas Career Type Indicator test --------------------------------------
# sample(df_occupations$occupation, 1) -> dsds
# 
# # dsds <- 'Musicians and Singers'
# # dsds <- 'Economists'
# 
# # map_df(
# map(
#   list_demode
#   # list_desdmode
#   , ~ .x %>% 
#     filter(occupation == dsds) %>% 
#     select(starts_with('item')) %>% 
#     fun_acti_type(
#       efa_model = efa_model
#       , chr_factor_labels = c(
#         'Ds', 'Eg', 'Hs',
#         'Mn', 'Tr', 'Ad', 
#         'So', 'Ah', 'Hz',
#         'An', 'Mt', 'Rb',
#         'In', 'Mc'
#       )
#       , dbl_scale_lb = 0
#     )
#   # , .id = 'model'
# ) -> list_acti
# 
# dsds
# list_acti
# dsds

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

















# [TEST] ------------------------------------------------------------------
# - Generalism test -------------------------------------------------------
fun_acti_generalism(
  dbl_profile = 
    rnorm(50, 50, 25) %>%
    pmax(0) %>%
    pmin(100)
  , dbl_scale_lb = 0
)

fun_acti_generalism(
  dbl_profile = 
    rnorm(50, 50, 5) %>%
    pmax(0) %>%
    pmin(100)
  , dbl_scale_lb = 0
)

fun_acti_generalism(
  dbl_profile = 
    rnorm(50, 50, 0) %>%
    pmax(0) %>%
    pmin(100)
  , dbl_scale_lb = 0
)

# - Indispensability test -------------------------------------------------
fun_acti_indispensability(
  dbl_profile =
    rnorm(50, 50, 25) %>% 
    pmax(0) %>% 
    pmin(100)
  , dbl_scale_lb = 0
) %>% round(4)

# - Competency test -------------------------------------------------------
fun_acti_competency(
  dbl_profile = 
    rnorm(50, 100, 25) %>%
    pmax(0) %>%
    pmin(100)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

fun_acti_competency(
  dbl_profile = 
    rnorm(50, 50, 25) %>%
    pmax(0) %>%
    pmin(100)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

fun_acti_competency(
  dbl_profile = 
    rnorm(50, 50, 5) %>%
    pmax(0) %>%
    pmin(100)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

fun_acti_competency(
  dbl_profile = 
    rnorm(50, 50, 0) %>%
    pmax(0) %>%
    pmin(100)
  , dbl_scale_lb = 0
  , dbl_scale_ub = 100
)

# - Numerical ACTI test ---------------------------------------------------
df_occupations %>% 
  slice_sample(n = 10) -> 
  dsds

fun_acti_type(
  df_data = dsds
  , chr_factor_labels = c(
    'Ds', 'Eg', 'Hs',
    'Mn', 'Tr', 'Ad',
    'So', 'Ah', 'Hz',
    'An', 'Mt', 'Rb',
    'In', 'Mc'
  )
  , chr_data_id = 
    dsds$occupation
  , efa_model = efa_model
  , dbl_scale_lb = 0
)
