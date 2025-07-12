# # [SETUP] -----------------------------------------------------------------
# # - Packages ----------------------------------------------------------------
# # CRAN packages
# chr_pkg <- c(
#   'devtools' #GitHub packages (temp)
#   , 'ggplot2', 'scales' #Data visualization
#   , 'readr' #Read data (temp)
#   , 'vctrs' #Data frame subclasses
#   , 'tidyr', 'dplyr' #Data wrangling
# )
# 
# # Git packages
# chr_git <- c(
#   'CaoBittencourt' = 'atlas.gene',
#   'CaoBittencourt' = 'atlas.aeq',
#   'CaoBittencourt' = 'atlas.comp',
#   'CaoBittencourt' = 'atlas.class',
#   'CaoBittencourt' = 'atlas.ftools'
# )
# 
# # Activate / install CRAN packages
# lapply(
#   chr_pkg
#   , function(pkg){
# 
#     if(!require(pkg, character.only = T)){
# 
#       install.packages(pkg)
# 
#     }
# 
#     require(pkg, character.only = T)
# 
#   }
# )
# 
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
# 
# rm(chr_pkg, chr_git)
# 
# [FUNCTIONS] ---------------------------
# - ACTI type function --------------------------------------------------------
fun_acti_type <- function(
    df_data
    , efa_model
    , chr_factor_labels = NULL
    , chr_id_col = NULL
    , dbl_scale_lb = 0
    , dbl_scale_ub = 100
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
    "'chr_id_col' must be either NULL or a string indicating an ID column in 'df_data'." =
      any(
        is.null(chr_id_col)
        , all(
          is.character(chr_id_col)
          , chr_id_col %in% names(df_data)
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
  
  # Data wrangling
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  dbl_scale_ub[[1]] -> dbl_scale_ub
  
  chr_id_col[[1]] -> chr_id_col
  
  if(!is.null(chr_id_col)){
    
    df_data[[chr_id_col]] ->
      df_data$id_profile
    
  } else {
    
    paste0(
      'Subject', 1:nrow(df_data)
    ) -> df_data$id_profile
    
  }
  
  rm(chr_id_col)
  
  df_data %>%
    select(
      id_profile
      , any_of(
        efa_model$
          model %>%
          colnames()
      )
    ) -> df_data
  
  # Generality
  apply(
    df_data %>% select(-id_profile)
    , 1, fun_gene_generality
    , dbl_scale_lb = dbl_scale_lb
  ) -> dbl_generality
  
  df_data$id_profile -> 
    names(dbl_generality)
  
  # Competence
  apply(
    df_data %>% select(-id_profile)
    , 1, fun_comp_competence
    , dbl_scale_lb = dbl_scale_lb
    , dbl_scale_ub = dbl_scale_ub
    , dbl_generality = dbl_generality
  ) -> dbl_competence
  
  df_data$id_profile -> 
    names(dbl_competence)
  
  # Factor scores
  fun_ftools_factor_scores(
    df_data = df_data
    , efa_model = efa_model
    , lgc_pivot = T
  ) -> df_factor_scores
  
  rm(efa_model)
  rm(df_data)
  
  # Name factors
  if(is.null(chr_factor_labels)){
    
    paste0(
      'F', 1:length(unique(
        df_factor_scores$factor
      ))) -> chr_factor_labels
    
  }
  
  rep(
    chr_factor_labels
    , nrow(df_factor_scores) / 
      length(chr_factor_labels)
  ) -> df_factor_scores$factor
  
  rm(chr_factor_labels)
  
  # Initial data frame
  tibble(
    id_profile = names(dbl_generality),
    competence = dbl_competence,
    generality = dbl_generality
  ) -> df_acti
  
  rm(dbl_generality)
  rm(dbl_competence)
  
  # Estimate ACTI scores
  df_factor_scores %>% 
    right_join(
      df_acti
    ) %>% 
    group_by(
      id_profile
    ) %>% 
    mutate(
      acti_score = 
        fun_aeq_aequivalence(
          dbl_profile = 
            factor_score
          , dbl_generality = 
            first(generality)
        )
      , acti_class = 
        fun_class_classifier(
          acti_score
          , dbl_scale_lb = 0
          , dbl_scale_ub = 1
          , int_levels = 3
          , chr_class_labels = 
            c('Min', 'Aux', 'Dom')
        )
      , factor = factor(factor)
    ) %>% 
    filter(
      acti_class != 'Min'
    ) -> df_acti
  
  rm(df_factor_scores)
  
  # Arrange
  df_acti %>%
    group_by(
      id_profile
    ) %>%
    arrange(
      desc(acti_score)
      , .by_group = T
    ) %>%
    drop_na() %>% 
    mutate(
      factor_rank = 
        row_number()
    ) %>% 
    ungroup() ->
    df_acti
  
  # Factor and font color
  df_acti %>%
    mutate(
      atom_color = if_else(
        acti_class == 'Aux'
        , as.character(acti_class)
        , as.character(factor)
      )
    ) -> df_acti
  
  # ACTI type acronym helper function
  fun_acti_type_helper <- function(df_data){
    
    # ACTI type acronym
    df_data %>%
      filter(
        acti_class == 'Dom'
      ) %>%
      pull(factor) %>%
      paste0(
        collapse = '-'
      ) -> chr_dom
    
    df_data %>%
      filter(
        acti_class != 'Dom'
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
    
    if(
      first(
        df_data$
        generality
      ) > 0.5
    ){
      
      paste0(
        'G:', chr_acti_type
      ) -> chr_acti_type
      
    } else {
      
      paste0(
        'S:', chr_acti_type
      ) -> chr_acti_type
      
    }
    
    # Output
    return(chr_acti_type)
    
  }
  
  # Calculate ACTI acronyms
  df_acti %>% 
    split(.$id_profile) %>% 
    sapply(
      fun_acti_type_helper
    ) %>% 
    as_tibble(
      rownames = 'id_profile'
    ) %>%
    rename(
      acti_type = 2
    ) %>% 
    left_join(
      df_acti
    ) -> df_acti
  
  # Relocate columns
  df_acti %>% 
    relocate(
      id_profile,
      acti_type,
      generality,
      competence,
      factor,
      factor_rank,
      factor_score,
      acti_score,
      acti_class,
      atom_color
    ) -> df_acti
  
  # 'df_acti' subclass
  df_acti %>%
    new_data_frame(
      class = c('df_acti', 'tbl')
    ) -> df_acti
  
  # Output
  return(df_acti)
  
}

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

# - ACTI specialist plotting function ------------------------------
# Specialist plotting function
fun_acti_plot_specialist <- function(df_acti){
  
  # Arguments validation
  stopifnot(
    "'df_acti' must be a data frame of the 'df_acti' class." =
      any(class(df_acti) == 'df_acti')
  )
  
  # Specialist molecule helper functions
  if(!any(nrow(df_acti) == 1:14)){
    
    # Warning
    warning("Invalid ACTI type.")
    
    # Output
    return(NULL)
    
  }
  
  if(nrow(df_acti) == 1){
    
    # Polygon
    fun_acti_plot_polygon(1) %>%
      mutate(
        y = y - 0.75,
        factor_rank = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    NULL -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 2){
    
    # Polygon
    fun_acti_plot_polygon(2) %>%
      mutate(
        factor_rank = c(2, 1),
        group = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-4, 2.5)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 3){
    
    # Polygon
    fun_acti_plot_polygon(3) %>%
      mutate(
        factor_rank = c(2, 3, 1),
        group = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 4){
    
    # Polygon
    fun_acti_plot_polygon(4) ->
      df_polygon
    
    df_polygon %>% 
      slice(1:4, 4) %>%
      mutate(
        factor_rank = c(2, 4, 3, 1, 1),
        group = c(1, 2, 1, 1, 2)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 5){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(4),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>%
      mutate(
        factor_rank = c(2, 3, 5, 4, 1),
        group = c(1, 2, 1, 2, 1)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 6){
    
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
        factor_rank = c(
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
    
  } 
  
  if(nrow(df_acti) == 7){
    
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
        factor_rank = c(
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
    
  } 
  
  if(nrow(df_acti) == 8){
    
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
        factor_rank = c(
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
    
  } 
  
  if(nrow(df_acti) == 9){
    
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
        factor_rank = c(
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
    
  } 
  
  if(nrow(df_acti) == 10){
    
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
        factor_rank = c(
          8, 10, 6, 4, 1, 
          5, 9, 7, 3, 2 
        )
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y, group = group) -> aes_map
    geom_line(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-3, 3)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 11){
    
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
        factor_rank = c(
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
    xlim(c(-4.25, 4.25)) -> scale_xlim
    ylim(c(-1.25, 1.25)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 12){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(3) %>%
        mutate(
          y = 1.25
        ) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 3
        ),
      
      fun_acti_plot_polygon(3) %>%
        mutate(
          y = 1.25
        ) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 3
        )
      
    ) -> df_polygon
    
    bind_rows(
      
      df_polygon %>% 
        mutate(
          y = y * 2
        ),
      
      df_polygon %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          y = y * 1.25
        )
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        factor_rank = c(
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
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-3, 3)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 13){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(3) %>%
        mutate(
          y = 1.25
        ) %>%
        fun_acti_plot_rotate(
          dbl_theta = -pi/2
        ) %>%
        mutate(
          x = x + 3
        ),
      
      fun_acti_plot_polygon(3) %>%
        mutate(
          y = 1.25
        ) %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>%
        mutate(
          x = x - 3
        )
      
    ) -> df_polygon
    
    bind_rows(
      
      df_polygon %>% 
        mutate(
          y = y * 2
        ),
      
      df_polygon %>%
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ) %>% 
        mutate(
          y = y * 1.25
        ),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        factor_rank = c(
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
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-3, 3)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 14){
    
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
        factor_rank = c(
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
    # xlim(c(-2.5, 2.5)) -> scale_xlim
    xlim(c(-2.75, 2.75)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } 
  
  # Plot ACTI molecule
  df_polygon %>%
    left_join(df_acti) %>%
    ggplot(aes_map) +
    geom_connection +
    geom_point(aes(
      size = factor_score,
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
    guides(
      size = 'none',
      color = 'none'
    ) +
    theme_void() ->
    plt_acti_molecule
  
  rm(df_acti)
  rm(df_polygon)
  
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
  if(!any(nrow(df_acti) == 1:14)){
    
    # Warning
    warning("Invalid ACTI type.")
    
    # Output
    return(NULL)
    
  }
  
  if(nrow(df_acti) == 1){
    
    # Polygon
    fun_acti_plot_polygon(1) %>%
      mutate(
        y = y - 0.75,
        factor_rank = 1
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    NULL -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 2){
    
    # Polygon
    fun_acti_plot_polygon(2) %>%
      fun_acti_plot_rotate(
        dbl_theta = pi/2
      ) %>% 
      mutate(
        factor_rank = c(2, 1)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-4, 4)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 3){
    
    # Polygon
    fun_acti_plot_polygon(3) %>% 
      mutate(
        factor_rank = c(1, 2, 3)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 4){
    
    # Polygon
    fun_acti_plot_polygon(4) %>%
      mutate(
        factor_rank = c(1, 3, 2, 4)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 5){
    
    # Polygon
    fun_acti_plot_polygon(5) %>% 
      mutate(
        factor_rank = c(3, 1, 4, 2, 5)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2.5, 2.5)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 6){
    
    # Polygon
    fun_acti_plot_polygon(6) %>% 
      fun_acti_plot_rotate(
        dbl_theta = pi/2
      ) %>% 
      mutate(
        factor_rank = c(
          5, 4, 2, 
          3, 6, 1
        )
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-1.5, 1.5)) -> scale_xlim
    ylim(c(-1.5, 1.5)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 7){
    
    # Polygon
    bind_rows(
      
      fun_acti_plot_polygon(6) %>% 
        fun_acti_plot_rotate(
          dbl_theta = pi/2
        ),
      
      tibble(x = 0, y = 0)
      
    ) -> df_polygon
    
    df_polygon %>% 
      mutate(
        factor_rank = c(
          6, 3, 2,
          4, 5, 1,
          7
        )
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
    
  } 
  
  if(nrow(df_acti) == 8){
    
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
      mutate(
        factor_rank = c(
          6, 3, 2, 4,
          5, 1, 8, 7
        )
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
    
  } 
  
  if(nrow(df_acti) == 9){
    
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
      mutate(
        factor_rank = c(
          4, 1, 3, 2,
          6, 8, 7, 5,
          9
        )
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
    
  } 
  
  if(nrow(df_acti) == 10){
    
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
      mutate(
        factor_rank = c(
          3, 10, 7, 5, 2,
          4, 9, 8, 6, 1
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
    
  } 
  
  if(nrow(df_acti) == 11){
    
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
      mutate(
        factor_rank = c(
          1, 3, 2, 4, 7,
          9, 6, 8, 5, 11,
          10
        )
      ) -> df_polygon
    
    df_polygon %>%
      slice(
        3, 7, 4, 8, 1, 5, 2, 6, 3,
        
        7, 11, 6, 2, 9, 4, 8, 10, 5, 1,
        
        10, 9, 11, 3
        
      ) -> df_polygon
    
    # Plot element
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2.5, 2.5)) -> scale_xlim
    ylim(c(-3.75, 3.75)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 12){
    
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
      mutate(
        factor_rank = c(
          4, 6, 9, 7, 8, 10,
          11, 3, 5, 12, 2, 1
        )
      ) -> df_polygon
    
    df_polygon %>%
      slice(
        11, 7, 8, 9, 10, 12, 6, 1, 2, 3,
        11, 7, 4, 3, 4, 5, 10, 5, 6, 5
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_path(color = 'lightgrey', linewidth = 1.25) -> geom_connection
    xlim(c(-1.75, 1.75)) -> scale_xlim
    ylim(c(-1.75, 3.75)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 13){
    
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
      mutate(
        factor_rank = c(
          11, 6, 2, 4, 9, 3,
          5, 8, 10, 7, 1, 12,
          13
        )
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
    xlim(c(-2.5, 2.5)) -> scale_xlim
    ylim(c(-1.75, 1.75)) -> scale_ylim
    
  } 
  
  if(nrow(df_acti) == 14){
    
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
      mutate(
        factor_rank = c(
          9, 7, 2, 5, 11, 3, 6,
          10, 4, 12, 8, 1, 13, 14
        )
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
    xlim(c(-2.75, 2.75)) -> scale_xlim
    ylim(c(-1.75, 1.75)) -> scale_ylim
    
  } 
  
  # Plot ACTI molecule
  df_polygon %>%
    left_join(df_acti) %>%
    ggplot(aes_map) +
    geom_connection +
    geom_point(aes(
      size = factor_score,
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
    guides(
      size = 'none',
      color = 'none'
    ) +
    theme_void() ->
    plt_acti_molecule
  
  rm(df_acti)
  rm(df_polygon)
  
  # Output
  return(plt_acti_molecule)
  
}

# - ACTI molecule plotting function ---------------------------------------
fun_acti_plot_molecule <- function(df_acti, chr_factor_pal = NULL){
  
  # Arguments validation
  stopifnot(
    "'df_acti' must be a ACTI data frame." =
      any(class(df_acti) == 'df_acti')
  )
  
  stopifnot(
    "'chr_factor_pal' must be either NULL or a named character vector." = 
      any(
        is.character(chr_factor_pal),
        is.null(chr_factor_pal)
      )
  )
  
  # Data wrangling
  # Check if valid colors
  if(!is.null(chr_factor_pal)){
    
    tryCatch(
      expr = {col2hcl(chr_factor_pal)}
      , error = function(e){
        
        # Warning
        warning("'chr_factor_pal' are not valid colors.")
        
        # Output
        return(NULL)
        
      }
    ) -> chr_factor_pal
    
  }
  
  # Generate palette if NULL
  if(is.null(chr_factor_pal)){
    
    hue_pal()(
      length(levels(
        df_acti$factor
      ))
    ) -> chr_factor_pal
    
  }
  
  # Valid factor names
  if(any(
    !length(names(chr_factor_pal)),
    !all(
      names(chr_factor_pal) %in%
      levels(df_acti$factor)
    )
  )){
    
    # Assign valid names
    levels(df_acti$factor) ->
      names(chr_factor_pal)
    
  }
  
  # Auxiliary factors
  c(
    chr_factor_pal,
    'Aux' = 'lightgrey'
  ) -> chr_factor_pal
  
  # Conditionally apply plotting functions
  df_acti %>% 
    split(.$id_profile) %>% 
    lapply(
      function(acti){
        
        if(first(acti$generality) > 0.5){
          
          # If generalist, call generalist function
          fun_acti_plot_generalist(acti) ->
            plt_acti_molecule
          
        } else {
          
          # If specialist, call specialist function
          fun_acti_plot_specialist(acti) ->
            plt_acti_molecule
          
        }
        
        # Output
        return(plt_acti_molecule)
        
      }
    ) -> list_plt_acti_molecule
  
  # Apply manual palette
  list_plt_acti_molecule %>% 
    lapply(
      function(plt_acti_molecule){
        
        plt_acti_molecule +
          scale_color_manual(
            values = chr_factor_pal
            , aesthetics = 'colour'
          ) -> plt_acti_molecule
        
        # Output
        return(plt_acti_molecule)
        
      }
    ) -> list_plt_acti_molecule
  
  # Output
  return(list_plt_acti_molecule)
  
}


# # [TEST] ------------------------------------------------------------------
# # - Data ------------------------------------------------------------------
# library(readr)
# 
# read_rds(
#   'C:/Users/Cao/Documents/Github/atlas-research-dev/data/efa/efa_equamax_14factors.rds'
# ) -> efa_model
# 
# read_csv(
#   'C:/Users/Cao/Documents/Github/Atlas-Research-dev/Data/df_occupations_2023_efa.csv'
# ) -> df_occupations
# 
# # read_csv(
# #   'https://docs.google.com/spreadsheets/d/e/2PACX-1vSVdXvQMe4DrKS0LKhY0CZRlVuCCkEMHVJHQb_U-GKF21CjcchJ5jjclGSlQGYa5Q/pub?gid=1515296378&single=true&output=csv'
# # ) -> df_input
# 
# # - Numerical ACTI test ---------------------------------------------------
# df_occupations %>%
#   slice_sample(n = 10) ->
#   dsds
# 
# fun_acti_type(
#   df_data = dsds
#   , chr_factor_labels = c(
#     'Ds', 'Eg', 'Hs',
#     'Mn', 'Tr', 'Ad',
#     'So', 'Ah', 'Hz',
#     'An', 'Mt', 'Rb',
#     'In', 'Mc'
#   )
#   , chr_id_col =
#     'occupation'
#   , efa_model = efa_model
#   , dbl_scale_lb = 0
# )
# 
# # - ACTI Molecules --------------------------------------------------------------------
# c(
#   'Ds', 'Eg', 'Hs',
#   'Mn', 'Tr', 'Ad',
#   'So', 'Ah', 'Hz',
#   'An', 'Mt', 'Rb',
#   'In', 'Mc'
# ) -> chr_factor_pal
# 
# df_occupations %>%
#   filter(
#     occupation == 'Crematory Operators'
#   ) -> dsds
# 
# fun_acti_type(
#   df_data = dsds
#   , chr_factor_labels = c(
#     'Ds', 'Eg', 'Hs',
#     'Mn', 'Tr', 'Ad',
#     'So', 'Ah', 'Hz',
#     'An', 'Mt', 'Rb',
#     'In', 'Mc'
#   )
#   , chr_id_col =
#     'occupation'
#   , efa_model = efa_model
#   , dbl_scale_lb = 0
# ) -> df_acti
# 
# map_df(
#   1:nrow(df_acti)
#   , ~ df_acti %>%
#     slice_head(
#       n = .x
#     ) %>%
#     mutate(
#       generality = 0
#       , id_profile =
#         paste0(
#           id_profile,
#           '_specialist'
#         )
#       , id_profile =
#         paste0(
#           id_profile, .x
#         )
#     )
# ) %>%
#   bind_rows(
#     map_df(
#       1:nrow(df_acti)
#       , ~ df_acti %>%
#         slice_head(
#           n = .x
#         ) %>%
#         mutate(
#           id_profile =
#             paste0(
#               id_profile,
#               '_generalist'
#             )
#           , id_profile =
#             paste0(
#               id_profile, .x
#             )
#         ))
#   ) -> df_acti
# 
# df_acti %>%
#   mutate(
#     id_profile =
#       str_replace_all(
#         id_profile
#         , ' ', '_'
#       )
#     , id_profile =
#       str_to_lower(
#         id_profile
#       )
#     , id_profile = factor(
#       id_profile
#       , levels = unique(
#         id_profile
#       )
#     )
#   ) -> df_acti
# 
# df_acti %>%
#   fun_acti_plot_molecule() ->
#   list_plt_acti
# 
# list_plt_acti
