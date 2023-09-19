# SETUP ----------------------------------------------------------------
# region: Packages 
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

#endregion
# FUNCTIONS ----------------------------------------------------------------
#region: Generality Function 
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

#endregion
#region: Indispensability Function 
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

#endregion
#region: Competency Function 
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

#endregion
#region: Classifier Function 
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

#endregion
#region: ACTI Type Function 
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

#endregion
# PLOTTING FUNCTIONS ----------------------------------------------------------------
#region: Polygon Helper Function 
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

#endregion
#region: Rotation Matrix Helper Function 
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

#endregion
#region: Specialist Plotting Function 
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
  
  rm(df_acti)
  rm(df_polygon)
  
  # Output
  return(plt_acti_molecule)
  
}

#endregion
#region: Generalist Plotting Function 
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
        rank = c(1, 2, 3)
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
        rank = c(1, 3, 2, 4)
      ) -> df_polygon
    
    # Plot elements
    aes(x = x, y = y) -> aes_map
    geom_polygon(color = 'lightgrey', linewidth = 1.25, fill = NA) -> geom_connection
    xlim(c(-2, 2)) -> scale_xlim
    ylim(c(-2, 2)) -> scale_ylim
    
  } else if(nrow(df_acti) == 5){
    
    # Polygon
    fun_acti_plot_polygon(5) %>% 
      mutate(
        rank = c(3, 1, 4, 2, 5)
      ) -> df_polygon
    
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
      ) %>% 
      mutate(
        rank = c(
          5, 4, 2, 
          3, 6, 1
        )
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
      mutate(
        rank = c(
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
      mutate(
        rank = c(
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
      mutate(
        rank = c(
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
      mutate(
        rank = c(
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
      mutate(
        rank = c(
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
      mutate(
        rank = c(
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
      mutate(
        rank = c(
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
      mutate(
        rank = c(
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
  
  rm(df_acti)
  rm(df_polygon)
  
  # Output
  return(plt_acti_molecule)
  
}

#endregion
#region: ACTI Molecule Plotting Function 
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
  if(!length(names(chr_factor_pal))){
    
    paste0('F', 1:length(chr_factor_pal)) ->
      names(chr_factor_pal)
    
  }
  
  if(first(df_acti$generalism) > 0.5){
    
    # If generalist, call generalist function
    fun_acti_plot_generalist(df_acti) ->
      plt_acti_molecule
    
  } else {
    
    # If specialist, call specialist function
    fun_acti_plot_specialist(df_acti) ->
      plt_acti_molecule
    
  }
  
  # Apply manual palette
  plt_acti_molecule + 
    scale_color_manual(
      values = chr_factor_pal
      , aesthetics = 'colour'
    ) -> plt_acti_molecule
  
  # Output
  return(plt_acti_molecule)
  
  }

#endregion
# TESTING ----------------------------------------------------------------
#region: Data 
library(readr)

read_rds(
  'C:/Users/Cao/Documents/Github/atlas-research/data/efa/efa_equamax_14factors.rds'
) -> efa_model

read_csv(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations_2023_efa.csv'
) -> df_occupations

View(df_occupations)

#endregion
#region: Test ACTI Plotting Function 
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
  filter(
    occupation == 'Crematory Operators'
  ) -> dsds
 
fun_acti_type(
  df_data = dsds
  # df_data = df_occupations
  , chr_factor_labels = c(
    'Ds', 'Eg', 'Hs',
    'Mn', 'Tr', 'Ad',
    'So', 'Ah', 'Hz',
    'An', 'Mt', 'Rb',
    'In', 'Mc'
  )
  , chr_data_id = 
    dsds$occupation
  # df_occupations$occupation
  , efa_model = efa_model
  , dbl_scale_lb = 0
) -> df_acti

df_acti %>% 
  split(.$occupation) %>% 
  map(
    ~ .x %>% 
      fun_acti_plot_molecule(
        chr_factor_pal
      )
  ) -> list_plt_acti
  
map(
  1:nrow(df_acti)
  , ~ df_acti %>%
    mutate(generalism = 0) %>%
    slice_head(n = .x) %>% 
    fun_acti_plot_molecule(
      chr_factor_pal
    )
) -> list_plt_acti_specialist

map(
  1:nrow(df_acti)
  , ~ df_acti %>%
    slice_head(n = .x) %>% 
    fun_acti_plot_molecule(
      chr_factor_pal
    )
) -> list_plt_acti_generalist

list_plt_acti_specialist

list_plt_acti_generalist

df_acti %>%
  group_by(
    occupation
  ) %>% 
  reframe(
    acti_type = 
      first(acti_type)
  ) %>% 
  left_join(
    df_occupations %>% 
      select(
        occupation,
        employment_variants
      )
  ) %>% 
  mutate(
    pct_population = 
      employment_variants / 
      sum(employment_variants)
  ) %>%
  group_by(
    acti_type
  ) %>% 
  reframe(
    pct_population = 
      sum(pct_population)
  ) %>% 
  arrange(desc(
    pct_population
  )) %>% 
  mutate(
    pct_cum_sum =
      cumsum(pct_population)
  ) -> df_acti_freq

library(atlas.plot)

df_acti_freq %>%
  fun_plot.bar(aes(
    x = acti_type,
    y = pct_population
  )
  , .coord_polar = T
  , .fun_format.y = percent
  , .list_labs = list(
    y = 'ACTI Types Frequency'
  ))

#endregion