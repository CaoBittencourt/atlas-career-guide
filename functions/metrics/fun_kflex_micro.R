# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
# [FUNCTIONS] --------------------------------------------------------------
# - Human Capital Micro-Flexibility ---------------------------------------
fun_kflex_micro <- function(
    
  df_data_rows
  , dbl_scale_ub = 100
  , dbl_scale_lb = 0
  , dbl_weights = NULL
  , efa_model = NULL
  
){
  
  # Arguments validation
  stopifnot(
    "'df_data_rows' must be a data frame." = 
      is.data.frame(df_data_rows)
  )
  
  stopifnot(
    "'dbl_scale_ub' must be numeric." = 
      is.numeric(dbl_scale_ub)
  )
  
  stopifnot(
    "'dbl_scale_lb' must be numeric." = 
      is.numeric(dbl_scale_lb)
  )
  
  stopifnot(
    "'dbl_weights' must be either NULL or numeric." = 
      any(
        is.null(dbl_weights)
        , is.numeric(dbl_weights)
      )
  )
  
  stopifnot(
    "'efa_model' must be either NULL or an object of class 'factanal' or 'fa'." =
      any(
        is.null(efa_model)
        , str_to_lower(class(
          efa_model
        )) == 'fa'
        , str_to_lower(class(
          efa_model
        )) == 'factanal'
      )
  )
  
  # Data wrangling
  # Bounds
  dbl_scale_ub[[1]] -> dbl_scale_ub
  dbl_scale_lb[[1]] -> dbl_scale_lb
  
  # EFA model
  if(length(efa_model)){
    
    loadings(dsds)[,] %>% 
      as_tibble(
        rownames = 'item'
      ) -> df_loadings
    
    df_data_rows %>% 
      select(all_of(
        df_loadings$item
      )) -> df_data_rows
    
    df_loadings %>%
      set_names(
        c(
          'item'
          , df_loadings[-1] %>%
            names() %>%
            str_extract(
              '[[:digit:]]+'
            ) %>%
            paste0('factor',.)
        )
      ) %>%
      relocate(
        item
        , str_sort(
          names(.)
          , numeric = T
        )
      ) %>%
      pivot_longer(
        cols = !item
        , names_to = 'factor'
        , values_to = 'loading'
      ) -> df_loadings
    
    df_loadings %>% 
      group_by(item) %>% 
      filter(
        loading == 
          max(loading)
      ) %>% 
      ungroup() %>% 
      select(
        item,
        factor
      ) -> df_loadings
    
  }
  
  # Numeric data
  df_data_rows %>% 
    select(where(
      is.numeric
    )) %>% 
    as.matrix() -> 
    mtx_data_rows
  
  rm(df_data_rows)
  
  # Weights
  if(length(dbl_weights)){
    
    dbl_weights[
      1:nrow(mtx_data_rows)
    ] -> dbl_weights
    
  } else {
    
    rep(
      1, nrow(mtx_data_rows)
    ) -> dbl_weights
    
  }
  
  sqrt(dbl_weights) ->
    dbl_weights
  
  mtx_data_rows *
    dbl_weights ->
    mtx_data_rows
  
  # BVLS regressions
  set_names(
    1:ncol(mtx_data_rows)
    , colnames(mtx_data_rows)
  )  %>% 
    map(
      ~ coef(bvls(
        mtx_data_rows[,-.x]
        , mtx_data_rows[,.x]
        , bl = rep(
          dbl_scale_lb
          , ncol(mtx_data_rows) - 1
        )
        , bu = rep(
          dbl_scale_ub
          , ncol(mtx_data_rows) - 1
        )
      )) %>% 
        set_names(colnames(
          mtx_data_rows[,-.x]
        ))
    ) %>% 
    bind_rows(
      .id = 'item'
    ) %>% 
    relocate(
      item
      , colnames(
        mtx_data_rows
      )
    ) -> df_kflex_micro
  
  # Calculate capital micro-flexibility
  # Default value
  df_kflex_micro_intra_factor <- NULL
  
  # Intra-Factor Micro-Flexibility
  if(length(efa_model)){
    
    df_kflex_micro %>% 
      full_join(
        df_loadings
      ) %>%
      split(.$factor) %>%
      map(
        ~ .x[,.x$item] %>%
          rowMeans(
            na.rm = T
          ) %>% 
          set_names(
            .x$item
          ) %>% 
          as_tibble(
            rownames = 'item'
          )
      ) %>% 
      bind_rows(
        .id = 'factor'
      ) %>%
      rename(
        'item.kflex_micro' = 3
      ) -> df_kflex_micro_intra_factor
    
  }

  # Overall Micro-Flexibility
  rowMeans(
    df_kflex_micro[-1]
    , na.rm = T
  ) %>%
    set_names(
      df_kflex_micro[[1]]
    ) %>%
    as_tibble(
      rownames = 'item'
    ) %>%
    rename(
      'item.kflex_micro' = 2
    ) -> df_kflex_micro
  
  # Output
  return(compact(list(
    'intra_factor_micro_kflex' = df_kflex_micro_intra_factor,
    'overall_micro_kflex' = df_kflex_micro
  )))
  
}
