# [SETUP] -----------------------------------------------------------------
# - Packages --------------------------------------------------------------
pkg <- c(
  'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# - Functions -------------------------------------------------------------


# [FUNCTIONS] -------------------------------------------------------------
# - Interchangeability ----------------------------------------------------
fun_interchangeability <- function(.dbl_similarity){
  
  # Arguments validation
  stopifnot(
    "'.dbl_similarity' must be numeric." =
      is.numeric(.dbl_similarity)
  )
  
  .dbl_similarity -> s
  
  # Interchangeability coefficients
  # Coefficient 1
  # dbl_interchangeability <- s
  
  # Coefficient 2
  # dbl_interchangeability <- s ^ 2
  # dbl_interchangeability <- s ^ 4
  
  # Coefficient 3
  # dbl_interchangeability <- s * s + (1 - s) * s ^ 2
  
  # Coefficient 4
  # dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^(1/s))
  
  # Coefficient 5
  # dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^4)
  
  # Coefficient 6
  # dbl_interchangeability <- s ^ ((1/s)^(1/s))
  dbl_interchangeability <- s ^ ((1/s)^(4*(1/s)))
  
  # Output
  return(dbl_interchangeability)
  
}

# - Employability ---------------------------------------------------------
fun_employability <- function(
    .int_employment
    , .dbl_interchangeability
){
  
  # Arguments validation
  stopifnot(
    "'.int_employment' must be numeric." =
      is.numeric(.int_employment)
  )
  
  stopifnot(
    "'.dbl_interchangeability' must be numeric." =
      is.numeric(.dbl_interchangeability)
  )
  
  stopifnot(
    "'.int_employment' and '.dbl_interchangeability' must be the same length." =
      length(.int_employment) ==
      length(.dbl_interchangeability)
  )
  
  # Coerce employment to integer
  round(.int_employment) -> .int_employment
  
  # Rename variables
  .int_employment -> n
  .dbl_interchangeability -> I
  
  rm(.int_employment)
  rm(.dbl_interchangeability)
  
  # Estimate employability coefficient
  sum(I * n) / sum(n) -> dbl_employability
  
  # Output
  return(dbl_employability)
  
}

# - Utility-Consistent Employability ---------------------------------------------------------
fun_employability.optimal <- function(
    .int_employment
    , .dbl_wage.current
    , .dbl_wages.market
    , .dbl_interchangeability
){
  
  # Arguments validation
  stopifnot(
    "'.int_employment' must be numeric." =
      is.numeric(.int_employment)
  )
  
  stopifnot(
    "'.dbl_wage.current' must be numeric." =
      is.numeric(.dbl_wage.current)
  )
  
  stopifnot(
    "'.dbl_wages.market' must be numeric." =
      is.numeric(.dbl_wages.market)
  )
  
  stopifnot(
    "'.dbl_interchangeability' must be numeric." =
      is.numeric(.dbl_interchangeability)
  )
  
  stopifnot(
    "'.int_employment', '.dbl_wages.market', and '.dbl_interchangeability' must be the same length." =
      all(
        length(.int_employment) ==
          length(.dbl_interchangeability)
        , length(.dbl_wages.market) ==
          length(.dbl_interchangeability)
      )
  )
  
  # Coerce wage to one element
  .dbl_wage.current[[1]] -> .dbl_wage.current
  
  # Coerce employment to integer
  round(.int_employment) -> .int_employment
  
  # Rename variables
  .int_employment -> n
  .dbl_interchangeability -> I
  
  rm(.int_employment)
  rm(.dbl_interchangeability)
  
  # Calculate utility of changing jobs
  as.numeric(
    .dbl_wages.market > 
      .dbl_wage.current
  ) -> U
  
  # Estimate employability coefficient
  sum(I * n * U) / sum(n) -> dbl_employability.optimal
  
  # Output
  return(dbl_employability.optimal)
  
}

# - Estimate coefficients for a single professional profile -----------
fun_employability.workflow <- function(
    .df_data
    , .df_query
    , .int_employment
    , .dbl_wages.market = NULL
    , .dbl_wage.current = NULL
    , .dbl_scale.ub = 100
    , .dbl_overqualification.threshold = 0
){
  
  # Arguments validation
  stopifnot(
    "'.df_data' must be a data frame." =
      is.data.frame(.df_data)
  )
  
  stopifnot(
    "'.dbl_wages.market' must be numeric." =
      any(
        is.numeric(.dbl_wages.market)
        , is.null(.dbl_wages.market)
      )
  )
  
  stopifnot(
    "'.int_employment' and '.dbl_wages.market' must be the same length." =
      any(
        length(.int_employment) ==
          length(.dbl_wages.market)
        , is.null(.dbl_wages.market)
      )
  )
  
  # Data wrangling
  intersect(
    .df_query %>%
      # select(where(
      #   is.numeric
      # )) %>%
      names()
    , .df_data %>%
      # select(where(
      #   is.numeric
      # )) %>%
      names()
  ) -> chr_cols
  
  .df_query %>%
    select(
      chr_cols
    ) -> .df_query
  
  .df_data %>%
    select(
      chr_cols
    ) -> df_data.numeric
  
  # Calculate interchangeability
  fun_knn.alpha(
    .df_data = 
      df_data.numeric
    , .df_query = 
      .df_query
    , .dbl_scale.ub = 
      .dbl_scale.ub
    , .dbl_overqualification.threshold = 
      .dbl_overqualification.threshold
  ) %>% 
    mutate(
      interchangeability = 
        fun_interchangeability(
          similarity
        )
    ) -> df_knn.alpha
  
  # Calculate employability
  df_knn.alpha %>% 
    reframe(
      employability =
        fun_employability(
          .int_employment =
            .int_employment
          , .dbl_interchangeability =
            interchangeability
        )
    ) -> df_employability
  
  
  # If wages are provided, calculate utility-consistent employability
  if(length(.dbl_wages.market)){
    
    stopifnot(
      "'.dbl_wage.current' missing with no default." = 
        !is.null(.dbl_wage.current)
    )
    
    stopifnot(
      "'.dbl_wage.current' must be numeric." = 
        is.numeric(.dbl_wage.current)
    )
    
    df_employability %>%
      mutate(
        employability.optimal = 
          fun_employability.optimal(
            .int_employment = 
              .int_employment
            , .dbl_wage.current = 
              .dbl_wage.current
            , .dbl_wages.market = 
              .dbl_wages.market
            , .dbl_interchangeability =
              df_knn.alpha$
              interchangeability
          )) -> df_employability
    
  }
  
  # Output
  return(list(
    'interchangeability' = df_knn.alpha
    , 'employability' = df_employability
  ))
  
}

# - Estimate coefficients for multiple professional profiles -----------
fun_employability.workflow.m <- function(
    .df_data
    , .int_employment
    , .dbl_wages = NULL
    , .dbl_scale.ub = 100
    , .dbl_overqualification.threshold = 0
){
  
  # Arguments validation
  stopifnot(
    "'.df_data' must be a data frame." =
      is.data.frame(.df_data)
  )
  
  stopifnot(
    "'.dbl_wages' must be numeric." =
      any(
        is.numeric(.dbl_wages)
        , is.null(.dbl_wages)
      )
  )
  
  stopifnot(
    "'.int_employment' and '.dbl_wages.market' must be the same length." =
      any(
        length(.int_employment) ==
          length(.dbl_wages)
        , is.null(.dbl_wages)
      )
  )
  
  # Calculate interchangeability for each row
  .df_data %>%
    group_nest(
      row_number()
    ) %>%
    pull(data) %>%
    map(
      ~ fun_knn.alpha(
        .df_data = .df_data
        , .df_query = .x
        , .dbl_scale.ub =
          .dbl_scale.ub
        , .dbl_overqualification.threshold = 
          .dbl_overqualification.threshold
      ) %>% 
        mutate(
          interchangeability = 
            fun_interchangeability(
              similarity
            )
        )
    ) -> list_knn.alpha
  
  # Calculate employability for each row
  list_knn.alpha %>% 
    map(
      ~ .x %>% 
        reframe(
          employability = 
            fun_employability(
              .int_employment = 
                .int_employment
              , .dbl_interchangeability =
                interchangeability
            ))
    ) %>% 
    bind_rows() -> df_employability
  
  
  # If wages are provided, calculate utility-consistent employability 
  if(length(.dbl_wages)){
    
    df_employability %>%
      mutate(
        employability.optimal = 
          map_dbl(
            list_knn.alpha
            , ~ fun_employability.optimal(
              .int_employment = 
                .int_employment
              , .dbl_wage.current = 
                .dbl_wages[n()]
              , .dbl_wages.market = 
                .dbl_wages
              , .dbl_interchangeability =
                .x$interchangeability
            ))
      ) -> df_employability
    
  }
  
  # Join employability back to original data frame
  .df_data %>% 
    bind_cols(
      df_employability
    ) -> df_employability
  
  # Output
  return(list(
    'interchangeability' = list_knn.alpha
    , 'employability' = df_employability
  ))
  
}

# - Competitiveness -------------------------------------------------------
fun_competitiveness <- function(.int_applicants, .int_jobs){
  
  # Arguments validation
  stopifnot(
    "'.int_applicants' must be numeric." =
      is.numeric(.int_applicants)
  )
  
  stopifnot(
    "'.int_jobs' must be numeric." =
      is.numeric(.int_jobs)
  )
  
  # Coerce to integer
  round(.int_applicants) -> .int_applicants
  round(.int_jobs) -> .int_jobs
  
  pmax(.int_applicants, 0) -> .int_applicants
  pmax(.int_jobs, 0) -> .int_jobs
  
  # Jobs-to-applicants ratio
  .int_applicants / 
    .int_jobs  -> dbl_competitiveness
  
  # 1 -
  #   .int_jobs / 
  #   .int_applicants -> dbl_competitiveness
  # 
  # Output
  return(dbl_competitiveness)
  
}

# dsds --------------------------------------------------------------------
start <- Sys.time()
fun_employability.workflow.m(
  .df_data = df_occupations.ai
  , .int_employment = 
    df_occupations$
    employment2
  , .dbl_wages = 
    df_occupations$
    annual_wage_2021
  , .dbl_scale.ub = 100
  , .dbl_overqualification.threshold = 33
) -> dsdsds
end <- Sys.time()
end - start

fun_competitiveness(
  .int_applicants = df_occupations$employment2[1] * 0
  , .int_jobs = df_occupations$employment2[1]
  )

dsdsds$
  employability %>% 
  select(
    !ends_with('.l')
  ) %>% 
  arrange(employability)

fun_employability.workflow(
  .df_data = 
    df_occupations.ai %>% 
    select(names(df_sample))
  , .df_query = df_sample
  , .int_employment = 
    df_occupations$
    employment2
  , .dbl_wages.market = 
    df_occupations$
    annual_wage_2021
  , .dbl_wage.current = 
    6000 * 12
  , .dbl_scale.ub = 100
  , .dbl_overqualification.threshold = 33
)[[1]] %>% 
  select(
    !ends_with('.l')
  ) %>% 
  arrange(distance)

fun_knn.alpha(
  .df_data = df_occupations.ai[1,]
  , .df_query = 
    df_occupations.ai %>% 
    filter(str_detect(
      str_to_lower(
        occupation
      )
      , 'chef'
    ))
  , .dbl_scale.ub = 100
) %>% 
  # mutate(
  #   .before = 1
  #   , row.sum = 
  #     rowSums(.)
  #   , occupation =
  #     df_occupations$
  #     occupation
  # ) %>% view
  select(
    !ends_with('.l')
  ) %>%
  # mutate(across(
  #   .cols = where(is.numeric)
  #   , .fns = ~ round(.x, 2)
  # )) %>% 
  arrange(
    -distance
  ) %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    )
    , 'chief'
  ))


lalala[[2]] - 
  lalala[[1]] -> df_dist


df_dist[
  lalala[[2]] <= 17
  & df_dist < -17
] <- 0

lalala[1,1:3]
lalala[[1]][1,1:3]
lalala[[2]][1,1:3]
df_dist[1,1:3]


fun_knn.alpha(
  .df_data = 
    df_occupations.ai #%>% 
  # select(names(df_sample))
  # , .df_query = df_sample
  , .df_query = 
    df_occupations.ai %>% 
    slice_sample(n = 1)
  , .dbl_scale.ub = 100
  , .dbl_overqualification.threshold = 0
) %>% 
  select(
    !ends_with('.l')
  ) %>%
  arrange(
    distance
  ) -> dsds

dsds %>% 
  print(n = 30)
dsds %>% 
  mutate(
    interchangeability = 
      fun_interchangeability(similarity)
  ) %>% 
  arrange(interchangeability)



dsds %>% 
  arrange(
    -distance
  ) %>%
  print(n = 30)

dsdsds$
  employability %>% 
  select(
    !ends_with('.l')
  ) %>%
  arrange(
    -employability
  ) %>% 
  mutate(across(
    .cols = where(is.numeric)
    , .fns = ~ round(.x, 2)
  ))

fun_knn.alpha(
  .df_data = df_occupations.ai
  , .df_query = 
    df_occupations.ai %>% 
    filter(str_detect(
      str_to_lower(
        occupation
      )
      , 'fire'
    )) %>% 
    slice_sample(n = 1)
  , .dbl_scale.ub = 100
) %>% 
  select(
    !ends_with('.l')
  ) %>% 
  arrange(
    distance
  )

fun_knn.alpha(
  .df_data = df_occupations.ai
  , .df_query = 
    df_occupations.ai %>% 
    filter(str_detect(
      str_to_lower(
        occupation
      )
      , 'model'
    )) %>% 
    slice_sample(n = 1)
  , .dbl_scale.ub = 100
) %>% 
  select(
    !ends_with('.l')
  ) %>% 
  arrange(
    -distance
  )


fun_employability.workflow(
  .df_data = df_occupations.ai
  , .df_query = df_sample
  , .int_employment = 
    df_occupations$
    employment2
  , .dbl_wage.current = 
    6000 * 12
  , .dbl_wages.market = 
    df_occupations$
    annual_wage_2021
  , .dbl_scale.ub = 100
)[[1]] %>% 
  select(
    occupation
    , distance
    , similarity
    , interchangeability
  ) %>% 
  mutate(
    interchangeability = 
      round(interchangeability, 4)
  ) %>%
  arrange(
    -interchangeability
    # -distance
  ) %>% 
  print(n = 100)

fun_employability.workflow(
  .df_data = df_occupations.ai
  , .df_query = df_sample
  , .int_employment = 
    df_occupations$
    employment2
  , .dbl_wage.current = 
    6000 * 12
  , .dbl_wages.market = 
    df_occupations$
    annual_wage_2021
  , .dbl_scale.ub = 100
)[[1]] %>% 
  select(
    occupation
    , distance
    , similarity
    , interchangeability
  ) %>% 
  arrange(
    distance
  ) %>% 
  print(n = 100)

# dsdsds$
#   interchangeability[[
#     sample(1:873,1)
#   ]] %>% 
dsdsds$
  interchangeability[[
    df_occupations %>% 
      mutate(
        n = row_number()
      ) %>% 
      filter(str_detect(
        str_to_lower(
          occupation
        )
        , 'doct|medic|physician'
      )) %>% 
      slice_sample(n = 1) %>% 
      pull(n)
  ]] %>% 
  select(
    occupation
    , distance
    , similarity
    , interchangeability
  ) %>% 
  arrange(
    distance
  ) %>%
  slice(
    1:10
    , 862:873
  ) %>% 
  print(n = nrow(.))

dsdsds$
  interchangeability[[
    df_occupations %>% 
      mutate(
        n = row_number()
      ) %>% 
      filter(str_detect(
        str_to_lower(
          occupation
        )
        , 'maid'
      )) %>% 
      slice_sample(n = 1) %>% 
      pull(n)
  ]] %>% 
  select(
    occupation
    , distance
    , similarity
    , interchangeability
  ) %>% 
  arrange(
    distance
  ) %>%
  slice(
    1:10
    , 862:873
  ) %>% 
  print(n = nrow(.))

df_occupations %>% 
  mutate(
    n = row_number()
  ) %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    )
    , 'doct|medic|physician'
  )) %>% 
  slice_sample(n = 1) %>% 
  pull(n)

dsdsds$
  interchangeability[[
    sample(1:873,1)
  ]] %>% 
  select(
    occupation
    , distance
    , similarity
    , interchangeability
  ) %>% 
  arrange(
    distance
  ) %>% 
  pivot_longer(
    cols = c(
      similarity
      , interchangeability
    )
    , names_to = 'metric'
    , values_to = 'value'
  ) %>% 
  fun_plot.density(aes(
    x = value
    , fill = metric
  )
  , .list_axis.x.args = list(
    limits = c(-.1, 1.1)
    , breaks = seq(0, 1, 0.25)
  )
  , .fun_format.x = percent
  )



dsdsds$
  employability %>% 
  select(
    occupation
    , employability
    , employability.optimal
  ) %>% 
  arrange(desc(
    employability
  )) %>% 
  print(n = 100)

fun_employability.m(
  .df_data = df_occupations.ai
  , .int_employment = 
    df_occupations$
    employment2
  , .dbl_wages = 
    df_occupations$
    annual_wage_2021
  , .dbl_scale.ub = 100
) -> lalala


