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
# # - Education -------------------------------------------------------------
# fun_education <- function(
    #     
#   .dbl_years_education
#   , .dbl_years_education_min
#   
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'.dbl_years_education' must be numeric." = 
#       is.numeric(.dbl_years_education)
#   )
#   
#   stopifnot(
#     "'.dbl_years_education_min' must be numeric." = 
#       is.numeric(.dbl_years_education_min)
#   )
#   
#   # Coerce arguments
#   .dbl_years_education[[1]] -> 
#     .dbl_years_education
#   
#   # Apply education similarity function
#   # Coefficient 1
#   .dbl_years_education >=
#     .dbl_years_education_min -> 
#     dbl_education
#   
#   as.numeric(dbl_education) -> 
#     dbl_education
#   
#   # # Coefficient 2
#   # .dbl_years_education /
#   #   .dbl_years_education_min ->
#   #   dbl_education
#   # 
#   # pmin(dbl_education, 1) -> 
#   #   dbl_education
#   
#   # Output
#   return(dbl_education)
#   
# }
# 
# df_occupations %>% 
#   mutate(
#     education_viable = 
#       fun_education(
#         .dbl_years_education = 21
#         , education_years
#       )
#   ) %>% 
#   filter(education_viable == 1) %>% 
#   filter(str_detect(str_to_lower(
#     occupation
#   ), 'medicine|pharma|physician|nurse'
#   ))
# 
# source('C:/Users/Cao/Documents/Github/atlas-research/functions/methods/fun_efa.R')
# install.packages('kselection')
# library(kselection)
# 
# norm(c(210,220) / norm(c(210,220), '2'), '2')
# 
# map(
#   df_occupations
#   , ~ .x
# ) -> lalala
# 
# lalala[2]
# 
# map_df(
#   df_occupations.t[-1]
#   , ~ .x / norm(.x, type = '2')
# ) -> dsds
# 
# map_df(
#   df_occupations.t[-1] / 100
#   , ~ fun_interchangeability(
#     .dbl_similarity = .x
#     , .dbl_scaling = 4
#   )
# ) -> dsds
# 
# dist(
#   t(dsds)
#   # t(df_occupations.t[-1])
#   , method = 'euclidean'
# ) -> dsds
# 
# wss <- (nrow(as.matrix(dsds))-1)*sum(apply(as.matrix(dsds),2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(dsds,
#                                      centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")
# 
# 
# kmeans(
#   dsds
#   , centers = 8
#   , iter.max = 500
# )$cluster %>%
#   as_tibble(
#     rownames = 'occupation'
#   ) %>%
#   rename(
#     kmeans_cluster = value
#   ) -> df_kmeans
# 
# fa(
#   r = df_occupations.t[-1]
#   , rotate = 'equamax'
#   , nfactors = 10
# ) -> lalala
# 
# loadings(lalala)[,] %>% 
#   as_tibble(
#     rownames = 'occupation'
#   ) %>% 
#   pivot_longer(
#     cols = -1
#     , names_to = 'factor'
#     , values_to = 'factor.score'
#   ) %>% 
#   group_by(occupation) %>% 
#   filter(
#     factor.score ==
#       max(factor.score)
#   ) %>% 
#   ungroup() -> dsdsds
# 
# df_occupations %>% 
#   filter(str_detect(
#     str_to_lower(
#       occupation
#     ), 'nurse|physician|medicine|hospital|art therapist'
#   )) %>% view
# 
# dsdsds %>% 
#   full_join(
#     df_occupations %>%
#       select(
#         occupation
#         , career_cluster
#       )
#   ) %>%
#   relocate(
#     occupation
#     , career_cluster
#   ) %>% view
#   # filter(str_detect(
#   #   str_to_lower(
#   #     occupation
#   #   ), 'nurse|physician|medicine|hospital|art therapist'
#   # )) %>% view
#   filter(
#     factor == 
#       nth(unique(
#         factor
#       ), 3)
#   ) %>% view
# 
# df_kmeans %>% 
#   full_join(
#     df_occupations
#   ) %>%
#   select(
#     occupation
#     , career_cluster
#     , kmeans_cluster
#   ) %>% 
#   filter(
#     kmeans_cluster == 1
#   ) %>% 
#   view
# # group_by(kmeans_cluster) %>%
# # tally()
# 
# hclust(dsds) -> lalala
# 
# cutree(lalala, h = 4) %>% view
# cutree(lalala, k = 10) %>% view
# 
# plot(lalala)
# 
# fun_efa.fa(
#   .df_data.numeric = 
#     df_occupations.t[-1] / 100
#   , .int_nfactors = 5
#   , .chr_rotation = 'equamax'
#   , .remove_unacceptable_MSAi.items = F
#   , .remove_under_loading.items = F
#   , .remove_cross_loading.items = F
# ) -> efa_occupations
# 
# 
# 
# df_occupations %>% 
#   select(
#     education_years
#     , entry_level_education
#   )
# 

# # - Interchangeability ----------------------------------------------------
# fun_interchangeability <- function(.dbl_similarity){
#   
#   # Arguments validation
#   stopifnot(
#     "'.dbl_similarity' must be numeric." =
#       is.numeric(.dbl_similarity)
#   )
#   
#   .dbl_similarity -> s
#   
#   # Interchangeability coefficients
#   # Coefficient 1
#   # dbl_interchangeability <- s
#   
#   # Coefficient 2
#   # dbl_interchangeability <- s ^ 2
#   # dbl_interchangeability <- s ^ 4
#   
#   # Coefficient 3
#   # dbl_interchangeability <- s * s + (1 - s) * s ^ 2
#   
#   # Coefficient 4
#   # dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^(1/s))
#   
#   # Coefficient 5
#   # dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^4)
#   
#   # Coefficient 6
#   dbl_interchangeability <- s ^ ((1/s)^(1/s))
#   # dbl_interchangeability <- s ^ ((1/s)^(3*(1/s)))
#   
#   # Output
#   return(dbl_interchangeability)
#   
# }
# 
# # curve(x^1)
# # curve(
# #   fun_interchangeability
# #   , add = T
# # )

# # - Interchangeability ----------------------------------------------------
# fun_interchangeability <- function(
#     # Similarity scores
#   .dbl_similarity
#   # Scaling factor
#   , .dbl_scaling = 1
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'.dbl_similarity' must be numeric." =
#       is.numeric(.dbl_similarity)
#   )
#   
#   stopifnot(
#     "'.dbl_scaling' must be numeric." =
#       is.numeric(.dbl_scaling)
#   )
#   
#   # Data wrangling
#   .dbl_scaling[[1]] -> sigma
#   
#   .dbl_similarity -> s
#   
#   # Interchangeability coefficients
#   # Coefficient 1
#   # dbl_interchangeability <- s
#   
#   # Coefficient 2
#   # dbl_interchangeability <- s ^ sigma
#   
#   # Coefficient 3
#   # dbl_interchangeability <- s * s + (1 - s) * s ^ sigma
#   
#   # Coefficient 4
#   # dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^(1/s))
#   
#   # Coefficient 5
#   # dbl_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^sigma)
#   
#   # Coefficient 6
#   # dbl_interchangeability <- s ^ ((1/s)^(1/s))
#   dbl_interchangeability <- s ^ ((1/s)^(sigma*(1/s)))
#   
#   # Output
#   return(dbl_interchangeability)
#   
# }

# - Interchangeability ---------------------------------------------------
fun_interchangeability <- function(

  .mtx_similarity
  , .dbl_scaling = 1
  , .dbl_years_education = NULL
  , .dbl_years_education_min = NULL

){

  # Argument validation
  stopifnot(
    "'.mtx_similarity' must be numeric." =
      is.numeric(as.matrix(.mtx_similarity))
  )

  stopifnot(
    "'.dbl_scaling' must be numeric." =
      is.numeric(.dbl_scaling)
  )

  stopifnot(
    "'.dbl_years_education' must be numeric." =
      any(
        is.null(.dbl_years_education)
        , is.numeric(.dbl_years_education)
      )
  )

  stopifnot(
    "'.dbl_years_education_min' must be a numeric vector the same length as '.mtx_similarity'." =
      any(
        is.null(.dbl_years_education_min)
        , is.numeric(.dbl_years_education_min)
      )
  )

  # Data wrangling
  .dbl_scaling[[1]] -> sigma

  cbind(.mtx_similarity) -> s

  # Apply coefficient
  # Coefficient 1
  # mtx_interchangeability <- s

  # Coefficient 2
  # mtx_interchangeability <- s ^ sigma

  # Coefficient 3
  # mtx_interchangeability <- s * s + (1 - s) * s ^ sigma

  # Coefficient 4
  # mtx_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^(1/s))

  # Coefficient 5
  # mtx_interchangeability <- s * s + (1 - s) * s ^ ((1/s)^sigma)

  # Coefficient 6
  # mtx_interchangeability <- s ^ ((1/s)^(1/s))
  mtx_interchangeability <- s ^ ((1/s)^((sigma/s)))

  if(all(
    length(.dbl_years_education_min)
    , length(.dbl_years_education)
  )){

    .dbl_years_education_min[
      seq(1, nrow(s))
    ] -> edu_min

    .dbl_years_education[[1]] /
      edu_min -> edu_ratio

    edu_ratio ^
      ((1/edu_ratio) ^
         ((edu_min/edu_ratio))) -> mtx_edu

    pmin(mtx_edu, 1) *
      mtx_interchangeability *
      mtx_interchangeability ->
      mtx_interchangeability

  }

  # Rownames and Colnames
  if(length(rownames(s))){

    rownames(s) ->
      rownames(mtx_interchangeability)
  }

  if(length(colnames(s))){

    colnames(s) ->
      colnames(mtx_interchangeability)
  }

  # Output
  return(mtx_interchangeability)

}

# df_models %>%
#   full_join(
#     df_occupations
#   ) %>%
#   mutate(
#     I =
#       fun_interchangeability(
#         .mtx_similarity =
#           bvls.wgt
#         , .dbl_scaling = 10
#         , .dbl_years_education = 21
#         , .dbl_years_education_min =
#           education_years
#       ) %>% as.numeric()
#   ) %>%
#   select(
#     occupation
#     , I
#     , bvls.wgt
#     , employment2
#   ) %>%
#   mutate(
#     jobs =
#       round(I * employment2)
#   ) %>%
#   filter(str_detect(
#     str_to_lower(occupation)
#     # , 'hospitalist'
#     # , '^maids'
#     # , '^fallers'
#     # , 'statistician'
#     # , '^statistician'
#     , 'economist'
#   ))
#
#
# plot(
#   fun_interchangeability(
#     seq(0,1,0.01)
#     , .dbl_scaling =
#       df_occupations %>%
#       filter(str_detect(
#         str_to_lower(occupation)
#         # , 'hospitalist'
#         # , '^maids'
#         # , '^fallers'
#         # , 'statistician'
#         # , '^statistician'
#         , 'economist'
#       )) %>%
#       reframe(
#         kflex = fun_kflex(
#           c_across(ends_with('.l'))
#           , .dbl_scale.lb = 0
#           , .dbl_scale.ub = 100
#         )
#       ) %>%
#       pull()
#   )
# )

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

# # - Estimate coefficients for a single professional profile -----------
# fun_employability.workflow <- function(
    #     .df_data
#     , .df_query
#     , .int_employment
#     , .dbl_wages.market = NULL
#     , .dbl_wage.current = NULL
#     , .dbl_scale.ub = 100
#     , .dbl_overqualification.threshold = 0
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'.df_data' must be a data frame." =
#       is.data.frame(.df_data)
#   )
#   
#   stopifnot(
#     "'.dbl_wages.market' must be numeric." =
#       any(
#         is.numeric(.dbl_wages.market)
#         , is.null(.dbl_wages.market)
#       )
#   )
#   
#   stopifnot(
#     "'.int_employment' and '.dbl_wages.market' must be the same length." =
#       any(
#         length(.int_employment) ==
#           length(.dbl_wages.market)
#         , is.null(.dbl_wages.market)
#       )
#   )
#   
#   # Data wrangling
#   intersect(
#     .df_query %>%
#       # select(where(
#       #   is.numeric
#       # )) %>%
#       names()
#     , .df_data %>%
#       # select(where(
#       #   is.numeric
#       # )) %>%
#       names()
#   ) -> chr_cols
#   
#   .df_query %>%
#     select(
#       chr_cols
#     ) -> .df_query
#   
#   .df_data %>%
#     select(
#       chr_cols
#     ) -> df_data.numeric
#   
#   # Calculate interchangeability
#   fun_knn.alpha(
#     .df_data = 
#       df_data.numeric
#     , .df_query = 
#       .df_query
#     , .dbl_scale.ub = 
#       .dbl_scale.ub
#     , .dbl_overqualification.threshold = 
#       .dbl_overqualification.threshold
#   ) %>% 
#     mutate(
#       interchangeability = 
#         fun_interchangeability(
#           similarity
#         )
#     ) -> df_knn.alpha
#   
#   # Calculate employability
#   df_knn.alpha %>% 
#     reframe(
#       employability =
#         fun_employability(
#           .int_employment =
#             .int_employment
#           , .dbl_interchangeability =
#             interchangeability
#         )
#     ) -> df_employability
#   
#   
#   # If wages are provided, calculate utility-consistent employability
#   if(length(.dbl_wages.market)){
#     
#     stopifnot(
#       "'.dbl_wage.current' missing with no default." = 
#         !is.null(.dbl_wage.current)
#     )
#     
#     stopifnot(
#       "'.dbl_wage.current' must be numeric." = 
#         is.numeric(.dbl_wage.current)
#     )
#     
#     df_employability %>%
#       mutate(
#         employability.optimal = 
#           fun_employability.optimal(
#             .int_employment = 
#               .int_employment
#             , .dbl_wage.current = 
#               .dbl_wage.current
#             , .dbl_wages.market = 
#               .dbl_wages.market
#             , .dbl_interchangeability =
#               df_knn.alpha$
#               interchangeability
#           )) -> df_employability
#     
#   }
#   
#   # Output
#   return(list(
#     'interchangeability' = df_knn.alpha
#     , 'employability' = df_employability
#   ))
#   
# }
# 
# # - Estimate coefficients for multiple professional profiles -----------
# fun_employability.workflow.m <- function(
    #     .df_data
#     , .int_employment
#     , .dbl_wages = NULL
#     , .dbl_scale.ub = 100
#     , .dbl_overqualification.threshold = 0
# ){
#   
#   # Arguments validation
#   stopifnot(
#     "'.df_data' must be a data frame." =
#       is.data.frame(.df_data)
#   )
#   
#   stopifnot(
#     "'.dbl_wages' must be numeric." =
#       any(
#         is.numeric(.dbl_wages)
#         , is.null(.dbl_wages)
#       )
#   )
#   
#   stopifnot(
#     "'.int_employment' and '.dbl_wages.market' must be the same length." =
#       any(
#         length(.int_employment) ==
#           length(.dbl_wages)
#         , is.null(.dbl_wages)
#       )
#   )
#   
#   # Calculate interchangeability for each row
#   .df_data %>%
#     group_nest(
#       row_number()
#     ) %>%
#     pull(data) %>%
#     map(
#       ~ fun_knn.alpha(
#         .df_data = .df_data
#         , .df_query = .x
#         , .dbl_scale.ub =
#           .dbl_scale.ub
#         , .dbl_overqualification.threshold = 
#           .dbl_overqualification.threshold
#       ) %>% 
#         mutate(
#           interchangeability = 
#             fun_interchangeability(
#               similarity
#             )
#         )
#     ) -> list_knn.alpha
#   
#   # Calculate employability for each row
#   list_knn.alpha %>% 
#     map(
#       ~ .x %>% 
#         reframe(
#           employability = 
#             fun_employability(
#               .int_employment = 
#                 .int_employment
#               , .dbl_interchangeability =
#                 interchangeability
#             ))
#     ) %>% 
#     bind_rows() -> df_employability
#   
#   
#   # If wages are provided, calculate utility-consistent employability 
#   if(length(.dbl_wages)){
#     
#     df_employability %>%
#       mutate(
#         employability.optimal = 
#           map_dbl(
#             list_knn.alpha
#             , ~ fun_employability.optimal(
#               .int_employment = 
#                 .int_employment
#               , .dbl_wage.current = 
#                 .dbl_wages[n()]
#               , .dbl_wages.market = 
#                 .dbl_wages
#               , .dbl_interchangeability =
#                 .x$interchangeability
#             ))
#       ) -> df_employability
#     
#   }
#   
#   # Join employability back to original data frame
#   .df_data %>% 
#     bind_cols(
#       df_employability
#     ) -> df_employability
#   
#   # Output
#   return(list(
#     'interchangeability' = list_knn.alpha
#     , 'employability' = df_employability
#   ))
#   
# }
# 

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
  
  # Output
  return(dbl_competitiveness)
  
}

# # dsds --------------------------------------------------------------------
# start <- Sys.time()
# fun_employability.workflow.m(
#   .df_data = df_occupations.ai
#   , .int_employment = 
#     df_occupations$
#     employment2
#   , .dbl_wages = 
#     df_occupations$
#     annual_wage_2021
#   , .dbl_scale.ub = 100
#   # , .dbl_overqualification.threshold = 33
#   , .dbl_overqualification.threshold = 0
# ) -> dsdsds
# end <- Sys.time()
# end - start
# 
# fun_competitiveness(
#   .int_applicants = 
#     df_occupations$
#     employment2[1] * 1.25
#   , .int_jobs = 
#     df_occupations$
#     employment2[1]
# )
# 
# dsdsds$
#   employability %>% 
#   select(
#     !ends_with('.l')
#   ) %>%
#   arrange(
#     employability
#   )
# 
# fun_knn.alpha(
#   .df_data = 
#     df_occupations.ai %>% 
#     select(names(df_sample))
#   , .df_query = df_sample
#   , .dbl_scale.ub = 100
# ) %>%
#   select(
#     !ends_with('.l')
#   ) %>% 
#   mutate(
#     interchangeability = 
#       fun_interchangeability(
#         similarity
#       ) %>% 
#       round(4)
#   ) %>% 
#   arrange(
#     distance
#   ) %>% 
#   print(n = 20)
#   
# 
# df_occupations.ai %>% 
#   slice_sample(n = 1) -> dsds
# 
# fun_knn.alpha(
#   .df_data = 
#     df_occupations.ai
#   , .df_query = dsds
#   , .dbl_scale.ub = 100
# ) %>%
#   select(
#     !ends_with('.l')
#   ) %>% 
#   arrange(
#     distance
#   ) %>% 
#   mutate(
#     interchangeability = 
#       fun_interchangeability(
#         similarity
#       )
#   ) %>% 
#   print(n = 20)
# 
# fun_employability.workflow(
#   .df_data = 
#     df_occupations.ai %>% 
#     select(names(df_sample))
#   , .df_query = df_sample
#   , .int_employment = 
#     df_occupations$
#     employment2
#   , .dbl_wages.market = 
#     df_occupations$
#     annual_wage_2021
#   , .dbl_wage.current = 
#     6000 * 12
#   , .dbl_scale.ub = 100
#   # , .dbl_overqualification.threshold = 100
#   , .dbl_overqualification.threshold = NULL
# )[[1]] %>%
#   select(
#     !ends_with('.l')
#   ) %>% view
#   arrange(
#     distance
#   )
# 
# fun_knn.alpha(
#   .df_data = df_occupations.ai[1,]
#   , .df_query = 
#     df_occupations.ai %>% 
#     filter(str_detect(
#       str_to_lower(
#         occupation
#       )
#       , 'chef'
#     ))
#   , .dbl_scale.ub = 100
# ) %>% 
#   # mutate(
#   #   .before = 1
#   #   , row.sum = 
#   #     rowSums(.)
#   #   , occupation =
#   #     df_occupations$
#   #     occupation
#   # ) %>% view
#   select(
#     !ends_with('.l')
#   ) %>%
#   # mutate(across(
#   #   .cols = where(is.numeric)
#   #   , .fns = ~ round(.x, 2)
#   # )) %>% 
#   arrange(
#     -distance
#   ) %>% 
#   filter(str_detect(
#     str_to_lower(
#       occupation
#     )
#     , 'chief'
#   ))
# 
# 
# lalala[[2]] - 
#   lalala[[1]] -> df_dist
# 
# 
# df_dist[
#   lalala[[2]] <= 17
#   & df_dist < -17
# ] <- 0
# 
# lalala[1,1:3]
# lalala[[1]][1,1:3]
# lalala[[2]][1,1:3]
# df_dist[1,1:3]
# 
# 
# fun_knn.alpha(
#   .df_data = 
#     df_occupations.ai #%>% 
#   # select(names(df_sample))
#   # , .df_query = df_sample
#   , .df_query = 
#     df_occupations.ai %>% 
#     slice_sample(n = 1)
#   , .dbl_scale.ub = 100
#   , .dbl_overqualification.threshold = 0
# ) %>% 
#   select(
#     !ends_with('.l')
#   ) %>%
#   arrange(
#     distance
#   ) -> dsds
# 
# dsds %>% 
#   print(n = 30)
# dsds %>% 
#   mutate(
#     interchangeability = 
#       fun_interchangeability(similarity)
#   ) %>% 
#   arrange(interchangeability)
# 
# 
# 
# dsds %>% 
#   arrange(
#     -distance
#   ) %>%
#   print(n = 30)
# 
# dsdsds$
#   employability %>% 
#   select(
#     !ends_with('.l')
#   ) %>%
#   arrange(
#     -employability
#   ) %>% 
#   mutate(across(
#     .cols = where(is.numeric)
#     , .fns = ~ round(.x, 2)
#   ))
# 
# fun_knn.alpha(
#   .df_data = df_occupations.ai
#   , .df_query = 
#     df_occupations.ai %>% 
#     filter(str_detect(
#       str_to_lower(
#         occupation
#       )
#       , 'fire'
#     )) %>% 
#     slice_sample(n = 1)
#   , .dbl_scale.ub = 100
# ) %>% 
#   select(
#     !ends_with('.l')
#   ) %>% 
#   arrange(
#     distance
#   )
# 
# fun_knn.alpha(
#   .df_data = df_occupations.ai
#   , .df_query = 
#     df_occupations.ai %>% 
#     filter(str_detect(
#       str_to_lower(
#         occupation
#       )
#       , 'model'
#     )) %>% 
#     slice_sample(n = 1)
#   , .dbl_scale.ub = 100
# ) %>% 
#   select(
#     !ends_with('.l')
#   ) %>% 
#   arrange(
#     -distance
#   )
# 
# 
# fun_employability.workflow(
#   .df_data = df_occupations.ai
#   , .df_query = df_sample
#   , .int_employment = 
#     df_occupations$
#     employment2
#   , .dbl_wage.current = 
#     6000 * 12
#   , .dbl_wages.market = 
#     df_occupations$
#     annual_wage_2021
#   , .dbl_scale.ub = 100
# )[[1]] %>% 
#   select(
#     occupation
#     , distance
#     , similarity
#     , interchangeability
#   ) %>% 
#   mutate(
#     interchangeability = 
#       round(interchangeability, 4)
#   ) %>%
#   arrange(
#     -interchangeability
#     # -distance
#   ) %>% 
#   print(n = 100)
# 
# fun_employability.workflow(
#   .df_data = df_occupations.ai
#   , .df_query = df_sample
#   , .int_employment = 
#     df_occupations$
#     employment2
#   , .dbl_wage.current = 
#     6000 * 12
#   , .dbl_wages.market = 
#     df_occupations$
#     annual_wage_2021
#   , .dbl_scale.ub = 100
# )[[1]] %>% 
#   select(
#     occupation
#     , distance
#     , similarity
#     , interchangeability
#   ) %>% 
#   arrange(
#     distance
#   ) %>% 
#   print(n = 100)
# 
# # dsdsds$
# #   interchangeability[[
# #     sample(1:873,1)
# #   ]] %>% 
# dsdsds$
#   interchangeability[[
#     df_occupations %>% 
#       mutate(
#         n = row_number()
#       ) %>% 
#       filter(str_detect(
#         str_to_lower(
#           occupation
#         )
#         , 'doct|medic|physician'
#       )) %>% 
#       slice_sample(n = 1) %>% 
#       pull(n)
#   ]] %>% 
#   select(
#     occupation
#     , distance
#     , similarity
#     , interchangeability
#   ) %>% 
#   arrange(
#     distance
#   ) %>%
#   slice(
#     1:10
#     , 862:873
#   ) %>% 
#   print(n = nrow(.))
# 
# dsdsds$
#   interchangeability[[
#     df_occupations %>% 
#       mutate(
#         n = row_number()
#       ) %>% 
#       filter(str_detect(
#         str_to_lower(
#           occupation
#         )
#         , 'maid'
#       )) %>% 
#       slice_sample(n = 1) %>% 
#       pull(n)
#   ]] %>% 
#   select(
#     occupation
#     , distance
#     , similarity
#     , interchangeability
#   ) %>% 
#   arrange(
#     distance
#   ) %>%
#   slice(
#     1:10
#     , 862:873
#   ) %>% 
#   print(n = nrow(.))
# 
# df_occupations %>% 
#   mutate(
#     n = row_number()
#   ) %>% 
#   filter(str_detect(
#     str_to_lower(
#       occupation
#     )
#     , 'doct|medic|physician'
#   )) %>% 
#   slice_sample(n = 1) %>% 
#   pull(n)
# 
# dsdsds$
#   interchangeability[[
#     sample(1:873,1)
#   ]] %>% 
#   select(
#     occupation
#     , distance
#     , similarity
#     , interchangeability
#   ) %>% 
#   arrange(
#     distance
#   ) %>% 
#   pivot_longer(
#     cols = c(
#       similarity
#       , interchangeability
#     )
#     , names_to = 'metric'
#     , values_to = 'value'
#   ) %>% 
#   fun_plot.density(aes(
#     x = value
#     , fill = metric
#   )
#   , .list_axis.x.args = list(
#     limits = c(-.1, 1.1)
#     , breaks = seq(0, 1, 0.25)
#   )
#   , .fun_format.x = percent
#   )
# 
# 
# 
# dsdsds$
#   employability %>% 
#   select(
#     occupation
#     , employability
#     , employability.optimal
#   ) %>% 
#   arrange(desc(
#     employability
#   )) %>% 
#   print(n = 100)
# 
# fun_employability.m(
#   .df_data = df_occupations.ai
#   , .int_employment = 
#     df_occupations$
#     employment2
#   , .dbl_wages = 
#     df_occupations$
#     annual_wage_2021
#   , .dbl_scale.ub = 100
# ) -> lalala
# 
# 
