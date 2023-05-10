# ------ CODE -------------------------------------------------------------
# --- SETUP -------------------------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
# Load all the packages that will be used in every file
# This way, packages are loaded only once
pkg <- c(
  'psych' #Factor Analysis
  , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
  , 'jsonify' #Work with JSON (faster than jsonlite)
  , 'ggthemes' #Data visualization
  , 'tidyverse', 'stringi', 'glue' #Data wrangling
  , 'tinytex' #LaTeX
  , 'modeest' #Mode
  , 'knitr' #Knitr
  , 'readxl' #Import excel (use other package?)
  , 'rlang'
  , 'scales'
  , 'viridis'
  , 'geomtextpath'
  , 'aws.s3' #Work with AWS Lambda and R
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# BUCKETS -------------------------------------------------------------
list_bucket.fun <- 's3://atlas-platform-dev/'

list_bucket.data <- 's3://atlas-platform-dev/'

list_bucket.render <- 's3://atlas-bubble-dev/'

# OUTPUT NAME -------------------------------------------------------------
save_object(
  'R/fun_unique_name2.R'
  , file = '/tmp/fun_unique_name2.R'
  , bucket = list_bucket.fun
  , region = 'us-east-2'
)

source('/tmp/fun_unique_name2.R')

fun_name.unique(chr_suffix = '.pdf') -> chr_file.name

# ------ HANDLER FUNCTION -------------------------------------------------
fun_handler <- function(body, ...){  
  
  # FILES --------------------------------------------------------------
  # Function files
  list(
    # KNN matching
    'fun_matching.R'
    # Factor scores
    , 'fun_scores.R'
    # Automated plotting
    , 'fun_plots.R'
    # Dynamic text
    , 'fun_texts.R'
    , 'fun_commas.R'
    # Capital flexibility
    , 'fun_kflex.R'
    # Random unique ID
    #  , 'fun_unique_name.R'
  ) -> list_fun
  
  list(
    # Default texts
    'list_df_text.R'
    # Factors
    , 'list_factors.R'
  ) -> list_data
  
  # Render files
  list(
    # Render matching report
    'render_to_bucket.R'
  ) -> list_render
  
  
  # --- DATA ----------------------------------------------------------------
  # EFA-REDUCED OCCUPATIONS DATA FRAME -----------------------------------------------
  save_object(
    "db/occupations.csv"
    , file = "/tmp/occupations.csv"
    , bucket = list_bucket.data
    , region = 'us-east-2'
  )
  
  save_object(
    "db/career_finder_report.xlsx"
    , file = "/tmp/career_finder_report.xlsx"
    , bucket = list_bucket.data
    , region = 'us-east-2'
  )
  
  read_csv(
    '/tmp/occupations.csv'
    , show_col_types = F
  ) -> df_occupations
  
  # Select only necessary variables
  df_occupations %>%
    mutate(
      across(
        .cols = all_of(
          list_factors %>%
            flatten() %>% 
            flatten_chr()
        )
        , .fns = function(x){x/100}
      )
    ) -> df_occupations
  
  # --- S3 SOURCE (SETUP) ----------------------------------------------------------
  # save_object('R/fun_kflex.R', file = '/tmp/fun_kflex.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # save_object('R/fun_matching.R', file = '/tmp/fun_matching.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # save_object('R/geom_dumbbell.R', file = '/tmp/geom_dumbbell.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # save_object('R/fun_plots.R', file = '/tmp/fun_plots.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # save_object('R/fun_scores.R', file = '/tmp/fun_scores.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # save_object('R/fun_texts.R', file = '/tmp/fun_texts.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # save_object('R/list_df_text.R', file = '/tmp/list_df_text.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # save_object('R/list_factors.R', file = '/tmp/list_factors.R', bucket = list_bucket.fun, region = 'us-east-2')
  # 
  # source('/tmp/fun_kflex.R')
  # 
  # source('/tmp/fun_matching.R')
  # 
  # source('/tmp/geom_dumbbell.R')
  # 
  # source('/tmp/fun_plots.R')
  # 
  # source('/tmp/fun_scores.R')
  # 
  # source('/tmp/fun_texts.R')
  # 
  # source('/tmp/list_df_text.R')
  # 
  # source('/tmp/list_factors.R')
  
  # Download and source files
  Map(
    function(list_files, list_bucket){
      
      # Download files
      map(
        list_files
        , ~ 
          save_object(
            glue('R/{.x}')
            , file = glue('/tmp/{.x}')
            , list_bucket
            , region = 'us-east-2' 
          )
      )
      
      # Source files
      source(glue('/tmp/{.x}'))
      
    }
    , list_files = list(list_fun, list_data)
    , list_bucket = list(list_bucket.fun, list_bucket.data)
  )
  
  # # Download files
  # map(
  #   list_fun
  #   , ~ 
  #     save_object(
  #       glue('R/{.x}')
  #       , file = glue('/tmp/{.x}')
  #       , list_bucket.fun
  #       , region = 'us-east-2' 
  #     )
  # )
  # 
  # map(
  #   list_data
  #   , ~ 
  #     save_object(
  #       glue('R/{.x}')
  #       , file = glue('/tmp/{.x}')
  #       , list_bucket.data
  #       , region = 'us-east-2' 
  #     )
  # )
  # # Source files
  # map(
  #   list_fun
  #   , ~ source(glue('/tmp/{.x}'))
  # )
  # 
  # map(
  #   list_data
  #   , ~ source(glue('/tmp/{.x}'))
  # )
  
  # EFA-REDUCED QUERY VECTOR -----------------------------------------------
  # User questionnaire data frame
  from_json(body) %>%
    as_tibble() %>% 
    dplyr::select(
      user_name
      , chr_education
      , chr_language
      , list_factors %>%
        flatten() %>%
        flatten_chr()
    ) %>%
    mutate(
      across(
        .cols = 
          list_factors %>%
          flatten() %>%
          flatten_chr()
        , .fns = function(x){
          recode(x
                 , '1' = 0.00
                 , '2' = 0.17
                 , '3' = 0.33
                 , '4' = 0.50
                 , '5' = 0.67
                 , '6' = 0.83
                 , '7' = 1.00
          )}
      )
    ) -> df_input
  
  # Level of education filter
  df_input %>% 
    pull(chr_education) -> chr_education
  
  df_input %>%
    select(-chr_education) -> df_input
  
  toupper(chr_education[[1]]) -> chr_education
  
  # Language filter
  map(
    list_df.text
    , ~ .x %>% filter(
      language == unique(df_input$chr_language)
    )
  ) -> list_df.text
  
  df_input %>%
    select(-chr_language) -> df_input
  
  # LEVEL OF EDUCATION FILTER -----------------------------------------------
  # All levels of education
  df_occupations %>% 
    pull(entry_level_education) %>% 
    unique() -> chr_education.levels
  
  # Highly qualified only
  if(chr_education %in% 'HIGH'){
    
    chr_education.levels[!(
      
      chr_education.levels %in%
        c(
          "Some college, no degree"
          , "High school diploma or equivalent"
          , "Associate's degree"
          , "No formal educational credential"
        )
      
    )] -> chr_education.levels
    
  }
  
  # Unqualified only
  if(chr_education %in% 'LOW'){
    
    c(
      "Some college, no degree"
      , "High school diploma or equivalent"
      , "Associate's degree"
      , "No formal educational credential"
    ) -> chr_education.levels
    
  }
  
  # APPLY LEVEL OF EDUCATION FILTER -----------------------------------------
  # Select necessary variables and filter by level of education
  df_occupations %>% 
    select(
      occupation
      , entry_level_education
      , annual_wage_2021
      , list_factors %>%
        flatten() %>%
        flatten_chr()
    ) %>% 
    filter(
      entry_level_education %in% 
        chr_education.levels
    ) -> df_occupations
  
  # --- SETUP -----------------------------------------------------------
  # PARAMETERS  --------------------------------------------------------------
  # KNN parameters
  dbl_threshold <- 0
  
  # Scales
  seq_scale.1_5 <- seq(0,1,.25)
  seq_scale.1_6 <- round(seq(0, 0.9, 1/6), 2)
  seq_scale.1_7 <- seq(.33, .83, .17/2)
  seq_scale.1_8 <- round(seq(0,1,1/7), 2)
  
  # Recommendation cutoff
  dbl_recommended.cutoff <- 0.67
  
  # Colors
  list(
    'green' = '#4AF7B0'
    , 'purple1' = '#753AF9'
    , 'purple2' = '#301866'
    , 'purple3' = '#3854FB'
    , 'blue1' = '#56D0F5'
    , 'blue2' = '#ABF4D4'
    , 'blue3' = '#43DED1'
    , 'blue4' = '#182766'
    , 'red' = '#CE3527'
    
    , 'abilities' = '#C92618'
    , 'knowledge' = '#FF9E1F'
    , 'skills' = '#50915D'
    
    , 'black' = '#212121'
    , 'grey' = '#D4D5D8'
  ) -> list_pal.atlas
  
  # ------- RESULTS --------------------------------------------------------
  # KNN MATCHING ---------------------------------------------------------------
  fun_KNN.matching(
    .df_data.numeric = 
      df_occupations %>% 
      select(
        occupation
        , list_factors %>%
          flatten() %>% 
          flatten_chr()
      )
    , .vec_query.numeric = df_input
    , .int_k = nrow(df_occupations)
    , .imput.over_qualification = T
    , .dbl_over_qualification.threshold = dbl_threshold
    , .dbl_decimals = 4
  ) %>% 
    full_join(df_occupations) -> df_KNN.output
  
  # FACTOR SCORES (USER) -----------------------------------------------------------
  fun_factor.scores2(
    .df_data = 
      rbind(
        df_input %>% 
          rename(
            id.unique = user_name
          ) %>% 
          select(
            id.unique
            , list_factors %>%
              flatten() %>% 
              flatten_chr()
          )
        , df_KNN.output %>% 
          arrange(desc(
            similarity
          )) %>% 
          slice(1,n()) %>% 
          select(
            occupation
            , list_factors %>%
              flatten() %>% 
              flatten_chr()
          ) %>% 
          rename(id.unique = 1)
      )
    , .list_factor.keys = list_factors
    , .lgc_pivot.long = T
    , .lgc_sample.averages = F
    , .lgc_totals = F
  ) -> list_factor.scores
  
  # TOP, BOT, MEDIAN MATCHES ------------------------------------------------
  # Slice top, bottom and median matches
  map(
    set_names(
      c(1, nrow(df_KNN.output), round(nrow(df_KNN.output) / 2))
      , c('top_match', 'bot_match', 'med_match')
    )
    , ~ 
      df_KNN.output %>%
      slice(.x) %>% 
      select(
        occupation
        , similarity
        , rank
        , rank.norm
      )
  ) -> list_df.matches
  
  # DUMBBELL DATA FRAME -----------------------------------------------------
  # Factor scores of top and bottom matches
  list_factor.scores$scores.long %>% 
    select(
      id.unique
      , contains('factor')
    ) %>%
    unique() %>%
    pivot_wider(
      names_from = id.unique
      , values_from = factor.score
    ) %>%
    rename(
      top_match = list_df.matches$top_match$occupation
      , bot_match = list_df.matches$bot_match$occupation
      , you = df_input$user_name
    ) %>% 
    mutate(
      top_match.diff = abs(top_match - you)
      , bot_match.diff = abs(bot_match - you)
    ) -> df_dumbbell
  
  # -------- DYNAMIC TEXTS --------------------------------------------------
  # DYNAMIC TEXTS -----------------------------------------------
  # Preliminary values for analyses
  flatten(list(
    # Username
    username = df_input$user_name
    # Number of occupations
    , nrow_occupations = nrow(df_KNN.output)
    # Categories and factors
    , chr_categories = 
      fun_text.commas(
        names(list_factors)
        , .chr_last.comma = list_df.text$last.comma$text
      )
    , ncategories = length(list_factors)
    , nfactors = length(flatten(list_factors))
    , map(setNames(
      1:length(list_factors)
      , paste0('chr_category', 1:length(list_factors)))
      , ~ names(list_factors[.x]) %>%
        fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    )
    , map(setNames(
      1:length(list_factors)
      , paste0('chr_category', 1:length(list_factors), '.factors'))
      , ~ names(flatten(list_factors[.x])) %>%
        fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    )
    , map(setNames(
      1:length(list_factors)
      , paste0('category', 1:length(list_factors), '.nfactors'))
      , ~ length(flatten(list_factors[.x]))
    )
    , nitems = length(flatten_chr(list_factors))
    # Top match
    , top_match.name = list_df.matches$top_match$occupation
    , top_match.similarity = percent(list_df.matches$top_match$similarity, .01)
    # Bottom match
    , bot_match.name = list_df.matches$bot_match$occupation
    , bot_match.similarity = percent(list_df.matches$bot_match$similarity, .01)
    # Median match
    , median_match.name = list_df.matches$med_match$occupation
    , median_match.similarity = percent(list_df.matches$med_match$similarity, .01)
    # Number of recommended occupations
    , n_recommended = sum(df_KNN.output$similarity >= dbl_recommended.cutff)
    # Scope analysis
    , text.scope = 
      list_df.text$recommended %>% 
      mutate(
        n.recommended = 
          df_KNN.output %>% 
          filter(similarity >= dbl_recommended.cutff) %>% 
          nrow()
        # Percent of compatibility scores > cutff
        , pct.recommended = n.recommended / nrow(df_KNN.output)
        , scope.interval = 
          pct.recommended %>%
          round(1) %>%
          findInterval(vec = seq_scale.1_6)
      ) %>% 
      filter(interval == scope.interval) %>% 
      pull(text)
    # Number of factors in the model
    , nfactors = length(flatten(list_factors))
    # Capital flexibility analysis
    , list_df.text$flexibility %>% 
      mutate(
        capital.flex = fun_capital.flex(df_KNN.output$similarity)
        , kflex.interval = 
          capital.flex %>% 
          round(1) %>% 
          findInterval(seq_scale.1_7)
      ) %>% 
      filter(kflex.interval == interval) %>%
      select(starts_with('text')) %>% 
      c()
    # Top match most similar and dissimilar factor tallies
    , top_match.factors.similar.tally =  
      df_dumbbell %>% 
      slice_min(top_match.diff) %>% 
      nrow()
    , top_match.factors.dissimilar.tally =  
      df_dumbbell %>% 
      slice_max(top_match.diff) %>% 
      nrow()
    # Top match most similar and dissimilar factors
    , top_match.factors.similar = 
      df_dumbbell %>% 
      slice_min(top_match.diff) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    # , top_match.factors.dissimilar = 
    , top_match.dissimilar = 
      df_dumbbell %>% 
      slice_max(top_match.diff) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    # Top match most similar and dissimilar factors differences
    , top_match.diff.min = 
      df_dumbbell %>% 
      slice_min(top_match.diff) %>% 
      pull(top_match.diff) %>%
      mean() %>% 
      round(4) * 100
    , top_match.diff.max = 
      df_dumbbell %>% 
      slice_max(top_match.diff) %>% 
      pull(top_match.diff) %>%
      mean() %>%
      round(4) * 100
    # Top match underqualified factors
    , top_match.underqualified =
      df_dumbbell %>%
      filter(
        top_match > you
      ) %>%
      pull(factor) %>%
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    
    # Top match overqualified factors
    , top_match.overqualified = 
      df_dumbbell %>% 
      filter(
        top_match <= you
      ) %>% 
      mutate(
        lgc_n = all(
          n() >= round(0.5 * nrow(df_dumbbell)) 
          , n() < nrow(df_dumbbell) - 1
        )
        , int_n = ifelse(first(lgc_n), 3, n())
        , chr_comma = ifelse(
          first(lgc_n)
          , ', '
          ,list_df.text$last.comma$text
        )
      ) %>% 
      slice(1:first(int_n)) %>% 
      mutate(
        factor = 
          fun_text.commas(
            factor
            , .chr_last.comma = first(chr_comma))
      ) %>% 
      slice(1) %>%
      pull(factor)
    # Bot match most similar and dissimilar factor tallies
    , bot_match.factors.similar.tally =  
      df_dumbbell %>% 
      slice_min(bot_match.diff) %>% 
      nrow()
    , bot_match.factors.dissimilar.tally =  
      df_dumbbell %>% 
      slice_max(bot_match.diff) %>% 
      nrow()
    # Bot match most similar and dissimilar factors
    , bot_match.factors.similar = 
      df_dumbbell %>% 
      slice_min(bot_match.diff) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    , bot_match.factors.dissimilar = 
      df_dumbbell %>% 
      slice_max(bot_match.diff) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    # Bot match most similar and dissimilar factors differences
    , bot_match.diff.min = 
      df_dumbbell %>% 
      slice_min(bot_match.diff) %>% 
      pull(bot_match.diff) %>%
      mean() %>%
      round(4) * 100
    , bot_match.diff.max = 
      df_dumbbell %>% 
      slice_max(bot_match.diff) %>% 
      pull(bot_match.diff) %>%
      mean() %>%
      round(4) * 100
    # Bot match underqualified factors
    , bot_match.underqualified = 
      df_dumbbell %>% 
      filter(
        bot_match > you
      ) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    # Bot match overqualified factors
    , bot_match.overqualified = 
      df_dumbbell %>% 
      filter(
        bot_match <= you
      ) %>% 
      pull(factor) %>%
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    # Top 3 strengths
    , user.strengths = 
      df_dumbbell %>%
      slice_max(you, n = 3) %>% 
      slice(1:3) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    , top_match.strengths = 
      df_dumbbell %>%
      slice_max(top_match, n = 3) %>% 
      slice(1:3) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    , bot_match.strengths = 
      df_dumbbell %>%
      slice_max(bot_match, n = 3) %>% 
      slice(1:3) %>% 
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    , bot_match.strengths.common.n = 
      inner_join(
        df_dumbbell %>% 
          slice_max(you, n = 3) %>%
          select(factor)
        , df_dumbbell %>% 
          slice_max(bot_match, n = 3) %>%
          select(factor)
      ) %>%
      nrow()
    , bot_match.strengths.common = 
      inner_join(
        df_dumbbell %>% 
          slice_max(you, n = 3) %>%
          select(factor)
        , df_dumbbell %>% 
          slice_max(bot_match, n = 3) %>%
          select(factor)
      ) %>%
      pull(factor) %>% 
      fun_text.commas(.chr_last.comma = list_df.text$last.comma$text)
    # Top and bot matches capacity
    , top_match.underqualified.n = 
      df_dumbbell %>% 
      filter(
        top_match > you
      ) %>% 
      nrow()
    , top_match.overqualified.n = 
      df_dumbbell %>% 
      filter(
        top_match <= you
      ) %>% 
      nrow()
    , bot_match.underqualified.n = 
      df_dumbbell %>% 
      filter(
        bot_match > you
      ) %>% 
      nrow()
    , bot_match.overqualified.n = 
      df_dumbbell %>% 
      filter(
        bot_match <= you
      ) %>% 
      nrow()
  )) -> list_text
  
  # Preliminary imputations
  map_if(
    list_df.text
    , ~ !any(.x$complexity == 'complex', na.rm = T)
    , ~ fun_text.dynamic(.x, list_text)
  ) -> list_df.text
  
  # Top match factor similarity analysis
  list_df.text$top_match.similar %>%
    filter(
      list_text$top_match.factors.similar.tally >= factors.tally
    ) %>% 
    slice(n()) %>%
    pull(text) -> list_text$top_match.similar
  
  # Top match underqualification analysis
  list_df.text$top_match.underqualified %>% 
    mutate(
      factors.tally = as.numeric(factors.tally)
      , factors.interval = 
        findInterval(
          factors.tally
          , factors.tally
        )
      , interval = 
        findInterval(
          x = list_text$top_match.underqualified.n
          , vec = factors.tally
        )
      , .before = 1
    ) %>% 
    filter(
      factors.interval == interval
    ) %>% 
    pull(text) -> list_text$top_match.underqualified
  
  # Top match overqualification analysis
  list_text$top_match.overqualified <- ''
  
  if(list_text$top_match.underqualified.n > 0){
    
    if(
      list_df.text$top_match.overqualified %>% 
      pull(factors.tally) %>%
      as.numeric() %>% 
      max(na.rm = T) <= 2
    ){
      
      list_df.text$top_match.overqualified %>% 
        filter(as.numeric(factors.tally) <= 2)
      
    } else {
      
      list_df.text$top_match.overqualified %>%
        mutate(
          factors.tally = 
            map_dbl(
              factors.tally
              , ~ eval(parse(text = .x))
            ))
      
    } %>% 
      mutate(
        factors.interval = 
          findInterval(
            factors.tally
            , factors.tally
          )
        , interval = 
          findInterval(
            x = list_text$top_match.overqualified.n
            , vec = factors.tally
          )
        , .before = 1
      ) %>% 
      filter(
        factors.interval == interval
      ) %>% 
      pull(text) -> list_text$top_match.overqualified
    
  }
  
  # Bot match factor (dis)similarity analysis
  list_df.text$bot_match.similar %>%
    filter(
      list_text$bot_match.factors.similar.tally >= factors.tally
    ) %>% 
    slice(n()) %>%
    pull(text) -> list_text$bot_match.similar
  
  list_df.text$bot_match.dissimilar %>%
    filter(
      list_text$bot_match.factors.dissimilar.tally >= factors.tally
    ) %>% 
    slice(n()) %>%
    pull(text) -> list_text$bot_match.dissimilar
  
  # Bot match underqualification analysis
  list_df.text$bot_match.underqualified %>% 
    mutate(
      factors.tally = as.numeric(factors.tally)
      , factors.interval = 
        findInterval(
          factors.tally
          , factors.tally
        )
      , interval = 
        findInterval(
          x = list_text$bot_match.underqualified.n
          , vec = factors.tally
        )
      , .before = 1
    ) %>%
    filter(
      factors.interval == interval
    ) %>% 
    pull(text) -> list_text$bot_match.underqualified
  
  # Bot match overqualification analysis
  if(
    list_df.text$bot_match.overqualified %>% 
    pull(factors.tally) %>%
    as.numeric() %>% 
    max(na.rm = T) <= 2
  ){
    
    list_df.text$bot_match.overqualified %>% 
      filter(as.numeric(factors.tally) <= 2)
    
  } else {
    
    list_df.text$bot_match.overqualified %>%
      mutate(
        factors.tally = 
          map_dbl(
            factors.tally
            , ~ eval(parse(text = .x))
          ))
    
  } %>% 
    mutate(
      factors.interval = 
        findInterval(
          factors.tally
          , factors.tally
        )
      , interval = 
        findInterval(
          x = list_text$bot_match.overqualified.n
          , vec = factors.tally
        )
      , .before = 1
    ) %>% 
    filter(
      factors.interval == interval
    ) %>% 
    pull(text) -> list_text$bot_match.overqualified
  
  # Bot match common strengths analysis
  list_df.text$bot_match.strengths.common %>% 
    filter(
      factors.tally == list_text$bot_match.strengths.common.n
    ) %>% 
    pull(text) -> list_text$bot_match.strengths.common
  
  # Capacity analysis
  map(
    list(
      'top_match.capacity' = list_text$top_match.overqualified.n
      , 'bot_match.capacity' = list_text$bot_match.overqualified.n
    )
    , ~
      list_df.text$capacity %>% 
      mutate(
        pct.over = .x / list_text$nfactors
        , n.interval = 
          pct.over %>% 
          round(1) %>% 
          findInterval(vec = seq_scale.1_5) 
      ) %>% 
      filter(interval == n.interval) %>% 
      slice(1) %>% 
      pull(text)
  ) %>% 
    c(list_text) -> list_text
  
  list_df.text$capacity.same %>%
    mutate(
      'same' = 
        list_text$top_match.overqualified.n == 
        list_text$bot_match.overqualified.n 
    ) %>% 
    filter(
      overqualified.same == same
    ) %>% 
    pull(text) -> list_text$top_bot_match.capacity.same
  
  # Apply remaining textual input
  map(
    list_df.text
    , ~ fun_text.dynamic(.x, list_text)
  ) -> list_df.text
  
  list_df.text$sections %>% 
    mutate(text = ifelse(
      section == 'date'
      , format(Sys.Date(), text)
      , text
    )) -> list_df.text$sections
  
  # Text list
  as.list(list_df.text$sections$text) -> list_report.texts
  
  # Section titles
  list_df.text$sections.title %>% 
    mutate(title = paste(strrep('#', level), title)) %>% 
    pull(title) %>%
    as.list() -> list_report.titles
  
  # Captions
  list_df.text$plots %>% 
    pull(plot.caption) %>% 
    unique() %>% 
    as.list() -> list_plots.caption
  
  # Text elements
  list_df.text$text.elements %>% 
    pull(title) %>% 
    as.list() -> list_text.elements
  
  # -------- TABLES ---------------------------------------------------------
  # TOP 7, BOTTOM 3 MATCHES -----------------------------------------
  # Table of Top 7 Bottom 3 Matches
  df_KNN.output %>%
    slice(1:7, seq(max(rank) - 2, max(rank))) %>%
    mutate(
      similarity = percent(similarity, accuracy = .01)
      , annual_wage_2021 = dollar(
        annual_wage_2021, accuracy = .01
        # annual_wage_2021, accuracy = 1, prefix = 'U$ '
        # annual_wage_2021, accuracy = 1, prefix = 'USD '
      )
    ) %>% 
    select(
      rank
      , occupation
      , annual_wage_2021
      , similarity
    ) -> df_top7.bot3
  
  list_df.text$matching.table %>% 
    pull(text) -> names(df_top7.bot3)
  
  # -------- PLOTS ----------------------------------------------------------
  # [CIRCULAR BAR PLOT] MATCHING PERCENTAGES -----------------------------------------------------
  df_KNN.output %>% 
    mutate(
      recommended = ifelse(
        round(similarity, 2) >= dbl_recommended.cutoff
        | is.na(similarity)
        , list_df.text$plots %>% 
          filter(order == min(order)) %>% 
          pull(plot.color) %>%
          nth(1)
        , list_df.text$plots %>% 
          filter(order == min(order)) %>% 
          pull(plot.color) %>%
          nth(2)
      )
    ) %>%
    fun_plot.bar(aes(
      x = rank
      , y = similarity
      , fill = recommended
    )
    , .theme = theme_ridges(center_axis_labels = T) +
      theme(
        title = element_text(hjust = 0.5)
        , plot.title.position = 'plot'
        , legend.position = 'bottom'
        , legend.justification = 'center'
        , legend.key.size = unit(0.5,'cm')
        , legend.key.width = unit(2,'cm')
        # , plot.margin = margin(1, 1, 1, 1,'cm')
        , plot.margin = margin(0, 0, 0, 0,'cm')
      )
    , .fun_format.y = function(x){percent(x,accuracy = 1)}
    , .coord_polar = T
    , .fun_polar.labels = percent
    , .list_axis.y.args = list(
      breaks = seq(0, 1, length.out = 5)
    )
    , .list_geom.param = list(
      position = c(position_dodge2(0.5, 'single'))
      , width = 0.5
    )
    , .list_labels.param = list(
      color = list_pal.atlas$black
    )
    , .chr_manual.pal = setNames(
      c(
        list_pal.atlas$purple3
        , list_pal.atlas$grey
      )
      , list_df.text$plots %>% 
        filter(order == min(order)) %>% 
        pull(plot.color) %>%
        unique()
    )
    , .chr_manual.aes = c(
      'fill', 'color'
    )
    , .list_legend = list(
      color = 'none'
    )
    , .list_labs = list(
      y = 
        list_df.text$plots %>% 
        filter(order == unique(order)[1]) %>% 
        pull(plot.y) %>%
        unique()
      , fill = NULL
    )) -> plt_match.polar
  
  # [LINE CHART] PROFESSIONAL COMPATIBILITY CURVE -------------------------------------------------------------------
  df_KNN.output %>%
    fun_plot.line(aes(
      x = rank.norm
      , y = similarity
      , color = similarity >= dbl_recommended.cutff
    )
    , .dbl_limits.y = c(0,1)
    , .chr_manual.pal = c(
      list_pal.atlas$grey
      , list_pal.atlas$purple3
    )
    , .list_legend = list(color = 'none')
    , .fun_format.y = label_percent()
    , .reorder_fct = F
    , .reorder_desc = F
    , .theme = ggridges::theme_ridges(font_size = 11, center_axis_labels = T) +
      theme(
        plot.margin = margin(0, 0, 0, 0)
        , axis.text.x = element_blank()
        , axis.text.y = element_text(vjust = 0.5)
      )
    , .list_labs = list(
      title = NULL
      , subtitle = NULL
      , x = 
        list_df.text$plots %>% 
        filter(order == unique(order)[2]) %>%
        pull(plot.x)
      , y = 
        list_df.text$plots %>% 
        filter(order == unique(order)[2]) %>%
        pull(plot.y)
    )) +
    geom_segment(
      x = 0
      # , xend = 1 - list_df.matches$bot_match$similarity
      , xend = 1
      # , y = list_df.matches$bot_match$similarity
      , y = 0
      , yend = 1
      , linewidth = 0.25
      , color = list_pal.atlas$black
      , linetype = 2
    ) -> plt_line.rank
  
  c(
    plt_line.rank$layers
    , geom_textvline(
      xintercept = 
        (list_text$nrow_occupations - list_text$n_recommended) / 
        (list_text$nrow_occupations - 1)
      , label = 
        list_df.text$plots %>% 
        filter(order == unique(order)[2]) %>%
        pull(plot.color)
      , color = list_pal.atlas$purple3
      , fontface = 'bold'
      , linetype = 1
      , linewidth = 1.35
      , hjust = 0.125
      , vjust = -0.5
    )
  ) -> plt_line.rank$layers
  
  # [DENSITY] PROFESSIONAL COMPATIBILITY DISTRIBUTION -----------------------
  df_KNN.output %>%
    fun_plot.histogram(aes(
      x = similarity
      , y = after_stat(density)
    )
    , .dbl_limits.y = c(0,1.25*max(density(df_KNN.output$similarity)$y))
    , .list_axis.x.args = list(
      limits = c(-0.1,1.1)
      , breaks = seq(0,1,.25)
    )
    , .fun_format.x = percent_format(accuracy = 1)
    , .list_labs = list(
      title = NULL
      , subtitle = NULL
      , x = 
        list_df.text$plots %>% 
        filter(order == unique(order)[3]) %>%
        pull(plot.x)
      , y = NULL
    )
    , .theme = ggridges::theme_ridges(font_size = 11, center_axis_labels = T) +
      theme(
        plot.margin = margin(0, 0, 0, 0)
        , axis.text.y = element_blank()
      )
    ) +
    geom_density(aes(
      x = similarity
    )
    , size = 1.2
    ) + 
    geom_textvline(
      xintercept = dbl_recommended.cutff
      , label = 
        list_df.text$plots %>% 
        filter(order == unique(order)[3]) %>%
        pull(plot.color)
      , color = list_pal.atlas$green
      , fontface = 'bold'
      , linetype = 1
      , linewidth = 1.35
      , hjust = 0.125
      , vjust = -0.5
    ) -> plt_density
  
  # [DUMBBELL PLOT] USER VS TOP MATCH / BOTTOM MATCH ------------------------------------
  map(
    setNames(
      c(
        list_text$bot_match.name
        , list_text$top_match.name
      )
      , c('bot_match', 'top_match')
    )
    , ~ 
      list_factor.scores$scores.long %>%
      filter(
        id.unique %in% c(list_text$username, .x
        )) %>%
      mutate(id.unique = fct_inorder(id.unique)) %>% 
      fun_plot.dumbbell2(aes(
        x = factor.score
        , y = factor
        , color = id.unique
      )
      , .sym_facets = category
      , .int_facets = 1
      , .chr_scales = 'free_y'
      , .list_labs = list(
        title = NULL
        , x = 
          list_df.text$plots %>% 
          filter(plot.section == 'top_match') %>% 
          pull(plot.x) %>% 
          unique()
        , y = NULL
        , color = NULL
      )
      , .reorder_fct = T
      , .reorder_desc = T
      , .chr_manual.pal = set_names(
        c(
          list_pal.atlas$purple3
          , list_pal.atlas$green
          , list_pal.atlas$red
        )
        , c(
          list_text$username
          , list_text$top_match.name
          , list_text$bot_match.name
        ))
      , .list_axis.x.args = list(
        limits = c(-0.1, 1.1)
        , breaks = seq(0, 1, 0.25)
      )
      , .fun_format.x = percent_format(accuracy = 1)
      , .fun_format.y = function(y){y}
      , .theme = theme_ridges(center_axis_labels = T) +
        theme(
          title = element_text(hjust = 0.5)
          , plot.title.position = 'plot'
          , legend.position = 'bottom'
          , legend.justification = 'center'
          , strip.background = element_blank()
          , plot.margin = margin(0, 0, 0, 0,'cm')
          , axis.text.y = element_text(vjust = 0.5)
        ))
  ) -> list_plt.dumbbells
  
  # --- RENDER -----------------------------------------------------------
  # RENDER R MARKDOWN REPORT --------------------------------------------------
  Sys.setenv(HOME = '/opt/pandoc/bin')
  
  Sys.setenv(FONTCONFIG_PATH = '/opt/fonts')
  
  file.path(
    '/tmp'
    , chr_file.name
  ) -> outfile
  
  save_object(
    'Rmd/matching_report2.Rmd'
    , file = '/tmp/matching_report2.Rmd'
    , bucket = list_bucket.data
    , region = 'us-east-2'
  )
  
  rmarkdown::render(
    '/tmp/matching_report2.Rmd'
    , output_file = outfile
    , output_dir = '/tmp'
    , intermediates_dir = '/tmp'
    , knit_root_dir = '/tmp'
    , quiet = type.convert(
      Sys.getenv('PANDOC_QUIET')
      , as.is = T
    )
  )
  
  logger::log_debug("Rendering complete for", chr_file.name)
  
  chr_s3_object <- paste0("user-pdf-reports/", chr_file.name)
  
  put_object(
    outfile
    , object = chr_s3_object
    , bucket = list_bucket.render
    , region = 'us-east-2'
  )
  
  # ------ OUTPUT -----------------------------------------------------------
  return(list(
    "statusCode" = 200
    , "headers" = list("Content-Type" = "application/json")
    , "body" = pretty_json(
      paste0('{ "filename": "', chr_s3_object , '" }' )
    )
  ))
  
}