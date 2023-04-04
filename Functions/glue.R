library(tidyverse)
library(glue)

# DEFAULT TEXTS FOR IMPUTATION
map(
  excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/career_finder_report2.xlsx')
  , ~ read_excel('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/career_finder_report2.xlsx', sheet = .x)
) -> list_df_text

names(list_df_text) <- excel_sheets('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/career_finder_report2.xlsx')

# Remove carriage returns
list_df_text %>%
  map(function(df){
    
    df %>% 
      mutate(across(
        where(is.character)
        , ~ str_remove_all(.x, "\r") %>% 
          str_remove_all("\\\\n") %>% 
          str_replace_all("\n", "  \n")
      ))
    
  }) -> list_df_text

list_df_text[str_detect(names(list_df_text),'\\.')] -> list_df.glue

list_df.glue %>%
  # map(~ .x %>% filter(language == 'en')) -> list_df.glue
  map(~ .x %>% filter(language == 'pt')) -> list_df.glue
  # map(~ .x %>% filter(language == 'es')) -> list_df.glue

# [FUNCTION] DYNAMIC TEXT DATA FRAME --------------------------------------
fun_text.dynamic <- function(
    
  # Data frame
  .df_text = tibble()
  # Input list
  , .list_input = list()
  # NA action
  # , .chr_na = NULL
  , .chr_na = ''
  
){
  
  # Data types
  stopifnot(
    "'.df_text' must be a data frame." = 
      is.data.frame(.df_text)
  )
  
  stopifnot(
    "'.list_input' must be a list of textual inputs." = 
      c(
        is.list(.list_input)
        , !is.data.frame(.list_input)
      )
  )
  
  stopifnot(
    "'.chr_na' must be either NULL or a character element." =
      !length(.chr_na) |
      length(.chr_na) &
      is.character(.chr_na)
  )
  
  # Glue texts
  .df_text %>% 
    rowwise() %>%
    mutate(across(
      .cols = !where(is.numeric)
      ,.fns = function(text){
        
        glue_data(
          .list_input
          , text
          , .na = .chr_na
        )
        
      })) %>% 
    ungroup() %>% 
    return()
  
}

# VALUES FOR DYNAMIC TEXTS -----------------------------------------------
# Last comma 
paste0(list_df.glue$last.comma$text, ' ') -> chr_last.comma

# Preliminary analyses
# Top match
df_KNN.output %>% 
  slice(1) %>% 
  select(
    occupation
    , similarity
    , rank
    , rank.norm
  ) -> df_top.match

# Bottom match
df_KNN.output %>% 
  slice(n()) %>% 
  select(
    occupation
    , similarity
    , rank
    , rank.norm
  ) -> df_bot.match


# Median match
if(nrow(df_KNN.output) %% 2 == 0){
  
  df_KNN.output %>%
    slice(n()/2) %>%
    select(
      occupation
      , similarity
      , rank
      , rank.norm
    ) -> df_med.match
  
} else {
  
  df_KNN.output %>% 
    dplyr::filter(
      similarity == quantile(
        similarity, 0.50
      )) %>% 
    slice(1) %>%
    select(
      occupation
      , similarity
      , rank
      , rank.norm
    ) -> df_med.match
  
}

# Preliminary values for analyses
flatten(list(
  # Username
  username = chr_text.user
  # Number of occupations
  , nrow_occupations = nrow(df_KNN.output)
  # Categories and factors
  , chr_categories = 
    fun_text.commas(
      names(list_factors)
      , .chr_last.comma = chr_last.comma
    )
  , ncategories = length(list_factors)
  , nfactors = length(flatten(list_factors))
  , map(setNames(
    1:length(list_factors)
    , paste0('chr_category', 1:length(list_factors)))
    , ~ names(list_factors[.x])
  )
  , map(setNames(
    1:length(list_factors)
    , paste0('chr_category', 1:length(list_factors), '.factors'))
    , ~ names(flatten(list_factors[.x])) %>%
      fun_text.commas(.chr_last.comma = chr_last.comma)
  )
  , map(setNames(
    1:length(list_factors)
    , paste0('category', 1:length(list_factors), '.nfactors'))
    , ~ length(flatten(list_factors[.x]))
  )
  , nitems = length(flatten_chr(list_factors))
  # Top match
  , top_match.name = df_top.match$occupation
  , top_match.similarity = percent(df_top.match$similarity, .01)
  # Bottom match
  , bot_match.name = df_bot.match$occupation
  , bot_match.similarity = percent(df_bot.match$similarity, .01)
  # Median match
  , median_match.name = df_med.match$occupation
  , median_match.similarity = percent(df_med.match$similarity, .01)
  # Number of recommended occupations
  , n_recommended = sum(df_KNN.output$similarity >= dbl_recommended.cutff)
  # Scope analysis
  , text.scope = 
    list_df.glue$recommended %>% 
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
  , list_df.glue$flexibility.glue %>% 
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
    slice_min(top.match.diff) %>% 
    nrow()
  , top_match.factors.dissimilar.tally =  
    df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    nrow()
  # Top match most similar and dissimilar factors
  , top_match.factors.similar = 
    df_dumbbell %>% 
    slice_min(top.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  # , top_match.factors.dissimilar = 
  , top_match.dissimilar = 
    df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  # Top match most similar and dissimilar factors differences
  , top_match.diff.min = 
    df_dumbbell %>% 
    slice_min(top.match.diff) %>% 
    pull(top.match.diff) %>%
    mean() %>% 
    round(4) * 100
  , top_match.diff.max = 
    df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    pull(top.match.diff) %>%
    mean() %>%
    round(4) * 100
  # Top match underqualified factors
  , top_match.underqualified = 
    df_dumbbell %>% 
    filter(
      top.match > you
    ) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  # Top match overqualified factors
  , top_match.overqualified = 
    df_dumbbell %>% 
    filter(
      top.match <= you
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
        , chr_last.comma
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
    slice_min(bot.match.diff) %>% 
    nrow()
  , bot_match.factors.dissimilar.tally =  
    df_dumbbell %>% 
    slice_max(bot.match.diff) %>% 
    nrow()
  # Bot match most similar and dissimilar factors
  , bot_match.factors.similar = 
    df_dumbbell %>% 
    slice_min(bot.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  , bot_match.factors.dissimilar = 
    df_dumbbell %>% 
    slice_max(bot.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  # Bot match most similar and dissimilar factors differences
  , bot_match.diff.min = 
    df_dumbbell %>% 
    slice_min(bot.match.diff) %>% 
    pull(bot.match.diff) %>%
    mean() %>%
    round(4) * 100
  , bot_match.diff.max = 
    df_dumbbell %>% 
    slice_max(bot.match.diff) %>% 
    pull(bot.match.diff) %>%
    mean() %>%
    round(4) * 100
  # Bot match underqualified factors
  , bot_match.underqualified = 
    df_dumbbell %>% 
    filter(
      bot.match > you
    ) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  # Bot match overqualified factors
  , bot_match.overqualified = 
    df_dumbbell %>% 
    filter(
      bot.match <= you
    ) %>% 
    pull(factor) %>%
    fun_text.commas(.chr_last.comma = chr_last.comma)
  # Top 3 strengths
  , user.strengths = 
    df_dumbbell %>%
    slice_max(you, n = 3) %>% 
    slice(1:3) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  , top_match.strengths = 
    df_dumbbell %>%
    slice_max(top.match, n = 3) %>% 
    slice(1:3) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  , bot_match.strengths = 
    df_dumbbell %>%
    slice_max(bot.match, n = 3) %>% 
    slice(1:3) %>% 
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  , bot_match.strengths.common.n = 
    inner_join(
      df_dumbbell %>% 
        slice_max(you, n = 3) %>%
        select(factor)
      , df_dumbbell %>% 
        slice_max(bot.match, n = 3) %>%
        select(factor)
    ) %>%
    nrow()
  , bot_match.strengths.common = 
    inner_join(
      df_dumbbell %>% 
        slice_max(you, n = 3) %>%
        select(factor)
      , df_dumbbell %>% 
        slice_max(bot.match, n = 3) %>%
        select(factor)
    ) %>%
    pull(factor) %>% 
    fun_text.commas(.chr_last.comma = chr_last.comma)
  # Top and bot matches capacity
  , top_match.underqualified.n = 
    df_dumbbell %>% 
    filter(
      top.match > you
    ) %>% 
    nrow()
  , top_match.overqualified.n = 
    df_dumbbell %>% 
    filter(
      top.match <= you
    ) %>% 
    nrow()
  , bot_match.underqualified.n = 
    df_dumbbell %>% 
    filter(
      bot.match > you
    ) %>% 
    nrow()
  , bot_match.overqualified.n = 
    df_dumbbell %>% 
    filter(
      bot.match <= you
    ) %>% 
    nrow()
)) -> list_text

# Preliminary imputations
map_if(
  list_df.glue
  , ~ !any(.x$complexity == 'complex', na.rm = T)
  , ~ fun_text.dynamic(.x, list_text)
) -> list_df.glue

# Top match factor similarity analysis
list_df.glue$top_match.similar %>%
  filter(
    list_text$top_match.factors.similar.tally >= factors.tally
  ) %>% 
  slice(n()) %>%
  pull(text) -> list_text$top_match.similar

# Top match underqualification analysis
list_df.glue$top_match.underqualified %>% 
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
    list_df.glue$top_match.overqualified %>% 
    pull(factors.tally) %>%
    as.numeric() %>% 
    max(na.rm = T) <= 2
  ){
    
    list_df.glue$top_match.overqualified %>% 
      filter(as.numeric(factors.tally) <= 2)
    
  } else {
    
    list_df.glue$top_match.overqualified %>%
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
list_df.glue$bot_match.similar %>%
  filter(
    list_text$bot_match.factors.similar.tally >= factors.tally
  ) %>% 
  slice(n()) %>%
  pull(text) -> list_text$bot_match.similar

list_df.glue$bot_match.dissimilar %>%
  filter(
    list_text$bot_match.factors.dissimilar.tally >= factors.tally
  ) %>% 
  slice(n()) %>%
  pull(text) -> list_text$bot_match.dissimilar

# Bot match underqualification analysis
list_df.glue$bot_match.underqualified %>% 
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
  list_df.glue$bot_match.overqualified %>% 
  pull(factors.tally) %>%
  as.numeric() %>% 
  max(na.rm = T) <= 2
){
  
  list_df.glue$bot_match.overqualified %>% 
    filter(as.numeric(factors.tally) <= 2)
  
} else {
  
  list_df.glue$bot_match.overqualified %>%
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
list_df.glue$bot_match.strengths.common %>% 
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
    list_df.glue$capacity.glue %>% 
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

list_df.glue$capacity.same %>%
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
  list_df.glue
  , ~ fun_text.dynamic(.x, list_text)
) -> list_df.glue

list_df.glue$sections.glue %>% pull(text)
