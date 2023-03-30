library(tidyverse)
library(glue)

df_vals <- tibble(
  lalala = '123'
  , dsds = '321'
)

df_text <- tibble(
  language = c('en-us', 'pt-br')
  , text = c('lalala {lalala} lalala {dsds}', 'dsdsds {dsds} dsdsds {lalala}')
)

df_text
df_vals

df_text %>% 
  rowwise() %>% 
  mutate(across(
    .cols = !is.numeric
    ,.fns = ~ glue_data(df_vals, .x)
  ))

map(
  df_text$text
  , ~ glue_data(df_vals, .x)
)

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
  map(~ .x %>% filter(language == 'en')) -> list_df.glue
# map(~ .x %>% filter(language == 'pt')) -> list_df.glue

chr_text.user

tibble(
  factors.tally = c(1, NA, length(flatten(list_factors)))
  # factors.tally = c(1, 2, NA)
  , text = c('one', 'all', 'dsds')
) %>%
  slice(
    1
    , rep(
      which(is.na(.))
      , each = 
        max(factors.tally, na.rm = T) -
        min(factors.tally, na.rm = T) - 1
    )
    , n()
  ) %>% 
  mutate(
    factors.tally = 
      min(factors.tally, na.rm = T):
      max(factors.tally, na.rm = T)
  )


expand(
  factors.tally = 
    seq(
      min(factors.tally, na.rm = T)
      , max(factors.tally, na.rm = T)
    )
)


# [FUNCTION] DYNAMIC TEXT DATA FRAME --------------------------------------
fun_text.dynamic <- function(
    
  # Data frame
  .df_text = tibble()
  # Input list
  , .list_input = list()
  # NA action
  , .chr_na = NULL
  
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
      .cols = !is.numeric
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
    fun_text.commas()
  , top_match.factors.dissimilar = 
    df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas()
  # Top match most similar and dissimilar factors differences
  , top_match.diff.min = 
    df_dumbbell %>% 
    slice_min(top.match.diff) %>% 
    pull(top.match.diff) %>%
    round(4) * 100
  , top_match.diff.max = 
    df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    pull(top.match.diff) %>%
    round(4) * 100
  # Top match underqualified factors
  , top_match.underqualified = 
    df_dumbbell %>% 
    filter(
      top.match > you
    ) %>% 
    pull(factor) %>% 
    fun_text.commas()
  , top_match.overqualified = 
    df_dumbbell %>% 
    filter(
      top.match <= you
    ) %>% 
    pull(factor) %>% 
    fun_text.commas()
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
    fun_text.commas()
  , bot_match.factors.dissimilar = 
    df_dumbbell %>% 
    slice_max(bot.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas()
  # Bot match most similar and dissimilar factors differences
  , bot_match.diff.min = 
    df_dumbbell %>% 
    slice_min(bot.match.diff) %>% 
    pull(bot.match.diff) %>%
    round(4) * 100
  , bot_match.diff.max = 
    df_dumbbell %>% 
    slice_max(bot.match.diff) %>% 
    pull(bot.match.diff) %>%
    round(4) * 100
  # Top 3 strengths
  , user.strengths = 
    df_dumbbell %>%
    slice_max(you, n = 3) %>% 
    slice(1:3) %>% 
    pull(factor) %>% 
    fun_text.commas()
  , top_match.strengths = 
    df_dumbbell %>%
    slice_max(top.match, n = 3) %>% 
    slice(1:3) %>% 
    pull(factor) %>% 
    fun_text.commas()
  , bot_match.strengths = 
    df_dumbbell %>%
    slice_max(bot.match, n = 3) %>% 
    slice(1:3) %>% 
    pull(factor) %>% 
    fun_text.commas()
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
  fun_text.dynamic(list_text) %>% 
  mutate(
    factors.tally = as.numeric(factors.tally)
  ) %>% 
  slice(
    1
    , rep(
      which(is.na(.))
      , each = 
        max(factors.tally, na.rm = T) -
        min(factors.tally, na.rm = T) - 1
    )
    , n()
  ) %>% 
  mutate(
    factors.tally = 
      min(factors.tally, na.rm = T):
      max(factors.tally, na.rm = T)
  ) %>% 
  filter(
    factors.tally == list_text$top_match.factors.similar.tally
  ) %>% 
  pull(text) -> list_text$top_match.factors.similar

# Top match factor dissimilarity analysis
list_df.glue$top_match.dissimilar %>% 
  fun_text.dynamic(list_text) %>% 
  mutate(
    factors.tally = as.numeric(factors.tally)
  ) %>% 
  slice(
    1
    , rep(
      which(is.na(.))
      , each = 
        max(factors.tally, na.rm = T) -
        min(factors.tally, na.rm = T) - 1
    )
    , n()
  ) %>% 
  mutate(
    factors.tally = 
      min(factors.tally, na.rm = T):
      max(factors.tally, na.rm = T)
  ) %>% 
  filter(
    factors.tally == list_text$top_match.factors.dissimilar.tally
  ) %>% 
  pull(text) -> list_text$top_match.factors.dissimilar

list_df.glue$sections.glue %>% 
  filter(
    section == 'finishing_remarks'
  ) %>% 
  # pull(text)
  pull(text)

# list_df_text.capacity$bot.match %>% 
#   mutate(
#     text = if_else(
#       text == list_df_text.capacity$top.match$text
#       , paste(text, 'as well')
#       , text
#     )
#   ) -> list_df_text.capacity$bot.match

# Top match underqualification analysis
list_df.glue$top_match.underqualified %>%
  fun_text.dynamic(list_text) %>% 
  mutate(
    factors.tally = as.numeric(factors.tally)
  ) %>% 
  slice(
    1
    , rep(
      which(is.na(.))
      , each = 
        max(factors.tally, na.rm = T) -
        min(factors.tally, na.rm = T) - 1
    )
    , n()
  ) %>% 
  mutate(
    factors.tally = 
      min(factors.tally, na.rm = T):
      max(factors.tally, na.rm = T)
  ) %>% 
  filter(
    factors.tally == list_text$top_match.underqualified.n
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
          )
        , factors.tally = 
          ifelse(
            is.na(factors.tally)
            , min(factors.tally, na.rm = T) + 1
            , factors.tally
          )
      )
    
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

# Bot match underqualification analysis
# Bot match overqualification analysis

# Bot match common strengths analysis

# Capacity analysis
map(
  list(
    'top_match.capacity' = list_text$top_match.overqualified.n
    , 'bot_match.capacity' = list_text$bot_match.overqualified.n
  )
  , ~
    list_df_text$capacity %>% 
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


list_df.glue$sections.glue %>% 
  filter(section == 'top_match') %>% 
  fun_text.dynamic(list_text) %>% 
  pull(text)

# Apply remaining textual input
map(
  list_df.glue
  , ~ fun_text.dynamic(.x, list_text)
) -> list_df.glue

# 
# map(
#   setNames(
#     list_df.glue$sections.glue$section
#     , list_df.glue$sections.glue$section
#   )
#   , ~ 
#     list_df.glue$sections.glue %>% 
#     filter(
#       section == .x
#     ) %>%
#     fun_text.dynamic(list_text) %>% 
#     pull(text)
# )

list_df.glue$sections.glue %>% 
  # slice(1:3) %>% 
  slice(1) %>% 
  rowwise() %>% 
  mutate(across(
    .cols = !is.numeric
    ,.fns = ~ glue_data(list_text, .x)
  )) %>% 
  pull(text)

list_df.glue$sections.glue %>% 
  slice(3) %>% 
  rowwise() %>% 
  mutate(across(
    .cols = !is.numeric
    ,.fns = ~ glue_data(list_text, .x)
  )) %>% 
  pull(text)

# Recommended occupations (higher than cutff)
list_df_text$recommended %>% 
  mutate(
    n.recommended = 
      df_KNN.output %>% 
      filter(similarity >= dbl_recommended.cutff) %>% 
      nrow()
    # Percent of compatibility scores > cutff
    , pct.recommended = n.recommended / nrow(df_KNN.output)
    , n.interval = 
      pct.recommended %>%
      round(1) %>%
      findInterval(vec = seq_scale.1_6)
  ) %>% 
  filter(interval == n.interval) -> df_text.recommended

c(
  list_df.glue$flexibility.glue %>% 
    mutate(
      capital.flex = fun_capital.flex(df_KNN.output$similarity)
      , n.interval = 
        capital.flex %>% 
        round(1) %>% 
        findInterval(seq_scale.1_7)
    ) %>% 
    filter(n.interval == interval) %>%
    select(starts_with('text')) %>% 
    as.list()
)

# Specialization
list_df_text$flexibility %>% 
  mutate(
    capital.flex = fun_capital.flex(df_KNN.output$similarity)
    , n.interval = 
      capital.flex %>% 
      round(1) %>% 
      findInterval(vec = seq_scale.1_7) 
  ) %>% 
  filter(interval == n.interval) -> df_text.flexibility

# Top match comments
df_dumbbell %>% 
  filter(top.match > you) %>% 
  pull(factor) -> chr_top.underqualified

df_dumbbell %>% 
  filter(top.match < you) %>% 
  pull(factor) -> chr_top.overqualified

case_when(
  list_text$top_match.overqualified.n == 0 ~ 'not a single'
  , list_text$top_match.overqualified.n == nrow(df_dumbbell) ~ 'all'
  , T ~ list_text$top_match.overqualified.n %>% 
    english() %>% 
    as.character()
)




dsdsd %>% 
  filter(
    factors.tally == list_text$top_match.overqualified.n
  )


case_when(
  list_text$top_match.overqualified.n == 0 ~ ' whatsover'
  , list_text$top_match.overqualified.n == nrow(df_dumbbell) ~ 's'
  , list_text$top_match.overqualified.n == 1 ~ 
    list_text$top_match.overqualified %>%
    fun_text.commas() %>% 
    paste0('(viz. ',.,')') %>% 
    paste('', .)
  , T ~ list_text$top_match.overqualified %>%
    fun_text.commas() %>% 
    paste0('(viz. ',.,')') %>%
    paste('s', .)
)

case_when(
  length(chr_top.overqualified) == 0 ~ 'none whatsover'
  , length(chr_top.overqualified) == nrow(df_dumbbell) ~ 'all of them'
  , length(chr_top.overqualified) == (nrow(df_dumbbell) - 1) ~ 'all the others' 
  , length(chr_top.overqualified) <= 4 & 
    length(chr_top.overqualified) > 0 ~ 
    chr_top.overqualified %>%
    fun_text.commas()
  , T ~ chr_top.overqualified %>%
    head(3) %>% 
    fun_text.commas(.chr_last.comma = ', ') %>% 
    paste0(', and so on')
) -> chr_top.overqualified.viz

df_dumbbell %>% 
  slice_max(you, n = 3) %>% 
  slice(1:3) %>% 
  pull(factor) %>% 
  str_sort() %>% 
  fun_text.commas() -> chr_top.3str

df_dumbbell %>% 
  slice_max(top.match, n = 3) %>% 
  slice(1:3) %>% 
  pull(factor) %>%
  str_sort() %>% 
  fun_text.commas()  -> chr_top.match.3str

if_else(
  chr_top.3str == chr_top.match.3str
  , 'the exact same'
  , chr_top.match.3str
) -> chr_top.match.3str

# Bot match comments
df_dumbbell %>% 
  filter(bot.match > you) %>% 
  pull(factor) -> chr_bot.underqualified

df_dumbbell %>% 
  filter(bot.match < you) %>% 
  pull(factor) -> chr_bot.overqualified

case_when(
  length(chr_bot.underqualified) == 0 ~ 'no particular'
  , length(chr_bot.underqualified) == nrow(df_dumbbell) ~ 'all'
  , T ~ chr_bot.underqualified %>%
    fun_text.commas() %>%
    paste('the', .)
) -> chr_bot.underqualified.viz 

case_when(
  length(chr_bot.overqualified) == 0 ~ 'none'
  , length(chr_bot.overqualified) == nrow(df_dumbbell) ~ 'every aspect'
  , length(chr_bot.overqualified) >= (nrow(df_dumbbell) - 3) &
    length(chr_bot.overqualified) != nrow(df_dumbbell) ~ 'the rest of them'
  , T ~ chr_bot.overqualified %>%
    fun_text.commas()
) -> chr_bot.overqualified.viz

df_dumbbell %>% 
  slice_max(you, n = 3) %>% 
  slice(1:3) %>% 
  pull(factor) %>% 
  str_sort() -> chr_bot.3str

df_dumbbell %>% 
  slice_max(bot.match, n = 3) %>% 
  slice(1:3) %>%  
  pull(factor) -> chr_bot.match.3str

intersect(
  chr_bot.3str
  , chr_bot.match.3str
) -> chr_bot.3str

case_when(
  length(chr_bot.3str) == length(chr_bot.match.3str) ~ 'everything'
  , length(chr_bot.3str) == 0 ~ 'nothing'
  , T ~ chr_bot.3str %>%
    str_sort() %>% 
    fun_text.commas()
) -> chr_bot.3str

chr_bot.match.3str %>%
  str_sort() %>% 
  fun_text.commas() -> chr_bot.match.3str

# Finishing remarks
list_df_text$capacity %>% 
  mutate(
    pct.over.top = length(chr_top.overqualified) / nrow(df_dumbbell)
    , n.interval = 
      pct.over.top %>%
      round(1) %>%
      findInterval(vec = seq_scale.1_5) 
  ) %>% 
  filter(interval == n.interval) -> df_text.top.capacity

map(
  list(
    'top.match' = chr_top.overqualified
    , 'bot.match' = chr_bot.overqualified
  )
  , function(x){
    
    list_df_text$capacity %>% 
      mutate(
        pct.over = length(x) / nrow(df_dumbbell)
        , n.interval = 
          pct.over %>% 
          round(1) %>% 
          findInterval(vec = seq_scale.1_5) 
      ) %>% 
      filter(interval == n.interval) %>% 
      return()
    
  }
) -> list_df_text.capacity

list_df_text.capacity$bot.match %>% 
  mutate(
    text = if_else(
      text == list_df_text.capacity$top.match$text
      , paste(text, 'as well')
      , text
    )
  ) -> list_df_text.capacity$bot.match




list_df.glue$sections.glue


df_occupations




apply(
  df_text$text
  , 1
  , glue_data()
)

glue::glue_data(
  df
  , df_text %>% 
    select(text) %>%
    flatten_chr()
)

glue::glue_data_col(
  df
  , df_text %>% 
    select(text)
)

glue::glue_data_col(
  df
  , '
  Dsdsds is {dsds}. 
  Lala is {lalala}.
  '
)

df %>%
  glue_data(' 
  Dsdsds is {dsds}. 
  Lala is {lalala}.
  ')
