# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'psych' #Factor Analysis
  , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
  , 'jsonify' #Work with JSON (faster than jsonlite)
  , 'ggthemes' #Data visualization
  , 'tidyverse', 'glue', 'stringi' #Data wrangling
  , 'tinytex' #LaTeX
  , 'knitr' #Knitr
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Install TinyTex
if(!tinytex::is_tinytex()){
  tinytex::install_tinytex()
}

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})

# FUNCTIONS ---------------------------------------------------------------
# KNN matching
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/KNN_Matching.R')
# Factor scores
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Factor_Scores.R')
# Automated plotting
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Auto_plots.R')
# Dynamic text
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Dynamic_text.R')
# Capital flexibility => Variance-adjusted skewness of professional compatibility curve
source('C:/Users/Cao/Documents/Github/Atlas-Research/Functions/Capital_Flexibility.R')

# PARAMETERS --------------------------------------------------------------
# Selected respondent
# chr_text.user <- 'Martijn'
# chr_text.user <- 'Cao'
# chr_text.user <- 'Acilio'
# chr_text.user <- 'Gabriel'
# chr_text.user <- 'Random'
# chr_text.user <- 'Random2'
# chr_text.user <- 'Random3'
# chr_text.user <- 'Random4'
chr_text.user <- 'Random5'

# KNN parameters
dbl_threshold <- 0.17

# Dynamic text parameters
chr_text.blank <- '___'
seq_scale.1_6 <- round(seq(0, 0.9, 1/6), 2)
# seq_scale.1_5 <- seq_scale.1_6[-c(1,2)]
seq_scale.1_7 <- c(.33, .33 + .17/2, .50, .50 + .17/2, .67, .67 + .17/2)
seq_scale.1_5 <- seq(0,1,.25)


dbl_recommended.cutff <- 0.67

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
  , 'black' = '#212121'
  , 'grey' = '#D4D5D8'
) -> list_atlas.pal

# DATA --------------------------------------------------------------------
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.EFA.R')

# USER INPUT DATA FRAME
df_input <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv')

# DEFAULT TEXTS FOR IMPUTATION
source('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/Default_texts.R')

# ------- DATA -----------------------------------------------------------
# EFA-REDUCED OCCUPATIONS DATA FRAME -----------------------------------------------
# Select only necessary variables
df_occupations %>% 
  # select(
  #   occupation
  #   , entry_level_education
  #   , annual_wage_2021
  #   , all_of(
  #     list_factors %>%
  #       flatten() %>% 
  #       flatten_chr()
  #   )
  # ) %>%
  mutate(
    across(
      .cols = all_of(
        list_factors %>%
          flatten() %>%
          flatten_chr()
      )
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
  ) -> df_occupations

# EFA-REDUCED QUERY VECTOR -----------------------------------------------
# Select user
df_input %>% 
  filter(Name == chr_text.user) -> df_input

# EFA-reduced data frame
df_input %>%
  select(
    all_of(
      list_factors %>%
        flatten() %>% 
        flatten_chr()
    )
  ) %>%  
  mutate(
    across(
      .cols = all_of(
        list_factors %>%
          flatten() %>% 
          flatten_chr()
      )
      , .fns = function(x){
        # recode((x + 2)
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

# ------- RESULTS --------------------------------------------------------
# KNN MATCHING ---------------------------------------------------------------
fun_KNN.matching(
  .df_data.numeric = df_occupations %>% 
    select(
      occupation
      , all_of(
        list_factors %>%
          flatten() %>% 
          flatten_chr()
      ))
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = dbl_threshold
  , .dbl_decimals = 4
) %>% 
  full_join(df_occupations) -> df_KNN.output

# df_KNN.output %>%
#   # df_occupations %>%
#   filter(
#     entry_level_education %in% c(
#       "Bachelor's degree"
#       , "Doctoral or professional degree"
#       # , "Associate's degree"i
#       , "Master's degree"
#     )
#   ) %>%
#   View()

# FACTOR SCORES (USER) -----------------------------------------------------------
fun_factor.scores(
  .df_data.numeric = df_input
  , .list_factor.keys = list_factors
  , .lgc_pivot.long = T
) -> list_factor.scores

# DUMBBELL DATA FRAME -----------------------------------------------------
# Factor scores of top and bottom matches
df_KNN.output %>%
  slice(1, nrow(.)) -> df_matches.topbot

df_matches.topbot %>%
  pull(occupation) -> chr_matches.topbot

df_matches.topbot %>%
  fun_factor.scores(
    .list_factor.keys = list_factors
  ) %>%
  first() %>%
  mutate(
    occupation = chr_matches.topbot
    , .before = 1
  ) %>%
  # User, top match and bottom match data frame
  bind_rows(
    list_factor.scores$factor.scores %>%
      mutate(
        occupation = 'you'
        , .before = 1
      )
  ) %>%
  pivot_longer(
    cols = -occupation
    , names_to = 'factor'
    , values_to = 'score'
  ) %>%
  pivot_wider(
    id_cols = factor
    , names_from = occupation
    , values_from = score
  ) %>%
  rename(
    top.match = 2
    , bot.match = 3
  ) %>% 
  mutate(
    top.match.diff = abs(top.match - you)
    , bot.match.diff = abs(bot.match - you)
  ) -> df_dumbbell

# -------- DYNAMIC TEXTS --------------------------------------------------
# VALUES FOR DYNAMIC TEXTS -----------------------------------------------
# Number of occupations
int_n.occupations <- nrow(df_KNN.output)

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
  slice(int_n.occupations) %>% 
  select(
    occupation
    , similarity
    , rank
    , rank.norm
  ) -> df_bot.match

# Median match
df_KNN.output %>% 
  filter(
    similarity == quantile(
      similarity, .50
    )
  ) %>% 
  slice(1) %>%
  select(
    occupation
    , similarity
    , rank
    , rank.norm
  ) -> df_med.match

# Recommended occupations (higher than cutff)
df_KNN.output %>% 
  filter(similarity >= dbl_recommended.cutff) %>% 
  nrow() -> int_n.recommended

# Percent of compatibility scores > cutff
(int_n.recommended / int_n.occupations) %>% 
  round(1) %>%
  findInterval(
    vec = seq_scale.1_6
  ) %>% 
  recode(
    '1' = 'extremely narrow'
    , '2' = 'very narrow'
    , '3' = 'rather narrow'
    , '4' = 'rather wide'
    , '5' = 'very wide'
    , '6' = 'extremely wide'
  ) -> chr_n.recommended

# Variance-adjusted skewness of professional compatibility curve interpretation
fun_capital.flex(df_KNN.output$similarity) %>%
  round(1) %>% 
  findInterval(
    # vec = seq_scale.1_6
    # vec = seq_scale.1_5
    vec = seq_scale.1_7
    , all.inside = T
  ) %>% 
  recode(
    '1' = 'exceptionally right-skewed'
    , '2' = 'largely right-skewed'
    , '3' = 'somewhat right-skewed'
    , '4' = 'somewhat normally distributed'
    , '5' = 'somewhat left-skewed'
    , '6' = 'largely left-skewed'
    , '7' = 'exceptionally left-skewed'
  ) -> chr_text.broadness
# recode(
#   '1' = 'exceptionally right-skewed'
#   , '2' = 'largely right-skewed'
#   , '3' = 'somewhat right-skewed'
#   , '4' = 'somewhat left-skewed'
#   , '5' = 'largely left-skewed'
#   , '6' = 'exceptionally left-skewed'
# ) -> chr_text.broadness
# recode(
#   '1' = 'largely right-skewed'
#   , '2' = 'somewhat right-skewed'
#   , '3' = 'somewhat normally distributed'
#   , '4' = 'somewhat left-skewed'
#   , '5' = 'largely left-skewed'
# ) -> chr_text.broadness


chr_text.broadness %>%
  recode(
    'exceptionally right-skewed' = 'your professional profile is specialized to an enormous extent, and it is almost certainly best for you to stick to a niche career path in which you excel, all else being equal'
    , 'largely right-skewed' = 'your professional profile is quite a bit specialized, and you would likely do better pursuing a niche career path, all else being equal'
    , 'somewhat right-skewed' = 'your professional profile is a little bit specialized, and you would likely do better not investing in too many different career paths, all else being equal'
    , 'somewhat normally distributed' = 'your professional profile is neither too broad nor too specialized, and your range of both highly recommended or highly incompatible occupations is limited, as your compatibility scores concentrate in the middle of the distribution. In other words, you\'re unlikely to be very good at many different career paths, and also unlikely to be very bad, having a reasonable compatibility with most occupations. But generally speaking, it is wiser to focus on the right end of the distribution, where you will probably excel'
    , 'somewhat left-skewed' = 'your professional profile is not too specialized, and you can likely thrive in a few different career paths, all else being equal'
    , 'largely left-skewed' = 'you could thrive in many different career paths, all else being equal'
    , 'exceptionally left-skewed' = 'you should fare essentially the same in most career paths, all else being equal'
  ) -> chr_text.broadness.interpretation
# recode(
#   'exceptionally right-skewed' = 'your professional profile is specialized to an enormous extent, and it is almost certainly best for you to stick to a niche career path in which you excel, all else being equal'
#   , 'largely right-skewed' = 'your professional profile is quite a bit specialized, and you would likely do better pursuing a niche career path, all else being equal'
#   , 'somewhat right-skewed' = 'your professional profile is a little bit specialized, and you would likely do better not investing in too many different career paths, all else being equal'
#   , 'somewhat left-skewed' = 'your professional profile is not too specialized, and you can likely thrive in a few different career paths, all else being equal'
#   , 'largely left-skewed' = 'you could thrive in many different career paths, all else being equal'
#   , 'exceptionally left-skewed' = 'you should fare essentially the same in most career paths, all else being equal'
# ) -> chr_text.broadness.interpretation
# recode(
#     'largely right-skewed' = 'your professional profile is specialized to a significant extent, and it is almost certainly best for you to stick to a niche career path in which you excel, all else being equal'
#     , 'somewhat right-skewed' = 'your professional profile is quite a bit specialized, and you would likely do better pursuing a niche career path, all else being equal'
# , 'somewhat normally distributed' = 'your professional profile is not too specialized nor is it too broad as well, and your range of either recommended or incompatible occupations will be restricted, as most of your compatibility scores concentrate in the middle of the distribution. In other words, you\'re unlikely to be very good at many different career paths, and also unlikely to be very bad. But generally speaking, it is wiser to focus on the right end of the distribution, where you\'ll probably excel'
#     , 'somewhat left-skewed' = 'your professional profile is not too specialized, and you can likely thrive in a few different career paths, all else being equal'
#     , 'largely left-skewed' = 'you could thrive in many different career paths, all else being equal'
#   ) -> chr_text.broadness.interpretation

# Top match comments
df_dumbbell %>% 
  filter(top.match > you) %>% 
  pull(factor) -> chr_top.underqualified

df_dumbbell %>% 
  filter(top.match < you) %>% 
  pull(factor) -> chr_top.overqualified

chr_top.underqualified %>% 
  length() %>% 
  as.character() %>%
  str_replace('0', 'not a single') %>% 
  str_replace(
    as.character(
      nrow(df_dumbbell)
    ), 'all'
  ) -> chr_top.underqualified.n

case_when(
  length(chr_top.underqualified) == 0 ~ ' whatsover'
  , length(chr_top.underqualified) == nrow(df_dumbbell) ~ 's'
  , length(chr_top.underqualified) == 1 ~ 
    chr_top.underqualified %>%
    paste0('"',.,'"') %>%
    paste0(collapse = ', ') %>%
    stri_replace_last_fixed(', ', ', and ') %>%
    paste0('(viz. ',.,')') %>% 
    paste('', .)
  , T ~ chr_top.underqualified %>%
    paste0('"',.,'"') %>%
    paste0(collapse = ', ') %>%
    stri_replace_last_fixed(', ', ', and ') %>%
    paste0('(viz. ',.,')') %>%
    paste('s', .)
) -> chr_top.underqualified.viz

case_when(
  length(chr_top.overqualified) == 0 ~ 'none whatsover'
  , length(chr_top.overqualified) == nrow(df_dumbbell) ~ 'all of them'
  , length(chr_top.overqualified) == (nrow(df_dumbbell) - 1) ~ 'all the others' 
  , length(chr_top.overqualified) <= 4 & 
    length(chr_top.overqualified) > 0 ~ 
    chr_top.overqualified %>%
    paste0('"',.,'"') %>%
    paste0(collapse = ', ') %>%
    stri_replace_last_fixed(', ', ', and ') 
  , T ~ chr_top.overqualified %>%
    head(3) %>% 
    paste0('"',.,'"') %>%
    paste0(collapse = ', ') %>%
    paste0(., ', and so on')
) -> chr_top.overqualified.viz

df_dumbbell %>% 
  slice_max(you, n = 3) %>% 
  slice(1:3) %>% 
  pull(factor) %>% 
  str_sort() %>% 
  paste0('"', . , '"') %>% 
  paste0(collapse = ', ') %>% 
  stri_replace_last_fixed(', ', ', and ') -> chr_top.3str

df_dumbbell %>% 
  slice_max(top.match, n = 3) %>% 
  slice(1:3) %>% 
  pull(factor) %>%
  str_sort() %>% 
  paste0('"', . , '"') %>% 
  paste0(collapse = ', ') %>% 
  stri_replace_last_fixed(', ', ', and ') -> chr_top.match.3str

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
    paste0('"',.,'"') %>%
    paste0(collapse = ', ') %>%
    stri_replace_last_fixed(', ', ', and ') %>%
    paste('the', .)
) -> chr_bot.underqualified.viz 

case_when(
  length(chr_bot.overqualified) == 0 ~ 'none'
  , length(chr_bot.overqualified) == nrow(df_dumbbell) ~ 'every aspect'
  , length(chr_bot.overqualified) >= (nrow(df_dumbbell) - 3) &
    length(chr_bot.overqualified) != nrow(df_dumbbell) ~ 'the rest of them'
  , T ~ chr_bot.overqualified %>%
    paste0('"',.,'"') %>%
    paste0(collapse = ', ') %>%
    stri_replace_last_fixed(', ', ', and ')
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
    paste0('"', . , '"') %>% 
    paste0(collapse = ', ') %>% 
    stri_replace_last_fixed(', ', ', and ')
) -> chr_bot.3str

chr_bot.match.3str %>%
  str_sort() %>% 
  paste0('"', . , '"') %>% 
  paste0(collapse = ', ') %>% 
  stri_replace_last_fixed(', ', ', and ') -> chr_bot.match.3str

# Finishing remarks
chr_text.broadness %>%
  recode(
  'exceptionally right-skewed' = 'an exceptionally specialized or niche'
  , 'largely right-skewed' = 'a very specialized or niche'
  , 'somewhat right-skewed' = 'a somewhat specialized or niche'
  , 'somewhat normally distributed' = 'a not too broad, nor too specialized, but fairly distributed'
  , 'somewhat left-skewed' = 'a somewhat broad or multidisciplinary'
  , 'largely left-skewed' = 'a very broad or multidisciplinary'
  , 'exceptionally left-skewed' = 'an exceptionally broad or multidisciplinary'
) -> chr_text.broadness2

chr_text.broadness %>%
  recode(
  'exceptionally right-skewed' = 'it is almost definitely wiser for you to invest in one, maybe two, fields of expertise in which you have more ability'
  , 'largely right-skewed' = 'it is probably wiser for you to invest in not many different fields of expertise, but concentrate on those in which you have more ability'
  , 'somewhat right-skewed' = 'that even though it is still possible for you to have more than one field of expertise, it would be probably wiser not to spread out your efforts too much, but concentrate on those fields in which you have more ability'
  , 'somewhat normally distributed' = 'you have reasonable similarity with many different careers, but actually good or bad matches are reduced in quantity. However, despite having adequate matching percentages with many occupations, you should probably focus on those fields in which you have more ability'
  , 'somewhat left-skewed' = 'you may actually find yourself to be a good fit for more than a single career path'
  , 'largely left-skewed' = 'you can probably benefit quite a bit from investing in more than a single career path'
  , 'exceptionally left-skewed' = 'you will almost definitely benefit from investing in more than a single career path, and specialization may actually hinder your ability to exercize all of your different competencies'
) -> chr_text.broadness2.interpretation

(length(chr_top.overqualified) / nrow(df_dumbbell)) %>% 
  findInterval(
    vec = seq_scale.1_5
  ) %>% 
  recode(
    '1' = 'mostly underqualified'
    , '2' = 'somewhat underqualified'
    , '3' = 'somewhat overqualified'
    , '4' = 'mostly overqualified'
    , '5' = 'completely overqualified'
  ) -> chr_top.capacity

(length(chr_bot.overqualified) / nrow(df_dumbbell)) %>% 
  findInterval(
    vec = seq_scale.1_5
  ) %>% 
  recode(
    '1' = 'mostly underqualified'
    , '2' = 'somewhat underqualified'
    , '3' = 'somewhat overqualified'
    , '4' = 'mostly overqualified'
    , '5' = 'completely overqualified'
  ) -> chr_bot.capacity

if_else(
  chr_top.capacity == chr_bot.capacity
  , paste(chr_bot.capacity, 'as well')
  , chr_bot.capacity
) -> chr_bot.capacity

# GENERATE DYNAMIC TEXTS ------------------------------------------------------------
# Report Title
chr_text.report.title <- glue('Professional Profile — {chr_text.user}')

# Introduction dynamic text
fun_text.dynamic(
  .chr_text = chr_text.intro
  , .chr_pattern = chr_text.blank
  , chr_text.user
  , int_n.occupations
  , int_n.occupations
) -> chr_text.intro.dynamic

# Circular bar plot commentary
fun_text.dynamic(
  .chr_text = chr_text.circular_plot
  , .chr_pattern = chr_text.blank
  , int_n.occupations
) -> chr_text.circular_plot.dynamic

# Top / bot matches table commentary
fun_text.dynamic(
  .chr_text = chr_text.topbot.table
  , .chr_pattern = chr_text.blank
  , df_top.match$occupation
  , 100 * df_top.match$similarity
  , df_bot.match$occupation
  , 100 * df_bot.match$similarity
  , df_med.match$occupation
  , 100 * df_med.match$similarity
  , chr_n.recommended
  , int_n.recommended
) -> chr_text.topbot.table.dynamic

# Professional compatibility curve
fun_text.dynamic(
  .chr_text = chr_text.compatibility.curve.intro
  , .chr_pattern = chr_text.blank
) -> chr_text.compatibility.curve.intro.dynamic

fun_text.dynamic(
  .chr_text = chr_text.compatibility.curve
  , .chr_pattern = chr_text.blank
) -> chr_text.compatibility.curve.dynamic

# Professional compatibility distribution
fun_text.dynamic(
  .chr_text = chr_text.distribution.intro
  , .chr_pattern = chr_text.blank
) -> chr_text.distribution.intro.dynamic

fun_text.dynamic(
  .chr_text = chr_text.distribution
  , .chr_pattern = chr_text.blank
  , chr_text.broadness
  , chr_text.broadness.interpretation
) -> chr_text.distribution.dynamic

# Categories and factors
fun_text.dynamic(
  .chr_text = chr_text.factors.intro
  , .chr_pattern = chr_text.blank
) -> chr_text.factors.intro.dynamic

fun_text.dynamic(
  .chr_text = chr_text.factors1
  , .chr_pattern = chr_text.blank
  , length(list_factors)
  , paste0(
    paste0('"', names(list_factors), '"') 
    , collapse = ', '
  ) %>% 
    stri_replace_last_fixed(', ', ', and ')
  , length(list_factors)
  , length(flatten(list_factors))
) -> chr_text.factors1.dynamic

fun_text.dynamic(
  .chr_text = chr_text.factors2
  , .chr_pattern = chr_text.blank
  , names(list_factors)[1]
  , length(names(list_factors[[1]]))
  , paste0(
    paste0('"', names(list_factors[[1]]), '"') 
    , collapse = ', '
  ) %>% 
    stri_replace_last_fixed(', ', ', and ')
  , names(list_factors)[2]
  , paste0(
    paste0('"', names(list_factors[[2]]), '"') 
    , collapse = ', '
  ) %>% 
    stri_replace_last_fixed(', ', ', and ')
  , names(list_factors)[3]
  , length(names(list_factors[[3]]))
  , paste0(
    paste0('"', names(list_factors[[3]]), '"') 
    , collapse = ', '
  ) %>% 
    stri_replace_last_fixed(', ', ', and ')
  , length(flatten_chr(list_factors))
) -> chr_text.factors2.dynamic

# Top match
fun_text.dynamic(
  .chr_text = chr_text.top.intro
  , .chr_pattern = chr_text.blank
) -> chr_text.top.intro.dynamic

fun_text.dynamic(
  .chr_text = chr_text.top
  , .chr_pattern = chr_text.blank
  , df_top.match$occupation
  , df_dumbbell %>% 
    slice_min(top.match.diff) %>% 
    pull(factor) %>% 
    paste0('"',.,'"') %>% 
    paste0(collapse = ', ') %>% 
    stri_replace_last_fixed(', ', ', and ')
  , (df_dumbbell %>% 
       slice_min(top.match.diff) %>% 
       nrow() > 1) %>%
    if_else('s','')
  , df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    pull(factor) %>% 
    paste0('"',.,'"') %>% 
    paste0(collapse = ', ') %>% 
    stri_replace_last_fixed(', ', ', and ')
  , df_dumbbell %>% 
    slice_min(top.match.diff) %>% 
    slice(1) %>% 
    pull(top.match.diff) %>% 
    round(4) * 100
  , df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    slice(1) %>% 
    pull(top.match.diff) %>% 
    round(4) * 100
  , chr_top.underqualified.n
  , chr_top.underqualified.viz
  , chr_top.overqualified.viz
  , chr_top.3str
  , df_top.match$occupation
  , chr_top.match.3str
) -> chr_text.top.dynamic

# Bottom match
fun_text.dynamic(
  .chr_text = chr_text.bot.intro
  , .chr_pattern = chr_text.blank
  , df_bot.match$occupation
) -> chr_text.bot.intro.dynamic

fun_text.dynamic(
  .chr_text = chr_text.bot
  , .chr_pattern = chr_text.blank
  , df_dumbbell %>% 
    slice_min(bot.match.diff) %>% 
    pull(factor) %>% 
    paste0('"',.,'"') %>% 
    paste0(collapse = ', ') %>% 
    stri_replace_last_fixed(', ', ', and ')
  , (df_dumbbell %>% 
       slice_min(bot.match.diff) %>% 
       nrow() > 1) %>%
    if_else('s','')
  , (df_dumbbell %>% 
       slice_min(bot.match.diff) %>% 
       nrow() > 1) %>%
    if_else('are','is')
  , (df_dumbbell %>% 
       slice_min(bot.match.diff) %>% 
       nrow() > 1) %>%
    if_else('s','')
  , (df_dumbbell %>% 
       slice_max(bot.match.diff) %>% 
       nrow() > 1) %>%
    if_else('are','is')
  , df_dumbbell %>% 
    slice_max(bot.match.diff) %>% 
    pull(factor) %>% 
    paste0('"',.,'"') %>% 
    paste0(collapse = ', ') %>% 
    stri_replace_last_fixed(', ', ', and ')
  , df_dumbbell %>% 
    slice_min(bot.match.diff) %>% 
    slice(1) %>% 
    pull(bot.match.diff) %>% 
    round(4) * 100
  , df_dumbbell %>% 
    slice_max(bot.match.diff) %>% 
    slice(1) %>% 
    pull(bot.match.diff) %>% 
    round(4) * 100
  , chr_bot.underqualified.viz
  , (length(chr_bot.underqualified) > 1) %>% 
    if_else('s', '')
  , chr_bot.overqualified.viz
  , df_bot.match$occupation
  , chr_bot.match.3str
  , chr_bot.3str
) -> chr_text.bot.dynamic

# Finishing remarks
fun_text.dynamic(
  .chr_text = chr_finishing.remarks
  , .chr_pattern = chr_text.blank
  , int_n.recommended
  , chr_n.recommended
  , chr_text.broadness2
  , chr_text.broadness2.interpretation
  , df_top.match$occupation
  , df_bot.match$occupation
  , chr_top.capacity
  , chr_bot.capacity
  , chr_text.user
) -> chr_finishing.remarks.dynamic

# Captions for dynamic reporting with R Markdown
chr_text.caption.circular <- 'Professional Compatibility Ranking'
chr_text.caption.table <- 'Your Top 7 and Bottom 3 Career Matches'
chr_text.caption.line <- 'Professional Compatibility Curve'
chr_text.caption.dist <- 'Professional Compatibility Distribution'
chr_text.caption.dumbbell.top <- paste('Your Best Career Match —', str_to_title(chr_matches.topbot[1]))
chr_text.caption.dumbbell.bot <- paste('Your Worst Career Match —', str_to_title(chr_matches.topbot[2]))

# -------- TABLES ---------------------------------------------------------
# TOP 7, BOTTOM 3 MATCHES ----------------------------------------------------------
df_KNN.output %>%
  slice(1:7, seq(max(rank) - 2, max(rank))) %>%
  mutate(
    similarity = percent(similarity, accuracy = .01)
    , annual_wage_2021 = dollar(annual_wage_2021, accuracy = .01)
  ) %>% 
  select(
    rank
    , occupation
    , annual_wage_2021
    , similarity
  ) %>% 
  rename(
    Rank = rank
    , Occupation = occupation
    , Wage = annual_wage_2021
    , Compatibility = similarity
  ) -> df_top7.bot3

# -------- PLOTS ----------------------------------------------------------
# [CIRCULAR BAR PLOT] MATCHING PERCENTAGES -----------------------------------------------------
# Empty columns
int_NA <- 51
mtx_NA <- matrix(NA, int_NA, ncol(df_KNN.output))
colnames(mtx_NA) <- colnames(df_KNN.output)

# Circular bar plot
mtx_NA %>%
  rbind(df_KNN.output) %>%
  mutate(
    n = row_number()
    , n = factor(n)
    , recommended = if_else(
      round(similarity, 2) >= dbl_recommended.cutff
      | is.na(similarity)
      , true = 'Recommended'
      , false = 'Not Recommended'
    )
  ) %>% 
  fun_plot.bar(aes(
    x = n
    , y = similarity
    , fill = recommended
  ) 
  , .list_labs = list(
    title = NULL
    , subtitle = NULL
    , fill = NULL
  )
  , .chr_manual.pal = c(
    'Recommended' = list_atlas.pal$purple3
    , 'Not Recommended' = list_atlas.pal$grey
  )
  , .coord_polar = T
  , .reorder_fct = T
  , .reorder_desc = T
  , .fun_axis.y = scale_y_continuous
  , .list_axis.y.args = list(
    limits = c(-0.55,1.1)
  )
  # , .theme = ggridges::theme_ridges(font_size = 11, ) +
  , .theme = theme_void() + 
    theme(
      # legend.position = 'bottom'
      legend.position = c(0.5,0.05)
      , legend.direction = 'horizontal'
      , panel.grid = element_blank()
      , panel.border = element_blank()
      , plot.margin = margin(0, 0, 0, 0)
      , plot.title = element_blank()
      , plot.subtitle = element_blank()
      , axis.title = element_blank()
      , axis.text = element_blank()
      , axis.ticks = element_blank()
      , axis.line = element_blank()
    ) 
  ) -> plt_match.polar

plt_match.polar +
  coord_polar(
    # start = -6.12
    start = -6.1
  ) +
  map(
    seq(0,1,0.25)
    , function(y){
      
      annotate(
        x = '26'
        , y = y + 0.1
        , label = percent(y)
        , geom = 'text'
        , color = '#212121'
        , fontface = 'bold'
        , size = 3
      )
      
    }) + 
  annotate(
    x = '26'
    , y = -0.55
    , label = str_replace_all(
      'Professional Compatibility'
      , ' ', '\n')
    , geom = 'text'
    , color = list_atlas.pal$purple3
    , fontface = 'bold'
    , size = 4
  ) -> plt_match.polar

plt_match.polar$layers <- c(
  geom_hline(
    yintercept = c(0, 0.25, 0.5, 0.75)
    , color = list_atlas.pal$grey
    , size = 0.5
  )
  , plt_match.polar$layers
)

plt_match.polar$layers <- c(
  geom_hline(
    yintercept = 1
    , color = list_atlas.pal$grey
    , size = 2
  )
  , plt_match.polar$layers
)

# [LINE CHART] PROFESSIONAL COMPATIBILITY CURVE -------------------------------------------------------------------
df_KNN.output %>%
  fun_plot.line(aes(
    x = rank.norm
    , y = similarity
    , color = similarity >= dbl_recommended.cutff
  )
  , .dbl_limits.y = c(0,1)
  , .chr_manual.pal = c(
    list_atlas.pal$grey
    , list_atlas.pal$purple3
  )
  , .list_legend = list(color = 'none')
  , .fun_format.y = label_percent()
  , .reorder_fct = F
  , .reorder_desc = F
  , .theme = ggridges::theme_ridges(font_size = 11, center_axis_labels = T) +
    theme(
      plot.margin = margin(0, 0, 0, 0)
      # plot.margin = margin(
      #   t = 1.5, b = 1.5, l = 0, r = 0
      #   , unit = 'cm'
      # )
      , axis.text.x = element_blank()
    )
  , .list_labs = list(
    title = NULL
    , subtitle = NULL
    , x = str_to_title('ranking')
    , y = str_to_title('professional compatibility')
  )) +
  geom_segment(
    x = 0
    # , xend = 1 - df_bot.match$similarity
    , xend = 1
    # , y = df_bot.match$similarity
    , y = 0
    , yend = 1
    , size = 0.25
    , color = list_atlas.pal$black
    , linetype = 2
  ) -> plt_line.rank

c(
  plt_line.rank$layers
  , geom_textvline(
    xintercept = (int_n.occupations - int_n.recommended) / (int_n.occupations - 1)
    , label = 'Recommended'
    , color = list_atlas.pal$purple3
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
    , x = str_to_title('professional compatibility')
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
    , label = 'Recommended'
    , color = list_atlas.pal$green
    , fontface = 'bold'
    , linetype = 1
    , linewidth = 1.35
    , hjust = 0.125
    , vjust = -0.5
  ) -> plt_density

# [DUMBBELL PLOT] USER VS TOP MATCH / BOTTOM MATCH ------------------------------------
# Top match comparison
df_dumbbell %>%
  fun_plot.dumbbell(aes(
    x = you
    , xend = top.match
    , y = factor
  )
  , .list_axis.x.args = list(
    limits = c(-.1,1.1)
    , breaks = seq(0,1,.25)
  )
  , .fun_format.x = label_percent()
  , .fun_format.y = function(y){y}
  , .fun_format.labels = label_percent(accuracy = .01)
  , .list_labs = list(
    # title = str_to_title('best career match')
    title = NULL
    # , subtitle = str_to_title(glue(
    #   'your most compatible occupation is: {chr_matches.topbot[1]}.'
    #
    # ))
    # , subtitle = str_to_title(chr_matches.topbot[1])
    , subtitle = NULL
    , x = str_to_title('factor score')
    , y = NULL
  )
  ) -> plt_top.match

# Bottom match comparison
df_dumbbell %>%
  fun_plot.dumbbell(aes(
    x = you
    , xend = bot.match
    , y = factor
  )
  , .list_geom.param = list(
    color = 'lightgrey'
    , colour_x = list_atlas.pal$blue4
    , colour_xend = list_atlas.pal$red
    , size_x = 5.4
    , size_xend = 5.4
    , size = 2
  )
  , .list_labels1.param = list(
    fontface = 'bold'
    , color = list_atlas.pal$blue4
    , size = 3.33
    , vjust = -1.5
    , hjust = 0.5
  )
  , .list_labels2.param = list(
    fontface = 'bold'
    , color = list_atlas.pal$red
    , size = 3.33
    , vjust = 2.25
    , hjust = 0.5
  )
  , .list_axis.x.args = list(
    limits = c(-.1,1.1)
    , breaks = seq(0,1,.25)
  )
  , .fun_format.x = label_percent()
  , .fun_format.y = function(y){y}
  , .fun_format.labels = label_percent(accuracy = .01)
  , .list_labs = list(
    # title = str_to_title('worst career match')
    title = NULL
    # , subtitle = str_to_title(glue(
    #   'your least compatible occupation is: {chr_matches.topbot[2]}.'
    # ))
    # , subtitle = str_to_title(chr_matches.topbot[2])
    , subtitle = NULL
    , x = str_to_title('factor score')
    , y = NULL
  )
  ) -> plt_bot.match

# patchwork::wrap_plots(
#   plt_top.match
#   , plt_bot.match +
#     theme(axis.text.y = element_blank())
#   , ncol = 2
# ) -> plt_topbot.match

# -------- RENDER -----------------------------------------------------------
# RENDER R MARKDOWN REPORT --------------------------------------------------
rmarkdown::render(
  'C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/Matching_Report.Rmd'
  , output_file = paste0('Matching Report (', chr_text.user, ').pdf')
)

