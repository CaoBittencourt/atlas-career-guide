# ------- SETUP -----------------------------------------------------------
# PACKAGES ----------------------------------------------------------------
pkg <- c(
  'psych' #Factor Analysis
  , 'FNN' #Fast K-NN Algorithm (faster than the 'class' package)
  , 'jsonify' #Work with JSON (faster than jsonlite)
  , 'ggthemes' #Data visualization
  , 'tidyverse', 'glue' #Data wrangling
  , 'tinytex' #LaTeX
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
chr_text.user <- 'Cao'
# chr_text.user <- 'Gabriel'

# KNN parameters
dbl_threshold <- 0.17

# Dynamic text parameters
chr_text.blank <- '___'

# DATA --------------------------------------------------------------------
# EFA-REDUCED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.EFA.R')

# EFA-REDUCED POPULATION-WEIGHTED OCCUPATIONS DATA FRAME
source('C:/Users/Cao/Documents/Github/Atlas-Research/Data/df_occupations.pop.EFA.R')

# USER INPUT DATA FRAME
df_input <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv')

# DEFAULT TEXTS FOR IMPUTATION
source('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/Default_texts.R')

# ------- DATA -----------------------------------------------------------
# EFA-REDUCED OCCUPATIONS DATA FRAME -----------------------------------------------
# Select only necessary variables
df_occupations %>% 
  select(
    occupation
    , entry_level_education
    , all_of(
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
        recode((x + 2)
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
  .df_data.numeric = df_occupations
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = dbl_threshold
  , .dbl_decimals = 4
) -> df_KNN.output

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

# # FACTOR SCORES (POPULATION) -----------------------------------------------------------
# fun_factor.scores(
#   .df_data.numeric = df_occupations.pop
#   , .list_factor.keys = list_factors
#   , .lgc_pivot.long = F
# ) -> list_factor.scores.pop

# -------- PLOTS ----------------------------------------------------------
# [LINE CHART] PROFESSIONAL COMPATIBILITY CURVE -------------------------------------------------------------------
df_KNN.output %>% 
  arrange(similarity) %>% 
  mutate(n = row_number()) %>% 
  fun_plot.line(aes(
    x = n
    , y = similarity
  )
  , .dbl_limits.y = c(0,1)
  , .fun_format.y = label_percent()
  , .reorder_fct = F
  , .reorder_desc = F
  # , .reorder_fun = max
  , .theme = ggridges::theme_ridges(center_axis_labels = T) +
    theme(axis.text.x = element_blank())
  , .list_labs = list(
    title = str_to_title('professional compatibility curve')
    , subtitle = str_to_title('these are your professional matches ranked lowest to highest:')
    , x = str_to_title('ranking')
    , y = str_to_title('professional compatibility')
  )) -> plt_line.rank

# [LOLLIPOP CHART] TOP 3, BOTTOM 3 AND 6 SAMPLE MATCHES -----------------------------
df_KNN.output %>% 
  mutate(n = row_number()) %>% 
  filter(
    n %in% c(1:3, (nrow(.) - 2):nrow(.))
    | (#Get good matches with shorter names
      str_length(occupation) < 40
      & between(
        similarity
        , quantile(
          similarity, probs = 0.2 
        )
        , quantile(
          similarity, probs = 0.8
        )
      ))
  ) %>% 
  arrange(desc(similarity)) %>%
  slice(
    1:3 #Top 3 matches
    , sample(4:(nrow(.) - 3), 6) #6 sample matches
    , (nrow(.) - 2):nrow(.) #Bottom 3 matches
  ) %>% 
  fun_plot.lollipop(aes(
    x = occupation
    , y = similarity
    , label = percent(similarity)
  )
  , .fun_format.x = function(x){str_wrap(x, width = 40)}
  , .fun_format.y = label_percent()
  , .dbl_limits.y = c(0,1)
  , .list_labs = list(
    title = str_to_title(glue('a sample of your {nrow(df_KNN.output)} professional matches'))
    , subtitle = str_to_title('these are 10 of your career matches, including the top 3 and bottom 3:')
    # Substitute caption for note in R Markdown later
    , caption = 'To access your full professional matching report, visit https://www.go2atlas.com/ and become a member today.'
    , x = NULL
    , y = str_to_title('professional compatibility')
  )
  ) -> plt_match.10

# [DUMBBELL PLOT] USER VS TOP MATCH / BOTTOM MATCH ------------------------------------
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
  ) -> df_dumbbell

# Top match comparison
df_dumbbell %>%
  fun_plot.dumbbell(aes(
    x = you
    , xend = top.match
    , y = factor
  )
  , .dbl_limits.x = c(0,1)
  , .list_labs = list(
    title = str_to_title('best professional match')
    , subtitle = str_to_title(glue(
      'your most compatible occupation is: {chr_matches.topbot[1]}.'
      
    ))
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
    , colour_x = '#182766'
    , colour_xend = '#CE3527'
    , size_x = 5.4
    , size_xend = 5.4
    , size = 2
  )
  , .list_labels1.param = list(
    fontface = 'bold'
    , color = '#182766'
    , size = 3.33
    , vjust = -1.5
    , hjust = 0.5
  )
  , .list_labels2.param = list(
    fontface = 'bold'
    , color = '#CE3527'
    , size = 3.33
    , vjust = 2.25
    , hjust = 0.5
  )
  , .dbl_limits.x = c(0,1)
  , .list_labs = list(
    title = str_to_title('worst professional match')
    , subtitle = str_to_title(glue(
      'your least compatible occupation is: {chr_matches.topbot[2]}.'
    ))
    , x = str_to_title('factor score')
    , y = NULL
  )
  ) -> plt_bot.match

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
  ) %>%
  fun_plot.bar(aes(
    x = n
    , y = similarity
    # , fill = similarity
  )
  # , .scale_colors = scale_fill_viridis()
  , .list_labs = list(
    title = str_to_title('Your career matches')
    , subtitle = str_to_title("how compatible are you with each occupation?")
  )
  , .coord_polar = T
  , .reorder_fct = T
  , .reorder_desc = T
  , .fun_axis.y = scale_y_continuous
  , .list_axis.y.args = list(
    limits = c(-0.55,1.1)
  )
  , .theme = ggridges::theme_ridges() +
    theme(
      plot.title = element_text(hjust = 0.5)
      , axis.title = element_blank()
      , axis.text = element_blank()
      , axis.ticks = element_blank()
      , panel.grid = element_blank()
      , plot.subtitle = element_text(hjust = 0.5)
    )
  ) +
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
      
    }) -> plt_match.polar

plt_match.polar$layers <- c(
  geom_hline(
    yintercept = c(0, 0.25, 0.5, 0.75)
    , color = '#D4D5D8'
    , size = 0.5
  )
  , plt_match.polar$layers
)

plt_match.polar$layers <- c(
  geom_hline(
    yintercept = 1
    , color = '#D4D5D8'
    , size = 2
  )
  , plt_match.polar$layers
)

# # [DENSITY] SAMPLE DENSITY (TUTORIAL) -------------------------------------
# map(
#   df_occupations.pop %>% select(ends_with('.l'))
#   , ~ fun_plot.density(
#     .df_data = df_occupations.pop
#     , .aes_mapping = aes(x = .x)
#   )
# ) -> list_densities
# 
# list_densities[1]
# list_densities[2]
# list_densities[3]
# list_densities[4]
# list_densities[5]
# list_densities[6]
# list_densities[7]
# list_densities[8]
# list_densities[9]
# list_densities[10]
# list_densities[11]
# list_densities[12]
# list_densities[13]
# list_densities[14]
# list_densities[15]
# list_densities[16]
# list_densities[17]
# list_densities[18]
# list_densities[19]
# list_densities[20]
# list_densities[21]
# list_densities[22]
# list_densities[23]
# list_densities[24]
# list_densities[25]
# list_densities[26]
# list_densities[27]
# list_densities[28]
# list_densities[29]
# list_densities[30]
# list_densities[31]
# list_densities[32]
# list_densities[33]
# 
# 
# df_KNN.output %>%
#   select(ends_with('.l')) %>%
#   colnames() %>%
#   sample(1) -> chr_sample
# 
# df_input %>%
#   select(chr_sample) %>%
#   mutate(
#     occupation = 'you'
#     , .before = 1
#   ) %>%
#   bind_rows(
#     df_occupations.pop %>%
#       select(
#         occupation
#         , chr_sample
#       )
#   ) %>%
#   rename(value = 2) %>%
#   fun_plot.density(aes(
#     x = value
#   ))
# 
# 
# 
# # [DENSITIES] DENSITIES FOR EACH ITEM -------------------------------------

# # RENDER PLOTS ------------------------------------------------------------
# plt_line.rank
# plt_match.polar
# plt_match.10
# plt_top.match
# plt_bot.match

# -------- DYNAMIC TEXTS --------------------------------------------------
# NUMBERS FOR DYNAMIC TEXTS -----------------------------------------------
# Number of occupations
int_n.occupations <- nrow(df_KNN.output)

# Top match
df_KNN.output %>% 
  slice(1) %>% 
  select(
    occupation
    , similarity
  ) -> df_top.match

# Bottom match
df_KNN.output %>% 
  slice(int_n.occupations) %>% 
  select(
    occupation
    , similarity
  ) -> df_bot.match

# Median match
df_KNN.output %>% 
  filter(
    similarity == quantile(
      similarity, .50
    )
  ) %>% 
  select(
    occupation
    , similarity
  ) -> df_med.match

# Higher than 50% compatibility
df_KNN.output %>% 
  filter(round(similarity, 1) > 0.5) %>%
  nrow() -> int_n.above50pct

# Percent of compatibility scores > 50%
(int_n.above50pct / int_n.occupations) %>% 
  findInterval(
    # vec = round(seq(0, 1, 1/6), 2)
    vec = round(seq(0, .95, 1/6), 2)
  ) %>% 
  # recode(
  #   '1' = 'extremely narrow'
  #   , '2' = 'very narrow'
  #   , '3' = 'rather narrow'
  #   , '4' = 'medium'
  #   , '5' = 'rather wide'
  #   , '6' = 'very wide'
  #   , '7' = 'extremely wide'
  # ) -> chr_n.above50pct
  recode(
    '1' = 'extremely narrow'
    , '2' = 'very narrow'
    , '3' = 'rather narrow'
    , '4' = 'rather wide'
    , '5' = 'very wide'
    , '6' = 'extremely wide'
  ) -> chr_n.above50pct

# Variance-adjusted skewness of professional compatibility curve interpretation
fun_capital.flex(df_KNN.output$similarity) %>%
  findInterval(
    # vec = round(seq(0, 1, 1/6), 2)
    vec = round(seq(0, .9, 1/6), 2)
  ) %>% 
  recode(
    '1' = 'exceptionally right-skewed and invariant'
    , '2' = 'largely right-skewed and invariant'
    , '3' = 'somewhat right-skewed and invariant'
    , '4' = 'somewhat left-skewed and varied'
    , '5' = 'largely left-skewed and varied'
    , '6' = 'exceptionally left-skewed and varied'
  ) -> chr_text.broadness

# niche occupations
chr_text.broadness %>%
  recode(
    'exceptionally right-skewed and invariant' = 'your professional profile is specialized to an enormous extent, and it is almost certainly best for you to stick to a niche career path in which you excel, all else being equal'
    , 'largely right-skewed and invariant' = 'your professional profile is quite a bit specialized, and you would likely do better pursuing a niche career path, all else being equal'
    , 'somewhat right-skewed and invariant' = 'your professional profile is a little bit specialized, and you would likely do better not investing in too many different career paths, all else being equal'
    , 'somewhat left-skewed and varied' = 'your professional profile is not too specialized, and you can likely thrive in a few different career paths, all else being equal'
    , 'largely left-skewed and varied' = 'you could thrive in many different career paths, all else being equal'
    , 'exceptionally left-skewed and varied' = 'you should fare essentially the same in most career paths, all else being equal'
  ) -> chr_text.broadness.interpretation

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

# Professional compatibility curve commentary
fun_text.dynamic(
  .chr_text = chr_text.compatibility.curve
  , .chr_pattern = chr_text.blank
  , df_top.match$occupation
  , 100 * df_top.match$similarity
  , df_bot.match$occupation
  , 100 * df_bot.match$similarity
  , df_med.match$occupation
  , 100 * df_med.match$similarity
  , chr_n.above50pct
  , int_n.above50pct
  , int_n.occupations
  , chr_text.broadness
  , chr_text.broadness.interpretation
) -> chr_text.compatibility_curve.dynamic


# Numbers for dynamic reporting with R Markdown

# Captions for dynamic reporting with R Markdown
chr_text.caption1 <- 'Note: dsds'

# RENDER R MARKDOWN REPORT --------------------------------------------------
rmarkdown::render('C:/Users/Cao/Documents/Github/Atlas-Research/Reports/Matching Report/Matching_Report.Rmd')
