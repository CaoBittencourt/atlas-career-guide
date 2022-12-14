# --- SETUP -----------------------------------------------------------
# PARAMETERS  --------------------------------------------------------------
# KNN parameters
dbl_threshold <- 0.17

# Dynamic text parameters
chr_text.blank <- '___'

# Scales
seq_scale.1_5 <- seq(0,1,.25)
seq_scale.1_6 <- round(seq(0, 0.9, 1/6), 2)
seq_scale.1_7 <- c(.33, .33 + .17/2, .50, .50 + .17/2, .67, .67 + .17/2)
seq_scale.1_8 <- round(seq(0,1,1/7), 2)

# Recommendation cutoff
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

# --- DATA -----------------------------------------------------------
# EFA-REDUCED QUERY VECTOR -----------------------------------------------
# User questionnaire data frame
from_json(body) %>%
  as_tibble() %>%
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

# --- RESULTS --------------------------------------------------------
# KNN MATCHING ---------------------------------------------------------------
fun_KNN.matching(
  .df_data.numeric = df_occupations
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = dbl_threshold
  , .dbl_decimals = 4
) %>% 
  full_join(df_occupations) -> df_KNN.output

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

# --- DYNAMIC TEXTS --------------------------------------------------
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

# Mean
list_df_text$centrality %>%
  mutate(
    mean = mean(df_KNN.output$similarity) 
    , n.interval = 
      mean %>%
      round(1) %>%
      recode(
        '0' = -Inf
        , '1' = Inf
      ) %>%
      findInterval(vec = seq_scale.1_8)
  ) %>% 
  filter(interval == n.interval) -> df_text.mean

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
  length(chr_top.underqualified) == 0 ~ 'not a single'
  , length(chr_top.underqualified) == nrow(df_dumbbell) ~ 'all'
  , T ~ length(chr_top.underqualified) %>% 
    english() %>% 
    as.character()
) -> chr_top.underqualified.n

case_when(
  length(chr_top.underqualified) == 0 ~ ' whatsover'
  , length(chr_top.underqualified) == nrow(df_dumbbell) ~ 's'
  , length(chr_top.underqualified) == 1 ~ 
    chr_top.underqualified %>%
    fun_text.commas() %>% 
    paste0('(viz. ',.,')') %>% 
    paste('', .)
  , T ~ chr_top.underqualified %>%
    fun_text.commas() %>% 
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
    fun_text.commas()
  , T ~ chr_top.overqualified %>%
    head(3) %>% 
    fun_text.commas(.chr_last.comma = ', ') %>% 
    paste('and so on')
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

# GENERATE DYNAMIC TEXTS ------------------------------------------------------------
# Report Title
chr_text.report.title <- paste0('Professional Profile — ', chr_text.user)

# Introduction dynamic text
fun_text.dynamic(
  .chr_text = list_sections$introduction
  , .chr_pattern = chr_text.blank
  , chr_text.user
  , int_n.occupations
  , int_n.occupations
) -> chr_text.intro.dynamic

# Circular bar plot commentary
fun_text.dynamic(
  .chr_text = list_sections$circular_bar_chart
  , .chr_pattern = chr_text.blank
  , int_n.occupations
) -> chr_text.circular_plot.dynamic

# Top / bot matches table commentary
fun_text.dynamic(
  .chr_text = list_sections$top_bot_table
  , .chr_pattern = chr_text.blank
  , df_top.match$occupation
  , percent(df_top.match$similarity, accuracy = .01)
  , df_bot.match$occupation
  , percent(df_bot.match$similarity, accuracy = .01)
  , df_med.match$occupation
  , percent(df_med.match$similarity, accuracy = .01)
  , df_text.recommended$text
  , df_text.recommended$n.recommended
) -> chr_text.topbot.table.dynamic

# Professional compatibility curve
fun_text.dynamic(
  .chr_text = list_sections$line_chart_intro
  , .chr_pattern = chr_text.blank
) -> chr_text.compatibility.curve.intro.dynamic

fun_text.dynamic(
  .chr_text = list_sections$line_chart
  , .chr_pattern = chr_text.blank
) -> chr_text.compatibility.curve.dynamic

# Professional compatibility distribution
fun_text.dynamic(
  .chr_text = list_sections$distribution_intro
  , .chr_pattern = chr_text.blank
) -> chr_text.distribution.intro.dynamic

fun_text.dynamic(
  .chr_text = list_sections$distribution
  , .chr_pattern = chr_text.blank
  , df_text.flexibility$text1
  , df_text.flexibility$text2
) -> chr_text.distribution.dynamic

# Categories and factors
fun_text.dynamic(
  .chr_text = list_sections$factors_intro
  , .chr_pattern = chr_text.blank
) -> chr_text.factors.intro.dynamic

fun_text.dynamic(
  .chr_text = list_sections$factors
  , .chr_pattern = chr_text.blank
  , length(list_factors) %>% english() %>% as.character()
  , names(list_factors) %>% fun_text.commas()
  , length(list_factors) %>% english() %>% as.character()
  , length(flatten(list_factors)) %>% english() %>% as.character()
  , names(list_factors)[1]
  , length(names(list_factors[[1]])) %>% english() %>% as.character()
  , names(list_factors[[1]]) %>% fun_text.commas()
  , names(list_factors)[2]
  , names(list_factors[[2]]) %>% fun_text.commas()
  , names(list_factors)[3]
  , length(names(list_factors[[3]])) %>% english() %>% as.character()
  , names(list_factors[[3]]) %>% fun_text.commas()
  , length(flatten_chr(list_factors)) %>% english() %>% as.character()
) -> chr_text.factors.dynamic

# Top match
fun_text.dynamic(
  .chr_text = list_sections$top_match_intro
  , .chr_pattern = chr_text.blank
) -> chr_text.top.intro.dynamic

fun_text.dynamic(
  .chr_text = list_sections$top_match
  , .chr_pattern = chr_text.blank
  , df_top.match$occupation
  , df_dumbbell %>% 
    slice_min(top.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas()
  , (df_dumbbell %>% 
       slice_min(top.match.diff) %>% 
       nrow() > 1) %>%
    if_else('s','')
  , df_dumbbell %>% 
    slice_max(top.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas()
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
  .chr_text = list_sections$bot_match_intro
  , .chr_pattern = chr_text.blank
  , df_bot.match$occupation
) -> chr_text.bot.intro.dynamic

fun_text.dynamic(
  .chr_text = list_sections$bot_match
  , .chr_pattern = chr_text.blank
  , df_dumbbell %>% 
    slice_min(bot.match.diff) %>% 
    pull(factor) %>% 
    fun_text.commas()
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
    fun_text.commas()
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
  .chr_text = list_sections$finishing_remarks
  , .chr_pattern = chr_text.blank
  , df_text.recommended$n.recommended
  , df_text.recommended$text
  , df_text.flexibility$text3
  , df_text.flexibility$text4
  , df_top.match$occupation
  , df_bot.match$occupation
  , list_df_text.capacity$top.match$text
  , list_df_text.capacity$bot.match$text
  , chr_text.user
) -> chr_finishing.remarks.dynamic

# Captions for dynamic reporting with R Markdown
chr_text.caption.circular <- 'Professional Compatibility Ranking'
chr_text.caption.table <- 'Your Top 7 and Bottom 3 Career Matches'
chr_text.caption.line <- 'Professional Compatibility Curve'
chr_text.caption.dist <- 'Professional Compatibility Distribution'
chr_text.caption.dumbbell.top <- paste('Your Best Career Match —', str_to_title(chr_matches.topbot[1]))
chr_text.caption.dumbbell.bot <- paste('Your Worst Career Match —', str_to_title(chr_matches.topbot[2]))

# --- TABLES ---------------------------------------------------------
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

# --- PLOTS ----------------------------------------------------------
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
    , xend = 1
    , y = 0
    , yend = 1
    , size = 0.25
    , color = list_atlas.pal$black
    , linetype = 2
  ) -> plt_line.rank

c(
  plt_line.rank$layers
  , geom_textvline(
    xintercept = (int_n.occupations - df_text.recommended$n.recommended) / (int_n.occupations - 1)
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
    title = NULL
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
    title = NULL
    , subtitle = NULL
    , x = str_to_title('factor score')
    , y = NULL
  )
  ) -> plt_bot.match

# --- RENDER -----------------------------------------------------------
# RENDER R MARKDOWN REPORT --------------------------------------------------
rmarkdown::render(
  './matching_report.Rmd'
  , output_file = paste0('Matching Report (', chr_text.user, ').pdf')
)

