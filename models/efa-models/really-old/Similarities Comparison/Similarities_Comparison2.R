# ------- SETUP -------------------------------------------------------------------
# PACKAGES -----------------------------------------------------------------
pkg <- c(
  'psych', 'GPArotation' #Factor analysis
  , 'text2vec' #Cosine similarity
  , 'ggthemes', 'viridis', 'patchwork' #Data visualization
  , 'glue', 'tidyverse' #Data wrangling
)

# Activate / install packages
lapply(pkg, function(x)
  if(!require(x, character.only = T))
  {install.packages(x); require(x)})

# Package citation
# lapply(pkg, function(x)
#   {citation(package = x)})


# WORKING DIRECTORY -------------------------------------------------------
setwd('C:/Users/Cao/Documents/Github/Atlas-Research/Career-Choice-Models')

# KNN MATCHING ---------------------------------------------------------------
source('./KNN_Matching.R')


# ------- DATA -------------------------------------------------------------------------

# SKILLS FACTOR LIST -----------------------------------------------------------------
list_skill.factors <- list(
  'General' = c(
    # Factor 1 is composed of cognitive, non-technical, general skills (general competencies)
    'Judgment_and_Decision.L'
    , 'Active_Learning.L'
    , 'Critical_Thinking.L'
  )
  , 'Technical' = c(
    # Factor 2 is composed of mechanical, hands-on, specialist skills (technical)
    'Equipment_Selection.L'
    , 'Troubleshooting.L'
    , 'Operations_Monitoring.L'
  )
)

# ABILITIES FACTOR LIST ---------------------------------------------------
list_ablt.factors <- list(
  'Perception' = c(
    # Factor 1 is composed of perceptual abilities (perception):
    'Night_Vision.L'
    , 'Sound_Localization.L'
    , 'Glare_Sensitivity.L'
  )
  , 'Dexterity' = c(
    # Factor 2 is composed of manual abilities (dexterity):
    'Finger_Dexterity.L'
    , 'Arm_Hand_Steadiness.L'
    , 'Control_Precision.L'
  )
  , 'Robustness' = c(
    # Factor 3 is composed of bodily robustness, potency, and coordination (overall body robustness)
    'Stamina.L'
    , 'Gross_Body_Coordination.L'
    , 'Trunk_Strength.L'
  )
  , 'Intelligence' = c(
    # Factor 4 is composed of cognitive abilities (intelligence):
    'Inductive_Reasoning.L'
    , 'Problem_Sensitivity.L'
    , 'Deductive_Reasoning.L'
  )
)

# KNOWLEDGE FACTOR LIST ---------------------------------------------------
list_know.factors <- list(
  'Health' = c(
    # Factor 1 is composed of health-related fields of knowledge (health / help).
    'Therapy_and_Counseling.L'
    , 'Psychology.L'
    , 'Medicine_and_Dentistry.L'
  )
  , 'Building' = c(
    # Factor 2 is composed of engineering / building-related fields of knowledge (build).
    'Physics.L'
    , 'Engineering_and_Technology.L'
    , 'Mechanical.L'
  ) 
  , 'Business' = c(
    # Factor 3 is composed of financial and enterprising fields of knowledge (FGV).
    'Economics_and_Accounting.L'
    , 'Sales_and_Marketing.L'
    , 'Administration_and_Management.L'
  )
  , 'Arts_Humanities' = c(
    # Factor 4 is composed of arts and humanities (communists).
    'History_and_Archeology.L'
    , 'Geography.L'
    , 'Fine_Arts.L'
  ) 
)
# ALL FACTORS LIST -------------------------------------------------------------
list_factors <- list( 
  'Skills' = list_skill.factors
  , 'Abilities' = list_ablt.factors
  , 'Knowledge' = list_know.factors
)


# DATA (OCCUPATIONS) ------------------------------------------------------
# Occupations data frame
df_occupations <- readr::read_csv('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=563902602&single=true&output=csv')

# Matching data frame
# Only highly qualified professions
df_occupations %>%
  filter(
    Entry_level_Education %in% c(
      "Bachelor's degree"
      , "Doctoral or professional degree"
      , "Associate's degree"
      , "Master's degree"
    )
  ) -> df_occupations

# Select only necessary variables
df_occupations %>%
  select(
    Occupation
    , Career_Cluster
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
      , .fns = function(x){x/100}
    )
  ) -> df_occupations


# DATA (USERS) ------------------------------------------------------------
# User questionnaires data frame
df_input.all <- read_csv(url('https://docs.google.com/spreadsheets/d/e/2PACX-1vSphzWoCxoNaiaJcQUWKCMqUAT041Q8UqUgM7rSzIwYZb7FhttKJwNgtrFf-r7EgzXHFom4UjLl2ltk/pub?gid=725827850&single=true&output=csv'))

df_input.all %>% 
  select(
    Name
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
               , '1' = .0
               , '2' = .25
               , '3' = .5
               , '4' = .75
               , '5' = 1
        )}
    )
  ) -> df_input.all

# SELECTED USER -----------------------------------------------------------
# For this example, use Martijn' questionnaire 
# chr_user <- 'Martijn'
chr_user <- 'Cao'

df_input.all %>% 
  filter(Name == chr_user) %>%
  select(-Name) -> df_input

# ------- KNN MATCHING -------------------------------------------------------------------------
# NO IMPUTATION --------------------------------------------
fun_KNN.matching(
  .df_data.numeric = df_occupations
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = F
  , .dbl_decimals = 4
) -> df_KNN.output

# OVERQUALIFICATION IMPUTATION --------------------------------------------
# OVERQUALIFICATION IMPUTATION - 100% UNNECESSARY COMPETENCY (0)
# OVERQUALIFICATION IMPUTATION - 5-POINT LIKERT LOW / 4 (0.0625)
# OVERQUALIFICATION IMPUTATION - 5-POINT LIKERT LOW / 2 (0.125)
# OVERQUALIFICATION IMPUTATION - 5-POINT LIKERT LOW (0.25)
# OVERQUALIFICATION IMPUTATION - 10-POINT LIKERT VERY LOW / 2 (0.05)
# OVERQUALIFICATION IMPUTATION - 10-POINT LIKERT VERY LOW (0.10)
# OVERQUALIFICATION IMPUTATION - 10-POINT LIKERT LOW (0.20)
dbl_threshold <- 0

fun_KNN.matching(
  .df_data.numeric = df_occupations
  , .vec_query.numeric = df_input
  , .int_k = nrow(df_occupations)
  , .imput.over_qualification = T
  , .dbl_over_qualification.threshold = dbl_threshold
  , .dbl_decimals = 4
) -> df_KNN.output.sub

# ------- VISUALIZATION -------------------------------------------------------------------------

# LONG DATA FRAMES --------------------------------------------------------
df_KNN.output %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) -> df_KNN.output.long

df_KNN.output.sub %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) -> df_KNN.output.sub.long

# TOP MATCHES (NO IMPUTATION) ---------------------------------------------------------
df_KNN.output.long %>% 
  arrange(desc(Value)) %>% 
  group_by(Similarity) %>% 
  slice(1:15) %>% 
  ungroup() %>% 
  mutate(
    Occupation = fct_reorder(
      .f = Occupation
      , .x = Value
      , .fun = max
      , .desc = T
    )
    , Similarity = fct_reorder(
      .f = Similarity
      , .x = Value
      , .fun = max
      , .desc = F
    )
  ) %>%
  ggplot(aes(
    x = Value
    , y = Similarity
    , fill = Career_Cluster
  )) +
  geom_col() + 
  facet_wrap(
    facets = vars(Occupation)
    , nrow = 3
  ) + 
  labs(
    x = 'Similarity (%)'
    , y = 'Similarity Metric'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - No Imputation')
    , fill = 'Career Cluster'
  ) +
  scale_x_continuous(limits = c(-1,1)) + 
  ggthemes::scale_fill_gdocs() + 
  ggthemes::theme_hc() -> plt_top15

# TOP MATCHES (WITH IMPUTATION) ---------------------------------------------------------
df_KNN.output.sub.long %>% 
  arrange(desc(Value)) %>% 
  group_by(Similarity) %>% 
  slice(1:15) %>% 
  ungroup() %>% 
  mutate(
    Occupation = fct_reorder(
      .f = Occupation
      , .x = Value
      , .fun = max
      , .desc = T
    )
    , Similarity = fct_reorder(
      .f = Similarity
      , .x = Value
      , .fun = max
      , .desc = F
    )
  ) %>%
  ggplot(aes(
    x = Value
    , y = Similarity
    , fill = Career_Cluster
  )) +
  geom_col() + 
  facet_wrap(
    facets = vars(Occupation)
    , nrow = 3
  ) + 
  labs(
    x = 'Similarity (%)'
    , y = 'Similarity Metric'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - With Overqualification Imputation (at {dbl_threshold})')
    , fill = 'Career Cluster'
  ) +
  scale_x_continuous(limits = c(-1,1)) + 
  ggthemes::scale_fill_gdocs() + 
  ggthemes::theme_hc() -> plt_top15.sub

# HISTOGRAMS (NO IMPUTATION) --------------------------------------------------------
df_KNN.output.long %>%
  mutate(
    Similarity2 = Similarity
    , Similarity2 = fct_reorder(
      Similarity2, Value
      , .fun = max, .desc = T
    )
    , Similarity = fct_reorder(
      Similarity, Value
      , .fun = max, .desc = T
    )
  ) -> tmp

tmp %>%
  ggplot(aes(
    x = Value
  )) + 
  geom_histogram(
    data = tmp %>% 
      select(-Similarity)
    , aes(group = Similarity2)
    , binwidth = .1
    , fill = 'grey'
    , alpha = 0.5
  ) +
  geom_histogram(
    aes(fill = Similarity)
    , binwidth = .1
    , fill = '#fb5607'
  ) +
  geom_vline(
    xintercept = 0
    , linetype = 'dashed'
  ) + 
  geom_vline(
    xintercept = 0.5
    , linetype = 'dashed'
  ) + 
  facet_wrap(
    facets = vars(Similarity)
    , nrow = 4) + 
  labs(
    x = 'Similarity (%)'
    , y = 'Count'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - No Imputation')
  ) +
  scale_x_continuous(limits = c(-1,1)) + 
  ggthemes::theme_hc() -> plt_hist

# HISTOGRAMS (WITH IMPUTATION) --------------------------------------------------------
df_KNN.output.sub.long %>%
  mutate(
    Similarity2 = Similarity
    , Similarity2 = fct_reorder(
      Similarity2, Value
      , .fun = max, .desc = T
    )
    , Similarity = fct_reorder(
      Similarity, Value
      , .fun = max, .desc = T
    )
  ) -> tmp

tmp %>%
  ggplot(aes(
    x = Value
  )) + 
  geom_histogram(
    data = tmp %>% 
      select(-Similarity)
    , aes(group = Similarity2)
    , binwidth = .1
    , fill = 'grey'
    , alpha = 0.5
  ) +
  geom_histogram(
    aes(fill = Similarity)
    , binwidth = .1
    , fill = viridis::plasma(1)
  ) +
  geom_vline(
    xintercept = 0
    , linetype = 'dashed'
  ) + 
  geom_vline(
    xintercept = 0.5
    , linetype = 'dashed'
  ) + 
  facet_wrap(
    facets = vars(Similarity)
    , nrow = 4) + 
  labs(
    x = 'Similarity (%)'
    , y = 'Count'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - With Overqualification Imputation (at {dbl_threshold})')
  ) +
  scale_x_continuous(limits = c(-1,1)) + 
  ggthemes::theme_hc() -> plt_hist.sub

# HEATMAPS (NO IMPUTATION) ------------------------------------------------
df_KNN.output.long %>% 
  mutate(
    Occupation = fct_reorder(
      .f = Occupation
      , .x = Value
      , .fun = max
      , .desc = F
    )
    , Similarity = fct_reorder(
      .f = Similarity
      , .x = Value
      , .fun = sum
      , .desc = F
    )
  ) %>% 
  ggplot(aes(
    x = Occupation
    , y = Similarity
    , fill = Value
  )) +
  geom_tile() + 
  ggthemes::scale_fill_gradient2_tableau(
    palette = 'Red-Blue-White Diverging'
    , limits = c(-1,1)
  ) + 
  labs(
    x = 'Similarity Ranking'
    , y = 'Similarity Metric'
    , fill = 'Similarity (%)'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - No Imputation')
  ) + 
  ggthemes::theme_hc() + 
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  ) -> plt_heatmap

# HEATMAPS (WITH IMPUTATION) ------------------------------------------------
df_KNN.output.sub.long %>% 
  mutate(
    Occupation = fct_reorder(
      .f = Occupation
      , .x = Value
      , .fun = max
      , .desc = F
    )
    , Similarity = fct_reorder(
      .f = Similarity
      , .x = Value
      , .fun = sum
      , .desc = F
    )
  ) %>% 
  ggplot(aes(
    x = Occupation
    , y = Similarity
    , fill = Value
  )) +
  geom_tile() + 
  ggthemes::scale_fill_gradient2_tableau(
    palette = 'Red-Blue-White Diverging'
    , limits = c(-1,1)
  ) + 
  labs(
    x = 'Similarity Ranking'
    , y = 'Similarity Metric'
    , fill = 'Similarity (%)'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - With Overqualification Imputation (at {dbl_threshold})')
  ) + 
  ggthemes::theme_hc() + 
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  ) -> plt_heatmap.sub

# HEATMAPS (SIDE BY SIDE) -----------------------------------------------
plt_heatmaps <- plt_heatmap + plt_heatmap.sub

# LINE CHARTS (NO IMPUTATION) --------------------------------------------------------
df_KNN.output.long %>%
  mutate(
    Similarity2 = Similarity
    , Similarity2 = fct_reorder(
      Similarity2, Value
      , .fun = max, .desc = T
    )
    , Similarity = fct_reorder(
      Similarity, Value
      , .fun = max, .desc = T
    )
  ) %>% 
  group_by(Similarity) %>% 
  mutate(
    Occupation = fct_reorder(
      .f = Occupation
      , .x = Value
      , .fun = max
      , .desc = F
    )
  ) %>% 
  ungroup() -> tmp

tmp %>%
  ggplot(aes(
    x = Occupation
    , y = Value
    , group = 1
  )) + 
  geom_line(
    data = tmp %>% 
      select(-Similarity)
    , aes(group = Similarity2)
    , color = 'grey'
    , alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0
    , color = '#001219'
  ) + 
  geom_line(
    aes(color = Similarity)
    , color = '#fb5607'
    , size = 1.22
  ) +
  facet_wrap(
    facets = vars(Similarity)
    , nrow = 4) + 
  labs(
    x = 'Similarity Ranking'
    , y = 'Similarity (%)'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - No Imputation')
  ) + 
  scale_y_continuous(limits = c(-1,1)) + 
  ggthemes::theme_hc() + 
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  ) -> plt_line

# LINE CHARTS (WITH IMPUTATION) --------------------------------------------------------
df_KNN.output.sub.long %>%
  mutate(
    Similarity2 = Similarity
    , Similarity2 = fct_reorder(
      Similarity2, Value
      , .fun = max, .desc = T
    )
    , Similarity = fct_reorder(
      Similarity, Value
      , .fun = max, .desc = T
    )
  ) %>% 
  group_by(Similarity) %>% 
  mutate(
    Occupation = fct_reorder(
      .f = Occupation
      , .x = Value
      , .fun = max
      , .desc = F
    )
  ) %>% 
  ungroup() -> tmp

tmp %>%
  ggplot(aes(
    x = Occupation
    , y = Value
    , group = 1
  )) + 
  geom_line(
    data = tmp %>% 
      select(-Similarity)
    , aes(group = Similarity2)
    , color = 'grey'
    , alpha = 0.5
  ) +
  geom_hline(
    yintercept = 0
    , color = '#001219'
  ) + 
  geom_line(
    aes(color = Similarity)
    , color = viridis::plasma(1)
    , size = 1.22
  ) +
  facet_wrap(
    facets = vars(Similarity)
    , nrow = 4) + 
  labs(
    x = 'Similarity Ranking'
    , y = 'Similarity (%)'
    , title = glue('Similarity Metrics Comparison ({chr_user}) - With Overqualification Imputation (at {dbl_threshold})')
  ) + 
  scale_y_continuous(limits = c(-1,1)) + 
  ggthemes::theme_hc() + 
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  ) -> plt_line.sub

# LINE CHARTS (SIDE BY SIDE) -----------------------------------------------
plt_lines <- plt_line + plt_line.sub

# # PLOT EVERYTHING --------------------------------------------------------------
# plt_top15
# plt_top15.sub
# plt_hist
# plt_hist.sub
# plt_heatmap
# plt_heatmap.sub
# plt_heatmaps
# plt_line
# plt_line.sub
# plt_lines

# SAVE PLOTS --------------------------------------------------------------
ggsave(
  plot = plt_top15
  , filename = glue('1.Similarities_Comparison_Bar1_{chr_user}2.png')
  , width = 16
  , height = 10
)

ggsave(
  plot = plt_top15.sub
  , filename = glue('2.Similarities_Comparison_Bar2_{chr_user}2.png')
  , width = 16
  , height = 10
)

ggsave(
  plot = plt_hist
  , filename = glue('3.Similarities_Comparison_Hist1_{chr_user}2.png')
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_hist.sub
  , filename = glue('4.Similarities_Comparison_Hist2_{chr_user}2.png')
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_heatmap
  , filename = glue('5.Similarities_Comparison_Heatmap1_{chr_user}2.png')
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_heatmap.sub
  , filename = glue('6.Similarities_Comparison_Heatmap2_{chr_user}2.png')
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_heatmaps
  , filename = glue('7.Similarities_Comparison_Heatmap3_{chr_user}2.png')
  , width = 17
  , height = 8
)

ggsave(
  plot = plt_line
  , filename = glue('8.Similarities_Comparison_Lines1_{chr_user}2.png')
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_line.sub
  , filename = glue('9.Similarities_Comparison_Lines2_{chr_user}2.png')
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_lines
  , filename = glue('10.Similarities_Comparison_Lines3_{chr_user}2.png')
  , width = 16
  , height = 8
)
