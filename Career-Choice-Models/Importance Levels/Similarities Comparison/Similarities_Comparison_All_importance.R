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
    'Judgment_and_Decision.I'
    , 'Active_Learning.I'
    , 'Critical_Thinking.I'
  )
  , 'Technical' = c(
    # Factor 2 is composed of mechanical, hands-on, specialist skills (technical)
    'Equipment_Selection.I'
    , 'Troubleshooting.I'
    , 'Operations_Monitoring.I'
  )
)

# ABILITIES FACTOR LIST ---------------------------------------------------
list_ablt.factors <- list(
  'Perception' = c(
    # Factor 1 is composed of perceptual abilities (perception):
    'Night_Vision.I'
    , 'Sound_Localization.I'
    , 'Glare_Sensitivity.I'
  )
  , 'Dexterity' = c(
    # Factor 2 is composed of manual abilities (dexterity):
    'Finger_Dexterity.I'
    , 'Arm_Hand_Steadiness.I'
    , 'Control_Precision.I'
  )
  , 'Robustness' = c(
    # Factor 3 is composed of bodily robustness, potency, and coordination (overall body robustness)
    'Stamina.I'
    , 'Gross_Body_Coordination.I'
    , 'Trunk_Strength.I'
  )
  , 'Intelligence' = c(
    # Factor 4 is composed of cognitive abilities (intelligence):
    'Inductive_Reasoning.I'
    , 'Problem_Sensitivity.I'
    , 'Deductive_Reasoning.I'
  )
)

# KNOWLEDGE FACTOR LIST ---------------------------------------------------
list_know.factors <- list(
  'Health' = c(
    # Factor 1 is composed of health-related fields of knowledge (health / help).
    'Therapy_and_Counseling.I'
    , 'Psychology.I'
    , 'Medicine_and_Dentistry.I'
  )
  , 'Building' = c(
    # Factor 2 is composed of engineering / building-related fields of knowledge (build).
    'Physics.I'
    , 'Engineering_and_Technology.I'
    , 'Mechanical.I'
  ) 
  , 'Business' = c(
    # Factor 3 is composed of financial and enterprising fields of knowledge (FGV).
    'Economics_and_Accounting.I'
    , 'Sales_and_Marketing.I'
    , 'Administration_and_Management.I'
  )
  , 'Arts_Humanities' = c(
    # Factor 4 is composed of arts and humanities (communists).
    'History_and_Archeology.I'
    , 'Geography.I'
    , 'Fine_Arts.I'
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
  rename_with(
    .cols = ends_with('.L')
    , .fn = function(x){str_replace(x,'.L$','.I')}
  ) -> df_input.all

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

df_input.all %>% 
  drop_na() -> df_input.all

# For this example, use Martijn' questionnaire 
chr_names <- df_input.all$Name
names(chr_names) <- df_input.all$Name

# ------- KNN MATCHING -------------------------------------------------------------------------
# NO IMPUTATION ------------------------------------
lapply(
  chr_names
  , function(name){
    
    df_input.all %>% 
      filter(Name == name) -> df_input
    
    # NO IMPUTATION --------------------------------------------
    fun_KNN.matching(
      .df_data.numeric = df_occupations
      , .vec_query.numeric = df_input
      , .int_k = nrow(df_occupations)
      , .imput.over_qualification = F
      , .dbl_decimals = 4
    ) %>% 
      return(.)
    
  }) -> list_KNN.output

# OVERQUALIFICATION IMPUTATION --------------------------------------------
# OVERQUALIFICATION IMPUTATION - 100% UNNECESSARY COMPETENCY (0)
# OVERQUALIFICATION IMPUTATION - 5-POINT LIKERT LOW / 4 (0.0625)
# OVERQUALIFICATION IMPUTATION - 5-POINT LIKERT LOW / 2 (0.125)
# OVERQUALIFICATION IMPUTATION - 5-POINT LIKERT LOW (0.25)
# OVERQUALIFICATION IMPUTATION - 10-POINT LIKERT VERY LOW / 2 (0.05)
# OVERQUALIFICATION IMPUTATION - 10-POINT LIKERT VERY LOW (0.10)
# OVERQUALIFICATION IMPUTATION - 10-POINT LIKERT LOW (0.20)
dbl_threshold <- 0
# dbl_threshold <- 0.10
# dbl_threshold <- 0.25

lapply(
  chr_names
  , function(name){
    
    df_input.all %>% 
      filter(Name == name) -> df_input
    
    fun_KNN.matching(
      .df_data.numeric = df_occupations
      , .vec_query.numeric = df_input
      , .int_k = nrow(df_occupations)
      , .imput.over_qualification = T
      , .dbl_over_qualification.threshold = dbl_threshold
      , .dbl_decimals = 4
    ) %>% 
      return(.)
    
  }) -> list_KNN.output.sub

# ------- VISUALIZATION -------------------------------------------------------------------------
# BIND DATA FRAMES --------------------------------------------------------
list_KNN.output %>% 
  bind_rows(.id = 'Name') %>% 
  select(
    Name
    , Occupation
    , starts_with('Similarity.')
    , Euclidean_Distance
  ) %>%
  arrange(Euclidean_Distance) -> df_KNN.output

list_KNN.output.sub %>% 
  bind_rows(.id = 'Name') %>% 
  select(
    Name
    , Occupation
    , starts_with('Similarity.')
    , Euclidean_Distance
  ) %>%
  arrange(Euclidean_Distance) -> df_KNN.output.sub

# LONG DATA FRAMES --------------------------------------------------------
df_KNN.output %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) %>% 
  group_by(Similarity) %>% 
  arrange(Value, Euclidean_Distance) %>% 
  mutate(
    ID = row_number()
    , ID = factor(ID)
  ) %>%
  ungroup() -> df_KNN.output.long

df_KNN.output.sub %>%
  pivot_longer(
    cols = starts_with('Similarity.')
    , names_to = 'Similarity'
    , values_to = 'Value'
  ) %>% 
  group_by(Similarity) %>% 
  arrange(Value, Euclidean_Distance) %>% 
  mutate(
    ID = row_number()
    , ID = factor(ID)
  ) %>%
  ungroup() -> df_KNN.output.sub.long

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
    , title = 'Similarity Metrics Comparison (All users) - No Imputation'
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
    , title = glue('Similarity Metrics Comparison (All users) - With Overqualification Imputation (at {dbl_threshold})')
  ) +
  scale_x_continuous(limits = c(-1,1)) + 
  ggthemes::theme_hc() -> plt_hist.sub

# HEATMAPS (NO IMPUTATION) ------------------------------------------------
df_KNN.output.long %>% 
  mutate(
    Similarity = fct_reorder(
      .f = Similarity
      , .x = Value
      , .fun = sum
      , .desc = F
    )
  ) %>%
  ggplot(aes(
    x = ID
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
    , title = 'Similarity Metrics Comparison (All users) - No Imputation'
  ) + 
  ggthemes::theme_hc() + 
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  ) -> plt_heatmap

# HEATMAPS (WITH IMPUTATION) ------------------------------------------------
df_KNN.output.sub.long %>% 
  mutate(
    Similarity = fct_reorder(
      .f = Similarity
      , .x = Value
      , .fun = sum
      , .desc = F
    )
  ) %>%
  ggplot(aes(
    x = ID
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
    , title = glue('Similarity Metrics Comparison (All users) - With Overqualification Imputation (at {dbl_threshold})')
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
  ) -> tmp

tmp %>%
  ggplot(aes(
    x = ID
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
    , title = 'Similarity Metrics Comparison (All users) - No Imputation'
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
  ) -> tmp

tmp %>%
  ggplot(aes(
    x = ID
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
    , title = glue('Similarity Metrics Comparison (All users) - With Overqualification Imputation (at {dbl_threshold})')
  ) + 
  scale_y_continuous(limits = c(-1,1)) + 
  ggthemes::theme_hc() + 
  theme(
    axis.text.x = element_blank()
    , axis.ticks.x = element_blank()
  ) -> plt_line.sub

# LINE CHARTS (SIDE BY SIDE) -----------------------------------------------
plt_lines <- plt_line + plt_line.sub

# PLOT EVERYTHING --------------------------------------------------------------
plt_hist
plt_hist.sub
plt_heatmap
plt_heatmap.sub
plt_heatmaps
plt_line
plt_line.sub
plt_lines

# SAVE PLOTS --------------------------------------------------------------
ggsave(
  plot = plt_hist
  , filename = '1.Similarities_Comparison_Hist1_All_importance.png'
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_hist.sub
  , filename = '2.Similarities_Comparison_Hist2_All_importance.png'
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_heatmap
  , filename = '3.Similarities_Comparison_Heatmap1_All_importance.png'
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_heatmap.sub
  , filename = '4.Similarities_Comparison_Heatmap2_All_importance.png'
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_heatmaps
  , filename = '5.Similarities_Comparison_Heatmap3_All_importance.png'
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_line
  , filename = '6.Similarities_Comparison_Lines1_All_importance.png'
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_line.sub
  , filename = '7.Similarities_Comparison_Lines2_All_importance.png'
  , width = 16
  , height = 8
)

ggsave(
  plot = plt_lines
  , filename = '8.Similarities_Comparison_Lines3_All_importance.png'
  , width = 16
  , height = 8
)
