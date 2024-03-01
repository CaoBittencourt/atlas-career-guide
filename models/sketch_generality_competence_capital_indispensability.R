library(atlas.class)
library(openxlsx)

# Generality
df_occupations %>%
  select(
    occupation,
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
  ) %>% 
  pivot_longer(
    cols = -1,
    names_to = 'item',
    values_to = 'item_score'
  ) %>%
  group_by(
    occupation
  ) %>%
  reframe(
    generality =
      fun_gene_generality(
        item_score,
        dbl_scale_ub = 100,
        dbl_scale_lb = 0
      )
  ) %>%
  arrange(desc(
    generality
  )) %>% 
  mutate(
    class = 
      fun_class_classifier(
        generality
        , int_levels = 4
        , chr_class_labels = c(
          'Very Specialist',
          'Specialist',
          'Generalist',
          'Very Generalist'
        )
      )
  ) -> df_generality

# Human Capital Indispensability
df_occupations %>%
  select(
    occupation,
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
  ) %>% 
  pivot_longer(
    cols = -1,
    names_to = 'item',
    values_to = 'item_score'
  ) %>%
  group_by(
    occupation
  ) %>%
  mutate(
    item_indispensability =
      fun_kind_indispensability(
        item_score,
        dbl_scale_lb = 0
      )
    , item_indispensability = 
      round(
        item_indispensability
        , 4
      )
  ) %>% 
  group_by(
    occupation
  ) %>% 
  arrange(desc(
    item_score
  ), .by_group = T
  ) %>% 
  mutate(
    item_class = 
      fun_class_classifier(
        item_indispensability
        , int_levels = 4
        , chr_class_labels = c(
          'Irrelevant',
          'Auxiliary',
          'Important',
          'Crucial'
        )
      )
  ) -> df_kind

df_occupations %>%
  select(
    occupation,
    starts_with('skl_'),
    starts_with('abl_'),
    starts_with('knw_')
  ) %>% 
  pivot_longer(
    cols = -1,
    names_to = 'item',
    values_to = 'item_score'
  ) %>%
  group_by(
    occupation
  ) %>%
  reframe(
    competence =
      fun_comp_competence(
        item_score,
        dbl_scale_ub = 100,
        dbl_scale_lb = 0
      )
  ) %>%
  arrange(desc(
    competence
  )) %>% 
  mutate(
    class = 
      fun_class_classifier(
        competence
        , int_levels = 5
        , chr_class_labels = c(
          'Very Low Level',
          'Low Level',
          'Mid Level',
          'High Level',
          'Very High Level'
        )
      )
  ) -> df_competence

openxlsx::write.xlsx(
  x = df_kind
  , file = 'human_capital_indispensability.xlsx'
)

openxlsx::write.xlsx(
  x = df_generality
  , file = 'generality.xlsx'
)

openxlsx::write.xlsx(
  x = df_competence
  , file = 'competence.xlsx'
)