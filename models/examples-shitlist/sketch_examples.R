df_occupations %>% 
  ggplot(aes(
    # x = basic.mathematics.l
    x = mathematics.l
    # x = english_language.l
    , weight = employment2
  )) + 
  geom_density()

df_occupations %>% 
  filter(str_detect(
    str_to_lower(
      occupation
    ), 'account'
  )) %>% 
  pull(economics_and_accounting.l)

df_occupations %>% 
  select(
    employment2
    , ends_with('.l')
  ) %>% 
  names() %>% 
  sample(1)

# scheduling_work_and_activities
# resolving_conflicts_and_negotiating_with_others
# deal_with_unpleasant_or_angry_people
# deductive_reasoning
# biology

  pivot_longer(
    cols = -1
  ) %>% 
  filter(
    value != 0
  ) %>% 
  reframe(
    mean = 
      weighted.mean(
      value, employment2
    )
    , sd = 
      sqrt(wtd.var(
        value, employment2
      ))
  )
  ggplot(aes(
    x = value
    , weight = employment2
  )) + 
  geom_density()
