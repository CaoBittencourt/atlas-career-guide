df_occupations %>% 
  select(
    ends_with('.l')
  ) -> df_items

df_occupations$
  employment2 ->
  dbl_weights

df_items %>% 
  as.matrix() *
  sqrt(dbl_weights) ->
  df_items_wgt

set_names(
  1:ncol(df_items)
  , names(df_items)
)  %>% 
  map(
    ~ coef(bvls(
      df_items_wgt[,-.x]
      , df_items_wgt[,.x]
      , bl = rep(0, ncol(df_items) - 1)
      , bu = rep(100, ncol(df_items) - 1)
    )) %>% 
      set_names(colnames(
        df_items_wgt[,-.x]
      ))
  ) -> list_kflex_micro

list_kflex_micro %>% 
  bind_rows(.id = 'item') %>% 
  relocate(
    item
    , names(df_items)
  ) -> df_kflex_micro

rowMeans(
  df_kflex_micro[-1]
  , na.rm = T
) %>% 
  set_names(
    df_kflex_micro[[1]]
  ) %>% 
  as_tibble(
    rownames = 'item'
  ) %>% 
  rename(
    'item.kflex_micro' = 2
  ) %>% 
  arrange(desc(
    item.kflex_micro
  )) %>% 
  print(n = nrow(.))

df_kflex_micro %>% 
  mutate(across(
    .cols = where(is.numeric)
    ,.fns = ~ round(.x,4)
  )) %>% 
  view

# df_kflex_micro[-1] %>% 
#   pivot_longer(
#     cols = everything()
#     , names_to = 'item'
#     , values_to = 'item.kflex_micro'
#   ) %>% 
#   filter(item == first(item)) %>% 
#   select(2) == 
#   df_kflex_micro[1,-1] %>% 
#   pivot_longer(cols=everything()) %>% 
#   select(2)

df_kflex_micro[
  is.na(df_kflex_micro)
] <- 1

view(df_kflex_micro)

list_kflex_micro$
  active_listening.l %>% 
  view

map_df(
  list_kflex_micro
  , ~ sum(.x > 0)
) %>% 
  pivot_longer(
    cols = everything()
    , names_to = 'item'
    , values_to = 'item_contributions'
  ) %>% 
  arrange(desc(
    item_contributions
  ))


list_kflex_micro$
  critical_thinking.l %>% 
  view

list_kflex_micro$
  active_listening.l %>% 
  round(4) %>% 
  set_names(
    names(df_items)[-1]
  ) %>% view

map2(
  .x = 
    set_names(
      1:ncol(df_items)
      , names(df_items)
    ) 
  , .y = 
    as.matrix(df_items) * 
    sqrt(dbl_weights)
  , ~ coef(bvls(
    as.matrix(df_items) * 
      sqrt(dbl_weights)
    , df_items[[.x]] * 
      sqrt(dbl_weights)
    , bl = rep(0, ncol(df_items))
    , bu = rep(100, ncol(df_items))
  ))
)

do.call(
  rbind
  , map(
    1:length(list_kflex_micro)
    , ~ 
      list_kflex_micro[[.x]] %>% 
      as_tibble()
    # select(2) %>% 
    add_row(
      item.composition = -1
      , .before = .x
    ) %>% 
      flatten_dbl() * (-1)
  )
) -> mtx_eq.system

do.call(
  rbind
  , map(
    1:length(lalala$solutions.tidy)
    , ~ 
      lalala$solutions.tidy[[.x]] %>% 
      select(2) %>% 
      add_row(
        item.composition = -1
        , .before = .x
      ) %>% 
      flatten_dbl() * (-1)
  )
) -> mtx_eq.system


map(
  names(df_items)
)
list_kflex_micro

bvls::bvls(
  A = 
    as.matrix(df_items[-1]) * 
    sqrt(dbl_weights)
  , b = df_items[[1]] * 
    sqrt(dbl_weights)
  , bl = rep(0, ncol(df_items) - 1)
  , bu = rep(100, ncol(df_items) - 1)
)

lm.wfit(
  x = as.matrix(df_items[-1])
  , y = df_items[[1]]
  , w = dbl_weights
) -> dsds

bvls(
  as.matrix(df_items[-1])
  , df_items[[1]]
  , bl = rep(0,199)
  , bu = rep(1,199)
) -> dsds

dsds[[1]]
coef(dsds)

view(round(coef(dsds), 4))



















