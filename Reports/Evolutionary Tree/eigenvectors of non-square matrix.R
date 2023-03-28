library(tidyverse)
library(scales)
# df_occupations.efa.comp %>%
df_occupations %>%
  select(
    ends_with('.l')
  ) %>% 
  mutate(across(
    .cols =  ends_with('.l')
    ,.fns = ~ .x * 100
  )) %>% 
  t() %>%
  svd() -> list_svd

# Singular values of original (rectangular) matrix (one for each variable)
list_svd$d %>% view

tibble(
  singular.values = list_svd$d
  , n = 1:length(list_svd$d)
  ) %>%
  ggplot(aes(
    x = n
    , y = singular.values
  )) +
  geom_area(
    fill = 'purple'
    , alpha = 0.5
  )

# Left singular vectors of original (rectangular) matrix (one for each variable)
list_svd$u %>% view

# Right singular vectors of original (rectangular) matrix (one for each row)
list_svd$v %>% view

# df_occupations.efa.comp %>% 
df_occupations %>%
  select(
    ends_with('.l')
  ) %>% 
  mutate(across(
    .cols =  ends_with('.l')
    ,.fns = ~ .x * 100
  )) %>%  
  # as.matrix() -> lalala
  t() -> lalala

colnames(lalala) <- df_occupations$occupation
# rownames(lalala) <- df_occupations$occupation

lalala %>%
  prcomp(scale = T) -> list_pca

summary(list_pca)
list_pca$sdev
list_pca$rotation %>% view
list_pca$center
list_pca$x
# list_pca$scale

tibble(
  var.pct = list_pca$sdev^2 / sum(list_pca$sdev^2)
  , var.sum = cumsum(var.pct)
  , 
) %>% 
  mutate(
    n = row_number()
  ) -> df_pca

df_pca %>%
  ggplot(aes(
    x = n
    , y = var.pct
  )) +
  geom_area(
    fill = 'purple'
    , alpha = 0.5
  )

plot(list_pca)
  
df_pca %>%
  ggplot(aes(
    x = n
    , y = var.sum
  )) +
  geom_area(
    fill = 'purple'
    , alpha = 0.5
  )

df_pca %>% 
  mutate(
    var.pct = percent(var.pct, accuracy = .01)
    ) %>% view

list_pca$x %>%
  as_tibble() %>% 
  select(1:2) %>% 
  ggplot(aes(
    x = PC1
    , y = PC2
  )) +
  geom_point() + 
  labs(
    x = paste('PC1', '-', percent(df_pca[1,]$var.pct, accuracy = .01))
    , y = paste('PC2', '-', percent(df_pca[2,]$var.pct, accuracy = .01))
  )

  