library(dplyr)
library(tidyr)
library(readr)
library(weights)

df_occupations <- read_csv('/home/Cao/Storage/github/atlas-research/data/occupations/df_occupations_2022.csv')

df_occupations %>%
  select(
    starts_with('skl'),
    starts_with('abl'),
    starts_with('knw')
  ) %>%
  t() ->
  dsds

as.data.frame(t(dsds)) %>%
  sapply(weighted.mean, w = df_occupations$employment_variants) -> weights

# df_occupations %>%
#   select(
#     employment_variants
#     ,starts_with('skl')
#     ,starts_with('abl')
#     ,starts_with('knw')
#   ) %>%
#   t() ->
#   dsds
#
# wtd.cors(
#   x = dsds[-1,]
#   , weight =
#     dsds[1,]
# )

dim(dsds)

replicate(
  n = 120
  , df_occupations$
    employment_variants
) -> lalala


hclust(
  d = dist(dsds),
  members = weights
) -> dsdsds


dsdsds %>%
  cutree(k = 5)
