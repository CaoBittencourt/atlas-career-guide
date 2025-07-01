# setup
# region: modules
options(box.path = Sys.getenv("ATLAS_MOD"))

# endregion
# region: imports
box::use(
  s = compare / similarity,
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
getOption("atlas.cao") |> readRDS() -> df_cao

df_occupations |>
  inner_join(df_cao) |>
  select(-cao) ->
df_occupations_cao

# endregion
# model
# region: new dispatch (my matches)
# note: is from/to inverted?
df_cao |>
  s$similarity(
    df_occupations_cao,
    s$similarity.methods$euclidean,
    bind = T
  ) ->
similarity_cao

similarity_cao |>
  arrange(desc(cao)) |>
  head(10)

similarity_cao |>
  arrange(desc(cao)) |>
  tail(10)

# endregion
# region: new dispatch (occupations)
# note: is from/to inverted?
df_occupations[1:19] |>
  s$similarity(
    df_occupations[1:19],
    s$similarity.methods$euclidean,
    bind = T
  ) ->
similarity
similarity

# endregion
