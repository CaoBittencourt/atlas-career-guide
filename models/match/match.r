# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  s = mod / compare / match,
  S = mod / compare / similarity,
  mod / utils / conform[...],
  vec = mod / compare / match / methods / vec,
  assert = mod / utils / assert
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
# region: cbmap mode (my matches)
# note: is from/to inverted?
df_cao |>
  s$similarity(
    df_occupations_cao,
    mode = "cbmap",
    bind = F,
    match_method = c(
      # "euclidean",
      "cobb-douglas",
      "gmme"
      # ,
      # "pearson",
      # "bvls",
      # "logit",
      # "probit"
    )
  ) ->
similarity_cao.cbmap

similarity_cao.cbmap |>
  lapply(arrange, desc(cao)) |>
  lapply(head, 10)

similarity_cao.cbmap |>
  lapply(arrange, desc(cao)) |>
  lapply(tail, 10)

# endregion
# region: cbmap mode (occupations)
# note: is from/to inverted?
df_occupations[1:19] |>
  s$similarity(
    df_occupations[1:19],
    mode = "cbmap",
    bind = F,
    match_method = c(
      # "euclidean"
      # ,
      # "cobb-douglas",
      # "gmme",
      # "pearson",
      # "bvls",
      "logit"
      # ,
      # "probit"
    )
  ) ->
similarity.cbmap
similarity.cbmap

# endregion
# region: new dispatch (my matches)
# note: is from/to inverted?
df_cao |>
  S$similarity(
    df_occupations_cao,
    S$similarity.methods$probit,
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
  S$similarity(
    df_occupations[1:19],
    S$similarity.methods$euclidean,
    bind = T
  ) ->
similarity
similarity

# endregion
