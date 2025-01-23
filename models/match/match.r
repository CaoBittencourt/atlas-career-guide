# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  s = mod / compare / match,
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
      "euclidean",
      "cobb-douglas",
      "gmme",
      "pearson",
      "bvls",
      "logit",
      "probit"
    )
  ) ->
similarity_cao

similarity_cao |>
  lapply(arrange, desc(cao)) |>
  lapply(head, 10)

similarity_cao |>
  lapply(arrange, desc(cao)) |>
  lapply(tail, 10)

# endregion
# region: cbmap mode (occupations)
# note: is from/to inverted?
df_occupations |>
  s$similarity(
    df_occupations,
    mode = "cbmap",
    bind = F,
    match_method = c(
      "euclidean"
      # ,
      # "cobb-douglas",
      # "gmme",
      # "pearson",
      # "bvls",
      # "logit",
      # "probit"
    )
  ) ->
similarity

# endregion
