# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  s = mod / compare / match
)

library(atlas.plot)

# endregion
# region: data
# skill set matrix
getOption("atlas.skills_mtx") |> readRDS() -> df_occupations
df_occupations[-1] -> df_occupations

# endregion
# model
# region: egmap mode
df_occupations[1:3] |>
  s$similarity(
    df_occupations[1:3],
    mode = "egmap",
    match_method = c(
      "euclidean",
      "bvls",
      "logit",
      "probit",
      "cobb-douglas",
      "gmme",
      "pearson"
    )
  ) ->
eg_similarity

eg_similarity |> print(n = nrow(eg_similarity))

# endregion
# region: vector mode
df_occupations[1:3] |>
  s$similarity(
    df_occupations[1:3],
    mode = "vector",
    match_method = c(
      # "euclidean",
      # "bvls",
      # "logit",
      # "probit",
      # "cobb-douglas",
      # "gmme",
      "pearson"
    )
  ) ->
vec_similarity

vec_similarity |> print(n = nrow(vec_similarity))

# endregion
# region: vmap mode
df_occupations[1:3] |>
  s$similarity(
    df_occupations[1:3],
    mode = "vmap",
    match_method = c(
      "euclidean",
      "bvls",
      "logit",
      "probit",
      "cobb-douglas",
      "gmme",
      "pearson"
    )
  ) ->
vm_similarity

vm_similarity |> print(n = nrow(vm_similarity))

# endregion
