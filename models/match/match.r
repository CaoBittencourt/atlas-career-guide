# setup
# region: modules
modular::project.options("atlas")

# endregion
# region: imports
box::use(
  s = mod / compare / match,
  mod / utils / conform[...],
  vec = mod / compare / match / methods / vec
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
# region: vector mode
# note: is from/to inverted?
box::use(eq = mod / describe / aeq)

df_cao |>
  # mutate(
  #   cao = eq$aeq(cao)
  # ) |>
  s$similarity(
    df_occupations_cao,
    # df_occupations_cao[-1] |>
    #   lapply(eq$aeq) |>
    #   as_tibble(),
    mode = "vector",
    bind = T,
    match_method = c(
      "euclidean",
      "cobb-douglas",
      "pearson"
    )
  ) ->
vec_similarity

# endregion
# # region: egmap mode
# df_occupations[1:3] |>
#   s$similarity(
#     df_occupations[1:3],
#     mode = "egmap",
#     match_method = c(
#       "euclidean",
#       "bvls",
#       "logit",
#       "probit",
#       "cobb-douglas",
#       "gmme",
#       "pearson"
#     )
#   ) ->
# eg_similarity

# eg_similarity |> print(n = nrow(eg_similarity))

# # endregion
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
